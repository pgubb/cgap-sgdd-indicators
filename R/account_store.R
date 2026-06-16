# R/account_store.R --- Persistence interface for optional user accounts
# ============================================================================
# Provider-agnostic interface for loading/saving a user's indicator sets so the
# rest of the app never depends on the chosen backend. Backend = Firebase
# Realtime Database (see USER_ACCOUNTS_EXPLORATION.md). Authentication
# (register / sign-in / current user / ID token) is handled in app.R via
# firebase::FirebaseEmailPassword; this file is persistence only.
#
# Contract
# --------
#   as_load_sets(uid, id_token) -> named list of character vectors, e.g.
#       list("My Indicators" = c("11","18"), "Credit" = c("42"))
#       Empty list() if the user has nothing stored (or on read failure).
#   as_save_sets(uid, sets, id_token) -> invisible(TRUE) on success; signals an
#       error on failure (callers tryCatch and degrade to session-only state).
#
# id_token is the Firebase ID token (JWT), from
# user$response$stsTokenManager$accessToken. RTDB REST accepts it as
# ?auth=<token>; the database rules then enforce auth.uid === uid.
#
# Storage shape (an ARRAY of {name, ids} objects, not a name-keyed map):
#   /user_sets/<uid> = {
#     "sets": [ {"name":"My Indicators","ids":["11","18"]}, {"name":"Credit"} ],
#     "updated": <ms>
#   }
# Why an array rather than { "<name>": [ids] }:
#   - set names may contain '.', '/', '#', '$', '[', ']' which are illegal RTDB
#     keys; as values they are safe.
#   - RTDB drops empty arrays, so an empty name-keyed entry would vanish; here an
#     empty set still persists its {name} object (ids just absent on read).
# ============================================================================

# Backend selector. "guest" = no-op (accounts disabled / signed-out users).
.ACCOUNT_BACKEND <- Sys.getenv("LENS_ACCOUNT_BACKEND", "firebase")  # "firebase" | "guest"

# --- Public API --------------------------------------------------------------

#' Load a user's saved sets.
#' @return named list of character vectors; empty list() if none / on failure.
as_load_sets <- function(uid, id_token = NULL) {
  if (is.null(uid) || !nzchar(uid)) return(list())
  switch(.ACCOUNT_BACKEND,
    firebase = tryCatch(
      .as_load_sets_firebase(uid, id_token),
      error = function(e) { warning("[account_store] load failed: ", conditionMessage(e)); list() }
    ),
    list()
  )
}

#' Persist a user's saved sets. Errors propagate so callers can notify + degrade.
#' @param sets named list of character vectors.
as_save_sets <- function(uid, sets, id_token = NULL) {
  if (is.null(uid) || !nzchar(uid)) return(invisible(FALSE))
  switch(.ACCOUNT_BACKEND,
    firebase = .as_save_sets_firebase(uid, sets, id_token),
    invisible(FALSE)
  )
}

# --- Firebase Realtime Database backend (REST) -------------------------------

.rtdb_url <- function(uid) {
  db <- Sys.getenv("FIREBASE_DATABASE_URL")
  if (!nzchar(db)) stop("FIREBASE_DATABASE_URL is not set", call. = FALSE)
  sprintf("%s/user_sets/%s.json", sub("/+$", "", db), utils::URLencode(uid, reserved = TRUE))
}

.as_load_sets_firebase <- function(uid, id_token) {
  if (is.null(id_token) || !nzchar(id_token)) stop("missing id_token", call. = FALSE)
  resp <- httr2::request(.rtdb_url(uid)) |>
    httr2::req_url_query(auth = id_token) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  if (httr2::resp_status(resp) >= 400) {
    stop("RTDB load HTTP ", httr2::resp_status(resp), ": ", httr2::resp_body_string(resp), call. = FALSE)
  }
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  raw <- if (is.null(body)) NULL else body$sets
  if (is.null(raw) || length(raw) == 0) return(list())

  # New format: list of {name, ids} objects (array, or "0"/"1"-keyed object).
  if (is.list(raw[[1]]) && !is.null(raw[[1]][["name"]])) {
    nm  <- vapply(raw, function(s) {
      n <- s[["name"]]; if (is.null(n)) NA_character_ else as.character(n)
    }, character(1))
    ids <- lapply(raw, function(s) as.character(unlist(s[["ids"]])))
    keep <- !is.na(nm)
    return(setNames(ids[keep], nm[keep]))
  }
  # Legacy format: name-keyed map { "<name>": [ids] }.
  lapply(raw, function(ids) as.character(unlist(ids)))
}

.as_save_sets_firebase <- function(uid, sets, id_token) {
  if (is.null(id_token) || !nzchar(id_token)) stop("missing id_token", call. = FALSE)
  # unname() -> JSON array. as.list() forces id arrays to survive (no auto_unbox
  # scalarization of length-1). Empty ids -> [] (dropped by RTDB) -> the {name}
  # object still persists, so empty sets keep their name.
  payload <- list(
    sets = unname(lapply(names(sets), function(nm) {
      list(name = nm, ids = as.list(as.character(sets[[nm]])))
    })),
    updated = round(as.numeric(Sys.time()) * 1000)
  )
  resp <- httr2::request(.rtdb_url(uid)) |>
    httr2::req_url_query(auth = id_token) |>
    httr2::req_method("PUT") |>
    httr2::req_body_json(payload, auto_unbox = TRUE) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  if (httr2::resp_status(resp) >= 400) {
    stop("RTDB save HTTP ", httr2::resp_status(resp), ": ", httr2::resp_body_string(resp), call. = FALSE)
  }
  invisible(TRUE)
}
