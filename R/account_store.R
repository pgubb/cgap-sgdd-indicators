# R/account_store.R --- Persistence interface for optional user accounts
# ============================================================================
# EXPLORATION SCAFFOLD (branch: explore/user-accounts) — NOT wired into app.R.
#
# Provider-agnostic interface for loading/saving a user's indicator sets so the
# rest of the app never depends on the chosen backend. The decided backend is
# Firebase Realtime Database (see USER_ACCOUNTS_EXPLORATION.md §9); swapping
# providers later should only touch the backend functions in this file.
#
# Contract
# --------
#   as_load_sets(uid) -> named list of character vectors, e.g.
#                        list("My Indicators" = c("11","18"), "Credit" = c("42"))
#                        Returns an empty list() if the user has nothing stored.
#   as_save_sets(uid, sets) -> invisible(TRUE) on success; signals an error on
#                        failure (callers should tryCatch and degrade to
#                        session-only behaviour rather than crash).
#
# Auth (register / sign-in / sign-out / current user) is handled separately in
# app.R via firebase::FirebaseEmailPassword — this file is persistence only.
# ============================================================================

# Sets are stored as a small JSON blob keyed by uid. Keeping ids as character
# avoids integer/JSON round-trip surprises; callers convert as needed.

# --- Backend selector --------------------------------------------------------
# Default "guest" backend is a no-op so the app works with accounts disabled.
.ACCOUNT_BACKEND <- Sys.getenv("LENS_ACCOUNT_BACKEND", "guest")  # "guest" | "firebase"

#' Load a user's saved sets.
#' @param uid Firebase user id (string).
#' @return named list of character vectors; empty list() if none.
as_load_sets <- function(uid) {
  if (is.null(uid) || !nzchar(uid)) return(list())
  switch(.ACCOUNT_BACKEND,
    firebase = .as_load_sets_firebase(uid),
    list()  # guest: nothing persisted
  )
}

#' Persist a user's saved sets.
#' @param uid  Firebase user id (string).
#' @param sets named list of character vectors.
#' @return invisible(TRUE) on success.
as_save_sets <- function(uid, sets) {
  if (is.null(uid) || !nzchar(uid)) return(invisible(FALSE))
  switch(.ACCOUNT_BACKEND,
    firebase = .as_save_sets_firebase(uid, sets),
    invisible(FALSE)  # guest: no-op
  )
}

# --- Firebase Realtime Database backend (TODO: Phase 1) ----------------------
# Implementation deferred until a Firebase project + config exist (see doc §9).
# Planned approach: REST against the RTDB at
#   {FIREBASE_DATABASE_URL}/user_sets/{uid}.json
# authorised with the signed-in user's ID token (so RTDB rules enforce
# auth.uid === uid). To be built and proven in an isolated firebase_demo.R
# before wiring into setManager.

.as_load_sets_firebase <- function(uid) {
  stop("Firebase backend not implemented yet (Phase 1). See USER_ACCOUNTS_EXPLORATION.md.")
}

.as_save_sets_firebase <- function(uid, sets) {
  stop("Firebase backend not implemented yet (Phase 1). See USER_ACCOUNTS_EXPLORATION.md.")
}
