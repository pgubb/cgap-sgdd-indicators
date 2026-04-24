# ============================================================================
# LENS Catalog — Google Sheets Data Connector
# ============================================================================
# Drop-in replacement for: load("data/indicators.RData")
#
# Usage in app.R:
#   # BEFORE:
#   load("data/indicators.RData")
#
#   # AFTER:
#   source("R/globals.R")          # MND_OBJ_2 must be loaded first
#   source("R/data_connector.R")
#   indicators <- load_indicators()
#
# Environment variables required:
#   LENS_SHEET_ID                - Google Sheet ID
#   LENS_SHEET_NAME              - Tab name (default: "indicators")
#   GOOGLE_SERVICE_ACCOUNT_KEY   - Path to JSON key file, or raw JSON string
#
# Caching:
#   Data is cached in-memory with a configurable TTL (default: 5 min).
#   All sessions within the same R process share the cache.
#   Call load_indicators(force_refresh = TRUE) to bypass the cache.
# ============================================================================

library(googlesheets4)
library(googledrive)
library(dplyr)

# --- Configuration ---
.lens_config <- list(
  sheet_id   = Sys.getenv("LENS_SHEET_ID", ""),
  sheet_name = Sys.getenv("LENS_SHEET_NAME", "indicators"),
  cache_ttl  = as.numeric(Sys.getenv("LENS_CACHE_TTL_SECONDS", "300"))  # 5 min
)

# --- In-memory cache ---
.lens_cache <- new.env(parent = emptyenv())
.lens_cache$data       <- NULL
.lens_cache$timestamp  <- NULL

# --- Authentication ---
#' Authenticate with Google via service account.
#' Called once at startup; token is reused automatically.
.lens_authenticate <- function() {
  key <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_KEY", "")
  
  if (!nzchar(key)) {
    message("[LENS] No GOOGLE_SERVICE_ACCOUNT_KEY found. ",
            "Falling back to interactive auth.")
    googlesheets4::gs4_auth()
    return(invisible())
  }
  
  # File path or raw JSON?
  if (file.exists(key)) {
    path <- key
  } else {
    path <- tempfile(fileext = ".json")
    writeLines(key, path)
    on.exit(unlink(path), add = TRUE)
  }
  
  googledrive::drive_auth(path = path)
  googlesheets4::gs4_auth(token = googledrive::drive_token())
  message("[LENS] Authenticated with service account.")
}

# --- Helpers ---

# Reverse lookup: objective name → mandate name (built once from MND_OBJ_2)
# Uses tolower keys for case-insensitive matching.
.obj_to_mandate <- {
  lkp <- character(0)
  for (mnd in names(MND_OBJ_2)) {
    for (obj in MND_OBJ_2[[mnd]]) {
      lkp[[tolower(obj)]] <- mnd
    }
  }
  lkp
}

#' Build "Mandate1 (Obj1, Obj2), Mandate2 (Obj3)" from delimited strings.
#' Uses the MND_OBJ_2 lookup to resolve which objectives belong to which
#' mandate — no assumptions about objective delimiters between groups.
#' Mandates are ";"-separated; objectives are always ","-separated.
.build_secondary_label <- function(mandates_str, objectives_str) {
  if (is.na(mandates_str) || !nzchar(mandates_str)) return(NA_character_)
  
  mandates <- trimws(strsplit(mandates_str, ";")[[1]])
  mandates_lc <- tolower(mandates)
  
  # Split ALL objectives by comma (flat list)
  objectives <- if (!is.na(objectives_str) && nzchar(objectives_str)) {
    trimws(strsplit(objectives_str, ",")[[1]])
  } else {
    character(0)
  }
  
  # Bucket each objective under its matching secondary mandate via lookup
  obj_by_mandate <- setNames(
    vector("list", length(mandates)),
    mandates_lc
  )
  
  for (obj in objectives) {
    parent <- .obj_to_mandate[tolower(obj)]
    if (!is.na(parent) && tolower(parent) %in% mandates_lc) {
      key <- tolower(parent)
      obj_by_mandate[[key]] <- c(obj_by_mandate[[key]], obj)
    }
  }
  
  # Assemble labels preserving original mandate casing and objective order
  labels <- vapply(seq_along(mandates), function(i) {
    objs <- obj_by_mandate[[mandates_lc[i]]]
    if (length(objs) > 0) {
      paste0(mandates[i], " (", paste(objs, collapse = ", "), ")")
    } else {
      mandates[i]
    }
  }, character(1))
  
  paste(labels, collapse = ", ")
}

# --- Core reader ---
#' Read raw data from Google Sheets and produce the indicators tibble.
#' Handles list columns, type coercion, empty strings, and computed columns.
.lens_read_sheet <- function() {
  if (!nzchar(.lens_config$sheet_id)) {
    stop("[LENS] LENS_SHEET_ID environment variable is not set.",
         call. = FALSE)
  }
  
  message("[LENS] Reading indicators from Google Sheets...")
  t0 <- Sys.time()
  
  raw <- googlesheets4::read_sheet(
    .lens_config$sheet_id,
    sheet = .lens_config$sheet_name
  )
  
  # 1. Flatten list columns (mixed types from Sheets API)
  raw <- raw %>%
    mutate(across(everything(), ~ {
      if (is.list(.)) {
        sapply(., function(x) {
          if (is.null(x) || length(x) == 0) NA_character_
          else as.character(x[[1]])
        })
      } else .
    }))
  
  # 1b. Normalize column names (Google Sheet may use different casing)
  if ("preset_MSME" %in% names(raw)) {
    raw <- raw %>% rename(preset_msme = preset_MSME)
  }

  # 2. Empty strings → NA
  raw <- raw %>%
    mutate(across(where(is.character), ~ ifelse(. == "", NA_character_, .)))
  
  # 3. Coerce numeric columns (safe: character → integer)
  int_cols <- c("indicator_id", "indicator_order", "main_mandate_order",
                "preset_digital", "preset_msme", "preset_finhealth", "sources_any", "FEMAMETER")
  for (col in intersect(int_cols, names(raw))) {
    raw[[col]] <- suppressWarnings(as.integer(as.character(raw[[col]])))
  }
  
  # 4. Recompute derived columns (always fresh from source data)
  raw <- raw %>%
    mutate(
      # Main mandate + objective label
      main_mandate_objective = ifelse(
        !is.na(main_mandate) & !is.na(main_objectives),
        paste0(main_mandate, " (", main_objectives, ")"),
        NA_character_
      ),
      # Secondary mandate + objective label (positional pairing)
      secondary_mandate_objective = mapply(
        .build_secondary_label,
        secondary_mandates, secondary_objectives,
        USE.NAMES = FALSE
      ),
      # Has any international initiative source?
      sources_any = {
        has_source <- (!is.na(GPFI) | !is.na(IMF) | !is.na(AFI) | !is.na(WEF))
        # Include FEMAMETER if column exists
        if ("FEMAMETER" %in% names(raw)) {
          has_source <- has_source | !is.na(FEMAMETER)
        }
        ifelse(has_source, 1L, NA_integer_)
      }
    )
  
  elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
  message(sprintf("[LENS] Loaded %d indicators in %s seconds.",
                  nrow(raw), elapsed))
  
  raw
}

# --- Public API ---

#' Load indicators, using cache when available.
#'
#' @param force_refresh  Logical. If TRUE, bypass the cache.
#' @return A tibble matching the structure of the old indicators.RData.
load_indicators <- function(force_refresh = FALSE) {
  now <- Sys.time()
  
  # Return cache if still valid
  if (!force_refresh &&
      !is.null(.lens_cache$data) &&
      !is.null(.lens_cache$timestamp) &&
      difftime(now, .lens_cache$timestamp, units = "secs") < .lens_config$cache_ttl) {
    message("[LENS] Serving cached data (",
            round(difftime(now, .lens_cache$timestamp, units = "secs")),
            "s old).")
    return(.lens_cache$data)
  }
  
  # Authenticate if needed (only once per process)
  if (!googlesheets4::gs4_has_token()) {
    .lens_authenticate()
  }
  
  # Read fresh data
  indicators <- tryCatch(
    .lens_read_sheet(),
    error = function(e) {
      # If we have stale cache, use it rather than crashing
      if (!is.null(.lens_cache$data)) {
        warning("[LENS] Failed to refresh data: ", e$message,
                "\n  Serving stale cache from ",
                format(.lens_cache$timestamp, "%H:%M:%S"), ".",
                call. = FALSE)
        return(.lens_cache$data)
      }
      stop("[LENS] Failed to load data and no cache available: ",
           e$message, call. = FALSE)
    }
  )
  
  # Update cache
  .lens_cache$data      <- indicators
  .lens_cache$timestamp <- now
  
  indicators
}

#' Force-refresh the cache. Useful for a "Refresh" button in the UI.
refresh_indicators <- function() {
  load_indicators(force_refresh = TRUE)
}