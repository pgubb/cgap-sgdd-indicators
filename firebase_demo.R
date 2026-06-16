# firebase_demo.R --- Isolated Firebase Auth proof-of-concept
# ============================================================================
# EXPLORATION (branch: explore/user-accounts). NOT part of the main app.
#
# Purpose: prove the email/password auth round-trip (register -> sign in ->
# signed-in user exposed to the server -> sign out) end-to-end against the real
# Firebase project, BEFORE wiring anything into the LENS app.
#
# Run:
#   1. Put your Firebase web config in .Renviron (see below), restart R.
#   2. shiny::runApp("firebase_demo.R")
#   3. Register an email/password, then sign in. Watch the status panel.
#
# .Renviron (these are publishable client config values, but keep them out of
# git anyway — .Renviron is gitignored):
#   FIREBASE_API_KEY=...
#   FIREBASE_PROJECT_ID=cgap-lens-xxxx
#   FIREBASE_AUTH_DOMAIN=cgap-lens-xxxx.firebaseapp.com   # optional (auto)
#   FIREBASE_APP_ID=1:...:web:...                          # optional
#   FIREBASE_DATABASE_URL=https://....firebaseio.com       # optional (Phase 3)
#
# The firebase package reads these env vars automatically via read_config()
# when no firebase.rds file is present.
# ============================================================================

library(shiny)
library(firebase)
source("R/account_store.R")  # as_load_sets / as_save_sets (Firebase RTDB)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

# Fail fast with a clear message if config is missing.
.has_fb_config <- nzchar(Sys.getenv("FIREBASE_API_KEY")) &&
  nzchar(Sys.getenv("FIREBASE_PROJECT_ID"))

ui <- fluidPage(
  useFirebase(),  # injects the Firebase JS SDK + reads config (env vars)
  tags$head(tags$style(HTML("
    body { font-family: system-ui, sans-serif; max-width: 560px; margin: 40px auto; }
    .panel { border: 1px solid #e0e0e0; border-radius: 10px; padding: 20px; margin-bottom: 16px; }
    .ok { color: #1a7f37; } .err { color: #b91c1c; } .muted { color: #666; font-size: 13px; }
    input { width: 100%; padding: 8px; margin: 4px 0 12px; box-sizing: border-box; }
    .btn-row .btn { margin-right: 8px; }
  "))),

  h3("Firebase Auth — proof of concept"),

  if (!.has_fb_config) {
    div(class = "panel err",
        strong("Missing Firebase config."),
        p(class = "muted",
          "Set FIREBASE_API_KEY and FIREBASE_PROJECT_ID in .Renviron and restart R."))
  },

  div(class = "panel",
    h4("Account"),
    textInput("email", "Email", placeholder = "you@example.com"),
    passwordInput("password", "Password", placeholder = "at least 6 characters"),
    div(class = "btn-row",
      actionButton("register", "Register", class = "btn btn-primary"),
      actionButton("signin", "Sign in", class = "btn btn-success"),
      reqSignin(actionButton("signout", "Sign out", class = "btn btn-outline-secondary"))
    )
  ),

  div(class = "panel",
    h4("Status"),
    uiOutput("status"),
    tags$hr(),
    h4("Signed-in user object", class = "muted"),
    verbatimTextOutput("user_dump")
  ),

  reqSignin(div(class = "panel",
    h4("Persistence test (Realtime Database)"),
    p(class = "muted", "Writes a sample set to /user_sets/<your-uid> and reads it back."),
    div(class = "btn-row",
      actionButton("save_sets", "Save sample sets", class = "btn btn-primary"),
      actionButton("load_sets", "Reload from DB", class = "btn btn-success")
    ),
    verbatimTextOutput("db_dump")
  ))
)

server <- function(input, output, session) {
  f <- FirebaseEmailPassword$new()

  # Registration result
  observeEvent(f$get_created(), {
    res <- f$get_created()
    if (isTRUE(res$success)) {
      showNotification("Account created — you can sign in now.", type = "message")
    } else {
      showNotification(paste("Registration failed:", res$response$message %||% "unknown error"),
                       type = "error", duration = 8)
    }
  })

  observeEvent(input$register, {
    req(input$email, input$password)
    f$create(input$email, input$password)
  })

  observeEvent(input$signin, {
    req(input$email, input$password)
    f$sign_in(input$email, input$password)
  })

  observeEvent(input$signout, {
    f$sign_out()
  })

  output$status <- renderUI({
    signed_in <- tryCatch(f$is_signed_in(), error = function(e) FALSE)
    if (isTRUE(signed_in)) {
      user <- f$get_signed_in()
      email <- tryCatch(user$response$email, error = function(e) NULL)
      uid   <- tryCatch(user$response$uid,   error = function(e) NULL)
      tagList(
        span(class = "ok", strong("Signed in")),
        p(class = "muted", paste0("email: ", email %||% "?", "  |  uid: ", uid %||% "?"))
      )
    } else {
      span(class = "muted", "Not signed in.")
    }
  })

  # Raw dump to discover exact field names returned by this firebase version
  output$user_dump <- renderPrint({
    str(tryCatch(f$get_signed_in(), error = function(e) NULL))
  })

  # --- Persistence test ---
  .uid <- function() tryCatch(f$get_signed_in()$response$uid, error = function(e) NULL)
  .tok <- function() tryCatch(f$get_signed_in()$response$stsTokenManager$accessToken, error = function(e) NULL)

  loaded <- reactiveVal(NULL)

  observeEvent(input$save_sets, {
    sample <- list("My Indicators" = c("11", "18", "341"), "Credit set" = c("42"))
    res <- tryCatch({ as_save_sets(.uid(), sample, .tok()); "OK" },
                    error = function(e) conditionMessage(e))
    if (identical(res, "OK")) {
      showNotification("Saved to RTDB.", type = "message")
      loaded(as_load_sets(.uid(), .tok()))
    } else {
      showNotification(paste("Save failed:", res), type = "error", duration = 10)
    }
  })

  fetched_at <- reactiveVal(NULL)

  observeEvent(input$load_sets, {
    v <- tryCatch(as_load_sets(.uid(), .tok()), error = function(e) list(.error = conditionMessage(e)))
    loaded(v)
    fetched_at(format(Sys.time(), "%H:%M:%S"))
    if (!is.null(v$.error)) {
      showNotification(paste("Reload failed:", v$.error), type = "error", duration = 10)
    } else {
      showNotification(sprintf("Reloaded %d set(s) from RTDB.", length(v)), type = "message")
    }
  })

  output$db_dump <- renderPrint({
    v <- loaded()
    if (is.null(v)) { cat("(nothing loaded yet)"); return(invisible()) }
    if (!is.null(fetched_at())) cat("fetched at", fetched_at(), "\n")
    str(v)
  })
}

shinyApp(ui, server)
