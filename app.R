options(shiny.autoload.r = FALSE)

# Load local environment vars (.Renviron) at app start so FIREBASE_* config is
# present even if the R session predates them. On shinyapps.io there is no
# .Renviron; a bundled firebase.env (FIREBASE_* publishable web config only)
# supplies the Firebase config instead.
if (file.exists(".Renviron")) readRenviron(".Renviron")
if (file.exists("firebase.env")) readRenviron("firebase.env")

library(shiny)
library(dplyr)
library(stringr)
library(bslib)
library(SnowballC)
library(firebase)

# Load data
load("data/indicators.RData")
if ("preset_MSME" %in% names(indicators)) {
  indicators <- indicators %>% rename(preset_msme = preset_MSME)
}

# Source modular components
source("R/globals.R")
source("R/utils.R")
source("R/modules/indicatorCard.R")
source("R/modules/filterPanel.R")
source("R/modules/setManager.R")
source("R/modules/selectedIndicatorsMulti.R")
source("R/account_store.R")                          # optional-account persistence (Firebase RTDB)


# UI ----------
ui <- page_navbar(
  
  title = div(
    style = paste0(
      "display: flex; ",
      "align-items: center; ",
      "gap: 15px; ",
      "padding: 0; ",
      "margin: 0; ",
      "height: 40px;"
    ),
    tags$img(
      src = "cgap_logo.png", 
      height = "40px",
      style = "display: block; vertical-align: middle;"
    ),
    div(
      style = paste0(
        "height: 40px; ",
        "width: 1px; ",
        "background-color: #d1d5db;"
      )
    ),
    span(
      style = paste0(
        "font-size: 40px; ",
        "font-weight: 700; ",
        "color: #1A5A80; ",
        "line-height: 40px; ",
        "font-family: 'Figtree', sans-serif; ",
        "display: inline-block; ",
        "vertical-align: middle;"
      ), 
      "LENS"
    ), 
    span(
      style = paste0(
        "font-size: 17px; ",
        "font-weight: 200; ",
        "font-style: Italic;", 
        "color: #1A5A80; ",
        "line-height: 17px; ",
        "font-family: 'Figtree', sans-serif; ",
        "display: inline-block; ",
        "vertical-align: top;"
      ), 
      "Beta"
    )
  ),
  
  header = tagList(
    useFirebase(),  # Firebase JS SDK (auth) — config from FIREBASE_* env vars
    tags$script(HTML("
      function toggleAuthPw(btn){
        var inp = document.getElementById('auth_password'); if(!inp) return;
        var show = inp.type === 'password';
        inp.type = show ? 'text' : 'password';
        var i = btn.querySelector('i'); if(i) i.className = show ? 'fas fa-eye-slash' : 'fas fa-eye';
        btn.setAttribute('aria-label', show ? 'Hide password' : 'Show password');
      }
      Shiny.addCustomMessageHandler('setAuthBusy', function(busy){
        var b = document.getElementById('auth_submit'); if(!b) return;
        b.disabled = !!busy;
        b.classList.toggle('is-loading', !!busy);
      });
      Shiny.addCustomMessageHandler('authMode', function(mode){
        var m = document.querySelector('.auth-modal'); if(m) m.setAttribute('data-mode', mode);
      });
    ")),
    includeCSS("www/custom.css"),
    tags$style(HTML(generate_sector_styles(SECTOR_COLORS))),
    indicatorCardJS(),

    # Preset memo drawer (slide-in panel, non-blocking)
    div(
      id = "memo-drawer",
      class = "memo-drawer",

      # Drawer header
      div(
        class = "memo-drawer-header",
        div(
          style = "display: flex; align-items: center; gap: 8px;",
          icon("file-lines", class = "fas", style = "font-size: 16px;"),
          span(id = "memo-drawer-title", "Preset Memo", style = "font-weight: 600; font-size: 16px;")
        ),
        tags$button(
          class = "memo-drawer-close",
          onclick = "closePresetMemo();",
          icon("times", class = "fas")
        )
      ),

      # Page navigation (populated by JS for multi-page memos)
      div(
        id = "memo-drawer-nav",
        class = "memo-drawer-nav"
      ),

      # Drawer body (populated by JS)
      div(
        id = "memo-drawer-body",
        class = "memo-drawer-body"
      )
    ),

    # Memo data and JS
    tags$script(HTML(paste0(
      "var presetMemos = ", jsonlite::toJSON(PRESET_MEMOS, auto_unbox = TRUE), ";\n",
      "var techGuideMemos = ", jsonlite::toJSON(TECH_GUIDE_MEMOS, auto_unbox = TRUE), ";\n",
      "var _currentMemoPages = null;

      function _renderMemoPage(pageIndex) {
        var page = _currentMemoPages[pageIndex];
        var body = document.getElementById('memo-drawer-body');
        var html = '';
        if (page.summary) {
          html += '<p class=\"memo-summary\">' + page.summary + '</p>';
        }
        page.sections.forEach(function(section) {
          html += '<div class=\"memo-section\">';
          html += '<h4>' + section.heading + '</h4>';
          html += '<div>' + section.body + '</div>';
          html += '</div>';
        });
        body.innerHTML = html;
        body.scrollTop = 0;

        // Update active pill
        var pills = document.querySelectorAll('.memo-page-pill');
        pills.forEach(function(pill, i) {
          pill.classList.toggle('active', i === pageIndex);
        });
      }

      function _renderMemoNav(pages) {
        var nav = document.getElementById('memo-drawer-nav');
        if (!pages || pages.length <= 1) {
          nav.innerHTML = '';
          nav.style.display = 'none';
          return;
        }
        nav.style.display = 'flex';
        var html = '';
        pages.forEach(function(page, i) {
          html += '<button class=\"memo-page-pill' + (i === 0 ? ' active' : '') + '\" ';
          html += 'onclick=\"_renderMemoPage(' + i + ')\">';
          html += page.label;
          html += '</button>';
        });
        nav.innerHTML = html;
      }

      // Shared opener for the slide-in drawer. `key` is a unique toggle id so
      // clicking the same source again closes the drawer.
      function _openMemoDrawer(memo, key) {
        if (!memo) return;
        var drawer = document.getElementById('memo-drawer');

        // Toggle: if already open with same content, close it
        if (drawer.classList.contains('open') && drawer.dataset.presetId === key) {
          drawer.classList.remove('open');
          drawer.dataset.presetId = '';
          return;
        }
        drawer.dataset.presetId = key;
        document.getElementById('memo-drawer-title').textContent = memo.title;

        if (memo.pages) {
          // Multi-page memo
          _currentMemoPages = memo.pages;
          _renderMemoNav(memo.pages);
          _renderMemoPage(0);
        } else {
          // Single-page memo (backward compatible)
          _currentMemoPages = null;
          _renderMemoNav(null);
          var body = document.getElementById('memo-drawer-body');
          var html = '<p class=\"memo-summary\">' + memo.summary + '</p>';
          (memo.sections || []).forEach(function(section) {
            html += '<div class=\"memo-section\">';
            html += '<h4>' + section.heading + '</h4>';
            html += '<div>' + section.body + '</div>';
            html += '</div>';
          });
          body.innerHTML = html;
          body.scrollTop = 0;
        }

        drawer.classList.add('open');
      }

      function openPresetMemo(presetId) {
        _openMemoDrawer(presetMemos[presetId], presetId);
      }

      function openTechGuide(mandateId) {
        _openMemoDrawer(techGuideMemos[mandateId], 'tg_' + mandateId);
      }

      function closePresetMemo() {
        var drawer = document.getElementById('memo-drawer');
        drawer.classList.remove('open');
        drawer.dataset.presetId = '';
      }

      // Close on Escape key
      document.addEventListener('keydown', function(e) {
        if (e.key === 'Escape') closePresetMemo();
      });
      "
    ))),

    # Active-set switcher (Browse banner dropdown)
    tags$script(HTML("
      function toggleSetMenu(e) {
        if (e) { e.stopPropagation(); e.preventDefault(); }
        var menu = document.getElementById('active-set-menu');
        if (!menu) return;
        var willOpen = !menu.classList.contains('open');
        menu.classList.toggle('open', willOpen);
        var caret = document.querySelector('.active-set-caret');
        if (caret) caret.style.transform = willOpen ? 'rotate(180deg)' : '';
        if (willOpen) {
          setTimeout(function(){ document.addEventListener('click', _closeSetMenuOnce); }, 0);
        } else {
          document.removeEventListener('click', _closeSetMenuOnce);
        }
      }
      function _closeSetMenuOnce(ev) {
        var sw = document.querySelector('.active-set-switcher');
        if (sw && sw.contains(ev.target)) return;
        var menu = document.getElementById('active-set-menu');
        if (menu) menu.classList.remove('open');
        var caret = document.querySelector('.active-set-caret');
        if (caret) caret.style.transform = '';
        document.removeEventListener('click', _closeSetMenuOnce);
      }
      function selectBannerSetEl(el) {
        if (!el) return;
        var name = el.getAttribute('data-set-name');
        var menu = document.getElementById('active-set-menu');
        if (menu) menu.classList.remove('open');
        document.removeEventListener('click', _closeSetMenuOnce);
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue('banner_set_select', {name: name, ts: Date.now()}, {priority: 'event'});
        }
      }
    "))
  ),
  
  sidebar = sidebar(
    width = 400,
    p(style = "font-size: 13px; color: #555; line-height: 1.5;",
      "A curated catalog of indicators for use with regulatory data that analysts combine with breakdowns — such as gender, customer type, or provider type — to support disaggregated analysis for more inclusive financial policies for both retail consumers and MSMEs. ",
      tags$a("See User Guide", onclick = "document.getElementById('show_about').click(); return false;",
             href = "#", style = "color: #1A5A80; font-weight: 600; text-decoration: none;"),
      " for details."
    ),
    filterPanelUI("filters")
  ),
  
  nav_spacer(),
  
  nav_panel(
    title = "BROWSE",
    
    layout_sidebar(
      sidebar = sidebar(
        position = "right",
        width = 300,
        class = "mandate-sidebar",
        h5("Quick navigation", style = "margin-bottom: 0px;"),
        "Jump to indicators grouped by their primary mandate",
        uiOutput("mandate_links")
      ),
      div(
        uiOutput("navigation_helper"),
        uiOutput("key"),
        div(
          id = "indicator_wrapper",
          class = "loading-overlay",
          div(
            id = "loading_spinner",
            class = "loading-spinner",
            style = "display: none;",
            div(class = "spinner")
          ),
          div(
            id = "indicator_container",
            class = "fade-content"
          )
        )
      )
    )
  ),
  
  nav_panel(
    title = uiOutput("selected_tab_title"),
    selectedIndicatorsMultiUI("selected")
  ),
  
  nav_item(
    actionButton(
      "show_about",
      "ABOUT",
      class = "btn-link",
      style = "color: inherit; text-decoration: none; border: none; background: transparent; font-size: 14px;"
    )
  ),

  nav_item(
    uiOutput("account_nav", inline = TRUE)
  )

)

# SERVER ----------
server <- function(input, output, session) {

  # --- Optional accounts (Firebase email/password auth) ---------------------
  fb <- FirebaseEmailPassword$new()

  # current_user: list(uid, email, token) when signed in, else NULL.
  # Uses get_signed_in() (is_signed_in() is deprecated). When signed out the
  # firebase package reports success = FALSE / no response.
  current_user <- reactive({
    u <- tryCatch(fb$get_signed_in(), error = function(e) NULL)
    if (is.null(u) || !isTRUE(u$success) || is.null(u$response$uid)) return(NULL)
    list(uid = u$response$uid,
         email = u$response$email,
         token = u$response$stsTokenManager$accessToken)
  })

  # Navbar control: "Sign in" when signed out, the email when signed in.
  output$account_nav <- renderUI({
    cu <- current_user()
    if (is.null(cu)) {
      actionButton("account_btn", "Sign in", class = "btn-link account-nav-btn",
                   icon = icon("right-to-bracket", class = "fas"))
    } else {
      actionButton("account_btn",
                   label = tagList(icon("circle-user", class = "fas"), cu$email),
                   class = "btn-link account-nav-btn")
    }
  })

  # --- Auth modal state ---
  auth_mode  <- reactiveVal("signin")   # "signin" | "register"
  auth_busy  <- reactiveVal(FALSE)
  auth_error <- reactiveVal(NULL)

  # Map raw Firebase auth errors to friendly, human messages.
  .auth_friendly_error <- function(res) {
    raw <- tolower(paste(c(res$response$code, res$response$message), collapse = " "))
    if (grepl("email-already-in-use|already in use", raw)) return("An account with that email already exists — try signing in instead.")
    if (grepl("invalid-credential|wrong-password|user-not-found|invalid-login", raw)) return("Incorrect email or password.")
    if (grepl("invalid-email", raw)) return("That doesn't look like a valid email address.")
    if (grepl("weak-password|at least 6", raw)) return("Password must be at least 6 characters.")
    if (grepl("too-many-requests", raw)) return("Too many attempts — please wait a moment and try again.")
    if (grepl("network", raw)) return("Network error — check your connection and try again.")
    m <- res$response$message
    if (is.null(m) || !nzchar(m)) "Something went wrong. Please try again." else m
  }

  # Open the auth modal (signed out) or the account modal (signed in).
  observeEvent(input$account_btn, {
    cu <- current_user()
    if (is.null(cu)) {
      auth_mode("signin"); auth_error(NULL); auth_busy(FALSE)
      showModal(modalDialog(
        div(class = "auth-modal", `data-mode` = "signin",
          uiOutput("auth_header"),
          uiOutput("auth_error"),
          div(class = "auth-field",
            tags$label("Email", class = "auth-label", `for` = "auth_email"),
            textInput("auth_email", NULL, placeholder = "you@example.com")
          ),
          div(class = "auth-field",
            div(class = "auth-label-row",
              tags$label("Password", class = "auth-label", `for` = "auth_password"),
              actionLink("auth_forgot", "Forgot password?", class = "auth-forgot")
            ),
            div(class = "auth-pw-wrap",
              passwordInput("auth_password", NULL, placeholder = "Your password"),
              tags$button(type = "button", class = "auth-pw-toggle",
                          onclick = "toggleAuthPw(this)", `aria-label` = "Show password",
                          icon("eye", class = "fas"))
            )
          ),
          actionButton("auth_submit", "Sign in", class = "btn auth-submit-btn", width = "100%"),
          div(class = "auth-toggle",
            uiOutput("auth_toggle_text", inline = TRUE), " ",
            actionLink("auth_toggle_mode", "Create an account")
          )
        ),
        title = NULL, footer = NULL, easyClose = TRUE, size = "s"
      ))
      updateTextInput(session, "auth_email", value = "")
      updateTextInput(session, "auth_password", value = "")
    } else {
      showModal(modalDialog(
        div(class = "account-modal",
          div(class = "account-avatar", icon("circle-user", class = "fas")),
          h3(class = "account-title", "Your account"),
          p(class = "account-email", cu$email),
          div(class = "account-note",
            icon("cloud-arrow-up", class = "fas"),
            span("Your indicator sets are saved to your account and restored whenever you sign in.")
          ),
          actionButton("auth_signout", "Sign out", class = "btn account-signout-btn", width = "100%")
        ),
        title = NULL, footer = NULL, easyClose = TRUE, size = "s"
      ))
    }
  })

  # Reactive display bits (contain NO inputs, so re-rendering won't reset typing)
  output$auth_header <- renderUI({
    reg <- identical(auth_mode(), "register")
    div(class = "auth-head",
      div(class = "auth-badge", icon(if (reg) "user-plus" else "right-to-bracket", class = "fas")),
      h3(class = "auth-title", if (reg) "Create your account" else "Welcome back"),
      p(class = "auth-sub", "Accounts are optional — save your indicator sets and pick up where you left off.")
    )
  })
  output$auth_error <- renderUI({
    err <- auth_error()
    if (is.null(err)) return(NULL)
    div(class = "auth-error", role = "alert",
        icon("circle-exclamation", class = "fas"), span(err))
  })
  output$auth_toggle_text <- renderUI({
    if (identical(auth_mode(), "register")) "Already have an account?" else "New to LENS?"
  })

  # Keep static button/link labels + the modal's data-mode in sync with mode.
  observeEvent(auth_mode(), {
    reg <- identical(auth_mode(), "register")
    updateActionButton(session, "auth_submit", label = if (reg) "Create account" else "Sign in")
    updateActionButton(session, "auth_toggle_mode", label = if (reg) "Sign in" else "Create an account")
    session$sendCustomMessage("authMode", auth_mode())
  })

  observeEvent(input$auth_toggle_mode, {
    auth_mode(if (identical(auth_mode(), "signin")) "register" else "signin")
    auth_error(NULL)
  })

  observeEvent(input$auth_forgot, {
    email <- input$auth_email
    if (is.null(email) || !nzchar(trimws(email))) {
      auth_error("Enter your email above first, then click Forgot password.")
      return()
    }
    fb$reset_password(email)
    auth_error(NULL)
    showNotification("Password reset email sent — check your inbox.", type = "message", duration = 6)
  })

  observeEvent(input$auth_submit, {
    if (isTRUE(isolate(auth_busy()))) return()
    email <- input$auth_email; pw <- input$auth_password
    if (is.null(email) || !nzchar(trimws(email)) || is.null(pw) || !nzchar(pw)) {
      auth_error("Please enter your email and password.")
      return()
    }
    auth_error(NULL); auth_busy(TRUE)
    session$sendCustomMessage("setAuthBusy", TRUE)
    if (identical(auth_mode(), "register")) fb$create(email, pw) else fb$sign_in(email, pw)
  })

  # Registration result: on success, sign the user straight in (stay busy).
  observeEvent(fb$get_created(), {
    res <- fb$get_created()
    if (is.null(res)) return()
    if (isTRUE(res$success)) {
      fb$sign_in(isolate(input$auth_email), isolate(input$auth_password))
    } else {
      auth_busy(FALSE); session$sendCustomMessage("setAuthBusy", FALSE)
      auth_error(.auth_friendly_error(res))
    }
  }, ignoreInit = TRUE)

  # Sign-in result: success closes the modal (via current_user); failure shows error.
  observeEvent(fb$get_signed_in(), {
    res <- fb$get_signed_in()
    if (is.null(res)) return()
    if (isTRUE(res$success)) {
      auth_busy(FALSE); session$sendCustomMessage("setAuthBusy", FALSE)
    } else if (isTRUE(isolate(auth_busy()))) {
      auth_busy(FALSE); session$sendCustomMessage("setAuthBusy", FALSE)
      auth_error(.auth_friendly_error(res))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$auth_signout, {
    fb$sign_out()
    removeModal()
  })

  # Close the auth modal on the signed-out -> signed-in transition only.
  .was_signed_in <- reactiveVal(FALSE)
  observeEvent(current_user(), {
    now_in <- !is.null(current_user())
    if (now_in && !.was_signed_in()) removeModal()
    .was_signed_in(now_in)
  }, ignoreNULL = FALSE)

  # Initialize set manager (hydrates/persists sets when current_user is set)
  set_manager <- selectedIndicatorsMultiServer(
    "selected",
    indicators_data = indicators,
    sector_colors = SECTOR_COLORS,
    current_user = current_user
  )
  
  # Filter module
  filter_values <- filterPanelServer("filters", indicators)
  
  # Filtered indicators with debouncing
  filtered_indicators <- reactive({
    filter_values$filtered_indicators()
  }) %>% 
    debounce(200)
  
  # Track when we need to re-render indicators
  filter_trigger <- reactiveVal(0)
  
  observeEvent(filtered_indicators(), {
    filter_trigger(filter_trigger() + 1)
  })
  
  # Navigation helper with active filters
  output$navigation_helper <- renderUI({
    indicators_data <- filtered_indicators()
    
    active_filters <- list(
      mandates = input$`filters-mandates`,
      objectives = filter_values$selected_objectives(),
      sectors = input$`filters-sectors`,
      use_cases = input$`filters-use_cases`,
      search = input$`filters-search`,
      presets_digital = input$`filters-presets_digital`,
      presets_msme = input$`filters-presets_msme`,
      presets_finhealth = input$`filters-presets_finhealth`,
      presets_di = input$`filters-presets_di`
    )
    
    # Get the active set name + all set names from set_manager
    active_set_name <- set_manager$active_set()
    all_sets <- names(set_manager$sets())

    enhanced_navigation_helper(
      indicators_data,
      indicators,
      active_filters,
      active_set_name = active_set_name,
      all_sets = all_sets
    )
  })

  # Switch active set from the Browse banner dropdown
  observeEvent(input$banner_set_select, {
    sel <- input$banner_set_select
    name <- if (is.list(sel)) sel$name else sel
    set_manager$set_active(name)
  }, ignoreInit = TRUE)
  
  # Key
  output$key <- renderUI({
    indicator_key()
  })
  
  # Mandate links
  output$mandate_links <- renderUI({
    indicators_data <- filtered_indicators()
    create_mandate_links(indicators_data)
  })
  
 
  # ADD ALL / REMOVE ALL BUTTON HANDLERS -------------

  
  # Add all filtered indicators to the active set
  observeEvent(input$add_all_filtered, {
    req(filtered_indicators())
    filtered_ids <- isolate(filtered_indicators()$indicator_id)

    if (length(filtered_ids) > 0) {
      set_manager$add_many_to_active(filtered_ids)

      showNotification(
        paste("Added", length(filtered_ids), "indicators to your set"),
        type = "message",
        duration = 3
      )

      # Re-render to update button states
      filter_trigger(filter_trigger() + 1)
    }
  })

  # Remove all filtered indicators from the active set
  observeEvent(input$remove_all_filtered, {
    req(filtered_indicators())
    filtered_ids <- isolate(filtered_indicators()$indicator_id)
    current_selected <- isolate(set_manager$get_active_indicators())
    to_remove <- intersect(filtered_ids, current_selected)

    if (length(to_remove) > 0) {
      set_manager$remove_many_from_active(to_remove)

      showNotification(
        paste("Removed", length(to_remove), "indicators from your set"),
        type = "message",
        duration = 3
      )

      filter_trigger(filter_trigger() + 1)
    } else {
      showNotification(
        "No filtered indicators are in your current set",
        type = "warning",
        duration = 3
      )
    }
  })
  

  # END ADD ALL / REMOVE ALL HANDLERS -----------
  
  # Render indicators only when filter_trigger changes
  observeEvent(filter_trigger(), {
    session$sendCustomMessage("showLoading", TRUE)
    
    indicators_data <- isolate(filtered_indicators())
    selected_codes <- isolate(set_manager$get_active_indicators()) 
    
    removeUI(selector = "#indicator_container > *", immediate = TRUE, multiple = TRUE)
    
    if (nrow(indicators_data) == 0) {
      insertUI(
        selector = "#indicator_container",
        ui = div(
          style = paste0(
            "text-align: center; padding: 48px 20px; margin-top: 24px; ",
            "background: #f8f9fa; border-radius: 8px; border: 1px solid rgba(0,0,0,0.06);"
          ),
          icon("filter", class = "fas", style = "font-size: 32px; color: #adb5bd; margin-bottom: 12px; display: block;"),
          h4("No indicators match your filters", style = "color: #6c757d; margin: 0 0 6px 0; font-size: 16px;"),
          p("Try broadening your search or resetting filters", style = "color: #adb5bd; margin: 0; font-size: 13px;")
        ),
        immediate = TRUE
      )
      session$sendCustomMessage("showLoading", FALSE)
      return()
    }
    
    mandates <- unique(indicators_data$main_mandate)
    
    for (mandate in mandates) {
      mandate_indicators <- indicators_data %>% 
        filter(main_mandate == mandate)
      
      title_case_mandate <- str_to_title(mandate)
      mandate_id <- paste0("mandate_", slugify(title_case_mandate))
      
      insertUI(
        selector = "#indicator_container",
        ui = tags$div(
          id = mandate_id,
          enhanced_mandate_header(title_case_mandate, nrow(mandate_indicators)),
          accordion(
            id = paste0(mandate_id, "_accordion"),
            !!!lapply(seq_len(nrow(mandate_indicators)), function(i) {
              ind <- mandate_indicators[i, ]
              indicatorCardModern(  
                ind$indicator_id,
                ind,
                SECTOR_COLORS,
                ind$indicator_id %in% selected_codes
              )
            }),
            open = FALSE
          )
        ),
        immediate = TRUE
      )
    }
    
    session$sendCustomMessage("setupSelectButtons", list())
    session$sendCustomMessage("hideLoadingDelayed", list(delay = 300))
  }, ignoreNULL = TRUE, ignoreInit = FALSE)
  
  # Handle selection events from JavaScript
  observeEvent(input$indicator_selected, {
    req(input$indicator_selected)
    
    id <- input$indicator_selected$id
    action <- input$indicator_selected$action
    
    isolate({
      if (action == "add") {
        set_manager$add_to_active(id)
      } else {
        set_manager$remove_from_active(id)
      }
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Re-map the card Add/Added buttons whenever the active set changes, so they
  # reflect the currently selected set (not whichever set was active at render).
  observeEvent(set_manager$active_set(), {
    selected_codes <- isolate(set_manager$get_active_indicators())
    session$sendCustomMessage(
      "updateSelectedButtons",
      list(ids = as.character(selected_codes))
    )
  }, ignoreInit = TRUE)
  
  # Selected tab title
  output$selected_tab_title <- renderUI({
    active_indicators <- set_manager$get_active_indicators()
    count <- length(active_indicators)
    
    if (count == 0) {
      span(
        icon("clipboard-list", lib = "font-awesome"),
        " YOUR INDICATOR SETS"
      )
    } else {
      span(
        icon("clipboard-check", lib = "font-awesome"),
        " YOUR INDICATOR SETS",
        span(
          paste0(count),
          style = "background-color: #198754; color: white; font-size: 12px; padding: 2px 8px; border-radius: 12px; margin-left: 5px;"
        )
      )
    }
  })
  
  # About modal
  observeEvent(input$show_about, {
    showModal(about_modal_content())
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
