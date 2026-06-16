options(shiny.autoload.r = FALSE)

# Load local environment vars (.Renviron) at app start so FIREBASE_* config is
# present even if the R session predates them. On deploy (.Renviron not present)
# the host-provided environment variables are used instead.
if (file.exists(".Renviron")) readRenviron(".Renviron")

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

  # Open the auth modal (signed out) or the account modal (signed in).
  observeEvent(input$account_btn, {
    cu <- current_user()
    if (is.null(cu)) {
      showModal(modalDialog(
        title = "Sign in or create an account", size = "s", easyClose = TRUE,
        textInput("auth_email", "Email", placeholder = "you@example.com", width = "100%"),
        passwordInput("auth_password", "Password", placeholder = "at least 6 characters", width = "100%"),
        p(style = "font-size: 12px; color: #6c757d;",
          "Accounts are optional — they let you save your indicator sets and sign back in later."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("auth_register", "Create account", class = "btn btn-outline-primary"),
          actionButton("auth_signin", "Sign in", class = "btn btn-primary")
        )
      ))
    } else {
      showModal(modalDialog(
        title = "Account", size = "s", easyClose = TRUE,
        p("Signed in as ", strong(cu$email), "."),
        p(style = "font-size: 12px; color: #6c757d;",
          "Your indicator sets are saved to your account automatically."),
        footer = tagList(
          modalButton("Close"),
          actionButton("auth_signout", "Sign out", class = "btn btn-outline-danger")
        )
      ))
    }
  })

  observeEvent(input$auth_register, {
    message("[auth] register clicked for: ", if (is.null(input$auth_email)) "<empty>" else input$auth_email)
    req(input$auth_email, input$auth_password)
    fb$create(input$auth_email, input$auth_password)
  })
  observeEvent(input$auth_signin, {
    message("[auth] sign-in clicked for: ", if (is.null(input$auth_email)) "<empty>" else input$auth_email)
    req(input$auth_email, input$auth_password)
    fb$sign_in(input$auth_email, input$auth_password)
  })

  # Surface sign-in results/errors (firebase reports them via get_signed_in()).
  observeEvent(fb$get_signed_in(), {
    res <- fb$get_signed_in()
    if (is.null(res)) return()
    if (isTRUE(res$success)) {
      message("[auth] signed in: ", res$response$email)
    } else {
      msg <- tryCatch(res$response$message, error = function(e) NULL)
      message("[auth] sign-in failed: ", if (is.null(msg)) "unknown" else msg)
      if (!is.null(msg)) showNotification(paste("Sign-in failed:", msg), type = "error", duration = 8)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$auth_signout, {
    fb$sign_out()
    removeModal()
  })

  # Registration result: on success, sign the user straight in.
  observeEvent(fb$get_created(), {
    res <- fb$get_created()
    if (isTRUE(res$success)) {
      showNotification("Account created. Signing you in…", type = "message")
      fb$sign_in(isolate(input$auth_email), isolate(input$auth_password))
    } else {
      msg <- tryCatch(res$response$message, error = function(e) NULL)
      showNotification(paste("Could not create account:", if (is.null(msg)) "unknown error" else msg),
                       type = "error", duration = 8)
    }
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
