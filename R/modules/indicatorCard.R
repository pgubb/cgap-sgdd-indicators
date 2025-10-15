# R/modules/indicatorCard.R - Module for individual indicator cards

# Enhanced indicator card with modern design
indicatorCardModern <- function(id, indicator, sector_colors, is_selected = FALSE) {
  ns <- NS(id)
  
  sector_color <- sector_colors[[indicator$main_sector]]
  
  # Create organization logos (if sources exist)
  show_sources <- if ("sources_any" %in% names(indicator)) {
    !is.na(indicator$sources_any) && indicator$sources_any != ""
  } else {
    (indicator$IMF == 1) || (indicator$GPFI == 1) || (indicator$AFI == 1) || (indicator$WEF == 1)
  }
  
  div(
    class = "indicator-card-modern",
    `data-indicator-id` = indicator$indicator_id,
    style = paste0(
      "background: linear-gradient(135deg, white 0%, #f8f9fa 100%); ",
      "border: 1px solid #e9ecef; ",
      "border-radius: 12px; ",
      "padding: 0; ",
      "margin-bottom: 20px; ",
      "box-shadow: 0 2px 8px rgba(0,0,0,0.06); ",
      "transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1); ",
      "overflow: hidden; ",
      "position: relative;"
    ),
    
    # Color accent bar
    div(
      style = paste0(
        "height: 4px; ",
        "background: linear-gradient(90deg, ", sector_color, " 0%, ", 
        adjustcolor(sector_color, alpha.f = 0.6), " 100%); ",
        "width: 100%;"
      )
    ),
    
    # Card header
    div(
      class = "card-header-modern",
      style = paste0(
        "padding: 20px 24px 16px 24px; ",
        "border-bottom: 1px solid #f1f3f4; ",
        "background: white; ",
        "cursor: pointer; ",
        "position: relative;"
      ),
      
      # Main content row
      div(
        style = "display: flex; justify-content: space-between; align-items: flex-start; gap: 20px;",
        
        # Left side - main info
        div(
          style = "flex: 1; min-width: 0;", # min-width prevents text overflow
          
          # Title and badges row
          div(
            style = "display: flex; flex-wrap: wrap; align-items: center; gap: 8px; margin-bottom: 12px;",
            
            h4(
              indicator$indicator_name,
              style = paste0(
                "margin: 0; ",
                "font-size: 18px; ",
                "font-weight: 600; ",
                "color: #1a1a1a; ",
                "line-height: 1.3; ",
                "flex: 1; ",
                "min-width: 200px;"
              )
            ),
            
            
            # Priority star
            if (!is.na(indicator$preset_foundation) && indicator$preset_foundation == 1) {
              div(
                class = "priority-badge",
                style = paste0(
                  "background: linear-gradient(135deg, #ffd700 0%, #ffed4e 100%); ",
                  "color: #856404; ",
                  "padding: 4px 8px; ",
                  "border-radius: 12px; ",
                  "font-size: 11px; ",
                  "font-weight: 600; ",
                  "display: flex; ",
                  "align-items: center; ",
                  "gap: 4px; ",
                  "box-shadow: 0 2px 4px rgba(255, 215, 0, 0.3);"
                ),
                icon("star", class = "fas", style = "font-size: 10px;"),
                "Featured"
              )
            }
          ),
          
          # Tags row
          div(
            style = "display: flex; flex-wrap: wrap; gap: 6px; margin-bottom: 8px;",
            
            # Mandate tag
            span(
              indicator$main_objectives,
              class = "tag mandate-tag",
              style = paste0(
                "background-color: #e3f2fd; ",
                "color: #1565c0; ",
                "padding: 4px 10px; ",
                "border-radius: 16px; ",
                "font-size: 12px; ",
                "font-weight: 500; ",
                "border: 1px solid #bbdefb;"
              )
            ),
            
            # Sector tag
            span(
              indicator$main_sector,
              class = "tag sector-tag",
              style = paste0(
                "background-color: ", sector_color, "; ",
                "color: #333; ",
                "padding: 4px 10px; ",
                "border-radius: 16px; ",
                "font-size: 12px; ",
                "font-weight: 500; ",
                "border: 1px solid ", adjustcolor(sector_color, red.f = 0.8, green.f = 0.8, blue.f = 0.8), ";"
              )
            )
          ),
          
          # Description preview
          p(
            substr(indicator$indicator_description, 1, 120),
            if(nchar(indicator$indicator_description) > 120) "...",
            style = paste0(
              "margin: 0; ",
              "color: #5f6368; ",
              "font-size: 14px; ",
              "line-height: 1.4;"
            )
          )
        ),
        
        # Right side - action button
        div(
          style = "flex-shrink: 0;",
          tags$button(
            id = paste0("select_", indicator$indicator_id),
            class = paste("btn select-indicator-btn-modern", 
                          if (is_selected) "btn-selected" else "btn-unselected"),
            `data-indicator-id` = indicator$indicator_id,
            `data-selected` = tolower(as.character(is_selected)),
            style = if (is_selected) {
              paste0(
                "background: linear-gradient(135deg, #198754 0%, #20c997 100%); ",
                "border: 1px solid #198754; ",
                "color: white; ",
                "padding: 8px 16px; ",
                "border-radius: 20px; ",
                "font-size: 13px; ",
                "font-weight: 500; ",
                "transition: all 0.2s ease; ",
                "box-shadow: 0 2px 8px rgba(25, 135, 84, 0.3);"
              )
            } else {
              paste0(
                "background: white; ",
                "border: 2px solid #e9ecef; ",
                "color: #495057; ",
                "padding: 8px 16px; ",
                "border-radius: 20px; ",
                "font-size: 13px; ",
                "font-weight: 500; ",
                "transition: all 0.2s ease;"
              )
            },
            
            icon(if (is_selected) "check" else "plus", class = "fas", 
                 style = "font-size: 11px; margin-right: 6px;"),
            span(class = "btn-text", 
                 if (is_selected) "Added" else "Add")
          )
        )
      ),
      
      # Expand/collapse indicator
      div(
        class = "expand-indicator",
        style = paste0(
          "position: absolute; ",
          "bottom: 8px; ",
          "left: 50%; ",
          "transform: translateX(-50%); ",
          "width: 24px; ",
          "height: 24px; ",
          "background: #f8f9fa; ",
          "border-radius: 50%; ",
          "display: flex; ",
          "align-items: center; ",
          "justify-content: center; ",
          "cursor: pointer; ",
          "transition: all 0.2s ease;"
        ),
        icon("chevron-down", class = "fas", style = "font-size: 12px; color: #6c757d;")
      )
    ),
    
    # Collapsible content
    div(
      class = "card-content-modern",
      style = paste0(
        "display: none; ",
        "padding: 24px; ",
        "background: #fafbfc; ",
        "border-top: 1px solid #f1f3f4; ",
        "font-size: 13px;"
      ),
      
      # Single column layout for detailed info
      div(
        style = "margin-bottom: 24px;",
        
        # Main details section
        div(
          style = "margin-bottom: 24px;",
          render_disagg_table_vertical(
            indicator, 
            columns = c("indicator_description", "indicator_long_description", "gender_questions","unit_of_analysis", "measurement_type", "main_mandate_objective", "secondary_mandate_objective", "main_sector", 
                        "secondary_sectors")
          )
        ),
        
        # Breakdowns section
        div(
          style = "margin-bottom: 24px;",
          render_disagg_table_generalized(indicator, c("disaggregation_vars"))
        ),
        
        # Sources section (if applicable)
        if (show_sources) {
          div(
            style = "padding: 16px; background: white; border-radius: 8px; border: 1px solid #e9ecef;",
            p(strong("Related indicators from:"), style = "margin-bottom: 12px; color: #495057;"),
            div(
              style = "display: flex; justify-content: flex-start; gap: 12px; align-items: center; flex-wrap: wrap; margin-bottom: 16px;",
              if (!is.na(indicator$IMF)) tags$img(src = "imf_logo.png", height = "32px", style = "opacity: 0.8;"),
              if (!is.na(indicator$GPFI)) tags$img(src = "gpfi_logo.png", height = "32px", style = "opacity: 0.8;"),
              if (!is.na(indicator$AFI)) tags$img(src = "afi_logo.png", height = "32px", style = "opacity: 0.8;"), 
              if (!is.na(indicator$WEF)) tags$img(src = "wef_logo.png", height = "32px", style = "opacity: 0.8;")
            ), 
            
            render_disagg_table_vertical(indicator, columns = c("GPFI", "IMF",  "AFI", "WEF"), delimiter = ";")
          )
        }
      )
      
    )
  )
}


indicatorCardJS <- function() {
  tags$script(HTML("
    $(document).ready(function() {
      
      // Setup event delegation for select buttons
      $(document).off('click', '.select-indicator-btn, .select-indicator-btn-modern');
      $(document).on('click', '.select-indicator-btn, .select-indicator-btn-modern', function(e) {
        e.preventDefault();
        e.stopPropagation();
        
        var btn = $(this);
        var indicatorId = btn.data('indicator-id');
        var isSelected = btn.data('selected') === true || btn.data('selected') === 'true';
        
        if (isSelected) {
          if (btn.hasClass('select-indicator-btn-modern')) {
            btn.removeClass('btn-selected').addClass('btn-unselected');
            btn.find('i').removeClass('fa-check').addClass('fa-plus');
            btn.find('.btn-text').text('Add');
          } else {
            btn.removeClass('btn-success').addClass('btn-outline-primary');
            btn.find('i').removeClass('fa-check-circle').addClass('fa-plus-circle');
            btn.find('.btn-text').text('Add to list');
          }
          btn.data('selected', false);
          
          Shiny.setInputValue('indicator_selected', {
            id: indicatorId,
            action: 'remove'
          }, {priority: 'event'});
        } else {
          if (btn.hasClass('select-indicator-btn-modern')) {
            btn.removeClass('btn-unselected').addClass('btn-selected');
            btn.find('i').removeClass('fa-plus').addClass('fa-check');
            btn.find('.btn-text').text('Added');
          } else {
            btn.removeClass('btn-outline-primary').addClass('btn-success');
            btn.find('i').removeClass('fa-plus-circle').addClass('fa-check-circle');
            btn.find('.btn-text').text('Remove from list');
          }
          btn.data('selected', true);
          
          Shiny.setInputValue('indicator_selected', {
            id: indicatorId,
            action: 'add'
          }, {priority: 'event'});
        }
      });
      
      // Card expansion/collapse functionality
      $(document).on('click', '.card-header-modern', function(e) {
        if ($(e.target).closest('.select-indicator-btn-modern').length > 0) {
          return;
        }
        
        e.preventDefault();
        e.stopPropagation();
        
        var header = $(this);
        var card = header.closest('.indicator-card-modern');
        var content = card.find('.card-content-modern');
        var expandIcon = header.find('.expand-indicator i');
        var isExpanded = card.hasClass('expanded');
        
        if (card.hasClass('transitioning')) {
          return;
        }
        
        card.addClass('transitioning');
        
        if (isExpanded) {
          card.removeClass('expanded');
          expandIcon.removeClass('fa-chevron-up').addClass('fa-chevron-down');
          
          var currentHeight = content[0].scrollHeight;
          content.css('max-height', currentHeight + 'px');
          
          setTimeout(function() {
            content.css('max-height', '0px');
          }, 10);
          
          setTimeout(function() {
            content.css('display', 'none');
            content.css('max-height', '');
            card.removeClass('transitioning');
          }, 350);
          
        } else {
          card.addClass('expanded');
          expandIcon.removeClass('fa-chevron-down').addClass('fa-chevron-up');
          
          content.css({
            'display': 'block',
            'max-height': '0px',
            'overflow': 'hidden'
          });
          
          var targetHeight = content[0].scrollHeight;
          
          setTimeout(function() {
            content.css('max-height', targetHeight + 'px');
          }, 10);
          
          setTimeout(function() {
            content.css({
              'max-height': 'none',
              'overflow': 'visible'
            });
            card.removeClass('transitioning');
          }, 350);
        }
      });
      
      $(document).on('click', '.expand-indicator', function(e) {
        e.preventDefault();
        e.stopPropagation();
        $(this).closest('.card-header-modern').trigger('click');
      });
      
      $(document).on('click', '.card-content-modern', function(e) {
        e.stopPropagation();
      });
      
      $(document).on('mouseenter', '.indicator-card-modern', function() {
        if (!$(this).hasClass('transitioning')) {
          $(this).addClass('card-hover');
        }
      }).on('mouseleave', '.indicator-card-modern', function() {
        $(this).removeClass('card-hover');
      });
      
      // Helper function to extract target ID from potentially full URLs
      function extractTargetId(href) {
        if (!href) return null;
        
        var hashIndex = href.lastIndexOf('#');
        if (hashIndex === -1) return null;
        
        var targetId = href.substring(hashIndex + 1);
        
        if (targetId.indexOf('mandate_') === 0) {
          return targetId;
        }
        
        return null;
      }
      
      // Enhanced scroll navigation with debugging
      $(document).on('click', '.mandate-link', function(e) {
        e.preventDefault();
        e.stopPropagation();
        
        var targetHref = $(this).attr('href');
        var targetId = extractTargetId(targetHref);
        
        if (!targetId) {
          console.warn('Could not extract target ID from href:', targetHref);
          return;
        }
        
        var target = $('#' + targetId);
        
        if (target.length === 0) {
          console.warn('Target element not found:', targetId);
          return;
        }
        
        // Close expanded cards
        $('.indicator-card-modern.expanded').each(function() {
          $(this).removeClass('expanded')
            .find('.card-content-modern').css('display', 'none').end()
            .find('.expand-indicator i').removeClass('fa-chevron-up').addClass('fa-chevron-down');
        });
        
        // Force layout recalculation
        target[0].offsetHeight;
        
        // Calculate the absolute position we want to scroll to
        var headerOffset = 100;
        var elementPosition = target[0].getBoundingClientRect().top;
        var offsetPosition = elementPosition + window.pageYOffset - headerOffset;
        
        // Store the target position for verification
        var targetScrollPosition = offsetPosition;
        
        console.log('Scrolling to position:', targetScrollPosition);
        console.log('Current position:', window.pageYOffset);
        console.log('Element offset:', target.offset().top);
        
        // Method 1: Use the native smooth scroll with fallback
        try {
          window.scrollTo({
            top: targetScrollPosition,
            behavior: 'auto' // Changed to auto for immediate scroll
          });
        } catch (e) {
          // Fallback for older browsers
          window.scrollTo(0, targetScrollPosition);
        }
        
        // Aggressive verification and correction
        var scrollAttempts = 0;
        var maxAttempts = 10;
        
        var verifyScroll = setInterval(function() {
          scrollAttempts++;
          var currentScroll = window.pageYOffset || document.documentElement.scrollTop;
          var difference = Math.abs(currentScroll - targetScrollPosition);
          
          console.log('Attempt', scrollAttempts, '- Current:', currentScroll, 'Target:', targetScrollPosition, 'Diff:', difference);
          
          if (difference < 5) {
            // Success!
            clearInterval(verifyScroll);
            console.log('Scroll successful!');
            
            // Add highlight effect
            target.find('.mandate-section-header').addClass('highlight-flash');
            setTimeout(function() {
              target.find('.mandate-section-header').removeClass('highlight-flash');
              updateActiveMandateLink();
            }, 1500);
            
          } else if (scrollAttempts >= maxAttempts) {
            // Give up after max attempts
            clearInterval(verifyScroll);
            console.log('Max attempts reached, final position:', currentScroll);
            
            // Still add highlight even if position isn't perfect
            target.find('.mandate-section-header').addClass('highlight-flash');
            setTimeout(function() {
              target.find('.mandate-section-header').removeClass('highlight-flash');
              updateActiveMandateLink();
            }, 1500);
            
          } else {
            // Try again with different methods
            if (scrollAttempts % 3 === 1) {
              window.scrollTo(0, targetScrollPosition);
            } else if (scrollAttempts % 3 === 2) {
              document.documentElement.scrollTop = targetScrollPosition;
              document.body.scrollTop = targetScrollPosition;
            } else {
              // Try scrollIntoView
              target[0].scrollIntoView({ behavior: 'auto', block: 'start' });
              // Then adjust for header
              setTimeout(function() {
                var newPos = target[0].getBoundingClientRect().top + window.pageYOffset - headerOffset;
                window.scrollTo(0, newPos);
              }, 10);
            }
          }
        }, 50); // Check every 50ms
      });
      
      // URL-safe active mandate highlighting
      function updateActiveMandateLink() {
        var scrollPos = $(window).scrollTop() + 200;
        var activeLink = null;
        var closestDistance = Infinity;
        
        $('.mandate-link').each(function() {
          var link = $(this);
          var targetHref = link.attr('href');
          var targetId = extractTargetId(targetHref);
          
          if (!targetId) return;
          
          var target = $('#' + targetId);
          
          if (target.length && target.offset()) {
            var targetTop = target.offset().top;
            var targetBottom = targetTop + target.outerHeight();
            var distance = Math.abs(scrollPos - targetTop);
            
            if ((scrollPos >= targetTop - 100 && scrollPos <= targetBottom) || 
                (distance < closestDistance && scrollPos >= targetTop - 200)) {
              activeLink = link;
              closestDistance = distance;
            }
          }
        });
        
        $('.mandate-link').removeClass('active');
        if (activeLink) {
          activeLink.addClass('active');
        }
      }
      
      var scrollTimeout;
      $(window).on('scroll', function() {
        if (scrollTimeout) {
          clearTimeout(scrollTimeout);
        }
        scrollTimeout = setTimeout(updateActiveMandateLink, 100);
      });
      
      updateActiveMandateLink();
    });
    
    // Custom message handlers
    Shiny.addCustomMessageHandler('setupSelectButtons', function(data) {
      $('.select-indicator-btn, .select-indicator-btn-modern').each(function() {
        var btn = $(this);
        var isSelected = btn.data('selected') === true || btn.data('selected') === 'true';
        
        if (btn.hasClass('select-indicator-btn-modern')) {
          if (isSelected) {
            btn.removeClass('btn-unselected').addClass('btn-selected');
            btn.find('i').removeClass('fa-plus').addClass('fa-check');
            btn.find('.btn-text').text('Added');
          } else {
            btn.removeClass('btn-selected').addClass('btn-unselected');
            btn.find('i').removeClass('fa-check').addClass('fa-plus');
            btn.find('.btn-text').text('Add');
          }
        }
      });
      
      $('.indicator-card-modern').each(function() {
        var card = $(this);
        var content = card.find('.card-content-modern');
        var expandIcon = card.find('.expand-indicator i');
        
        card.removeClass('expanded transitioning');
        content.css({
          'display': 'none',
          'max-height': ''
        });
        expandIcon.removeClass('fa-chevron-up').addClass('fa-chevron-down');
      });
    });
    
    Shiny.addCustomMessageHandler('showLoading', function(show) {
      if (show) {
        $('#loading_spinner').fadeIn(200);
        $('#indicator_container').addClass('loading-state');
        $('.indicator-card-modern').addClass('loading-skeleton');
      } else {
        $('#loading_spinner').fadeOut(200);
        $('#indicator_container').removeClass('loading-state');
        $('.indicator-card-modern').removeClass('loading-skeleton');
      }
    });
    
    Shiny.addCustomMessageHandler('hideLoadingDelayed', function(data) {
      setTimeout(function() {
        $('#loading_spinner').fadeOut(200);
        $('#indicator_container').removeClass('loading-state');
        $('.indicator-card-modern').removeClass('loading-skeleton');
      }, data.delay || 300);
    });
    
    Shiny.addCustomMessageHandler('openPdfInNewTab', function(data) {
      var newWindow = window.open('', '_blank');
      newWindow.document.write(data.html);
      newWindow.document.close();
      newWindow.focus();
    });
  "))
}

      
      
