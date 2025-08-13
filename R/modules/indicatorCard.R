# R/modules/indicatorCard.R - Module for individual indicator cards

# Create an accordion panel for an indicator
indicatorCardAccordion <- function(id, indicator, sector_colors, is_selected = FALSE) {
  ns <- NS(id)
  
  sector_color <- sector_colors[[indicator$main_sector]]
  
  # Create organization logos
  img_tags <- div(
    style = "display: flex; justify-content: flex-start; gap: 10px; align-items: center; margin-top: 10px;",
    if (!is.na(indicator$IMF)) tags$img(src = "imf_logo.png", height = "44px"),
    if (!is.na(indicator$GPFI)) tags$img(src = "gpfi_logo.png", height = "44px"),
    if (!is.na(indicator$AFI)) tags$img(src = "afi_logo.png", height = "44px"), 
    if (!is.na(indicator$WEF)) tags$img(src = "wef_logo.png", height = "44px")
  )
  
  # Check if sources_any field exists and has content
  show_sources <- if ("sources_any" %in% names(indicator)) {
    !is.na(indicator$sources_any) && !is.null(indicator$sources_any) && indicator$sources_any != ""
  } else {
    # Fallback: check if any of the individual source indicators are 1
    (indicator$IMF == 1) || (indicator$GPFI == 1) || (indicator$AFI == 1) || (indicator$WEF == 1)
  }
  
  accordion_panel(
    value = indicator$indicator_id,
    title = div(
      span(
        span(indicator$indicator_name, 
             style = "background-color: white; display: inline-block; padding:2px; border-radius: 4px; color:black; font-weight: bold; font-size: 14px"), 
        span(indicator$main_objectives, 
             style = "background-color: #E5E7E6; display: inline-block; padding:2px; border-radius: 4px; color:black; font-weight: normal; font-size: 12px;"), 
        span(indicator$main_sector, 
             style = paste0("background-color: ", sector_color, "; display: inline-block; padding:2px; border-radius: 4px; color:black; font-weight: normal; font-size: 12px")), 
        if (indicator$high_priority == "High priority") {
          tags$i(class = "fas fa-star", style = "color: gold; margin-left: 8px; font-size: 14px;")
        }
      )
    ),
    
    div(
      class = "card-body",
      style = "font-size: 14px;",
      
      render_disagg_table_vertical(indicator, 
                                   columns = c("main_mandate", "secondary_mandates", "main_objectives", "secondary_objectives", "main_sector", 
                                               "secondary_sectors", "indicator_description", "indicator_long_description", "gender_questions", 
                                               "unit_of_analysis", "measurement_type", "formula1", "formula2", "formula3"), 
                                   pre_columns = c("formula1", "formula2", "formula3")),

      render_disagg_table_generalized(indicator, c("essential_disagg", "nonessential_disagg")),
      
      render_disagg_table_generalized(indicator, c("use_cases"), delimiter = ",", mapping = USE_CASES),
      
      # Conditional rendering of sources section
      if (show_sources) {
        tagList(
          br(),
          p(strong("Equivalent or related indicators can also be found from the following sources:"), img_tags)
        )
      },
      
      render_disagg_table_vertical(indicator, columns = c("GPFI", "IMF",  "AFI", "WEF"), delimiter = ";"),
      
      
      # Action button
      div(
        style = "display: flex; justify-content: flex-end; margin-top: 10px;",
        tags$button(
          id = paste0("select_", indicator$indicator_id),
          class = paste("btn", 
                        if (is_selected) "btn-sm btn-success select-indicator-btn" 
                        else "btn-sm btn-outline-primary select-indicator-btn"),
          `data-indicator-id` = indicator$indicator_id,
          `data-selected` = tolower(as.character(is_selected)),
          icon(if (is_selected) "check-circle" else "plus-circle", lib = "font-awesome"),
          span(class = "btn-text", 
               if (is_selected) " Remove from list" else " Add to list")
        )
      )
    )
  )
}


# Original working indicatorCardJS function from R/modules/indicatorCard.R

indicatorCardJS <- function() {
  tags$script(HTML("
        // Setup event delegation for select buttons
        Shiny.addCustomMessageHandler('setupSelectButtons', function(data) {
            // Remove any existing delegated handlers
            $(document).off('click', '.select-indicator-btn');
            
            // Add new delegated handler
            $(document).on('click', '.select-indicator-btn', function(e) {
                e.preventDefault();
                
                var btn = $(this);
                var indicatorId = btn.data('indicator-id');
                var isSelected = btn.data('selected') === true || btn.data('selected') === 'true';
                
                // Update button immediately for instant feedback
                if (isSelected) {
                    // Currently selected, so deselect
                    btn.removeClass('btn-success').addClass('btn-outline-primary');
                    btn.find('i').removeClass('fa-check-circle').addClass('fa-plus-circle');
                    btn.find('.btn-text').text('Add to list');
                    btn.data('selected', false);
                    
                    // Send to Shiny
                    Shiny.setInputValue('indicator_selected', {
                        id: indicatorId,
                        action: 'remove'
                    }, {priority: 'event'});
                } else {
                    // Currently not selected, so select
                    btn.removeClass('btn-outline-primary').addClass('btn-success');
                    btn.find('i').removeClass('fa-plus-circle').addClass('fa-check-circle');
                    btn.find('.btn-text').text('Remove from list');
                    btn.data('selected', true);
                    
                    // Send to Shiny
                    Shiny.setInputValue('indicator_selected', {
                        id: indicatorId,
                        action: 'add'
                    }, {priority: 'event'});
                }
            });
        });
        
        // Loading indicator handler
        Shiny.addCustomMessageHandler('showLoading', function(show) {
            if (show) {
                $('#loading_spinner').fadeIn(200);
                $('#indicator_container').addClass('loading');
            } else {
                $('#loading_spinner').fadeOut(200);
                $('#indicator_container').removeClass('loading');
            }
        });
        
        // Delayed hide loading handler
        Shiny.addCustomMessageHandler('hideLoadingDelayed', function(data) {
            setTimeout(function() {
                $('#loading_spinner').fadeOut(200);
                $('#indicator_container').removeClass('loading');
            }, data.delay || 300);
        });
        
        // Open PDF in new tab handler
        Shiny.addCustomMessageHandler('openPdfInNewTab', function(data) {
            var newWindow = window.open('', '_blank');
            newWindow.document.write(data.html);
            newWindow.document.close();
            
            // Focus on the new window
            newWindow.focus();
        });
        
        // Print report handler (keep existing for compatibility)
        Shiny.addCustomMessageHandler('printReport', function(data) {
            var reportContent = document.getElementById(data.id);
            if (reportContent) {
                var printWindow = window.open('', '_blank');
                printWindow.document.write('<html><head><title>RGDD Report</title>');
                printWindow.document.write('<style>' + getReportStyles() + '</style>');
                printWindow.document.write('</head><body>');
                printWindow.document.write(reportContent.innerHTML);
                printWindow.document.write('</body></html>');
                printWindow.document.close();
                
                // Wait for content to load then print
                printWindow.onload = function() {
                    printWindow.print();
                };
            }
        });
        
        // Get report styles for printing
        function getReportStyles() {
            return `
                @page { size: A4; margin: 2cm; }
                body { font-family: Arial, sans-serif; line-height: 1.6; color: #333; }
                .header { text-align: center; margin-bottom: 30px; padding-bottom: 20px; border-bottom: 2px solid #007bff; }
                .header h1 { color: #007bff; margin-bottom: 10px; }
                .summary { background-color: #f8f9fa; padding: 15px; margin-bottom: 30px; }
                .indicator { page-break-inside: avoid; margin-bottom: 30px; border: 1px solid #ddd; padding: 20px; }
                .indicator h3 { color: #333; border-bottom: 1px solid #ddd; padding-bottom: 10px; }
                .sector-badge { display: inline-block; padding: 4px 8px; border-radius: 4px; font-size: 12px; }
                .objective-badge { display: inline-block; padding: 4px 8px; border-radius: 4px; font-size: 12px; background-color: #E5E7E6; }
                .field { margin-bottom: 12px; }
                .field-label { font-weight: bold; color: #555; }
                .comment-box { background-color: #f0f0f0; padding: 10px; margin-top: 10px; font-style: italic; }
                @media print { body { font-size: 11pt; } }
            `;
        }
        
        // Highlight active mandate link on scroll
        $(document).ready(function() {
            // Show initial loading spinner
            $('#loading_spinner').show();
            
            function highlightActiveMandate() {
                var scrollPos = $(window).scrollTop() + 100;
                
                $('.mandate-link').each(function() {
                    var currLink = $(this);
                    var refElement = $(currLink.attr('href'));
                    
                    if (refElement.length && refElement.position() && 
                        refElement.position().top <= scrollPos &&
                        refElement.position().top + refElement.height() > scrollPos) {
                        $('.mandate-link').removeClass('active');
                        currLink.addClass('active');
                    }
                });
            }
            
            // Highlight on scroll
            $(window).scroll(highlightActiveMandate);
            
            // Highlight on load
            highlightActiveMandate();
            
            // Smooth scroll when clicking mandate links
            $('.mandate-link').on('click', function(e) {
                e.preventDefault();
                var target = $(this).attr('href');
                $('html, body').animate({
                    scrollTop: $(target).offset().top - 80
                }, 500);
            });
        });
    "))
}