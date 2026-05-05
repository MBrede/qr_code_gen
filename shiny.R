library(shiny)
library(qrcode)
library(ggplot2)
library(png)
library(jpeg)
library(bslib)
library(stringr)
library(showtext)
library(grid)
library(lubridate)

all_fonts <- c(
  "Roboto", "Open Sans", "Lato", "Montserrat", "Playfair Display",
  "Raleway", "Poppins", "Inter", "Ubuntu", "Merriweather", "Oswald",
  "Bebas Neue", "Pacifico", "Caveat", "Fredoka", "IBM Plex Sans",
  "Courier Prime", "JetBrains Mono", "Source Code Pro"
)

showtext_auto()

available_fonts <- c("System Default" = "sans")

ui <- page_fillable(
  title = "QR Code Generator",
  theme = bs_theme(preset = "bootstrap"),
  
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = paste0(
        "https://fonts.googleapis.com/css2?",
        paste0("family=", gsub(" ", "+", all_fonts), collapse = "&"),
        "&display=swap"
      )
    )
  ),
  
  layout_sidebar(
    sidebar = sidebar(
      width = 350,
      
      radioButtons(
        "qr_type",
        "QR Code Type:",
        choices = list("Event" = "event", "Link" = "link", "vCard" = "vcard"),
        selected = "event"
      ),
      
      hr(),
      
      uiOutput("event_inputs"),
      uiOutput("link_inputs"),
      uiOutput("vcard_inputs"),
      
      hr(),
      
      # Independent Style Toggles
      radioButtons(
        "field_style", 
        "Field Points:", 
        choices = c("Square" = "square", "Rounded" = "rounded", "Molten" = "melted"),
        selected = "melted",
        inline = TRUE
      ),
      
      radioButtons(
        "finder_style", 
        "Orientation Squares:", 
        choices = c("Square" = "square","Molten" = "melted"),
        selected = "square",
        inline = TRUE
      ),
      
      conditionalPanel(
        condition = "input.field_style != 'square' || input.finder_style != 'square'",
        sliderInput("dot_radius", "Module Radius (Thickness):", 
                    min = 0.4, max = 0.6, value = 0.5, step = 0.01, ticks = FALSE)
      ),
      
      hr(),
      
      fileInput(
        "logo_upload",
        "Upload Logo (PNG/JPG):",
        accept = c("image/png", "image/jpeg")
      ),
      
      textAreaInput(
        "center_text",
        "Text for Center:",
        placeholder = "e.g., My\nSpecial\nMeetup",
        rows = 3
      ),
      
      numericInput(
        "text_wrap_width",
        "Characters per Line (for wrapping):",
        value = 15, min = 5, max = 50, step = 1
      ),
      
      fluidRow(
        column(8, selectizeInput(
          "google_font",
          "Google Font:",
          choices  = available_fonts,
          selected = "sans",
          options  = list(
            render = I("{
              option: function(item, escape) {
                var style = item.value === 'sans' ? '' : 'font-family:\"' + item.value + '\";font-size:15px;';
                return '<div style=\"' + style + 'padding:3px 8px;\">' + escape(item.label) + '</div>';
              },
              item: function(item, escape) {
                var style = item.value === 'sans' ? '' : 'font-family:\"' + item.value + '\";';
                return '<div style=\"' + style + '\">' + escape(item.label) + '</div>';
              }
            }")
          )
        )),
        column(4,
               tags$label("\u00a0", style = "display:block"),
               actionButton("load_fonts", "Load fonts", class = "btn-secondary btn-sm w-100")
        )
      ),
      uiOutput("font_load_status"),
      
      fluidRow(
        column(7, checkboxInput("override_fontsize", "Override font size", value = FALSE)),
        column(5, uiOutput("font_size_ui"))
      ),
      
      hr(),
      
      colourpicker::colourInput("text_color", "Text Color:",            value = "#000000"),
      colourpicker::colourInput("bg_color",   "Background Color:",      value = "#ffffff"),
      colourpicker::colourInput("fg_color",   "Foreground (QR) Color:", value = "#000000"),
      
      hr(),
      
      h4("Export Settings"),
      
      radioButtons(
        "export_format",
        "Export Format:",
        choices = list("PNG" = "png", "SVG" = "svg"),
        selected = "png"
      ),
      uiOutput("png_quality_warning"),
      
      conditionalPanel(
        condition = "input.export_format == 'png'",
        numericInput(
          "pixel_height",
          "Pixel Height:",
          value = 512, min = 100, max = 4000, step = 16
        )
      ),
      downloadButton("download_qr", "Download QR Code", class = "btn-primary w-100 mb-2"),
      uiOutput("ics_download_ui")
    ),
    
    navset_card_tab(
      title = "Preview",
      nav_panel("QR Code",  plotOutput("qr_preview", height = "600px")),
      nav_panel("Settings", verbatimTextOutput("qr_info"))
    )
  )
)

server <- function(input, output, session) {
  
  loaded_fonts <- reactiveVal(available_fonts)
  fonts_loaded <- reactiveVal(FALSE)
  
  observeEvent(input$load_fonts, {
    req(!fonts_loaded())
    withProgress(message = "Loading Google Fonts...", value = 0, {
      n         <- length(all_fonts)
      new_fonts <- available_fonts
      for (i in seq_along(all_fonts)) {
        f <- all_fonts[i]
        incProgress(1 / n, detail = f)
        tryCatch({
          font_add_google(f, f)
          new_fonts[f] <- f
        }, error = function(e) {})
      }
      loaded_fonts(new_fonts)
      fonts_loaded(TRUE)
      updateSelectizeInput(session, "google_font", choices = new_fonts, selected = if ("Roboto" %in% new_fonts) "Roboto" else "sans")
    })
  })
  
  output$font_load_status <- renderUI({
    if (fonts_loaded()) {
      n_loaded <- length(loaded_fonts()) - 1
      tags$small(class = "text-success d-block mb-2", paste0("\u2713 ", n_loaded, " Google fonts loaded"))
    }
  })
  
  output$font_size_ui <- renderUI({
    if (isTRUE(input$override_fontsize))
      numericInput("manual_fontsize", "Size:", value = 12, min = 1, max = 200, step = 1)
  })
  
  generate_ics_content <- function() {
    req(input$qr_type == "event", input$event_date, input$event_title)
    
    fmt_ics_time <- function(date, h=0, m=0, allday=FALSE) {
      if(allday) return(format(as.Date(date), "%Y%m%d"))
      format(as.POSIXct(paste0(date, " ", sprintf("%02d:%02d:00", h, m)), tz="UTC"), "%Y%m%dT%H%M%SZ")
    }
    
    start_str <- fmt_ics_time(input$event_date, input$event_start_h %||% 0, input$event_start_m %||% 0, input$event_allday)
    end_date <- if(isTRUE(input$event_sep_end)) input$event_end_date else input$event_date
    end_str <- fmt_ics_time(end_date, input$event_end_h %||% 0, input$event_end_m %||% 0, input$event_allday)
    
    paste0(
      "BEGIN:VCALENDAR\n",
      "VERSION:2.0\n",
      "PRODID:-//QR Gen//R Shiny//EN\n",
      "BEGIN:VEVENT\n",
      "SUMMARY:", input$event_title, "\n",
      "DTSTART:", start_str, "\n",
      "DTEND:", end_str, "\n",
      "LOCATION:", input$event_location %||% "", "\n",
      "END:VEVENT\n",
      "END:VCALENDAR"
    )
  }
  
  output$ics_download_ui <- renderUI({
    if(input$qr_type == "event") {
      downloadButton("download_ics", "Download .ics File", class = "btn-outline-info w-100")
    }
  })
  
  output$download_ics <- downloadHandler(
    filename = function() { paste0(URLencode(input$event_title), ".ics") },
    content = function(file) { writeLines(generate_ics_content(), file) }
  )
  
  wrap_text <- function(text, width = 15) {
    if (text == "") return("")
    lines         <- strsplit(text, "\n")[[1]]
    wrapped_lines <- character()
    for (line in lines) {
      if (nchar(line) <= width) {
        wrapped_lines <- c(wrapped_lines, line)
      } else {
        words        <- strsplit(line, " ")[[1]]
        current_line <- ""
        for (word in words) {
          if (nchar(current_line) + nchar(word) + 1 <= width) {
            current_line <- if (current_line == "") word else paste(current_line, word)
          } else {
            if (current_line != "") wrapped_lines <- c(wrapped_lines, current_line)
            current_line <- word
          }
        }
        if (current_line != "") wrapped_lines <- c(wrapped_lines, current_line)
      }
    }
    paste(wrapped_lines, collapse = "\n")
  }
  
  calculate_font_size <- function(text, canvas_in, base_size = 10) {
    lines     <- strsplit(text, "\n")[[1]]
    num_lines <- length(lines)
    max_chars <- max(nchar(lines), 1)
    size_reduction <- (num_lines - 1) * 0.8 + (max_chars - 5) * 0.15
    raw <- max(base_size - size_reduction, 1)
    raw * (canvas_in / 1.7)
  }
  
  resolve_font_size <- function(wrapped_text, canvas_in) {
    if (isTRUE(input$override_fontsize) && !is.null(input$manual_fontsize))
      input$manual_fontsize
    else
      calculate_font_size(wrapped_text, canvas_in = canvas_in)
  }
  
  draw_qr <- function(qr_obj, bg_color, fg_color, wrapped_text, font_size, font_family, 
                      text_color, field_style = "melted", finder_style = "square", dot_radius = 0.5, logo_info = NULL) {
    
    mat <- as.matrix(qr_obj)
    n <- ncol(mat)
    
    grid.newpage()
    grid.rect(gp = gpar(fill = bg_color, col = NA))
    pushViewport(viewport(width = unit(0.9, "snpc"), height = unit(0.9, "snpc")))
    
    # 1. DYNAMIC MASK: Find the exact bounds of the QR code (ignoring the quiet zone margins)
    m_top    <- min(which(rowSums(mat) > 0))
    m_bottom <- max(which(rowSums(mat) > 0))
    m_left   <- min(which(colSums(mat) > 0))
    m_right  <- max(which(colSums(mat) > 0))
    
    # Create the mask for the 3 main 7x7 Finder Patterns exactly where they are
    finder_mask <- matrix(FALSE, nrow = n, ncol = n)
    finder_mask[m_top:(m_top+6), m_left:(m_left+6)]         <- TRUE # Top-Left
    finder_mask[m_top:(m_top+6), (m_right-6):m_right]       <- TRUE # Top-Right
    finder_mask[(m_bottom-6):m_bottom, m_left:(m_left+6)]   <- TRUE # Bottom-Left
    
    mat_field <- mat & !finder_mask
    mat_finder <- mat & finder_mask
    
    # 2. Rendering helper for a target matrix and style
    draw_modules <- function(target_mat, style) {
      if (!any(target_mat)) return()
      
      coords <- which(target_mat, arr.ind = TRUE)
      
      # Circles logic (applies to both 'rounded' and 'melted')
      if (style %in% c("rounded", "melted")) {
        grid.circle(
          x = (coords[, "col"] - 0.5) / n, 
          y = (n - coords[, "row"] + 0.5) / n, 
          r = dot_radius / n, default.units = "npc", gp = gpar(fill = fg_color, col = NA)
        )
      }
      
      # Bridges logic (applies ONLY to 'melted')
      if (style == "melted") {
        h_pairs <- which(target_mat[, -n] & target_mat[, -1], arr.ind = TRUE)
        if (nrow(h_pairs) > 0) {
          grid.rect(
            x = h_pairs[, "col"] / n, 
            y = (n - h_pairs[, "row"] + 0.5) / n, 
            width = 1.05 / n, height = (dot_radius * 2) / n, default.units = "npc", gp = gpar(fill = fg_color, col = NA)
          )
        }
        v_pairs <- which(target_mat[-n, ] & target_mat[-1, ], arr.ind = TRUE)
        if (nrow(v_pairs) > 0) {
          grid.rect(
            x = (v_pairs[, "col"] - 0.5) / n, 
            y = (n - v_pairs[, "row"]) / n, 
            width = (dot_radius * 2) / n, height = 1.05 / n, default.units = "npc", gp = gpar(fill = fg_color, col = NA)
          )
        }
        q_pairs <- which(target_mat[-n, -n] & target_mat[-n, -1] & target_mat[-1, -n] & target_mat[-1, -1], arr.ind = TRUE)
        if (nrow(q_pairs) > 0) {
          grid.rect(
            x = q_pairs[, "col"] / n, 
            y = (n - q_pairs[, "row"]) / n, 
            width = 1.05 / n, height = 1.05 / n, default.units = "npc", gp = gpar(fill = fg_color, col = NA)
          )
        }
      }
      
      # Square logic (applies ONLY to 'square')
      if (style == "square") {
        grid.rect(
          x = (coords[, "col"] - 0.5) / n, 
          y = (n - coords[, "row"] + 0.5) / n, 
          width = 1 / n, height = 1 / n, default.units = "npc", gp = gpar(fill = fg_color, col = NA)
        )
      }
    }
    
    # Render the Field and Finders independently
    draw_modules(mat_field, field_style)
    draw_modules(mat_finder, finder_style)
    
    # --- 3. OVERLAY TEXT ---
    if (nchar(wrapped_text) > 0) {
      box_size <- font_size * 5
      # We consider the box rounded if ANY part of the QR is non-square
      if (field_style != "square" || finder_style != "square") {
        grid.roundrect(
          x = 0.5, y = 0.5,
          width = unit(box_size, "points"), height = unit(box_size, "points"),
          r = unit(box_size * 0.1, "points"), default.units = "npc", gp = gpar(col = NA, fill = bg_color)
        )
      } else {
        grid.rect(
          x = 0.5, y = 0.5,
          width = unit(box_size, "points"), height = unit(box_size, "points"),
          default.units = "npc", gp = gpar(col = NA, fill = bg_color)
        )
      }
      grid.text(
        label = wrapped_text, x = 0.5, y = 0.5, default.units = "npc",
        gp = gpar(fontsize = font_size, col = text_color, fontfamily = font_family, lineheight = 0.9)
      )
    }
    
    # --- 4. OVERLAY LOGO ---
    if (!is.null(logo_info)) {
      is_png <- grepl("\\.png$", tolower(logo_info$name))
      img <- tryCatch({
        if (is_png) png::readPNG(logo_info$datapath) else jpeg::readJPEG(logo_info$datapath)
      }, error = function(e) NULL)
      
      if (!is.null(img)) {
        max_npc <- 0.25 
        image_height <- dim(img)[1]
        image_width <- dim(img)[2]
        ar <- image_width / image_height
        
        if (ar >= 1) {
          final_w_npc <- max_npc
          final_h_npc <- max_npc / ar
        } else {
          final_h_npc <- max_npc
          final_w_npc <- max_npc * ar
        }
        
        is_rounded <- (field_style != "square" || finder_style != "square")
        
        if (is_rounded) {
          grid.roundrect(
            x = 0.5, y = 0.5,
            width = unit(final_w_npc * 1.1, "npc"), height = unit(final_h_npc * 1.1, "npc"),
            r = unit(min(final_w_npc, final_h_npc) * 0.1, "npc"), 
            default.units = "npc", gp = gpar(col = NA, fill = bg_color)
          )
        } else {
          grid.rect(
            x = 0.5, y = 0.5,
            width = unit(final_w_npc * 1.1, "npc"), height = unit(final_h_npc * 1.1, "npc"),
            default.units = "npc", gp = gpar(col = NA, fill = bg_color)
          )
        }
        
        grid.raster(img, x = 0.5, y = 0.5, width = unit(final_w_npc, "npc"), height = unit(final_h_npc, "npc"))
        
        if (is_rounded) {
          grid.roundrect(
            x = 0.5, y = 0.5,
            width = unit(final_w_npc, "npc"), height = unit(final_h_npc, "npc"),
            r = unit(min(final_w_npc, final_h_npc) * 0.1, "npc"), 
            default.units = "npc", gp = gpar(col = bg_color, fill = NA, lwd = 10) 
          )
        }
      }
    }
    popViewport()
  }
  
  output$event_inputs <- renderUI({
    if (input$qr_type == "event") {
      list(
        dateInput("event_date", "Event Date:", value = lubridate::today()),
        checkboxInput("event_allday",  "All day event",     value = FALSE),
        checkboxInput("event_sep_end", "Separate end date", value = FALSE),
        conditionalPanel(condition = "input.event_sep_end == true", dateInput("event_end_date", "End Date:", value = lubridate::today())),
        conditionalPanel(
          condition = "input.event_allday == false",
          fluidRow(
            column(6, numericInput("event_start_h", "Start Hour:", value = 10, min = 0, max = 23)),
            column(6, numericInput("event_start_m", "Start Min:",  value = 0,  min = 0, max = 59))
          ),
          fluidRow(
            column(6, numericInput("event_end_h", "End Hour:", value = 18, min = 0, max = 23)),
            column(6, numericInput("event_end_m", "End Min:",  value = 0,  min = 0, max = 59))
          )
        ),
        textInput("event_title", "Event Title:", placeholder = "Workshop Title"),
        textInput("event_location", "Location:", placeholder = "Event Location")
      )
    }
  })
  
  output$link_inputs <- renderUI({
    if (input$qr_type == "link") list(textInput("link_url", "URL:", placeholder = "https://example.com"))
  })
  
  output$vcard_inputs <- renderUI({
    if (input$qr_type == "vcard") {
      list(
        textInput("vcard_name", "Name:", placeholder = "Doe"),
        textInput("vcard_given", "Given:", placeholder = "John"),
        textInput("vcard_phone", "Phone:", placeholder = "+49..."),
        textInput("vcard_email", "Email:", placeholder = "john@example.com"),
        textInput("vcard_org", "Organization:", placeholder = "Company"),
        textInput("vcard_url", "Website:", placeholder = "https://example.com")
      )
    }
  })
  
  generate_qr <- reactive({
    req(input$qr_type)
    if (input$qr_type == "event") {
      req(input$event_date, input$event_title)
      allday <- isTRUE(input$event_allday)
      end_date <- if (isTRUE(input$event_sep_end)) req(input$event_end_date) else input$event_date
      if (allday) {
        start_time <- as.POSIXct(paste0(input$event_date, " 00:00:00"), tz = "UTC")
        end_time   <- as.POSIXct(paste0(end_date, " 23:59:00"), tz = "UTC")
      } else {
        start_time <- as.POSIXct(paste0(input$event_date, " ", sprintf("%02d:%02d:00", input$event_start_h %||% 0, input$event_start_m %||% 0)), tz = "UTC")
        end_time   <- as.POSIXct(paste0(end_date, " ", sprintf("%02d:%02d:00", input$event_end_h %||% 0,   input$event_end_m %||% 0)),   tz = "UTC")
      }
      qrcode::qr_event(start = start_time, end = end_time, title = input$event_title, location = input$event_location %||% "", ecl = "H")
    } else if (input$qr_type == "link") {
      req(input$link_url); qrcode::qr_code(input$link_url, ecl = "H")
    } else if (input$qr_type == "vcard") {
      req(input$vcard_name)
      qrcode::qr_vcard(family = input$vcard_name, given = input$vcard_given, phone = input$vcard_phone %||% NA, email = input$vcard_email %||% NA, organization = input$vcard_org %||% NA, url = input$vcard_url %||% NA, ecl = "H")
    }
  })
  
  output$qr_preview <- renderPlot({
    qr_obj <- generate_qr()
    req(qr_obj)
    
    logo_info <- input$logo_upload
    wrapped_text <- if (is.null(logo_info)) wrap_text(input$center_text, width = input$text_wrap_width) else ""
    font_size <- resolve_font_size(wrapped_text, canvas_in = 6)
    
    draw_qr(
      qr_obj, input$bg_color, input$fg_color, wrapped_text, font_size, 
      input$google_font, input$text_color, 
      field_style = input$field_style %||% "melted",
      finder_style = input$finder_style %||% "square",
      dot_radius = input$dot_radius %||% 0.5,
      logo_info = logo_info
    )
  }, res = 96)
  
  output$qr_info <- renderText({
    wrapped_text <- wrap_text(input$center_text, width = input$text_wrap_width)
    paste(
      "QR Type:", input$qr_type, 
      "\nField Style:", input$field_style,
      "\nFinder Style:", input$finder_style,
      if(input$field_style != "square" || input$finder_style != "square") paste("\nRadius:", input$dot_radius %||% 0.5) else "",
      "\nExport Format:", toupper(input$export_format)
    )
  })
  
  output$download_qr <- downloadHandler(
    filename = function() { paste0("qrcode_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$export_format) },
    content = function(file) {
      qr_obj <- generate_qr()
      logo_info <- input$logo_upload
      wrapped_text <- if (is.null(logo_info)) wrap_text(input$center_text, width = input$text_wrap_width) else ""
      canvas_in <- input$pixel_height / 96
      font_size <- resolve_font_size(wrapped_text, canvas_in = canvas_in)
      
      if (input$export_format == "png") {
        png(filename = file, width = input$pixel_height, height = input$pixel_height, bg = input$bg_color, res = 96)
      } else {
        svg(filename = file, width = input$pixel_height/96, height = input$pixel_height/96, bg = input$bg_color)
      }
      
      draw_qr(
        qr_obj, input$bg_color, input$fg_color, wrapped_text, font_size, 
        input$google_font, input$text_color, 
        field_style = input$field_style %||% "melted",
        finder_style = input$finder_style %||% "square",
        dot_radius = input$dot_radius %||% 0.5,
        logo_info = logo_info
      )
      
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)