library(shiny)
library(qrcode)
library(ggplot2)
library(png)
library(bslib)
library(stringr)
library(showtext)
library(grid)

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
      downloadButton("download_qr", "Download QR Code", class = "btn-primary w-100")
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
      
      updateSelectizeInput(
        session, "google_font",
        choices  = new_fonts,
        selected = if ("Roboto" %in% new_fonts) "Roboto" else "sans",
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
      )
    })
  })
  
  output$font_load_status <- renderUI({
    if (fonts_loaded()) {
      n_loaded <- length(loaded_fonts()) - 1
      tags$small(class = "text-success d-block mb-2",
                 paste0("\u2713 ", n_loaded, " Google fonts loaded"))
    }
  })
  
  output$font_size_ui <- renderUI({
    if (isTRUE(input$override_fontsize))
      numericInput("manual_fontsize", "Size:", value = 12, min = 1, max = 200, step = 1)
  })
  
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
    max_chars <- max(nchar(lines))
    
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
  
  draw_qr <- function(qr_obj, bg_color, fg_color, wrapped_text,
                      font_size, font_family, text_color) {
    plot(qr_obj, col = c(bg_color, fg_color))
    
    if (nchar(wrapped_text) > 0) {
      grid.rect(x=.5, y = .5,
                width=unit(font_size*5, "points"), 
                height=unit(font_size*5, "points"),
                gp = gpar(
                  col=NA,
                  fill=bg_color
                ))
      grid.text(
        label = wrapped_text,
        x     = 0.5,
        y     = 0.5,
        gp    = gpar(
          fontsize   = font_size,
          col        = text_color,
          fontfamily = font_family,
          lineheight = 0.9
        )
      )
    }
  }
  
  output$event_inputs <- renderUI({
    if (input$qr_type == "event") {
      list(
        dateInput("event_date", "Event Date:", value = lubridate::today()),
        checkboxInput("event_allday",  "All day event",     value = FALSE),
        checkboxInput("event_sep_end", "Separate end date", value = FALSE),
        conditionalPanel(
          condition = "input.event_sep_end == true",
          dateInput("event_end_date", "End Date:", value = lubridate::today())
        ),
        conditionalPanel(
          condition = "input.event_allday == false",
          fluidRow(
            column(6, numericInput("event_start_h", "Start Hour:", value = 10, min = 0, max = 23, step = 1)),
            column(6, numericInput("event_start_m", "Start Min:",  value = 0,  min = 0, max = 59, step = 1))
          ),
          fluidRow(
            column(6, numericInput("event_end_h", "End Hour:", value = 18, min = 0, max = 23, step = 1)),
            column(6, numericInput("event_end_m", "End Min:",  value = 0,  min = 0, max = 59, step = 1))
          )
        ),
        textInput("event_title",    "Event Title:", placeholder = "Workshop Title"),
        textInput("event_location", "Location:",    placeholder = "Event Location")
      )
    }
  })
  
  output$link_inputs <- renderUI({
    if (input$qr_type == "link")
      list(textInput("link_url", "URL:", placeholder = "https://example.com"))
  })
  
  output$vcard_inputs <- renderUI({
    if (input$qr_type == "vcard") {
      list(
        textInput("vcard_name",  "Name:",         placeholder = "Doe"),
        textInput("vcard_given", "Given:",        placeholder = "John"),
        textInput("vcard_phone", "Phone:",        placeholder = "+49 123 456789"),
        textInput("vcard_email", "Email:",        placeholder = "john@example.com"),
        textInput("vcard_org",   "Organization:", placeholder = "Company Name"),
        textInput("vcard_url",   "Website:",      placeholder = "https://example.com")
      )
    }
  })
  
  generate_qr <- reactive({
    req(input$qr_type)
    
    qr_obj <- NULL
    
    if (input$qr_type == "event") {
      req(input$event_date, input$event_title)
      
      allday   <- isTRUE(input$event_allday)
      sep_end  <- isTRUE(input$event_sep_end)
      end_date <- if (sep_end) req(input$event_end_date) else input$event_date
      
      if (allday) {
        start_time <- as.POSIXct(paste0(input$event_date, " 00:00:00"), tz = "UTC")
        end_time   <- as.POSIXct(paste0(end_date,         " 23:59:00"), tz = "UTC")
      } else {
        req(input$event_start_h, input$event_start_m, input$event_end_h, input$event_end_m)
        start_time <- as.POSIXct(paste0(input$event_date, " ", sprintf("%02d:%02d:00", input$event_start_h, input$event_start_m)), tz = "UTC")
        end_time   <- as.POSIXct(paste0(end_date,         " ", sprintf("%02d:%02d:00", input$event_end_h,   input$event_end_m)),   tz = "UTC")
      }
      
      qr_obj <- qrcode::qr_event(
        start    = start_time,
        end      = end_time,
        title    = input$event_title,
        location = input$event_location %||% "",
        ecl      = "H"
      )
    } else if (input$qr_type == "link") {
      req(input$link_url)
      qr_obj <- qrcode::qr_code(input$link_url, ecl = "H")
    } else if (input$qr_type == "vcard") {
      req(input$vcard_name)
      qr_obj <- qrcode::qr_vcard(
        family       = input$vcard_name,
        given        = input$vcard_given,
        phone        = input$vcard_phone %||% NA,
        email        = input$vcard_email %||% NA,
        organization = input$vcard_org %||% NA,
        url          = input$vcard_url %||% NA,
        ecl          = "H"
      )
    }
    
    qr_obj
  })
  
  final_qr <- reactive({
    qr_obj <- generate_qr()
    req(qr_obj)
    
    if (!is.null(input$logo_upload)) {
      tryCatch({
        qr_obj <- qrcode::add_logo(qr_obj, input$logo_upload$datapath)
      }, error = function(e) {
        showNotification("Error adding logo", type = "error")
      })
    }
    
    qr_obj
  })
  
  output$qr_preview <- renderPlot({
    qr_obj <- final_qr()
    req(qr_obj)
    
    wrapped_text <- if (is.null(input$logo_upload))
      wrap_text(input$center_text, width = input$text_wrap_width) else ""
    
    font_size <- resolve_font_size(wrapped_text, canvas_in = 6)
    
    draw_qr(qr_obj, input$bg_color, input$fg_color,
            wrapped_text, font_size, input$google_font, input$text_color)
  }, res = 96)
  
  output$qr_info <- renderText({
    wrapped_text <- wrap_text(input$center_text, width = input$text_wrap_width)
    canvas_in    <- input$pixel_height / 300
    font_size    <- resolve_font_size(wrapped_text, canvas_in = canvas_in)
    
    paste(
      "QR Type:", input$qr_type, "\n",
      "Background Color:", input$bg_color, "\n",
      "Foreground Color:", input$fg_color, "\n",
      "Text Color:", input$text_color, "\n",
      "Google Font:", input$google_font, "\n",
      "Text Wrap Width:", input$text_wrap_width, "chars\n",
      "Font Size:", round(font_size, 1), if (isTRUE(input$override_fontsize)) "(manual)" else "(auto)", "\n",
      "Export Size:", input$pixel_height, "px\n",
      "Export Format:", toupper(input$export_format), "\n\n",
      "Wrapped Text:\n", wrapped_text
    )
  })
  
  output$png_quality_warning <- renderUI({
    if (!is.null(input$export_format) && input$export_format == "png") {
      tags$div(
        class = "alert alert-warning p-2 mb-2",
        tags$small(
          tags$b("PNG sharpness scales with size."),
          " Use 1500px+ for crisp results. SVG is resolution-independent."
        )
      )
    }
  })
  
  output$download_qr <- downloadHandler(
    filename = function() {
      paste0("qrcode_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$export_format)
    },
    content = function(file) {
      qr_obj <- final_qr()
      req(qr_obj)
      
      wrapped_text <- if (is.null(input$logo_upload))
        wrap_text(input$center_text, width = input$text_wrap_width) else ""
      
      canvas_in <- input$pixel_height / 96
      font_size <- resolve_font_size(wrapped_text, canvas_in = canvas_in)
      
      if (input$export_format == "png") {
        showtext_opts(dpi = 96)
        png(filename = file, width = input$pixel_height, height = input$pixel_height,
            bg = input$bg_color, res = 96)
        draw_qr(qr_obj, input$bg_color, input$fg_color,
                wrapped_text, font_size, input$google_font, input$text_color)
        dev.off()
        showtext_opts(dpi = 96)
      } else if (input$export_format == "svg") {
        svg(filename = file,
            width  = input$pixel_height/96,
            height = input$pixel_height/96,
            bg     = input$bg_color)
        draw_qr(qr_obj, input$bg_color, input$fg_color,
                wrapped_text, font_size, input$google_font, input$text_color)
        dev.off()
      }
    }
  )
}

shinyApp(ui = ui, server = server)