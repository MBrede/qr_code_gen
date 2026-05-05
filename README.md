# QR Code Generator

A feature-rich R Shiny application for generating highly customizable, vector-perfect QR codes. 

**⚠️ Vibecoding Warning:** This application was entirely "vibecoded" into existence through iterative LLM prompting. It relies on custom grid graphic vector math achieved via trial and error. While highly functional, do not expect heavily documented, strictly typed, or test-driven code.

## Features

* **Custom `grid` Engine:** Bypasses standard base-R plots for complete control over vector aesthetics.
* **Independent Module Styling:** Toggle Field Points and Orientation Squares separately between Square, Rounded, or Melted styles.
* **Vector-Perfect "Melt" Effect:** Bridges adjacent QR modules mathematically without rasterizing, keeping SVG exports infinitely scalable and crisp.
* **Center Branding:** Add wrapped text (integrated with Google Fonts) or upload a company logo. The app automatically preserves the logo's aspect ratio and applies a clean "faux-clip" rounded border.
* **Event Support:** Generate calendar-ready Event QR codes alongside an instant `.ics` file download.
* **Export Options:** Download as SVG or high-resolution PNG with custom pixel scaling.
* **Total Color Control:** Independently select background, foreground, and text colors.

## How to Run

1. **Install Dependencies:**
   Run this in your R console to install the required packages:
   ```R
   install.packages(c("shiny", "qrcode", "ggplot2", "png", "jpeg", "bslib", "stringr", "showtext", "colourpicker", "lubridate"))
   ```

2. **Run the App:**
   ```R
   shiny::runApp()
   ```
