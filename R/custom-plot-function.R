#' Shiny Gadget for Plot Customization
#'
#' Launches a Shiny gadget to customize plots interactively, including the ability to set a custom title.
#'
#' @param data A data frame to use for the plot. Defaults to `mtcars`.
#' @return None. Opens a Shiny app.
#' @examples
#' if (interactive()) custom_plot_gadget(data = iris)
#' @export
custom_plot_gadget <- function(data = mtcars) {
  library(shiny)
  library(shinyWidgets)
  library(ggplot2)
  library(colourpicker)

  ui <- fluidPage(
    titlePanel("Custom Plot Gadget"),
    sidebarLayout(
      sidebarPanel(
        selectInput("xcol", "X-axis Variable:", names(data)),
        selectInput("ycol", "Y-axis Variable:", names(data)),
        textInput("plotTitle", "Plot Title:", value = "Custom Scatter Plot"),
        colourpicker::colourInput("color", "Point Color:", value = "blue"),
        selectInput("theme", "Theme:", choices = c("default", "minimal"))
      ),
      mainPanel(
        plotOutput("customPlot")
      )
    )
  )

  server <- function(input, output) {
    output$customPlot <- renderPlot({
      ggplot(data, aes_string(x = input$xcol, y = input$ycol)) +
        geom_point(color = input$color) +
        labs(title = input$plotTitle, x = input$xcol, y = input$ycol) +
        {
          if (input$theme == "default") theme_bw() else theme_minimal()
        } +
        theme(plot.title = element_text(hjust = 0.5))
    })
  }

  shinyApp(ui, server)
}
