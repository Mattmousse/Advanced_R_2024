require(shiny)
require(ggplot2)
theme_set(theme_bw())
require(latex2exp)


ui <- fluidPage(
  titlePanel("Exponent App"),
  br(),
  sliderInput(
    inputId = "n",
    label = "Choose the exponent n",
    value = 0,
    min = 0,
    max = 6,
    step = 0.5
  ),
  plotOutput("exPlot")
)

server <- function(input, output) {
  n <- reactive(input$n)
  output$exPlot <- renderPlot({
    ggplot() +
      geom_function(fun = function(x) x^n()) +
      xlim(-2, 2) +
      labs(title = TeX(paste("Curve of $y=x^n$ for $n=", n(), "$")), x = "x")
  })
}

shinyApp(ui = ui, server = server)
