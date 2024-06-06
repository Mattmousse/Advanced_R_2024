library(shiny)

# Define UI for application
ui <- fluidPage(
  titlePanel("Abalone Data Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      # Input: Select number of rows to display
      numericInput("rows", "Number of rows to view:", 10, min = 1, max = 100)
    ),
    
    mainPanel(
      # Output: Data table
      tableOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive expression to read the data
  abaloneData <- reactive({
    read.csv("C:/Users/mathi/Documents/Université/Semestre 8/R avancé/Aide Examen/shiny/Data/test.csv")
  })
  
  # Render the data table
  output$table <- renderTable({
    head(abaloneData(), n = input$rows)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
