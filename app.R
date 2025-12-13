library(shiny)
data <- read.csv("data.csv")

ui <- fluidPage(
  titlePanel("Music Skip Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variables", "Select Variable(s):",
        choices = c("danceability", "energy", "key", "loudness", "mode", 
                   "speechiness", "acousticness", "instrumentalness", 
                   "liveness", "valence", "tempo"),
        multiple = TRUE)
    ),
    mainPanel()
  )
)

server <- function(input, output) {
    input <- reactive({
        data[input$variables]
    })
}

shinyApp(ui, server)