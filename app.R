library(shiny)
library(ggeffects)
library(gridExtra)

data <- read.csv("data.csv")
genre_choices <- unique(data$playlist_genre)

ui <- navbarPage("Music Skip Prediction",
  tabPanel("GLM Model",
    sidebarLayout(
      sidebarPanel(
        p("Select variables to include in the glm model. These variables are numeric measures of the song's audio features."),
        selectInput("variables", "Select Variable(s):",
        choices = c("danceability", "energy", "key", "loudness", "mode", 
                   "speechiness", "acousticness", "instrumentalness", 
                   "liveness", "valence", "tempo"),
        multiple = TRUE),
        helpText("Investigate the model summary to see which predictors are the most significant.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Model Summary",
          h3("Model Summary"),
          verbatimTextOutput("modelSummary")
        ),
        tabPanel("Plots",
          h3("Investigate each variables ineraction with the skipped percentage"),
          plotOutput("plots")
        )
      )
    )
  ),
  tabPanel("Genre vs Skip",
    sidebarLayout(
      sidebarPanel(
        p("Select genres to investigate"), 
        selectInput("genres", "Select Genre(s):",
        choices = genre_choices,
        multiple = TRUE),
        helpText("Investigate the bar chart to see which genres have the best engagement.")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Bar Chart",
            h3("Genre vs Skip"),
            plotOutput("genre_vs_skip")
          )
        )
      )
    )
  )
)



server <- function(input, output) {
  df <- reactive({
    req(input$variables)
    data[, c(input$variables, "skip_percentage"), drop = FALSE]
  })
  
  glm_mod <- reactive({
    glm(skip_percentage ~ ., data = df(), family = "binomial")
  })

  output$modelSummary <- renderPrint({
    summary(glm_mod())
  })

  output$plots <- renderPlot({
    req(input$variables)
    plots <- lapply(input$variables, function(var) {
      plot(ggpredict(glm_mod(), terms = var))
    })
    if (length(plots) > 0) {
      do.call(grid.arrange, c(plots, ncol = 2))
    }
  })

  output$genre_vs_skip <- renderPlot({
    req(input$genres)
    genre_data <- data[data$playlist_genre %in% input$genres, ]
    avg_skip <- aggregate(skip_percentage ~ playlist_genre, data = genre_data, FUN = mean)
    barplot(avg_skip$skip_percentage, names.arg = avg_skip$playlist_genre,
            main = "Average Skip Percentage by Genre",
            xlab = "Genre", ylab = "Average Skip Percentage",
            las = 2)
  })
}

shinyApp(ui, server)
