library(shiny)
library(ggeffects)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(cluster)
library(psych)
library(factoextra)
library(corrplot)
library(plotly)

data <- read.csv("data.csv")
genre_choices <- unique(data$playlist_genre)

ui <- navbarPage("Spotify Music Analysis",
  tabPanel("Song Skip Prediction",
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
    # Page 1
    mainPanel(
      tabsetPanel(
        tabPanel("Model Summary",
          h3("Model Summary"),
          helpText("The model summary shows the coefficients of the glm model. 
          We can see the p-values of each of the variables as well as the overall model fit. 
          Each variable's estimate gives us the rate of change in log odds of skipping the song. 
          We can also see the impact of our model by taking the Null Deviance - Residual Deviance, giving us roughly the reduction in deviance achieved by the model."),
          verbatimTextOutput("modelSummary")
        ),
        tabPanel("Plots",
          h3("Investigate each variables ineraction with the skipped percentage"),
          plotOutput("plots")
        )
      )
    )
    )
  ),
  # Page 2
  tabPanel("Genre vs Engagement",
    sidebarLayout(
      sidebarPanel(
        p("Select genres to include in the box plots"), 
        selectInput("genres", "Select Genre(s):",
        choices = genre_choices,
        multiple = TRUE),
        helpText("Investigate the box plots to see which genres have the best engagement."),
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Skip Percentage by Genre",
            h3("Genre vs Skip"),
            helpText("Box plots show the summary statistics of the skip percentage by genre."), 
            plotOutput("genre_vs_skip")
          ),
           tabPanel("Playing Time Percentage by Genre",
             h3("Playing Time Percentage by Genre"),
             helpText("Playing time percentage is the mean listening time for a song divided by the duration of the song"),
             plotOutput("playing_time_percentage_by_genre"),
             conditionalPanel(
               condition = "input.genres.length > 0",
               helpText("*Note playing_time_percentage may exceed 100% due to the fact that some listeners repeated the song, rewinded, etc.")
             )
          )
        )
      )
    )
    ),
    # Page 3
    tabPanel("Genre vs Popularity Over Time ",
      sidebarLayout(
         sidebarPanel(
           p("Select genre to include in the linear regression model"), 
           selectInput("genre", "Select Genre:",
           choices = c("Select a genre..." = "", unique(data$playlist_genre)),
           selected = "",
           multiple = FALSE),
          helpText("Investigate the line chart to see how genres have changed in popularity over time.")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Linear Regression Summary",
              helpText("The linear regression summary gives us the p-values of each of the variables as well as the overall model fit. 
              Each variable's estimate gives us the rate of change in popularity over time. 
              The R-squared value tells us the proportion of variance in popularity that is explained by the model."),
              verbatimTextOutput("linear_regression_summary")
            ),
            tabPanel("Scatter Plot",
              h3("Genre vs Popularity Over Time"),
              plotOutput("genre_vs_popularity_over_time")
            )
          )
        )
      )
    ),
    # Page 4
    tabPanel("PCA and Clusters",
      sidebarLayout(
        sidebarPanel(
          p("Select variables to include in the PCA and clustering model"),
          selectInput("variables", "Select Variable(s):",
          choices = c("danceability", "energy", "key", "loudness", "mode", 
                     "speechiness", "acousticness", "instrumentalness", 
                     "liveness", "valence", "tempo"),
          multiple = TRUE),
          numericInput("k", "Number of Clusters:", min = 2, max = 10, value = "4"),
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("PCA",
              verbatimTextOutput("pca_summary"),
              helpText("The PCA summary shows the principal components and the variance explained by each component."),
              plotOutput("scree_plot"),
              helpText("The scree plot shows the eigenvalues (explained variance) of the principal components and tells us the optimal number of components to retain.")
            ),
              tabPanel("Clusters",
                helpText("The clustering plot shows the clusters of the data points based on the principal components."),
                helpText("The data points are randomly sampled from our dataset, so that the plot is not too crowded."),
                helpText("*Note: The clustering plot is only available when at least 2 variables are selected."),
                plotOutput("clustering", height = "800px")
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
    glm(skip_percentage ~ ., data = df(), family = "quasibinomial")
  })

  # page 1: model summary
  output$modelSummary <- renderPrint({
    summary(glm_mod())
  })

  # page 1: plots of skip percentage by each variable
  output$plots <- renderPlot({
    req(input$variables)
    plots <- lapply(input$variables, function(var) {
      plot(ggpredict(glm_mod(), terms = var))
    })
    if (length(plots) > 0) {
      do.call(grid.arrange, c(plots, ncol = 2))
    }
  })

  # page 2: box plot of skip percentage by genre
  output$genre_vs_skip <- renderPlot({
    req(input$genres)
    genre_data <- data[data$playlist_genre %in% input$genres, ]
    ggplot(genre_data, aes(x = playlist_genre, y = skip_percentage)) +
      geom_boxplot() +
      labs(
         x = "Genre",
         y = "Skip Percentage") +
      theme_classic()
  })

  # page 2: box plot of listening percentage by genre
  output$playing_time_percentage_by_genre <- renderPlot({
    req(input$genres)
    genre_data <- data[data$playlist_genre %in% input$genres, ]
    ggplot(genre_data, aes(x = playlist_genre, y = playing_time_percentage)) +
      geom_boxplot() +
      labs(
         x = "Genre",
         y = "Listening Percentage") +  
      theme_classic()
  })

  # page 3: line chart of popularity over time by genre
  output$genre_vs_popularity_over_time <- renderPlot({
    req(input$genre)
    genre_data <- data[data$playlist_genre == input$genre, ]
    genre_data_year <- genre_data %>% mutate(year = as.numeric(substr(track_album_release_date, 1, 4)))
    ggplot(genre_data_year, aes(x = year, y = track_popularity)) +
      geom_point() +
      labs(x = "Year", y = "Popularity") +
      geom_smooth(method = "lm") +
      theme_classic()
  })

  # page 3: linear regression summary of popularity over time by genre
  output$linear_regression_summary <- renderPrint({
    req(input$genre)
    genre_data <- data[data$playlist_genre == input$genre, ]
    genre_data_year <- genre_data %>% mutate(year = as.numeric(substr(track_album_release_date, 1, 4)))
    lm_model <- lm(track_popularity ~ year, data = genre_data_year)
    summary(lm_model)
  })

  # page 4: pca summary
  output$pca_summary <- renderPrint({
    req(input$variables)
    pca_data <- data[, input$variables]
    pca_data_scaled <- scale(pca_data)
    pca <- prcomp(pca_data_scaled)
    pca$rotation
  })

  # page 4: scree plots
  output$scree_plot <- renderPlot({
    req(input$variables)
    pca_data <- data[, input$variables]
    pca_data_scaled <- scale(pca_data)
    fa.parallel(pca_data_scaled, fa="pc")
  })

  # page 4: clustering
  output$clustering <- renderPlot({
    req(input$variables)
    if (length(input$variables) < 2) {
      return(NULL)
    }
    pca_data <- data[, input$variables, drop = FALSE]
    track_names <- data$track_name
    pca_data_scaled <- scale(pca_data)
    sampled_indices <- sample(nrow(pca_data_scaled), min(30, nrow(pca_data_scaled)))
    pca_data_sampled <- pca_data_scaled[sampled_indices, , drop = FALSE]
    track_names_sampled <- track_names[sampled_indices]
    
    # Set rownames to track names
    rownames(pca_data_sampled) <- track_names_sampled
    
    kmeans_model <- kmeans(pca_data_sampled, centers = input$k)
    fviz_cluster(kmeans_model, data = pca_data_sampled, repel = TRUE) +
      theme(plot.margin = margin(20, 20, 20, 20, "pt"))
  }, height = 800)
}

shinyApp(ui, server)
