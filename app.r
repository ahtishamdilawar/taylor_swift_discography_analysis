library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT) # For interactive tables
library(e1071)
library(shinylive)
library(httpuv)
# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Taylor Swift Discography Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Descriptive Stats", tabName = "descriptive_stats", icon = icon("table")),
      menuItem("Boxplots", tabName = "boxplots", icon = icon("chart-bar")),
      menuItem("Histograms", tabName = "histograms", icon = icon("chart-bar")),
      menuItem("Regression", tabName = "regression", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
         
           box(
            title = "Feature Explanations",
            width = 12,
            p(
              tags$b("Danceability:"),
              "Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable."
            ),
            p(
              tags$b("Energy:"),
              "Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy."
            ),
            p(
              tags$b("Instrumentalness:"),
              "Predicts whether a track contains no vocals. 'Ooh' and 'aah' sounds are treated as instrumental in this context. Rap or spoken word tracks are clearly 'vocal'. The closer the instrumentalness value is to 1.0, the greater likelihood the track contains no vocal content. Values above 0.5 are intended to represent instrumental tracks, but confidence is higher as the value approaches 1.0."
            ),
            p(
              tags$b("Valence:"),
              "A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry)."
            )
          ) ,box(title = "Dataset Preview", width = 12, DT::dataTableOutput("data_preview"))
        )
        
      ),
      
    tabItem(
  tabName = "descriptive_stats",
  fluidRow(
    box(
      title = "Duration Statistics",
      status = "primary",
      collapsible = TRUE,
      width = 12,
      infoBoxOutput("duration_stats_box")
    ),
    box(
      title = "Danceability Statistics",
      status = "success",
      collapsible = TRUE,
      width = 12,
      infoBoxOutput("danceability_stats_box")
    ),
    box(
      title = "Energy Statistics",
      status = "danger",
      collapsible = TRUE,
      width = 12,
      infoBoxOutput("energy_stats_box")
    )
  ),
  fluidRow(
    box(
      title = "Acousticness Statistics",
      status = "warning",
      collapsible = TRUE,
      width = 12,
      infoBoxOutput("acousticness_stats_box")
    ),
    box(
      title = "Tempo Statistics",
      status = "info",
      collapsible = TRUE,
      width = 12,
      infoBoxOutput("tempo_stats_box")
    ),
    box(
      title = "Valence Statistics",
      status = "info",
      collapsible = TRUE,
      width = 12,
      infoBoxOutput("valence_stats_box")
    )
  )
),
      
    # Boxplots Tab
tabItem(
  tabName = "boxplots",
  fluidRow(
    column(
      width = 12,
      box(title = "Duration by Explicit Content", plotOutput("boxplot_duration")),
      box(
        title = "Explanation: Duration by Explicit Content",
        status = "info",
        solidHeader = TRUE,
        width = 6,
        collapsible = TRUE,
        textOutput("skewness_duration")
      )
    ),
    column(
      width = 12,
      box(title = "Valence by Explicit Content", plotOutput("boxplot_valence")),
      box(
        title = "Explanation: Valence by Explicit Content",
        status = "info",
        solidHeader = TRUE,
        width = 6,
        collapsible = TRUE,
        textOutput("skewness_valence")
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      box(title = "Danceability by Explicit Content", plotOutput("boxplot_danceability")),
      box(
        title = "Explanation: Danceability by Explicit Content",
        status = "info",
        solidHeader = TRUE,
        width = 6,
        collapsible = TRUE,
        textOutput("skewness_danceability")
      )
    )
  )
)
,
      
      # Histograms Tab
     tabItem(
  tabName = "histograms",
  fluidRow(
    column(
      width = 12,
      box(title = "Distribution of Valence", plotOutput("histogram_valence")),
      box(
        title = "Explanation: Valence Distribution",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        textOutput("explanation_valence")
      )
    ),
    column(
      width = 12,
      box(title = "Distribution of Tempo", plotOutput("histogram_tempo")),
      box(
        title = "Explanation: Tempo Distribution",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        textOutput("explanation_tempo")
      )
    )
  )
)
,
      
      # Regression Tab
      tabItem(
  tabName = "regression",
  fluidRow(
    box(title = "Regression Plot", width = 12, plotOutput("reg_plot")),
    box(
      title = "Predict Valence",
      width = 12,
      numericInput("danceability_value", "Enter Danceability Value:", value = 0.5, min = 0, max = 1, step = 0.01),
      textOutput("predicted_valence")
    ),
    box(
      title = "Regression Coefficients & Explanation",
      width = 12,
      textOutput("regression_coefficients"),
      p("The regression model follows the equation: Valence = a + b * Danceability."),
      p("Here:"),
      tags$ul(
        tags$li("a: Intercept, representing the predicted Valence when Danceability is 0."),
        tags$li("b: Slope, showing how Valence changes for each unit increase in Danceability.")
      )
    )
  )
)

    )
  )
)

# Define Server Logic
server <- function(input, output) {
  # Load data
  data <- reactive({
    file_path <- "taylor_swift_discography_cleaned.csv"
    data <- read.csv(file_path)
    data <- data %>% mutate(Duration_sec = Duration..ms. / 1000)
    data <- data %>% select(-Duration..ms.)
    return(data)
  })

  # Overview
  output$data_preview <- DT::renderDataTable({
    DT::datatable(
        data(), 
        options = list(
            pageLength = 10,    # Number of rows per page
            scrollX = TRUE      # Enable horizontal scrolling if needed
        )
    )
})


 # Descriptive Stats Calculations
 output$duration_stats_box <- renderInfoBox({
  stats <- data() %>% 
    summarise(
      Mean = mean(Duration_sec, na.rm = TRUE),
      Median = median(Duration_sec, na.rm = TRUE),
      SD = sd(Duration_sec, na.rm = TRUE),
      Q1 = quantile(Duration_sec, 0.25, na.rm = TRUE),
      Q3 = quantile(Duration_sec, 0.75, na.rm = TRUE)
    )
  mode <- data()$Duration_sec %>%
    na.omit() %>%
    {.[which.max(tabulate(match(., unique(.))))]}  # Calculate mode
  
  infoBox(
    title = "Duration Stats",
    value =HTML( paste0(
      "Mean: ", round(stats$Mean, 2), " sec<br>",
      "Median: ", round(stats$Median, 2), " sec<br>",
      "SD: ", round(stats$SD, 2), " sec<br>",
      "Q1: ", round(stats$Q1, 2), " sec<br>",
      "Q3: ", round(stats$Q3, 2), " sec<br>",
      "Mode: ", round(mode, 2), " sec"
    )),
    subtitle = "Track Duration Statistics",
    icon = icon("clock"),
    color = "blue",
    fill = TRUE
  )
})
output$danceability_stats_box <- renderInfoBox({
  stats <- data() %>% 
    summarise(
      Mean = mean(Danceability, na.rm = TRUE),
      Median = median(Danceability, na.rm = TRUE),
      SD = sd(Danceability, na.rm = TRUE),
      Q1 = quantile(Danceability, 0.25, na.rm = TRUE),
      Q3 = quantile(Danceability, 0.75, na.rm = TRUE)
    )
  mode <- data()$Danceability %>%
    na.omit() %>%
    {.[which.max(tabulate(match(., unique(.))))]}  # Calculate mode
  
  infoBox(
    title = "Danceability Stats",
    value =HTML( paste0(
      "Mean: ", round(stats$Mean, 2), "<br>",
      "Median: ", round(stats$Median, 2), "<br>",
      "SD: ", round(stats$SD, 2), "<br>",
      "Q1: ", round(stats$Q1, 2), "<br>",
      "Q3: ", round(stats$Q3, 2), "<br>",
      "Mode: ", round(mode, 2)
    )),
    subtitle = "Danceability Statistics (0.0 to 1.0)",
    icon = icon("music"),
    color = "green",
    fill = TRUE
  )
})
output$energy_stats_box <- renderInfoBox({
  stats <- data() %>%
    summarise(
      Mean = mean(Energy, na.rm = TRUE),
      Median = median(Energy, na.rm = TRUE),
      SD = sd(Energy, na.rm = TRUE),
      Q1 = quantile(Energy, 0.25, na.rm = TRUE),
      Q3 = quantile(Energy, 0.75, na.rm = TRUE)
    )
  mode <- data()$Energy %>%
    na.omit() %>%
    {.[which.max(tabulate(match(., unique(.))))]}  # Calculate mode

  infoBox(
    title = "Energy Stats",
    value = HTML(paste0(
      "Mean: ", round(stats$Mean, 2), "<br>",
      "Median: ", round(stats$Median, 2), "<br>",
      "SD: ", round(stats$SD, 2), "<br>",
      "Q1: ", round(stats$Q1, 2), "<br>",
      "Q3: ", round(stats$Q3, 2), "<br>",
      "Mode: ", round(mode, 2)
    )),
    subtitle = "Energy Statistics (0.0 to 1.0)",
    icon = icon("fire"),
    color = "red",
    fill = TRUE
  )
})
output$acousticness_stats_box <- renderInfoBox({
  stats <- data() %>%
    summarise(
      Mean = mean(Acousticness, na.rm = TRUE),
      Median = median(Acousticness, na.rm = TRUE),
      SD = sd(Acousticness, na.rm = TRUE),
      Q1 = quantile(Acousticness, 0.25, na.rm = TRUE),
      Q3 = quantile(Acousticness, 0.75, na.rm = TRUE)
    )
  mode <- data()$Acousticness %>%
    na.omit() %>%
    {.[which.max(tabulate(match(., unique(.))))]}  # Calculate mode

  infoBox(
    title = "Acousticness Stats",
    value = HTML(paste0(
      "Mean: ", round(stats$Mean, 2), "<br>",
      "Median: ", round(stats$Median, 2), "<br>",
      "SD: ", round(stats$SD, 2), "<br>",
      "Q1: ", round(stats$Q1, 2), "<br>",
      "Q3: ", round(stats$Q3, 2), "<br>",
      "Mode: ", round(mode, 2)
    )),
    subtitle = "Likelihood of Acoustic Nature (0.0 to 1.0)",
    icon = icon("guitar"),
    color = "purple",
    fill = TRUE
  )
})
output$tempo_stats_box <- renderInfoBox({
  stats <- data() %>%
    summarise(
      Mean = mean(Tempo, na.rm = TRUE),
      Median = median(Tempo, na.rm = TRUE),
      SD = sd(Tempo, na.rm = TRUE),
      Q1 = quantile(Tempo, 0.25, na.rm = TRUE),
      Q3 = quantile(Tempo, 0.75, na.rm = TRUE)
    )
  mode <- data()$Tempo %>%
    na.omit() %>%
    {.[which.max(tabulate(match(., unique(.))))]}  # Calculate mode

  infoBox(
    title = "Tempo Stats",
    value = HTML(paste0(
      "Mean: ", round(stats$Mean, 2),  " BPM<br>",
      "Median: ", round(stats$Median, 2), " BPM<br>",
      "SD: ", round(stats$SD, 2), " BPM<br>",
      "Q1: ", round(stats$Q1, 2), " BPM<br>",
      "Q3: ", round(stats$Q3, 2), " BPM<br>",
      "Mode: ", round(mode, 2), " BPM"
    )),
    subtitle = "Track Tempo in BPM (Beats per Minute)",
    icon = icon("drum"),
    color = "yellow",
    fill = TRUE
  )
})
output$valence_stats_box <- renderInfoBox({
  stats <- data() %>%
    summarise(
      Mean = mean(Valence, na.rm = TRUE),
      Median = median(Valence, na.rm = TRUE),
      SD = sd(Valence, na.rm = TRUE),
      Q1 = quantile(Valence, 0.25, na.rm = TRUE),
      Q3 = quantile(Valence, 0.75, na.rm = TRUE)
    )
  mode <- data()$Valence %>%
    na.omit() %>%
    {.[which.max(tabulate(match(., unique(.))))]}  # Calculate mode

  infoBox(
    title = "Valence Stats",
    value = HTML(paste0(
      "Mean: ", round(stats$Mean, 2), "<br>",
      "Median: ", round(stats$Median, 2), "<br>",
      "SD: ", round(stats$SD, 2), "<br>",
      "Q1: ", round(stats$Q1, 2), "<br>",
      "Q3: ", round(stats$Q3, 2), "<br>",
      "Mode: ", round(mode, 2)
    )),
    subtitle = "Measure of Musical Positivity (0.0 to 1.0)",
    icon = icon("smile"),
    color = "yellow",
    fill = TRUE
  )
})

output$avg_duration_box <- renderInfoBox({
  avg_duration <- data() %>% summarise(Avg_Duration = mean(Duration_sec, na.rm = TRUE)) %>% pull(Avg_Duration)
  infoBox(
    title = "Average Duration",
    value = paste(round(avg_duration, 2), "sec"),
    subtitle = "Average track duration in seconds",
    icon = icon("clock"),
    color = "blue"
  )
})

output$avg_danceability_box <- renderInfoBox({
  avg_danceability <- data() %>% summarise(Avg_Danceability = mean(Danceability, na.rm = TRUE)) %>% pull(Avg_Danceability)
  infoBox(
    title = "Average Danceability",
    value = round(avg_danceability, 2),
    subtitle = "Measure of track's suitability for dancing (0.0 to 1.0)",
    icon = icon("music"),
    color = "green"
  )
})

output$avg_energy_box <- renderInfoBox({
  avg_energy <- data() %>% summarise(Avg_Energy = mean(Energy, na.rm = TRUE)) %>% pull(Avg_Energy)
  infoBox(
    title = "Average Energy",
    value = round(avg_energy, 2),
    subtitle = "Measure of intensity and activity (0.0 to 1.0)",
    icon = icon("fire"),
    color = "red"
  )
})

output$avg_acousticness_box <- renderInfoBox({
  avg_acousticness <- data() %>% summarise(Avg_Acousticness = mean(Acousticness, na.rm = TRUE)) %>% pull(Avg_Acousticness)
  infoBox(
    title = "Average Acousticness",
    value = round(avg_acousticness, 2),
    subtitle = "Measure of likelihood a track is acoustic (0.0 to 1.0)",
    icon = icon("guitar"),
    color = "purple"
  )
})

output$avg_tempo_box <- renderInfoBox({
  avg_tempo <- data() %>% summarise(Avg_Tempo = mean(Tempo, na.rm = TRUE)) %>% pull(Avg_Tempo)
  infoBox(
    title = "Average Tempo",
    value = paste(round(avg_tempo, 2), "BPM"),
    subtitle = "Average tempo in beats per minute",
    icon = icon("drum"),
    color = "yellow"
  )
})

output$avg_valence_box <- renderInfoBox({
  avg_valence <- data() %>% summarise(Avg_Valence = mean(Valence, na.rm = TRUE)) %>% pull(Avg_Valence)
  infoBox(
    title = "Average Valence",
    value = round(avg_valence, 2),
    subtitle = "Measure of musical positivity (0.0 to 1.0)",
    icon = icon("smile"),
    color = "warning"
  )
})

  # Boxplots
output$boxplot_duration <- renderPlot({
  ggplot(data(), aes(y = Duration_sec)) +
    geom_boxplot(fill = "cyan") +
    labs(title = "Distribution of Song Durations", x = NULL, y = "Duration (sec)") +
    theme_minimal()
})

output$boxplot_valence <- renderPlot({
  ggplot(data(), aes(y = Valence)) +
    geom_boxplot(fill = "purple") +
    labs(title = "Distribution of Valence", x = NULL, y = "Valence") +
    theme_minimal()
})

output$boxplot_danceability <- renderPlot({
  ggplot(data(), aes(y = Danceability)) +
    geom_boxplot(fill = "orange") +
    labs(title = "Distribution of Danceability", x = NULL, y = "Danceability") +
    theme_minimal()
})

  # Skewness Calculation for Duration
output$skewness_duration <- renderText({
  skewness_value <- skewness(data()$Duration_sec, na.rm = TRUE)
  skewness_text <- ifelse(
    skewness_value > 0,
    "The distribution of durations is positively skewed, meaning most songs have shorter durations, with a few having significantly longer durations.",
    ifelse(
      skewness_value < 0,
      "The distribution of durations is negatively skewed, meaning most songs have longer durations, with a few having significantly shorter durations.",
      "The distribution of durations is approximately symmetric."
    )
  )
  paste("Skewness Analysis: ", skewness_text)
})

# Skewness Calculation for Valence
output$skewness_valence <- renderText({
  skewness_value <- skewness(data()$Valence, na.rm = TRUE)
  skewness_text <- ifelse(
    skewness_value > 0,
    "The distribution of valence is positively skewed, meaning most songs have a lower valence score (less positive mood), with a few having a significantly higher valence.",
    ifelse(
      skewness_value < 0,
      "The distribution of valence is negatively skewed, meaning most songs have a higher valence score (more positive mood), with a few having significantly lower valence.",
      "The distribution of valence is approximately symmetric."
    )
  )
  paste("Skewness Analysis: ", skewness_text)
})

# Skewness Calculation for Danceability
output$skewness_danceability <- renderText({
  skewness_value <- skewness(data()$Danceability, na.rm = TRUE)
  skewness_text <- ifelse(
    skewness_value > 0,
    "The distribution of danceability is positively skewed, meaning most songs have a lower danceability score, with a few having significantly higher scores.",
    ifelse(
      skewness_value < 0,
      "The distribution of danceability is negatively skewed, meaning most songs have a higher danceability score, with a few having significantly lower scores.",
      "The distribution of danceability is approximately symmetric."
    )
  )
  paste("Skewness Analysis: ", skewness_text)
})
  # Histograms
  output$histogram_valence <- renderPlot({
    ggplot(data(), aes(x = Valence)) +
      geom_histogram(fill = "blue", color = "black", bins = 30) +
      labs(title = "Distribution of Valence", x = "Valence", y = "Frequency")
  })

  output$histogram_tempo <- renderPlot({
    ggplot(data(), aes(x = Tempo)) +
      geom_histogram(fill = "red", color = "black", bins = 30) +
      labs(title = "Distribution of Tempo", x = "Tempo", y = "Frequency")
  })
  # Explanation for Valence Distribution
output$explanation_valence <- renderText({
  valence_mean <- mean(data()$Valence, na.rm = TRUE)
  valence_sd <- sd(data()$Valence, na.rm = TRUE)
  
  paste(
    "The histogram shows the distribution of valence scores in the dataset, which measures musical positivity. ",
    "Higher valence indicates happier, more cheerful tracks, while lower valence reflects more somber tracks. ",
    "The average valence score is approximately ", round(valence_mean, 2), 
    " with a standard deviation of ", round(valence_sd, 2), 
    ", indicating the typical spread of valence values around the mean."
  )
})

# Explanation for Tempo Distribution
output$explanation_tempo <- renderText({
  tempo_mean <- mean(data()$Tempo, na.rm = TRUE)
  tempo_sd <- sd(data()$Tempo, na.rm = TRUE)
  
  paste(
    "The histogram illustrates the distribution of tempo values in the dataset, which is measured in beats per minute (BPM). ",
    "Higher tempo values typically indicate faster-paced tracks. ",
    "The average tempo is approximately ", round(tempo_mean, 2), 
    " BPM with a standard deviation of ", round(tempo_sd, 2), 
    ", showing the variation in tempo among the songs."
  )
})
  # Regression Plot and Prediction
 data_clean <- reactive({
  data() %>% select(Danceability, Valence) %>% drop_na()
})

# Reactive regression model
single_model <- reactive({
  lm(Valence ~ Danceability, data = data_clean())
})

# Regression plot
output$reg_plot <- renderPlot({
  ggplot(data_clean(), aes(x = Danceability, y = Valence)) +
    geom_point(color = "blue", alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(
      title = "Single Linear Regression: Valence ~ Danceability",
      x = "Danceability",
      y = "Valence"
    ) +
    theme_minimal()
})

# Predicted Valence
output$predicted_valence <- renderText({
  new_danceability <- input$danceability_value
  predicted_valence <- predict(single_model(), newdata = data.frame(Danceability = new_danceability))
  paste("For Danceability =", new_danceability, ", Predicted Valence =", round(predicted_valence, 3))
})

# Regression coefficients and explanation
output$regression_coefficients <- renderText({
  coeffs <- coef(single_model())
  intercept <- coeffs[1]
  slope <- coeffs[2]
  paste("a (Intercept) =", round(intercept, 3), ", b (Slope) =", round(slope, 3))
})
}

# Run the application 
shinyApp(ui = ui, server = server)
