ui <- fluidPage(
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  # ),
  # Application title
  titlePanel("Queen speech central"),
  
  # Sidebar with a slider input for number of bins
  radioButtons("topicmodel", "Topicmodel", list("STM" = "stm_model", "LDA" = "lda_model")),
  sliderInput("nTerms", "Number of terms to display", min = 10, max = 50, value = 30),
  
  # Show a plot of the generated distribution
  visOutput("topicVis"),
  column(width = 12,  
         column(12,
                sliderInput("slider_sentiment_of_words_n_words",
                            "Number of words (by frequency)",
                            min=1, max=n_dist_t_headword, value = 100 )
                ),
         highchartOutput("sentiment_of_words",height="500px")
  )
)