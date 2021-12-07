ui <- fluidPage(
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  # ),
  # Application title
  titlePanel("Queen speech central"),
  
  # Sidebar with a slider input for number of bins
  radioButtons("topicmodel", "Topicmodel", list("LDA" = "lda_model", "STM" = "stm_model")),
  sliderInput("nTerms", "Number of terms to display", min = 10, max = 50, value = 30),
  
  # Show a plot of the generated distribution
  visOutput("topicVis")
)