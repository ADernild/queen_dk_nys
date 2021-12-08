ui <- fluidPage(
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  # ),
  # Application title
  titlePanel("Queen speech central"),
  column(12,
         column(9,
                div(id = "intoduction",
                  p("Velkommen til. Dette er et dashboard til at se information om Dronning Margretes nytårstaler. Du kan se information hvilke ord hun siger mest, sentimentet i hendes taler, hvilke lande hun snakker om og hvilke emner hun taler mest om. Du kan sevlfølgelig bruge filtrene til at sætte dine specifikationer til hvad du gerne vil se. God fornøjelse. Gud bevare Danmark.")
                ),
                column(12,
                       h2("topic model"),
                       column(2,
                              h3("Topic model parameters <3"),
                                # Sidebar with a slider input for number of bins
                                radioButtons("topicmodel", "Topicmodel", list("STM" = "stm_model", "LDA" = "lda_model")),
                                sliderInput("nTerms", "Number of terms to display", min = 10, max = 50, value = 30),
                              ),
                       column(10,
                              h3("Topic models :-O"),
                                visOutput("topicVis")
                              )
                       )
         ),
         column(3,
                htmlOutput("wiki_infobox")
         )
  ),
  
  # Show a plot of the generated distribution
  column(width = 12,  
         column(12,
                sliderInput("slider_sentiment_of_words_n_words",
                            "Number of words (by frequency)",
                            min=1, max=n_dist_t_headword, value = 100 )
                ),
         highchartOutput("sentiment_of_words",height="500px")
  )
)
