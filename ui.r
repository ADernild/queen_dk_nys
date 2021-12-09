ui <- dashboardPage(
  dashboardHeader(title = "Queen speech central"
  ),
  dashboardSidebar(
    sidebarMenuOutput("menu")
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "royall_beautyfication.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "sizes_n_stuff.css"),
      tags$link(rel = "shortcut icon", href = "favicon.ico")
    ),
    # Index ----
    tabItems(
      tabItem(tabName = "index",
              h1("Queen speech central"),
              fluidRow(
                column(9,
                       box(width=12,
                           title = "All about the new years eve speeches of Queen Margret the 2. of Denmark",
                           div(id = "intoduction",
                               p("You are hearby invited to a look into the speaches of the royal majesty herself. This is a interactive dashboard, that alows you to select a category in the navigation menu on the left. You could look up the sentiment of her speaches, see a model of the topics she uses, look at a map, showing what countries she has spoken of through the years, or just see which words she uses the most. Enjoy your data ride."),
                               p(class="bold", "Gud bevare Danmark.")
                           )
                       ),
                       box(width=12,
                           title = "Speeches",
                           p("Theese are the speeches that are covvered:"),
                           p("todo")
                       )
                      ),
                column(3,
                       box(width=12,
                           div(id="wiki_infobox_wrap",
                               a(href="https://da.wikipedia.org/wiki/Margrethe_2.",
                                 target= "_blank",
                                 "Info indhentet via wikipedia (08/12/2021)."
                               ),
                               htmlOutput("wiki_infobox")
                           )
                       )
                )
              )
      ),
      # Topic Model ----
      tabItem(tabName = "tm",
          h2("Topic model"),
          fluidRow(
            box(width=12,
                title = "Topic model parameters <3",
                # Sidebar with a slider input for number of bins
                radioButtons("topicmodel", "Topicmodel", list("STM" = "stm_model", "LDA" = "lda_model")),
                sliderInput("nTerms", "Number of terms to display", min = 10, max = 50, value = 30)
            ),
            box(width=12,
                title = "Topic model :-O",
                visOutput("topicVis")
            )
          )
      ),
      # Sentiment ----
      tabItem(tabName = "sentiment",
              h2("Sentiment of new year eve speeches of Queen Margret, and the words used within"),
              fluidRow(
                tabBox(width=12, id = "speeches",
                       title="Sentiment of speaches", height = "150px",
                  tabPanel("Sentiment (Bubles)",
                           highchartOutput("sentiment_of_speech_bubles", height="50vh")
                  ),
                  tabPanel("Sentiment (Columns)",
                           highchartOutput("sentiment_of_speech_col_compare", height="50vh")
                  ),
                  tabPanel("Average sentiment columns",
                           highchartOutput("sentiment_of_speech_col_avg", height="50vh")
                  )
                )
              ),
              fluidRow(
                # Show a plot of the generated distribution
                box(width=12, title = "Word sentiment and frequency in speeches",
                    column(12,
                           helpText("Theese are the words that have impacted the sentiment of speaches. Some words have a larger sentiment than oters. Larger numbers are more positive, and negative numbers are more negative. In the moddel the most positive words have a polarity of 3. The most negative words have a polarity of -3. A sentiment of 0 would be true neutral. True neutral words do not impact the sentiment."),
                           sliderInput("slider_sentiment_of_words_n_words",
                                    "Number of words (by frequency)",
                                    min=1, max=n_dist_t_headword, value = 100 )
                    ),
                    box(width=6, title = "Sentiment of words",
                        highchartOutput("sentiment_of_words", height="25vh")
                    ),
                    box(width=6, title = "Frequency used",
                        highchartOutput("sentiment_of_words_freq", height="25vh"),
                    )
                )
              )
      ),
      # Map ----
      tabItem(tabName = "map",
              h2("A map of the countries mentioned in the new year eve speeches of Queen Margret"),
              fluidRow(
                box(width=12,  
                    p("todo")
                )
              )
      ),
      # Stats ----
      tabItem(tabName = "stats",
              h2("Sentiment of new year eve speeches of Queen Margret, and the words used within"),
              fluidRow(
                box(width=12,  
                    p("todo")
                )
              )
      )
    )
  )
)