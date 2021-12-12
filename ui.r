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
                       box(width=7,
                           title = "All about the new years eve speeches of Queen Margret the 2. of Denmark",
                           div(id = "intoduction",
                               p("You are hearby invited to a look into the speaches of the royal majesty herself. This is a interactive dashboard, that alows you to select a category in the navigation menu on the left. You could look up the sentiment of her speaches, see a model of the topics she uses, look at a map, showing what countries she has spoken of through the years, or just see which words she uses the most. Enjoy your data ride."),
                               p(class="bold", "GUD BEVARE DANMARK.")
                           )
                       ),
                       box(width=5,
                           title = "Speeches covered",
                           p("Theese are the speeches that are covered:"),
                           uiOutput("Covered_speech"),
                           helpText("Changes to language and year filter is relected here.")
                        )
                      ),
                column(3,
                       box(width=12,
                           div(id="wiki_infobox_wrap",
                               uiOutput("scrabing_info"),
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
                       title="Sentiment of speaches",
                  tabPanel("Sentiment (Bubles)",
                           highchartOutput("sentiment_of_speech_bubles", height="75vh")
                  ),
                  tabPanel("Sentiment (Columns)",
                           highchartOutput("sentiment_of_speech_col_compare", height="75vh")
                  ),
                  tabPanel("Sentiment range (Shankey)",
                           highchartOutput("sentiment_of_speech_sha_compare", height="75vh")
                  ),
                  tabPanel("Average sentiment",
                           highchartOutput("sentiment_of_speech_avg", height="75vh")
                  ),
                  fluidRow(
                    helpText("Press tab titles for different visualizatons."),
                    helpText("Using the filter in sidepanel will affect which years are featured, and categorize for if wors appear in a given year.")
                  )
                )
              ),
              fluidRow(
                # Show a plot of the generated distribution
                box(width=12, title = "Word sentiment and frequency in speeches",
                   fluidRow(
                     sliderInput("slider_sentiment_of_words_n_words",
                              "Number of words (by frequency)",
                              min=1, max=n_dist_t_headword, value = 100 )
                    ),
                    box(width=6, title = "Sentiment of words (-3:3)",
                        highchartOutput("sentiment_of_words", height="25vh")
                    ),
                    box(width=6, title = "Frequency used (n uses in total)",
                        highchartOutput("sentiment_of_words_freq", height="25vh"),
                    ),
                    fluidRow(
                       helpText("Theese are the words that have impacted the sentiment of speaches."),
                       helpText("Some words have a larger sentiment than others. Larger numbers are more positive, and negative numbers are more negative. In the moddel the most positive words have a polarity of 3. The most negative words have a polarity of -3."),
                       helpText("A sentiment of 0 would be true neutral. True neutral words do not impact the sentiment."),
                       helpText("The word slider only updates the plots, if the word has any ploarity and was features in the set year-range."),
                       helpText("Using the word filter will recategorize results when relevant."),
                   )
                )
              ),
              fluidRow(
                box(width = 12, title = "Sentiment statistics through years in speeches",
                    fluidRow(
                      valueBoxOutput("total_sum_sen"),
                      valueBoxOutput("total_pos_sen"),
                      valueBoxOutput("total_neg_sen")
                    ),
                    fluidRow(
                      valueBoxOutput("total_num_wor"),
                      valueBoxOutput("num_pos_sen"),
                      valueBoxOutput("num_neg_sen")
                    ),
                    fluidRow(
                      valueBoxOutput("mean_sum_sen"),
                      valueBoxOutput("mean_pos_sen"),
                      valueBoxOutput("mean_neg_sen")
                    ),
                    fluidRow(
                      valueBoxOutput("mean_num_wor")
                    )
                )
              ),
      ),
      # Map ----
      tabItem(tabName = "map",
              h2("A map of the countries mentioned in the new year eve speeches of Queen Margret"),
              fluidRow(
                box(width=12,  
                    leafletOutput("map", height = 740)
                )
              )
      ),
      # Stats ----
      tabItem(tabName = "stats",
              h2("Sentiment of new year eve speeches of Queen Margret, and the words used within"),
              fluidRow(
                box(width = 10, title="Wordcloud",
                    wordcloud2Output("wordcloud"),
                    helpText("Words in wordcloud is randomly sellected from aviable words. It is influenced by filter.")
                ),
                box(width=2, title = "Disclaimers",
                   helpText("Stopwords are filtered. This is done to avoid the most common words (like \"the\") to dominate the statistics."),
                   helpText("Words have been stemmed, to get better data for topics. This does remove information about word forms."),
                   helpText("Words have been lemmatized (replacing words with identical meaning with a headword), to improve topic analysis.")
                )
              )
      )
    )
  )
)