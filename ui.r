ui <- dashboardPage(
  dashboardHeader(title = "H.M. The Queen topic analyzer"
  ),
  dashboardSidebar(
    sidebarMenuOutput("menu")
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "sizes_n_stuff.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "design2.css"),
      uiOutput("royall_beautyfication"),
      tags$link(rel = "shortcut icon", href = "favicon.ico")
    ),
    conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                     tags$div(span("Loading..."), id = "loadmessage")
    ),
    # Index ----
    tabItems(
      tabItem(tabName = "index",
              h1("H.M. The Queen topic analyzer"),
              fluidRow(
                box(width=12,
                    title = "All about the new years eve speeches of Queen Margret the 2. of Denmark",
                    div(id = "intoduction",
                        p("You are hearby invited to a look into the speaches of the royal majesty herself. This is a interactive dashboard, that alows you to select a category in the navigation menu on the left. You could look up the sentiment of her speaches, see a model of the topics she uses, look at a map, showing what countries she has spoken of through the years, or just see which words she uses the most. Enjoy your data ride."),
                        p(class="bold", "GUD BEVARE DANMARK.")
                    )
                )
              ),
              fluidRow(class="box_align_layout",
                box(width=6,
                    title = "Speeches covered",
                    p("Theese are the speeches that are covered:"),
                    uiOutput("Covered_speech"),
                    helpText("Changes to language and year filter is relected here.")
                ),
                box(width=6,
                    div(id="wiki_infobox_wrap",
                        uiOutput("scrabing_info"),
                        htmlOutput("wiki_infobox")
                    )
                )
              )
      ),
      # Topic Model ----
      tabItem(tabName = "tm",
          h2("Topics"),
          fluidRow(class="box_align_layout",
            box(width=2,
                title = "Topic model parameters <3",
                # Sidebar with a slider input for number of bins
                radioButtons("topicmodel", "Topicmodel", list("STM" = "stm_model", "LDA" = "lda_model")),
                sliderInput("nTerms", "Number of terms to display", min = 10, max = 50, value = 30)
            ),
            box(width=10,
                title = "Topic model :-O",
                visOutput("topicVis")
            )
          )
      ),
      # Sentiment ----
      tabItem(tabName = "sentiment",
              h2("Sentiment of new year eve speeches of Queen Margret, and the words used within"),
              fluidRow(class="box_align_layout",
                tabBox(width=10, id = "speeches",
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
                  )
                ),
                box(width = 2, title = "Slider",
                    sliderInput("slider_sentiment_of_words_n_words",
                                "Number of words (by frequency)",
                                min=1, max=n_dist_t_headword, value = 100 ),
                    helpText("The word slider only updates the plots, if the word has any ploarity and was features in the set year-range."),
                    helpText("Using the word filter will recategorize results when relevant."),
                    
                )
              ),
              fluidRow(class="box_align_layout",
                # Show a plot of the generated distribution
                box(width=6, title = "Sentiment of words (-3:3)",
                    highchartOutput("sentiment_of_words", height="35vh"),
                    helpText("Some words have a larger sentiment than others. Larger numbers are more positive, and negative numbers are more negative. In the moddel the most positive words have a polarity of 3. The most negative words have a polarity of -3.")
                ),
                box(width=6, title = "Frequency used (n uses in total)",
                    highchartOutput("sentiment_of_words_freq", height="35vh"),
                    helpText("A sentiment of 0 would be true neutral. True neutral words do not impact the sentiment. Neutral sentiment is filtered on the sentiment tab.")
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
              )
      ),
      # Map ----
      tabItem(tabName = "map",
              h2("A map of the countries mentioned in the new year eve speeches of Queen Margret"),
              fluidRow(class="box_align_layout",
                box(width=10,  
                    leafletOutput("map", height = 740)
                ),
                box(width=2, title="Countries mentioned",
                    plotOutput("plot")
                )
              )
      ),
      # Stats ----
      tabItem(tabName = "stats",
              h2("Sentiment of new year eve speeches of Queen Margret, and the words used within"),
              fluidRow(class="box_align_layout",
                tabBox(width=10, id = "word_ussage", title="Speech length by words used",
                       tabPanel("Spline",
                                highchartOutput("speech_length_spline", height="50vh")
                        ),
                        tabPanel("Columns",
                                 highchartOutput("speech_length_col", height="50vh")
                        ),
                        tabPanel("Piechart",
                                 highchartOutput("speech_length_pie", height="50vh")
                        ),
                        fluidRow(
                          helpText("Todo.")
                       )
                ),
                box(width=2, title = "Disclaimers",
                    helpText("Stopwords are filtered. This is done to avoid the most common words (like \"the\") to dominate the statistics."),
                    helpText("Words have been stemmed, to get better data for topics. This does remove information about word forms."),
                    helpText("Words have been lemmatized (replacing words with identical meaning with a headword), to improve topic analysis.")
                )
              ),
              fluidRow(class="box_align_layout",
                box(width=2, title = "Speech statistics sliders",
                    sliderInput("slider_word_ussage",
                                "Top frequent numbers",
                                min=1, max=number_of_rarity, value = 15,
                                step = 1),
                    helpText("We recommend not going above a frequency of 15. It will take a long time to load with values above that threshold. Plots might also become too clustered to comprehend.")
                ),
                tabBox(width=10, id = "word_ussage", title="Most used words",
                       tabPanel("Piechart",
                                highchartOutput("word_ussage_pie", height="50vh")
                       ),
                       tabPanel("Stream graph",
                                highchartOutput("word_ussage_streamgraph", height="50vh"),
                                helpText("Stream graph is limited to a maximum frequency of 15. Any number set above, will result in 15 being featured.")
                       ),
                       tabPanel("Columns",
                                highchartOutput("word_ussage_col", height="50vh")
                       ),
                       tabPanel("Scatterplot",
                                highchartOutput("word_ussage_scatter", height="50vh"),
                                helpText("Scatterplot is limited to a maximum frequency of 15. Any number set above, will result in 15 being featured.")
                       ),
                       tabPanel("Wordcloud",
                           wordcloud2Output("wordcloud"),
                           helpText("Words in wordcloud is randomly sellected from aviable words. It is influenced by filter.")
                       ),
                       tabPanel("Table",
                                DTOutput("word_ussage_tbl")
                       )
                ),
              )
      )
    )
  )
)