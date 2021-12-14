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
                    title = "You are hereby invited to a look into the new year eve speeches of the royal majesty herself",
                    div(id = "intoduction",
                        p(class="what", "This is a dashboard. The dashboard is a tool to analyze topics in speeches."),
                        p(class="why", "Topics can be hard to document correctly. By using this tool, you can find relevant topics and the words within on the topics tab. From there you can then choose to explore the sentiment, countries, or statistics to get a better understanding of the topic. You could also do your own thing and look at pretty graphs and statistics. We won't judge."),
                        p(class="how", "This is an interactive dashboard. You can select tabs in the sidebar on the left to navigate to different sections. When sidebar is expanded (see toggle button in top left corner), you can apply filters and settings for your liking."),
                        p(class="bold", "GUD BEVARE DANMARK.")
                    )
                )
              ),
              fluidRow(class="box_align_layout",
                       box(width=4, title="How to interpret visualizations and information",
                           p("If you wish to have some guidance in your data journey, we have prepared assisting labels for each visualization."),
                           p("These labels will be located directly beneath the point of interest."),
                           hr(),
                           whatViz("What marking: A blue dot with bold dark-blue text indicates what kind of visualization/information is depicted."),
                           whyViz("Why marking: A Yellow circle with dark-yellow bold text indicates why and when to use you could use the visualization/information you can observe."),
                           howViz("How marking: A light-brown triangle with brown bold text indicates how you can interpret information in the visualization."),
                           p(helpText("Grey italic/slanted text indicates other kinds of help text. This could be descriptions on how operate inputs, information about limitations and bugs.")),
                           p("Black text is other kinds of information. It would usually be practical information.")
                       ),
                       box(width=4,
                           title = "Speeches covered",
                           p("These are the speeches that are covered:"),
                           uiOutput("Covered_speech"),
                           helpText("Changes to language and year filter is reflected here.")
                       ),
                       box(width=4,
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
                    helpText("Using the word filter will recategorize results when relevant.")
                    
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
                )
              )
      )
    )
  )
)