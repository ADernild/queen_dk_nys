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
                           uiOutput("Covered_speech"),
                           whatViz("This are the speeches that are covered in the dashboard, with your current filter."),
                           whyViz("You can use this to see the source from what you filtered."),
                           howViz("You can use language and year filters. To see all text in a language selected language, select all possible years."),
                           p(helpText("Notice: English version of speech are translations. Not all Danish speeches are translated. Some years are therefore not available."))
                       ),
                       box(width=4,
                           div(id="wiki_infobox_wrap",
                               uiOutput("scrabing_info"),
                               htmlOutput("wiki_infobox")
                           ),
                           whatViz("This is the info box from Wikipedia about the queen."),
                           whyViz("You can use this to familiarize yourself with the subject, or just want a quick read-up."),
                           howViz("You can read the table, or visit the source, above the table.")
                       )
              ),
              hr(),
              fluidRow(
                box(width=12, title="Filters",
                    whatViz("This dashboard utilizes filtering systems. The main filters are in the sidebar when expanded. Some visualizations have custom filters. They are documented when needed."),
                    whyViz("The filters can be used to filter the data you see. You can do this to inspect a certain subset of data. E.g., to look data relevant to you."),
                    howViz("To expand the sidebar, press the expand button in the top left corner."),
                )
              ),
              fluidRow(class="box_align_layout",
                       box(width=4, title="Language filter",
                           whatViz("The language filter set's what language/translation of speeches you inspect, and what language to display the Wikipedia info box."),
                           whyViz("For this dashboard, Danish would be optimal. It is the original language of the speeches. If you don't understand Danish, you can still use a limited set of speeches in English."),
                           howViz("Push the text or dot next to the language you want to use."),
                       ),
                       box(width=4, title="Year filters",
                           whatViz("Year filter refers to the group of filters affecting year. These are: the year input, the year slider and the year selector."),
                           whyViz("Filtering years can help you limit the data relevant to the topic you are researching."),
                           howViz("Year input: Push the text or dot next to the year input method you need or prefer"),
                           howViz("Year range: select the start year and end year by drag 'n drop the round dots."),
                           howViz("Year selector - remove year(s): click on a year and press Backspace-key or Delete-key to remove a year. Hold down Ctrl to select multiple. Alternatively, select input-field (e.g. by clicking or using tab button), and navigate the cursor with the arrow keys, and use Backspace-key or Delete-key to remove years."),
                           howViz("Year selector - Add year: select input-field (e.g. by clicking or using tab button) and type year. Press enter to add or select suggestion by clicking on it."),
                           p(helpText("Notice: Some visualizations will use the whole dataset as a reference regardless of your set filter.")),
                       ),
                       box(width=4, title="Featured words",
                           whatViz("Featured words is a selection of words from words featured in the speeches, that is used to filter or feature words in different kinds of ways."),
                           whyViz("When you are inspecting a topic, some words might seem important. So, you can filter for these words using the filter."),
                           howViz("Select word: Type word and press enter when done or click suggestion pop up."),
                           howViz("Remove word(s): click on a word and press Backspace-key or Delete-key to remove word Hold down Ctrl to select multiple. Alternatively, select input-field (e.g. by clicking or using tab button), and navigate the cursor with the arrow keys, and use Backspace-key or Delete-key to remove words.")
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