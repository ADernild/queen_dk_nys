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
              h2("Welcome and about"),
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
              )
      ),
      # Topic Model ----
      tabItem(tabName = "tm",
          h2("Topics"),
          fluidRow(class="box_align_layout",
            box(width=2, title = "Topic model parameters",
                # Sidebar with a slider input for number of bins
                whatViz("Number of terms to display sets the number of terms used in the topic model, by size of topic."),
                whyViz("Limiting the number of terms makes the topic model easier to interpret. Adding more, gives more information."),
                fluidRow(sliderInput("nTerms", "Number of terms to display", min = 10, max = 50, value = 30, sep="")),
                box(width = 12, class="infoViz", title = "How to use",
                    collapsible = T, collapsed = T,
                    howViz("Use the slider to select a number for your liking.")
                ),
                helpText("Notice: Topic model selector will result in a failure if LDA is selected.")
            ),
            box(width=10, title = "Topic model",
                whatViz("Topic model is a tool that maps topics. It does so by analyzing word relations by analyzing all speeches and mapping the relations as word matrixes. Then the model displays the topics to the left, and the words within on the right. Frequency of use is depicted by size for topics, and bar length for words. Topics are named 1:n, where 1 is the largest topic."),
                whyViz("The topics model can be used to get an understanding of themes present in the speeches throughout the years."),
                visOutput("topicVis"),
                box(width = 12, class="infoViz", title = "How to use",
                    collapsible = T, collapsed = T,
                    howViz("Topics (left): The sizes in the visualization in the body represent frequency and can be interpreted as so. To inspect a topic, it can be selected in the selector in the header, a topic circle can be hovered. Clicking a topic will keep it highlighted."),
                    howViz("Topic content (right body): Hovering a word will show what topics they are included in. Clicking a word will add an underline, but nothing else."),
                    howViz("Relevance metric slider (right header): The slider adjusts relevance for words to be part of a topic. To see it work, first highlight a topic (by clicking or using the top left filter).")
                )
            )
          ),
          fluidRow(class="box_align_layout",
                   box(width=5, title = textOutput("topics_sentce_title"),
                       whatViz("Samples of mentions."),
                       whyViz("To see examples of mentions."),
                       uiOutput("topicText"),
                       box(width = 12, class="infoViz", title = "How to use",
                           collapsible = T, collapsed = T,
                           howViz("Click on a a topic in the topic map above.")
                       )
                   ),
                    box(width=4, title = textOutput("topics_means_title"),
                      highchartOutput("sent_topic")
                    ),
                   box(width = 3, id="map_sentece_slider_wrapper", title = "Sentence options",
                       whatViz("A slider to set the maxium amount of sample sentences."),
                       whyViz("To adjust how many sentece samples you want to see."),
                       sliderInput("topis_sentence_slider", label = "Senteces allowed", min=1, max=10, value=5, step=1, sep=""),
                       box(width = 12, class="infoViz", title = "How to use",
                           collapsible = T, collapsed = T,
                           howViz("Drag the cirkle on the slider to any number.")
                       )
                   )
          )
      ),
      # Sentiment ----
      tabItem(tabName = "sentiment",
              h2("Sentiment"),
              fluidRow(class="box_align_layout",
                tabBox(width=12, id = "speeches",
                       title="Sentiment of speaches",
                  tabPanel("Sentiment relationship (Bubles)",
                           whatViz("Sentiment relationship shows the relationship between positive and negative and size of speeches."),
                           whyViz("The sentiment of speeches could indicate influences in topics. Influences from a year needs to be researched independently."),
                           highchartOutput("sentiment_of_speech_bubles", height="75vh"),
                           box(width = 12, class="infoViz", title = "How to use",
                               collapsible = T, collapsed = T,
                               howViz("Interpretation: The center position of a year-circle indicates it's sentiment. X-axis indicates positive sentiment from low (left) to high (right). Y-axis indicates negative sentiment from high (bottom) to low (top)."),
                               howViz("Tooltip: Hovering a year-circle shows its positive sentiment, negative sentiment, summed sentiment, sentiment label and words with polarity in year in a tooltip."),
                               howViz("Year filter: Using the year filter, will filter years featured."),
                               howViz("Featured words: Using the featured words, will group the years into years that include words in the filter, and years that do not. This will be displayed in the tooltip.")
                           )
                  ),
                  tabPanel("Sentiment by year (Columns)",
                           whatViz("Sentiment by year shows the positive-, negative- and summed sentiment by year. This model does so in columns."),
                           whyViz("Displaying sentiment in columns like this makes it easy to interpret sentiment sizes in relation between positive-, negative- and summed sentiment and difference between years."),
                           highchartOutput("sentiment_of_speech_col_compare", height="75vh"),
                           box(width = 12, class="infoViz", title = "How to use",
                               collapsible = T, collapsed = T,
                               howViz("Interpretation: Negatives are displayed left and positives right. The Sum shows their aggregated value. Observe the size difference."),
                               howViz("Series filter: By clicking on a series name, it can be disabled until clicked again, or updated by a filter."),
                               howViz("Tooltip: Hovering the values of a year will show the sentiment of enabled series."),
                               howViz("Year filter: Using the year filter, will filter years featured."),
                               howViz("Featured words: Using featured words will show the sentiment that subset had in each year.")
                           )
                  ),
                  tabPanel("Sentiment by year (Shankey)",
                           whatViz("Sentiment by year shows the positive-, negative- and summed sentiment by year. This model does so in ranges."),
                           whyViz("Displaying sentiment in connected ranges, makes it easy to interpret the relation of sentiment between years."),
                           highchartOutput("sentiment_of_speech_sha_compare", height="75vh"),
                           box(width = 12, class="infoViz", title = "How to use",
                               collapsible = T, collapsed = T,
                               howViz("Interpretation: Positive sentiment is the top point. Summed sentiment is the bottom point. The difference is the negative sentiment. Looking at the size of the changing sizes of the shankey can be used to observe changes in sentiment by year."),
                               howViz("Tooltip: Hovering a year displays a tooltip that shows the sentiment values of the year."),
                               howViz("Year filter: Using the year filter, will filter years featured.")
                           )
                  # ),
                  # tabPanel("Average sentiment",
                  #          highchartOutput("sentiment_of_speech_avg", height="75vh"),
                  #          whatViz("Average sentiment displays the average sentiment of by year."),
                  #          whyViz("The average sentiment can be used to observe sentiment of a year better in relation to the words used."),
                  #          howViz("Interpretation: the size of the bars indicate the size of average sentiment in relation to other years."),
                  #          howViz("Tooltip: hovering a bar displays the sentiment of the year."),
                  #          howViz("Year filter: Using the year filter, will filter years featured.")
                  )
                )
              ),
              fluidRow(class="box_align_layout",
                # Show a plot of the generated distribution
                box(width=5, title = "Sentiment of words",
                    whatViz("Sentiment of words displays words by sentiment."),
                    whyViz("Sentiment of words can help you identify what words influenced the sentiment."),
                    highchartOutput("sentiment_of_words", height="35vh"),
                    box(width = 12, class="infoViz", title = "How to use",
                        collapsible = T, collapsed = T,
                        howViz("Interpretation: the most positive words have a polarity of 3. The most negative words have a polarity of -3. "),
                        howViz("Tooltip: hovering a word will display the polarity of the word, and the sentiment category."),
                        howViz("Series filter: Click on a series name to disable it. Click again or set a new filter that affects the visualization to enable it again."),
                        howViz("Year filter: Setting the year filter will filter for the words used in those years."),
                        howViz("Featured words: Setting featured words that have sentient will make them be included before other words (otherwise words are included by frequency). If at least one word with sentiment is present, the series will be names after both if they are positive/negative and if they are included or not."),
                        howViz("Number of words: Number of words filters words included, from most to least frequent.")
                    ),
                    helpText("Notice: Words with a polarity of 0 is filtered.")
                ),
                box(width=5, title = "Frequency used (n uses in total)",
                    whatViz("Frequency used displays words by frequency."),
                    whyViz("Frequency used can help you identify what words influenced the sentiment."),
                    highchartOutput("sentiment_of_words_freq", height="35vh"),
                    box(width = 12, class="infoViz", title = "How to use",
                        collapsible = T, collapsed = T,
                        howViz("Interpretation: Larger bars have higher frequency."),
                        howViz("Series filter: Click on a series name to disable it. Click again or set a new filter that affects the visualization to enable it again."),
                        howViz("Year filter: Setting the year filter will filter for the words used in those years."),
                        howViz("Featured words: Setting featured words that have sentient will make them be included before other words (otherwise words are included by frequency). If at least one word with sentiment is present, the series will be names after both if they are positive/negative and if they are included or not."),
                        howViz("Number of words: Number of words filters words included, from most to least frequent.")
                    ),
                    helpText("Notice: Words with a polarity of 0 is filtered.")
                ),
                box(width = 2, title = "Number of words",
                    whatViz("Number of words is a slider to set the number of featured words."),
                    whyViz("Limiting the amount words featured will reduce clutter and make Sentiment of words and Frequency used easier to interpret."),
                    sliderInput("slider_sentiment_of_words_n_words",
                                "Number of words (by frequency)",
                                min=1, max=n_dist_t_headword, value = 100, step = 1, sep=""),
                    box(width = 12, class="infoViz", title = "How to use",
                        collapsible = T, collapsed = T,
                        howViz("Set a number by using the slider.")
                    )
                )
              ),
              fluidRow(
                box(width = 12, title = "Total sentiment",
                    fluidRow(
                      whatViz("Total sentiment displays the total sentiment statistics."),
                      whyViz("Total values can be useful to interpret for selections.")
                    ),
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
                    # fluidRow(
                    #   valueBoxOutput("mean_sum_sen"),
                    #   valueBoxOutput("mean_pos_sen"),
                    #   valueBoxOutput("mean_neg_sen")
                    # ),
                    # fluidRow(
                    #   valueBoxOutput("mean_num_wor")
                    # ),
                    fluidRow(
                      box(width = 12, class="infoViz", title = "How to use",
                          collapsible = T, collapsed = T,
                          howViz("Interpretation: Values are summed for different categories."),
                          howViz("Year filter: Setting the year filter will filter for the words used in those years."),
                          howViz("Featured words: Setting featured words will filter for the words used in that selection.")
                      )
                    )
                )
              )
      ),
      # Map ----
      tabItem(tabName = "map",
              h2("Map"),
              fluidRow(class="box_align_layout",
                box(width=8,  
                    whatViz("A world map, with countries mentioned in speeches."),
                    whyViz("To know where countries are, and how often they are mentioned."),
                    leafletOutput("map", height = 740),
                    # uiOutput("sentences"),
                    box(width = 12, class="infoViz", title = "How to use",
                        collapsible = T, collapsed = T,
                        howViz("Interpretation: Highlighted countries are mentioned. Darker colors represent more mentions."),
                        howViz("Tooltip: hovering or clicking brings up a tooltip, showing total mentions and amount of mentions in each relevant year."),
                        howViz("Year filter: Setting the year filter will filter for the countries mentioned in those years.")
                    )
                ),
                box(width=4, title="Countries mentioned",
                    whatViz("A bar chart showing the amount of times a country is mentioned for each year."),
                    whatViz("A boxplot showing the average sentiment of sentences in which a country is mentioned"),
                    whyViz("To compare the mentions of a specific country, to mentions of countries in general"),
                    highchartOutput("n_hist", height="50vh"),
                    highchartOutput("sent_box"),
                    box(width = 12, class="infoViz", title = "How to use",
                        collapsible = T, collapsed = T,
                        howViz("Tooltip bar chart: hovering over a bar shows the amount a specific country is mentioned in that year, compared to mentions of countries in general"),
                        howViz("Tooltip boxplot: hovering over a box shows distribution statistics of the average sentiment of sentences, in which a specific country is mentioned, compared to the distribution of average sentiment of sentences, in which any country is mentioned.")
                    )
                )
              ),
              fluidRow(class="box_align_layout",
                        box(width=8, title = textOutput("map_sentce_title"),
                           whatViz("Samples of mentions."),
                           whyViz("To see examples of mentions."),
                            uiOutput("sentences"),
                           box(width = 12, class="infoViz", title = "How to use",
                               collapsible = T, collapsed = T,
                               howViz("Click on a country in the map above."),
                           )
                        ),
                box(width = 4, id="map_sentece_slider_wrapper", title = "Sentence options",
                    whatViz("A slider to set the maxium amount of sample sentences."),
                    whyViz("To adjust how many sentece samples you want to see."),
                    sliderInput("map_sentence_slider", label = "Senteces allowed", min=1, max=149, value=5, step=1, sep=""),
                    box(width = 12, class="infoViz", title = "How to use",
                        collapsible = T, collapsed = T,
                        howViz("Drag the cirkle on the slider to any number."),
                    )
                )
              )
      ),
      # Stats ----
      tabItem(tabName = "stats",
              h2("Word statistics"),
              # fluidRow(class="box_align_layout",
              #   tabBox(width=12, id = "word_ussage", title="Speech length by words used",
              #          tabPanel("Spline",
              #                   highchartOutput("speech_length_spline", height="50vh"),
              #                   box(width = 12, class="infoViz", title = "How to use",
              #                       collapsible = T, collapsed = T,
              #                       whatViz(""),
              #                       whyViz(""),
              #                       howViz("Interpretation:"),
              #                       howViz("Tooltip:"),
              #                       howViz("Year filter: Setting the year filter will filter for the words used in those years."),
              #                       howViz("Featured words: Setting featured words will filter for the words set in selection.")
              #                   )
              #          ),
              #           tabPanel("Columns",
              #                    highchartOutput("speech_length_col", height="50vh"),
              #                    box(width = 12, class="infoViz", title = "How to use",
              #                        collapsible = T, collapsed = T,
              #                        whatViz(""),
              #                        whyViz(""),
              #                        howViz("Interpretation:"),
              #                        howViz("Tooltip:"),
              #                        howViz("Year filter: Setting the year filter will filter for the words used in those years."),
              #                        howViz("Featured words: Setting featured words will filter for the words set in selection.")
              #                    )
              #           ),
              #           tabPanel("Piechart",
              #                    highchartOutput("speech_length_pie", height="50vh"),
              #                    box(width = 12, class="infoViz", title = "How to use",
              #                        collapsible = T, collapsed = T,
              #                        whatViz(""),
              #                        whyViz(""),
              #                        howViz("Interpretation:"),
              #                        howViz("Tooltip:"),
              #                        howViz("Year filter: Setting the year filter will filter for the words used in those years."),
              #                        howViz("Featured words: Setting featured words will filter for the words set in selection.")
              #                    )
              #           )
              #   )
              # ),
              fluidRow(class="box_align_layout",
                box(width=2, title = "Speech statistics sliders",
                    whatViz("Top frequent numbers is a slider to filter for frequency of words in word themed visualizations."),
                    whyViz("Many words can be mentioned. Most of these visualizations can become to cluttered to interpret."),
                    sliderInput("slider_word_ussage",
                                "Top frequent numbers",
                                min=1, max=number_of_rarity, value = 15,
                                step = 1, sep=""),
                    box(width = 12, class="infoViz", title = "How to use",
                        collapsible = T, collapsed = T,
                        howViz("Use the slider by dragging the circle to a any number."),
                        p("Tip: Don't choose a too high number at first. Start low. Models can become to cluttered to interpret and it will take a long time to load visualizations.")
                    )
                ),
                tabBox(width=10, id = "word_ussage", title="Most used words",
                       tabPanel("Aggregated word frequency",
                                whatViz("Aggregated word frequency is a pie-chart displaying aggregated frequency of words."),
                                whyViz("Frequency is a major factor for determining topics."),
                                highchartOutput("word_ussage_pie", height="50vh"),
                                box(width = 12, class="infoViz", title = "How to use",
                                    collapsible = T, collapsed = T,
                                    howViz("Interpretation: Frequency can be read from the labels."),
                                    howViz("Tooltip: Hovering a word will display a tooltip showing the frequency of the word, and frequency in percentage in relation to selection. Clicking a word will highlight it."),
                                    howViz("Top frequent numbers: Top frequent numbers will filter for the most frequent numbers by the specified amount."),
                                    howViz("Year filter: Setting the year filter will filter for the words used in those years."),
                                    howViz("Featured words: Setting featured words will filter for the words set in selection.")
                                ),
                                helpText("Notice: Due to performance issues, stream graph is limited to a maximum frequency of 40. Any number set above, will result in 40 being featured.")
                       ),
                       tabPanel("Word frequency by year (Stream graph)",
                                collapsible = T, collapsed = T,
                                whatViz("This shows the frequency of words by year with a stream."),
                                whyViz("The model is useful for determining frequency of words and the relation with other words, especially overviewing changing frequency through the years."),
                                highchartOutput("word_ussage_streamgraph", height="50vh"),
                                box(width = 12, class="infoViz", title = "How to use",
                                    howViz("Interpretation: Words are represented by streams, and the width represents the frequency in a year of a selection of words."),
                                    howViz("Tooltip: Hovering above a year displays a tooltip for the year. It will display the frequency of each word in that year."),
                                    howViz("Top frequent numbers: Top frequent numbers will filter for the most frequent numbers by the specified amount."),
                                    howViz("Year filter: Setting the year filter will filter for the words used in those years."),
                                    howViz("Featured words: Setting featured words will filter for the words set in selection.")
                                ),
                                helpText("Notice: Due to performance issues, stream graph is limited to a maximum frequency of 20. Any number set above, will result in 20 being featured.")
                       ),
                       tabPanel("Word frequency by year (Columns)",
                                whatViz("This shows the frequency of words by year with stacked columns."),
                                whyViz("The model is useful for determining frequency of words and the relation with other words of a selection of words."),
                                highchartOutput("word_ussage_col", height="50vh"),
                                box(width = 12, class="infoViz", title = "How to use",
                                    collapsible = T, collapsed = T,
                                    howViz("Interpretation: Each year has a bar. The height of a bar is the total frequency of word selection. Each bar-part represents a word, and the height, the frequency of the word."),
                                    howViz("Tooltip: Hovering a year will display a tooltip. The tooltip will list the year, the total frequency, and the frequency of each year."),
                                    howViz("Top frequent numbers: Top frequent numbers will filter for the most frequent numbers by the specified amount."),
                                    howViz("Year filter: Setting the year filter will filter for the words used in those years."),
                                    howViz("Featured words: Setting featured words will filter for the words set in selection.")
                                ),
                                helpText("Notice: Due to performance issues, stream graph is limited to a maximum frequency of 30. Any number set above, will result in 30 being featured.")
                       ),
                       # tabPanel("Columns percent",
                       #          highchartOutput("word_ussage_col_per", height="50vh"),
                       #          box(width = 12, class="infoViz", title = "How to use",
                       #              collapsible = T, collapsed = T,
                       #              whatViz(""),
                       #              whyViz(""),
                       #              howViz("Interpretation:"),
                       #              howViz("Tooltip:"),
                       #              howViz("Top frequent numbers: Top frequent numbers will filter for the most frequent numbers by the specified amount."),
                       #              howViz("Year filter: Setting the year filter will filter for the words used in those years."),
                       #              howViz("Featured words: Setting featured words will filter for the words set in selection.")
                       #          )
                       # ),
                       tabPanel("Word frequency by year (scatterplot)",
                                whatViz("This model displays words as data point by year and frequency."),
                                whyViz("The model is useful for determining frequency of words in relation to other years in same year."),
                                whyViz("The model is useful for detecting large values,  big influencers, and potential outliers."),
                                highchartOutput("word_ussage_scatter", height="50vh"),
                                box(width = 12, class="infoViz", title = "How to use",
                                    collapsible = T, collapsed = T,
                                    howViz("Interpretation: Each year, each word is represented by a datapoint in relation to its frequency."),
                                    howViz("Tooltip: Hovering a datapoint, will display a tooltip for the point. The tool"),
                                    howViz("Top frequent numbers: Top frequent numbers will filter for the most frequent numbers by the specified amount."),
                                    howViz("Year filter: Setting the year filter will filter for the words used in those years. It will display the word, the year, frequency of year, and frequency of word in all of the years, even outside of set filter."),
                                    howViz("Featured words: Setting featured words will filter for the words set in selection.")
                                ),
                                helpText("Notice: Due to performance issues, scatterplot is limited to a maximum frequency of 20. Any number set above, will result in 20 being featured.")
                       ),
                       tabPanel("Wordcloud",
                           whatViz("Words in the wordcloud is randomly selected from available words."),
                           whyViz("The wordcloud can give an impression of what a selection of words contain. They are not very useful."),
                           wordcloud2Output("wordcloud"),
                           box(width = 12, class="infoViz", title = "How to use",
                               collapsible = T, collapsed = T,
                               howViz("Interpretation: There are words. Size represents frequency."),
                               howViz("Tooltip: Hovering the filling of a word displays a tooltip. The tooltip displays the frequency of the word."),
                               howViz("Top frequent numbers: Top frequent numbers will filter for the most frequent numbers by the specified amount."),
                               howViz("Year filter: Setting the year filter will filter for the words used in those years."),
                               howViz("Featured words: Setting featured words will filter for the words set in selection.")
                           )
                       # ),
                       # tabPanel("Table",
                       #          DTOutput("word_ussage_tbl"),
                       #          box(width = 12, class="infoViz", title = "How to use",
                       #              collapsible = T, collapsed = T,
                       #              whatViz(""),
                       #              whyViz(""),
                       #              howViz("Interpretation:"),
                       #              howViz("Tooltip:"),
                       #              howViz("Top frequent numbers: Top frequent numbers will filter for the most frequent numbers by the specified amount."),
                       #              howViz("Year filter: Setting the year filter will filter for the words used in those years."),
                       #              howViz("Featured words: Setting featured words will filter for the words set in selection.")
                       #          )
                       )
                )
              )
      ),
      # Data ----
      tabItem(tabName = "data",
              h2("Data source and handling"),
              fluidRow(class="box_align_layout",
                box(width=4, title = "Data preparetaion",
                    p("Stopwords are filtered. This is done to avoid the most common words (like \"the\") to dominate the statistics. This is done with stopwords defined by Bertel Torp, stopwords defined by snowball and custom stopwords found from interacting with data."),
                    p("Words have been stemmed, to get better data for topics. This does remove information about word forms. this is done with snowballC for R"),
                    p("Words have been lemmatized (replacing words with identical meaning with a headword), to improve topic analysis, by using udpipe for R.")
                ),
                box(width=4, title = "Topics",
                    p("Todo")
                ),
                box(width=4, title="Sentiment analysis",
                  p("For danish: Sentiment analysis is done by getting sentiment values from \"Det Danske Sprog- og Litteraturselskab (DSL, Society for Danish Language and Literature) and Center for Sprogteknologi, Københavns Universitet (CST, Centre for Language Technology, University of Copenhagen)\", and adding the the values to the words we collected on word level, stem level and lemmatized values."), 
                  p("For danish: Sentiment analysis is done by getting sentiment values from \"todo\", and adding the the values to the words we collected on word level, stem level and lemmatized values.")  
                )
              ),
              fluidRow(class="box_align_layout",
                       box(width=6,
                           title = "Speeches covered",
                           whatViz("This are the speeches that are covered in the dashboard, with your current filter."),
                           whyViz("You can use this to see the source from what you filtered."),
                           uiOutput("Covered_speech"),
                           box(width = 12, class="infoViz", title = "How to use",
                               collapsible = T, collapsed = T,
                               howViz("You can use language and year filters. To see all text in a language selected language, select all possible years.")
                           ),
                           p(helpText("Notice: English version of speech are translations. Not all Danish speeches are translated. Some years are therefore not available."))
                       ),
                       box(width=6,
                           div(id="wiki_infobox_wrap",
                               uiOutput("scrabing_info"),
                               whatViz("This is the info box from Wikipedia about the queen."),
                               whyViz("You can use this to familiarize yourself with the subject, or just want a quick read-up."),
                               htmlOutput("wiki_infobox")
                           ),
                           box(width = 12, class="infoViz", title = "How to use",
                               collapsible = T, collapsed = T,
                               howViz("You can read the table, or visit the source, above the table.")
                           )
                       )
              )
      ),
      # How to ----
      tabItem(tabName = "howto",
              h2("How to operate dashboard"),
              fluidRow(class="box_align_layout",
                box(width=6, title="How to interpret visualizations and information",
                    p("If you wish to have some guidance in your data journey, we have prepared assisting labels for each visualization."),
                    p("These labels will be located directly beneath the point of interest."),
                    hr(),
                    p("Click the icon in the top right cornor to hide/expand section."),
                    whatViz("What marking: A blue dot with bold dark-blue text indicates what kind of visualization/information is depicted."),
                    whyViz("Why marking: A Yellow circle with dark-yellow bold text indicates why and when to use you could use the visualization/information you can observe."),
                    box(width = 12, class="infoViz", title = "How to use",
                        collapsible = T, collapsed = F,
                        howViz("How marking: A light-brown triangle with brown bold text indicates how you can interpret information in the visualization."),
                    ),
                    p(helpText("Grey italic/slanted text indicates other kinds of help text. This could be descriptions on how operate inputs, information about limitations and bugs.")),
                    p("Black text is other kinds of information. It would usually be practical information.")
                ),
                box(width=6, title="Filters",
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
      )
    )
              
  )
)