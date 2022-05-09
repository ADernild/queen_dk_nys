ui <- dashboardPage(
  dashboardHeader(title = "TV2 Fyn Topic Tool"
  ),
  dashboardSidebar(
    sidebarMenuOutput("menu")
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "sizes_n_stuff.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "tv2fyn.css"),
      # uiOutput("royall_beautyfication"),
      tags$link(rel = "shortcut icon", href = "favicon.ico"),
      tags$script(src = "js.js")
    ),
    conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                     tags$div(span("Loading..."), id = "loadmessage")
    ),
    # Index/Home ----
    tabItems(
      tabItem(tabName = "index",
              h1("TV2 Fyn Topic Tool"),
              h2("Home"),
              fluidRow(class="box_align_layout",
                box(width=12,
                    title = "TV2 Fyn Topic Tool - A Dashboard Tool to Analyze Articles from TV2 Fyn",
                    div(id = "intoduction",
                        p(class="what", "This is a tool to analyze articles from TV2 Fyn, with emphasis on unseen categories (LDA), sentiment, words and positional data."),
                        p(class="why", "The tool can be used to analyze articles and detect patterns."),
                        p(class="how", "This is an interactive dashboard. You can select tabs in the sidebar on the left to navigate to different sections. When sidebar is expanded (see toggle button in top left corner), you can apply filters and settings for your liking."),
                        p(class="bold", "Together we make Funen better - Esben Seerup.")
                    )
                )#,
                # box(width=2,
                #     img(src='queen_logo.png', align="right")
                # )
              ),
              fluidRow(
                box(width = 12, title = "Summary of Selection",
                    fluidRow(
                      howViz("Use filter, or update through choosing topic on Topics-tab, to filter data, and the observed data will be updated here."),
                      whatViz("Statistics included displays the aggregated statistics for various covered topics."),
                      whyViz("Total values can give a idea about what is about to be covered.")
                    ),
                    fluidRow(
                      valueBoxOutput("total_covered"), # Amount of content covered
                      valueBoxOutput("total_word"), # Amount of unique words covered
                      valueBoxOutput("total_word_unique") # Amount of unique words covered
                    ),
                    fluidRow(
                      valueBoxOutput("total_amount_of_topics", width = 6), # Amount of topics covered
                      valueBoxOutput("total_sentences", width = 6) # Amount of words covered
                      # valueBoxOutput("total_featured_words") # Amount of featured words
                    ),
                    # fluidRow(
                    #   valueBoxOutput("total_countries_mentioned"), # Amount of countries mentioned
                    # ),
                    fluidRow(
                      valueBoxOutput("total_sum_sen"), # Amount of total sentiment
                      valueBoxOutput("total_pos_sen"), # Amount of positive sentiment
                      valueBoxOutput("total_neg_sen") # Amount of negative sentiment
                    ),
                    fluidRow(
                      valueBoxOutput("total_num_wor"), # Amount of words with sentiment
                      valueBoxOutput("num_pos_sen"), # Amount of words with positive sentiment
                      valueBoxOutput("num_neg_sen") # Amount of words with negative sentiment
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
                          howViz("Interpretation: Values are summed for different categories.")
                          # howViz("Featured words: Setting featured words will filter for the words used in that selection.")
                      )
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
                # whatViz("Options to set prefference to update featured words from topic model."),
                whatViz("Options to set prefference to update topic in filter."),
                whyViz("You can more easily inspect topic in other tabs by quicly obtaining their values."),
                radioButtons ("topic_r",
                              # label = "Update featured words with topics content",
                              label = "Update filter topic with topic model selection",
                              selected = 1,
                              choiceNames = c(
                                "Update on topic selection",
                                # "Update on word selection", 
                                "Do not update"),
                              choiceValues = c(
                                1,
                                # 2,
                                F
                                )
                ),
                p(helpText("Notice: \"Update on topic selection\" will clear previous selection(s).")),
                whatViz("Number of terms to display sets the number of terms used in the topic model, by size of topic."),
                whyViz("Limiting the number of terms makes the topic model easier to interpret. Adding more, gives more information."),
                fluidRow(sliderInput("nTerms", "Number of terms to display", min = 10, max = 100, value = 50, sep="")),
                box(width = 12, class="infoViz", title = "How to use",
                    collapsible = T, collapsed = T,
                    howViz("Setting update prefferece: Click a a setting that fits your needs the most."),
                    howViz("Choose terms to display: Use the slider to select a number by dragging the dot to any number.")
                )
            ),
            box(width=10, title = "Topic model",
                whatViz("Topic model is a tool that maps topics. It does so by analyzing word relations by analyzing all articles and mapping the relations as word matrixes. Then the model displays the topics to the left, and the words within on the right. Frequency of use is depicted by size for topics, and bar length for words. Topics are named 1:n, where 1 is the largest topic."),
                whyViz("The topics model can be used to get an understanding of themes present in the articles throughout the years."),
                p(helpText("Topics are automaticaly found during initilization of topics. They are not named, as automatic naming of topics has not been implemented, and no human effort has been allocated to interpret the 100 topics. :-)")),
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
                   box(width = 3, id="map_sentece_slider_wrapper", title = "Sentence options",
                       whatViz("A slider to set the maxium amount of sample sentences."),
                       whyViz("To adjust how many sentece samples you want to see."),
                       sliderInput("topis_sentence_slider", label = "Senteces allowed", min=0, max=10, value=5, step=1, sep=""),
                       box(width = 12, class="infoViz", title = "How to use",
                           collapsible = T, collapsed = T,
                           howViz("Drag the circle on the slider to any number.")
                       )
                   ),
                   box(width=5, title = textOutput("topics_sentce_title"),
                       whatViz("Samples of sentences belonging to chosen topic."),
                       whyViz("To see examples of sentences in chosen topic."),
                       uiOutput("topicText"),
                       box(width = 12, class="infoViz", title = "How to use",
                           collapsible = T, collapsed = T,
                           howViz("Click on a a topic in the topic map above.")
                       )
                   ),
                   box(width=4, title = textOutput("topics_means_title"),
                       whatViz("This boxplot shows the average sentiment of sentences belonging to a specific topic, compared to the rest of the topics."),
                       whyViz("To compare a specific topics sentiment to the rest of the topics."),
                       highchartOutput("sent_topic"),
                       box(width = 12, class="infoViz", title = "How to use",
                           collapsible = T, collapsed = T,
                           howViz("Click on a a topic in the topic map above to make it appear."),
                           howViz("Hover over a box in the boxplot to see descriptive statistics.")
                       )
                   )
          )
      ),
      # Sentiment ----
      tabItem(tabName = "sentiment",
              h2("Sentiment"),
              fluidRow(class="box_align_layout",
                tabBox(width=12, id = "speeches",
                       title="Sentiment of articles",
                  tabPanel("Sentiment by Article (Columns)",
                           whatViz("Sentiment by Article shows the positive-, negative- and summed sentiment by Article. This model does so in columns."),
                           whyViz("Displaying sentiment in columns like this makes it easy to interpret sentiment sizes in relation between positive-, negative- and summed sentiment and difference between Articles."),
                           highchartOutput("sentiment_of_speech_col_compare", height="75vh"),
                           box(width = 12, class="infoViz", title = "How to use",
                               collapsible = T, collapsed = T,
                               howViz("Interpretation: Negatives are displayed left and positives right. The Sum shows their aggregated value. Observe the size difference."),
                               howViz("Series filter: By clicking on a series name, it can be disabled until clicked again, or updated by a filter."),
                               howViz("Tooltip: Hovering the values of a data-point will show the sentiment of enabled series."),
                               howViz("UUID filter: Using the uuid-filter, will filter Articles featured.")
                               # howViz("Featured words: Using featured words will show the sentiment that subset had in each Article.")
                           )
                  ),
                  tabPanel("Sentiment relationship (Bubles)",
                           whatViz("Sentiment relationship shows the relationship between positive and negative and size of articles."),
                           whyViz("The sentiment of articles could indicate influences in topics. Influences from an Article needs to be researched independently."),
                           highchartOutput("sentiment_of_speech_bubles", height="75vh"),
                           box(width = 12, class="infoViz", title = "How to use",
                               collapsible = T, collapsed = T,
                               howViz("Interpretation: The center position of a circle indicates it's sentiment. X-axis indicates positive sentiment from low (left) to high (right). Y-axis indicates negative sentiment from high (bottom) to low (top)."),
                               howViz("Tooltip: Hovering a circle shows its positive sentiment, negative sentiment, summed sentiment, sentiment label and words with polarity in a tooltip."),
                               howViz("UUID filter: Using the uuid-filter, will filter Articles featured.")
                               # howViz("Featured words: Using the featured words, will group the Articles into Articles that include words in the filter, and Articles that do not. This will be displayed in the tooltip.")
                           )
                  ),
                  tabPanel("Sentiment by Article (Range and lines)",
                           whatViz("Sentiment by Article shows the positive-, negative- and summed sentiment by Articles. This model does so in ranges."),
                           whyViz("Displaying sentiment in connected ranges, makes it easy to interpret the relation of sentiment between them."),
                           highchartOutput("sentiment_of_speech_sha_compare", height="75vh"),
                           box(width = 12, class="infoViz", title = "How to use",
                               collapsible = T, collapsed = T,
                               howViz("Interpretation: Positive sentiment, Summed sentiment and negative sentiment is displayed. The range between positive and summed sentiment is highlighted. The difference between postive- and summed sentiment is the same value as the is the negative sentiment. Looking at the size of the changing sizes of the range can be used to observe changes in sentiment by Article."),
                               howViz("Tooltip: Hovering a data-point will display a tooltip that shows the sentiment values of the Article."),
                               howViz("UUID filter: Using the uuid-filter, will filter Articles featured.")
                               # howViz("Featured words: Using featured words will show the sentiment that subset had in each Article.")
                           )
                  ),
                  tabPanel("Average sentiment",
                           highchartOutput("sentiment_of_speech_avg", height="75vh"),
                           whatViz("Average sentiment displays the average sentiment of by Article."),
                           whyViz("The average sentiment can be used to observe sentiment of a Article better in relation to the words used."),
                           howViz("Interpretation: the size of the bars indicate the size of average sentiment in relation to other Articles."),
                           howViz("Tooltip: hovering a bar displays the sentiment of the Article."),
                           howViz("UUID filter: Using the uuid-filter, will filter Articles featured.")
                  )
                )
              ),
              fluidRow(class="box_align_layout",
                # Show a plot of the generated distribution
                box(width=5, title = "Sentiment of words",
                    whatViz("Sentiment of words displays words by sentiment."),
                    whyViz("Sentiment of words can help you identify what words influenced the sentiment."),
                    p(helpText("Notice: Words with a polarity of 0 is filtered.")),
                    highchartOutput("sentiment_of_words", height="35vh"),
                    box(width = 12, class="infoViz", title = "How to use",
                        collapsible = T, collapsed = T,
                        howViz("Interpretation: the most positive words have a polarity of 3. The most negative words have a polarity of -3. "),
                        howViz("Tooltip: hovering a word will display the polarity of the word, and the sentiment category."),
                        howViz("Series filter: Click on a series name to disable it. Click again or set a new filter that affects the visualization to enable it again."),
                        howViz("UUID filter: Using the uuid-filter, will filter Articles featured."),
                        # howViz("Featured words: Setting featured words that have sentient will make them be included before other words (otherwise words are included by frequency). If at least one word with sentiment is present, the series will be names after both if they are positive/negative and if they are included or not."),
                        howViz("Number of words: Number of words filters words included, from most to least frequent.")
                    )
                ),
                box(width=5, title = "Frequency used (n uses in total)",
                    whatViz("Frequency used displays words by frequency."),
                    whyViz("Frequency used can help you identify what words influenced the sentiment."),
                    p(helpText("Notice: Words with a polarity of 0 is filtered.")),
                    highchartOutput("sentiment_of_words_freq", height="35vh"),
                    box(width = 12, class="infoViz", title = "How to use",
                        collapsible = T, collapsed = T,
                        howViz("Interpretation: Larger bars have higher frequency."),
                        howViz("Series filter: Click on a series name to disable it. Click again or set a new filter that affects the visualization to enable it again."),
                        howViz("UUID filter: Using the uuid-filter, will filter Articles featured."),
                        # howViz("Featured words: Setting featured words that have sentient will make them be included before other words (otherwise words are included by frequency). If at least one word with sentiment is present, the series will be names after both if they are positive/negative and if they are included or not."),
                        howViz("Number of words: Number of words filters words included, from most to least frequent.")
                    )
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
              fluidRow(class="box_align_layout",
                box(width = 12, title = "Article sentiment inspection",
                    whatViz("A inpection tool for observing what words create polarity."),
                    whyViz("Inspecting an article might be pratical to find what exactly adds polarity."),
                    p(helpText("This is not meant for acctually reading articles.For optimization and easy polarity inspection, special characters, text formatting, images, whitespace and numbers have been removed.")),
                    uiOutput("single_article_inspecter"),
                    box(width = 12, class="infoViz", title = "How to use",
                        collapsible = T, collapsed = T,
                        howViz("Select only a single article to display article."),
                        whatViz("Words with positive polarity have a shade of blue."),
                        whatViz("Words with negative polarity have a shade of red."),
                        whatViz("Stopwords, words that are ignored, have a grey shade.")
                    )
                )
              )
      ),
      # Fyn Map ----
      tabItem(tabName = "fyn",
              h2("Locations"),
              fluidRow(class="box_align_layout",
                box(width=8, title = "Fyn komunes", # Fyn comunes
                    whatViz("A map of Fyn, with locations set in articles."),
                    whyViz("To know where locations are, and how often they are mentioned."),
                    leafletOutput("map_fyn_komunes", height = 740),
                    box(width = 12, class="infoViz", title = "How to use",
                        collapsible = T, collapsed = T,
                        howViz("Interpretation: Highlighted locations are locations used. Darker colors represent more ussage."),
                        howViz("Tooltip: hovering or clicking brings up a tooltip, showing total features.")
                    )
                ),
                box(width = 4, title = "Other zones", # Denmark
                    leafletOutput("map_fyn_danmark", height = 246),
                    leafletOutput("map_fyn_southjutland", height = 246),
                    leafletOutput("map_fyn_fyn", height = 246),
                    box(width = 12, class="infoViz", title = "How to use",
                        collapsible = T, collapsed = T,
                        howViz("Interpretation: Highlighted locations are locations used. Darker colors represent more ussage."),
                        howViz("Tooltip: hovering or clicking brings up a tooltip, showing total features.")
                    )
                )
              )
      ),
      # Map ----
      tabItem(tabName = "map",
              h2("Countries"),
              fluidRow(class="box_align_layout",
                box(width=8,  
                    whatViz("A world map, with countries mentioned in articles."),
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
                               howViz("Click on a country in the map above.")
                           )
                        ),
                box(width = 4, id="map_sentece_slider_wrapper", title = "Sentence options",
                    whatViz("A slider to set the maxium amount of sample sentences."),
                    whyViz("To adjust how many sentece samples you want to see."),
                    sliderInput("map_sentence_slider", label = "Senteces allowed", min=1, max=149, value=5, step=1, sep=""),
                    box(width = 12, class="infoViz", title = "How to use",
                        collapsible = T, collapsed = T,
                        howViz("Drag the cirkle on the slider to any number.")
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
                       tabPanel("Aggregated word freq.",
                                whatViz("Aggregated word frequency is a pie-chart displaying aggregated frequency of words."),
                                whyViz("Frequency is a major factor for determining topics."),
                                highchartOutput("word_ussage_pie", height="50vh"),
                                p(helpText("Notice: Due to performance issues, stream graph is limited to a maximum frequency of 40. Any number set above, will result in 40 being featured.")),
                                box(width = 12, class="infoViz", title = "How to use",
                                    collapsible = T, collapsed = T,
                                    howViz("Interpretation: Frequency can be read from the labels."),
                                    howViz("Tooltip: Hovering a word will display a tooltip showing the frequency of the word, and frequency in percentage in relation to selection. Clicking a word will highlight it."),
                                    howViz("Top frequent numbers: Top frequent numbers will filter for the most frequent numbers by the specified amount."),
                                    howViz("UUID filter: Using the uuid-filter, will filter Articles featured.")
                                    # howViz("Featured words: Setting featured words will filter for the words set in selection.")
                                )
                       ),
                       tabPanel("Word freq. by Article (Stream graph)",
                                collapsible = T, collapsed = T,
                                whatViz("This shows the frequency of words by Article with a stream."),
                                whyViz("The model is useful for determining frequency of words and the relation with other words, especially overviewing changing frequency between Articles."),
                                highchartOutput("word_ussage_streamgraph", height="50vh"),
                                p(helpText("Notice: Due to performance issues, stream graph is limited to a maximum frequency of 20. Any number set above, will result in 20 being featured.")),
                                box(width = 12, class="infoViz", title = "How to use",
                                    howViz("Interpretation: Words are represented by streams, and the width represents the frequency in a data-points."),
                                    howViz("Tooltip: Hovering above a data-point displays a tooltip for the data-point. It will display the frequency of each word in that data-point."),
                                    howViz("Top frequent numbers: Top frequent numbers will filter for the most frequent numbers by the specified amount."),
                                    howViz("UUID filter: Using the uuid-filter, will filter Articles featured.")
                                    # howViz("Featured words: Setting featured words will filter for the words set in selection.")
                                )
                       ),
                       tabPanel("Word freq. by A. (Columns)",
                                whatViz("This shows the frequency of words by Article with stacked columns."),
                                whyViz("The model is useful for determining frequency of words and the relation between articles."),
                                highchartOutput("word_ussage_col", height="50vh"),
                                p(helpText("Notice: Due to performance issues, stream graph is limited to a maximum frequency of 30. Any number set above, will result in 30 being featured.")),
                                box(width = 12, class="infoViz", title = "How to use",
                                    collapsible = T, collapsed = T,
                                    howViz("Interpretation: Each Article has a bar. The height of a bar is the total frequency of word selection. Each bar-part represents a word, and the height, the frequency of the word."),
                                    howViz("Tooltip: Hovering a data-point will display a tooltip. The tooltip will list the data-point, the total frequency, and the frequency of each data-point."),
                                    howViz("Top frequent numbers: Top frequent numbers will filter for the most frequent numbers by the specified amount."),
                                    howViz("UUID filter: Using the uuid-filter, will filter Articles featured.")
                                    # howViz("Featured words: Setting featured words will filter for the words set in selection.")
                                )
                       ),
                       tabPanel("Word freq. by A. (scatterplot)",
                                whatViz("This model displays words as data point by Article and frequency."),
                                whyViz("The model is useful for determining frequency of words in relation to other Articles."),
                                whyViz("The model is useful for detecting large values,  big influencers, and potential outliers."),
                                highchartOutput("word_ussage_scatter", height="50vh"),
                                p(helpText("Notice: Due to performance issues, scatterplot is limited to a maximum frequency of 20. Any number set above, will result in 20 being featured.")),
                                box(width = 12, class="infoViz", title = "How to use",
                                    collapsible = T, collapsed = T,
                                    howViz("Interpretation: Each Article, each word is represented by a datapoint in relation to its frequency."),
                                    howViz("Tooltip: Hovering a datapoint, will display a tooltip for the point. The tool"),
                                    howViz("Top frequent numbers: Top frequent numbers will filter for the most frequent numbers by the specified amount."),
                                    howViz("UUID filter: Using the uuid-filter, will filter Articles featured."),
                                    howViz("Featured words: Setting featured words will filter for the words set in selection.")
                                )
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
                               howViz("UUID filter: Using the uuid-filter, will filter Articles featured.")
                               # howViz("Featured words: Setting featured words will filter for the words set in selection.")
                           )
                       ),
                       tabPanel("Table",
                                DTOutput("word_ussage_tbl"),
                                box(width = 12, class="infoViz", title = "How to use",
                                    collapsible = T, collapsed = T,
                                    whatViz("A table with the used data."),
                                    whyViz("To manually inspect it or download it after formatting."),
                                    howViz("Interpretation: A table."),
                                    howViz("Top frequent numbers: Top frequent numbers will filter for the most frequent numbers by the specified amount."),
                                    howViz("UUID filter: Using the uuid-filter, will filter Articles featured.")
                                    # howViz("Featured words: Setting featured words will filter for the words set in selection.")
                                )
                       )
                )
              )
      ),
      # Data ----
      tabItem(tabName = "data",
              h2("Data source and handling"),
              fluidRow(class="box_align_layout",
                box(width=4, title = "Data preparetaion",
                    p("Stopwords are filtered. This is done to avoid the most common words (like \"at\") to dominate the statistics. This is done with stopwords defined by Bertel Torp, stopwords defined by snowball and custom stopwords found from interacting with data."),
                    p("Words have been stemmed, to get better data for topics. This does remove information about word forms. this is done with snowballC for R"),
                    p("Words have been lemmatized (replacing words with identical meaning with a headword), to improve topic analysis, by using udpipe for R.")
                ),
                box(width=4, title = "Topics",
                    p("Topics have been derived with a Structural Topic Model (STM) that is characterized by estimating topic distributions using a covariate matrix as well as a document-term matrix."),
                    p("The covariates used for estimating topic distributions and average sentiment is done on sentence level.")
                ),
                box(width=4, title="Sentiment analysis",
                  p("Sentiment analysis is done by getting sentiment values from \"Det Danske Sprog- og Litteraturselskab (DSL, Society for Danish Language and Literature) and Center for Sprogteknologi, Københavns Universitet (CST, Centre for Language Technology, University of Copenhagen)\", and adding the the values to the words we collected on word level, stem level and lemmatized values.")
                )
              ),
              fluidRow(class="box_align_layout",
                       # box(width=2,
                       #     title = "Sorting options?",
                       #     "Placeholder text for sorting options. Perhaps not."
                       # ),
                       box(width=12,
                           title = "Articles covered",
                           whatViz("This are the articles that are covered in the dashboard, with your current filter."),
                           whyViz("You can use this to see the source from what you filtered."),
                           uiOutput("Covered"),
                           box(width = 12, class="infoViz", title = "How to use",
                               collapsible = T, collapsed = T,
                               howViz("Any changes to the filter is applied to the list.")
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
                        howViz("How marking: A light-brown triangle with brown bold text indicates how you can interpret information in the visualization.")
                    ),
                    p(helpText("Grey italic/slanted text indicates other kinds of help text. This could be descriptions on how operate inputs, information about limitations and bugs.")),
                    p("Black text is other kinds of information. It would usually be practical information.")
                ),
                box(width=6, title="Filters",
                    whatViz("This dashboard utilizes filtering systems. The main filters are in the sidebar when expanded. Some visualizations have custom filters. They are documented when needed."),
                    whyViz("The filters can be used to filter the data you see. You can do this to inspect a certain subset of data. E.g., to look data relevant to you."),
                    howViz("To expand the sidebar, press the expand button in the top left corner.")
                )
              ),
              fluidRow(class="box_align_layout",
                       box(width=12, title="Article filters",
                           whatViz("Article filter refers to the group of filters affecting Article. These are: UUID, Article name and Topic."),
                           whyViz("Filtering Articles can help you limit the data relevant to the topic you are researching."),
                           howViz("UUID: Write the UUID of a article or find uuid articles by using the other Article filters or topic-tab."),
                           howViz("Article name - Add Article: select input-field (e.g. by clicking or using tab button) and type the name of an Article. Press enter to add or select suggestion by clicking on it."),
                           howViz("Article name - remove Article(s): click on a Article name and press Backspace-key or Delete-key to remove a Article. Hold down Ctrl to select multiple. Alternatively, select input-field (e.g. by clicking or using tab button), and navigate the cursor with the arrow keys, and use Backspace-key or Delete-key to remove Articles. Or use clear buttons"),
                           howViz("Article name - apply: Click 'Syncronize artciles and id'-button."),
                           howViz("Topic - Add Topic: select input-field (e.g. by clicking or using tab button) and type the name of an Topic. Press enter to add or select suggestion by clicking on it. Using the topic tab, can also set topic. See description on topic tab."),
                           howViz("Topic - remove Topic: click on a Topic name and press Backspace-key or Delete-key to remove a Topic. Or use clear buttons."),
                           howViz("Topic - apply: Click 'Add articles covered in topic'-button."),
                           p(helpText("Notice: Some visualizations will use the whole dataset as a reference regardless of your set filter."))
                       )
                       # box(width=6, title="Featured words",
                       #     whatViz("Featured words is a selection of words from words featured in the articles, that is used to filter or feature words in different kinds of ways."),
                       #     whyViz("When you are inspecting a topic, some words might seem important. So, you can filter for these words using the filter."),
                       #     howViz("Select word: Type word and press enter when done or click suggestion pop up."),
                       #     howViz("Remove word(s): click on a word and press Backspace-key or Delete-key to remove word Hold down Ctrl to select multiple. Alternatively, select input-field (e.g. by clicking or using tab button), and navigate the cursor with the arrow keys, and use Backspace-key or Delete-key to remove words."),
                       #     howViz("Remove words by filter: Click button labled: \"Clear words\""),
                       #     howViz("Regret removal by clear words: Click button labled: \"Regret clear\". It only works for last clearning.")
                       # )
             )
      )
    )
  )
)