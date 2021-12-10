server <- function(input, output) {

  # Sidebar Menu -------------------------------------------------------------
  output$menu <- renderMenu({
      sidebarMenu(
        menuItem("Welcome and about", tabName = "index", icon = icon("home")),
        menuItem("Topic model", tabName = "tm", icon = icon("comment-dots")),
        menuItem("Sentiments", tabName = "sentiment", icon = icon("theater-masks"), selected = T),
        menuItem("Map", tabName = "map", icon = icon("globe-europe")),
        menuItem("Word statistics", tabName = "stats", icon = icon("chart-pie")),
        div(id="sidebar-input",
            h3("Filters"),
            checkboxGroupInput("re",
                               label = "Options",
                               choices = "Allow reactive choises",
                               selected = "Allow reactive choises"
            ),
            radioButtons ("languages",
                          label = "New years eve speech language analyzed",
                          choices = languages,
                          selected = languages[1]
            ),
            helpText("Note: English version of speach are translations. Not all speeches are translated."),
            sliderInput("year", "Years range:",
                        min = year_min, max = year_max,
                        value = range(year_min,year_max),
                        step = 1
            ),
            selectizeInput("words",
                           label="Featured words", choices = words_tokens_all, multiple = TRUE)
          )
        )
  })
  
    # topicVis ----------------------------------------------------------------
  output$topicVis <- renderVis({
      ifelse(input$topicmodel == "lda_model",
             if(!is.null(input$nTerms)){
              with(lda_model,
                    createJSON(phi, theta, doc.length, vocab, term.frequency,
                             R = input$nTerms))
               },
             if(!is.null(input$nTerms)){
              with(stm_model,
                    toLDAvisJson(mod, docs, R = input$nTerms))}
      )
    })
  

  # Sentiment ---------------------------------------------------------------
  ## sentiment_of_speech_data -----------------------------------------------
  sentiment_of_speech_data <- reactive({
    data <- sentiment %>%
      group_by(year) %>% 
      filter(year %in% input$year[1]:input$year[2]) %>%
      arrange(desc(year))
  })

  ## sentiment of speeches ----------------------------------------------------
  output$sentiment_of_speech_bubles <- renderHighchart({
    data <- sentiment_of_speech_data()
    hchart(data,
           type="bubble",
           hcaes(x = sentiment_pos, y = sentiment_neg, z = n_words, name = year, group = year),
           color = "#ffff00",
           showInLegend = F,
           styledMode = T
    ) %>%
      hc_plotOptions(
        series = list(
          allowPointSelect = T,
          color = c("#00FFFF"),
          dataLabels = list(
            enabled= T,
            format = '{point.name}',
            color="FFF"
          ),
          marker = list(
            enabled = T,
            fillColor = '#00FFFF',
            enabledThreshold = F,
            states = "normal",
            symbol = "cirkle"
          ),
          visible = T
       )
      ) %>% 
      hc_yAxis(
        reversed = T,
        startOnTick = T
      ) %>% 
      hc_xAxis(
        startOnTick = T
      )
  })
  
  output$sentiment_of_speech_col_compare <- renderHighchart({
    data <- sentiment_of_speech_data() 
    highchart() %>% 
      hc_add_series(
        type = "bar",
        stack = 1,
        name="Positives",
        data = data$sentiment_pos
      ) %>% 
      hc_add_series(
        type = "bar",
        stack = 1,
        name="Negatives",
        data = data$sentiment_neg
      ) %>% 
      hc_add_series(
        type = "bar",
        stack = 2,
        name ="Sum",
        data = data$sentiment
      ) %>% 
      hc_plotOptions(
        series = list (
          stacking = 'normal'
        )
      ) %>% 
      hc_chart(
        inverted = F
      ) %>% 
      hc_xAxis(
        reversed = T,
        categories = data$year
      ) %>% 
      hc_legend(
        reversed = T
      ) %>% 
      hc_tooltip(
        shared = T
      )
    
  })
  
  output$sentiment_of_speech_col_avg <- renderHighchart({
    data <- sentiment_of_speech_data()
    hchart(data,
           hcaes(x = year, y = average_sentiment),
           type="column",
           colorByPoint = T
    ) %>% 
      hc_norevese()
  })

  
  ## sentiment_of_words_data -------------------------------------------------
  sentiment_of_words_data <- reactive({
    data <- tokens %>%
      filter(polarity != 0) %>% 
      group_by(headword) %>% 
      distinct(headword, .keep_all = TRUE) %>%
      arrange(desc(n_hword_total)) %>% 
      filter(year %in% input$year[1]:input$year[2]) %>%
      head(input$slider_sentiment_of_words_n_words) %>% 
      arrange(polarity, n_hword_total)
  })
  
  ## sentiment_of_words ------------------------------------------------------
  output$sentiment_of_words <- renderHighchart({
    data <- sentiment_of_words_data()
    hchart(data,
           hcaes(x = headword, y = polarity, group = sentiment_true),
           type="column") %>% 
      hc_yAxis(
        startOnTick = T,
        tickInterval = 1,
        min = -3,
        max = 3
      ) %>% 
      hc_tooltip(
        shared = TRUE,
        headerFormat = "<b>{point.key}</b><br>",
        pointFormat = "<span style=\"color: {point.color} \">\u25CF</span> {point.series.name}: {point.y}"
      ) %>% 
      hc_norevese()
  })

  output$sentiment_of_words_freq <- renderHighchart({
    data <- sentiment_of_words_data() %>% 
      arrange(n_hword_total, polarity)
    hchart(data,
           hcaes(x = headword, y = n_hword_total),
           type="column") %>% 
      hc_yAxis(
        startOnTick = T,
        tickInterval = round_any(max(data$n_hword_total)/4,10),
        min = 0,
        max = max(data$n_hword_total)
      ) %>% 
      hc_tooltip(
        shared = TRUE,
        headerFormat = "<b>{point.key}</b><br>",
        pointFormat = "<span style=\"color: {point.color} \">\u25CF</span> {point.series.name}: {point.y}"
      ) %>% 
      hc_norevese()
  })

  # wiki_infobox ------------------------------------------------------------
  output$wiki_infobox <- renderUI({
    includeHTML("www/queen_info_table.html")
  })
}
