server <- function(input, output) {

  # Sidebar Menu -------------------------------------------------------------
  output$menu <- renderMenu({
      sidebarMenu(
        menuItem("Welcome and about", tabName = "index", icon = icon("home"), selected = T),
        menuItem("Topic model", tabName = "tm", icon = icon("comment-dots")),
        menuItem("Sentiments", tabName = "sentiment", icon = icon("theater-masks")),
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
            radioButtons ("yearopt",
                          label = "Year input",
                          choices = c("Range", "Select inputs"),
                          selected = "Range"
            ),
            uiOutput("year"),
            selectizeInput("words",
                           label="Featured words", choices = words_tokens_all, multiple = TRUE)
          )
        )
  })
  
  output$year <- renderUI({
      req(input$yearopt)
      if(input$yearopt == "Range"){
        sliderInput("year_r", "Years range:",
                    min = year_min, max = year_max,
                    value = range(year_min,year_max),
                    step = 1
        )
        
      } else{
        selectizeInput("year_si",
                       label="Years", choices = years, selected = years, multiple = TRUE)
      }
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
    req(input$yearopt)
    data <- sentiment %>%
      group_by(year) %>% 
      arrange(year)
    if(input$yearopt == "Range"){
      req(input$year_r)
      filter(data, year %in% input$year_r[1]:input$year_r[2])
    } else{
      req(input$year_si)
      filter(data, year %in% input$year_si)
    }
  })

  ## sentiment of speeches ----------------------------------------------------
  output$sentiment_of_speech_bubles <- renderHighchart({
    data <- sentiment_of_speech_data()
    hchart(data,
           type="bubble",
           hcaes(x = sentiment_pos, y = sentiment_neg, z = (n_words/max(n_words)), size = n_words, s = sentiment, l = sentiment_label, name = year, group = year),
           showInLegend = F,
           stickyTracking = F,
           styledMode = T
    ) %>%
      hc_plotOptions(
        bubble = list(
          minSize = paste(15, "%", sep=""),
          maxSize = paste(30, "%", sep="")
        ),
        series = list(
          animation = list(
            duration = 500
          ),
          dataLabels = list(
            enabled= T,
            format = '{point.name}'
          ),
          marker = list(
            enabled = T,
            enabledThreshold = F,
            states = "normal",
            symbol = "cirkle"
          ),
          visible = T
       )
      ) %>% 
      hc_yAxis(
        reversed = T,
        startOnTick = T,
        gridLineWidth = T
      ) %>% 
      hc_xAxis(
        startOnTick = T,
        gridLineWidth = T
      ) %>%
      hc_tooltip(
        useHTML = T,
        headerFormat = '<table>',
        pointFormat = paste('<tr><th colspan="2"><h3>{point.name}</h3></th></tr>',
          '<tr><th>Overall sentiment:</th><td>{point.l}</td></tr>',
          '<tr><th>Positive sentiment (x):</th><td>{point.x}</td></tr>',
          '<tr><th>Negative sentiment (y):</th><td>{point.y}</td></tr>',
          '<tr><th>Summed sentiment:</th><td>{point.s}</td></tr>',
          '<tr><th>Words with polarity (size):</th><td>{point.size}</td></tr>',
          sep = ""),
        footerFormat = '</table>',
        followPointer = F,
        followTouchMove = F,
        snap = 100,
        hideDelay = 500
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
  
  output$sentiment_of_speech_avg <- renderHighchart({
    data <- sentiment_of_speech_data()
    hchart(data,
           hcaes(x = year, y = round(average_sentiment,2), group = sentiment_label),
           type="column",
           colorByPoint = T,
           styledMode = T
    ) %>% 
      hc_norevese()
  })

  
  ## sentiment_of_words_data -------------------------------------------------
  sentiment_of_words_data <- reactive({
    req(input$yearopt)
    req(input$slider_sentiment_of_words_n_words)
    data <- tokens %>%
      filter(polarity != 0) %>% 
      group_by(headword) %>% 
      distinct(headword, .keep_all = TRUE)
    if(input$yearopt == "Range"){
      req(input$year_r)
      data %>% filter(year %in% input$year_r[1]:input$year_r[2])
    } else{
      req(input$year_si)
      data %>% filter(year %in% input$year_si)
    }
    data %>% 
      arrange(desc(n_hword_total)) %>% 
      head(input$slider_sentiment_of_words_n_words) %>% 
      arrange(polarity, n_hword_total) %>% 
      return()
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
      hc_norevese() %>% 
      hc_xAxis(
        reversed = T
      )
  })

  output$sentiment_of_words_freq <- renderHighchart({
    data <- sentiment_of_words_data() %>% 
      arrange(n_hword_total, polarity)
    hchart(data,
           hcaes(x = headword, y = n_hword_total, group = sentiment_true),
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
      hc_norevese() %>% 
      hc_xAxis(
        reversed = T
      )
  })

  # wiki_infobox ------------------------------------------------------------
  output$wiki_infobox <- renderUI({
    includeHTML("www/queen_info_table.html")
  })
  
  # Map ---------------------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet(poly_prep(geojson, countries, input$year_r[1]:input$year_r[2])) %>% 
      addTiles() %>% 
      addPolygons(stroke = F, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor=~pal(n), label = ~ paste(ADMIN, "was said:", n, "times")) %>% 
      addLegend(pal = pal, values = ~n)
  })
}


