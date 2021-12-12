server <- function(input, output, session) {

  # UI output -----------------------------------------------------------------
  ## Sidebar Menu -------------------------------------------------------------
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
            selectizeInput("words", label="Featured words", choices = NULL,
                           multiple = TRUE)
            
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
        selectizeInput("year_si", label="Years", choices = years, selected = years,
                       multiple = TRUE)
      }
  })
  
  updateSelectizeInput(
    session, 'words', choices = words_tokens_all, server = TRUE
    )
  
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
  ## sentiment data ---------------------------------------------------------
  sentiment_of_speech_data <- reactive({
    req(input$yearopt)
    data <- sentiment %>%
      group_by(year) %>% 
      arrange(year)
    if(length(input$words) > 0){
      token_data <- tokens %>% 
        filter(headword %in% input$words) %>% 
        .$year %>% 
        unique() %>% 
        sort()
      data <- data %>% 
        rowwise() %>% 
        mutate(fwords = ifelse(year %in% token_data, "Yes", "No"))
    }
    if(input$yearopt == "Range"){
      req(input$year_r)
      filter(data, year %in% input$year_r[1]:input$year_r[2])
    } else{
      req(input$year_si)
      filter(data, year %in% input$year_si)
    }
  })
  
  sentiment_of_words_data <- reactive({
    req(input$yearopt)
    req(input$slider_sentiment_of_words_n_words)
    data <- tokens %>%
      filter(polarity != 0) %>% 
      group_by(headword) %>% 
      distinct(headword, .keep_all = TRUE)
    
    if(length(input$words) > 0 && cmatch(data$headword, input$words)){
      data <- data %>%
        # filter(headword %in% input$words) %>% 
        mutate(fwords = ifelse(headword %in% input$words, "featured word: Yes", "featured word: No")) %>% 
        mutate(sentiment_true = paste(sentiment_true, fwords, sep=": "))
    }
    
    if(input$yearopt == "Range"){
      data <- data %>% filter(year %in% input$year_r[1]:input$year_r[2])
    } else{
      req(input$year_si)
      data <- data %>% filter(year %in% input$year_si)
    }
    
    if("fwords" %in% colnames(data)){
      data <- data %>% 
        arrange(desc(fwords), desc(n_hword_total))
    } else{
      data <- data %>% 
        arrange(desc(n_hword_total))
    }
    data <- data %>% head(input$slider_sentiment_of_words_n_words) 
    if("fwords" %in% colnames(data)){
      data %>% arrange(fwords, polarity, n_hword_total) %>% 
        return()
    } else{
      data %>% arrange(polarity, n_hword_total) %>% 
        return()
    }
  })
  
  sentiment_of_speech_data_filtered <- reactive({
    req(input$yearopt)
    req(input$words)
    
    data <- sentiment_of_words_data() %>%
      rowwise() %>% 
      mutate(polarity_pos = as.numeric(ifelse(polarity > 0, polarity, 0)),
             polarity_neg = as.numeric(ifelse(polarity < 0, polarity, 0))) %>% 
      group_by(year) %>%
      filter(headword %in% input$words) %>% 
      mutate(n_pos = as.numeric(ifelse(polarity>0, n_in_year, 0)),
             n_neg = as.numeric(ifelse(polarity<0, n_in_year, 0))) %>% 
      mutate(sentiment = sum(n_in_year*polarity),
             sentiment_pos = sum(n_in_year*polarity_pos),
             sentiment_neg = sum(n_in_year*polarity_neg),
             average_sentiment = mean(n_in_year*polarity),
             n_words = sum(n_in_year),
             n_words_pos = sum(n_pos),
             n_words_neg = sum(n_neg)
      ) %>% 
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
    if("fwords" %in% colnames(data)){
      hc <- hchart(data,
                   type="bubble",
                   hcaes(x = sentiment_pos, y = sentiment_neg, z = (n_words/max(n_words)), size = n_words, s = sentiment, l = sentiment_label, name = year, group = fwords, fwords = fwords),
                   showInLegend = F,
                   stickyTracking = F,
                   styledMode = T
      ) %>% 
        hc_tooltip(
          useHTML = T,
          headerFormat = '<table>',
          pointFormat = paste('<tr><th colspan="2"><b>{point.name}</b></th></tr>',
                              '<tr><th>Featured words in year :</th><td>{point.fwords}</td></tr>',
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
    } else{
      hc <- hchart(data,
             type="bubble",
             hcaes(x = sentiment_pos, y = sentiment_neg, z = (n_words/max(n_words)), size = n_words, s = sentiment, l = sentiment_label, name = year, group = year),
             showInLegend = F,
             stickyTracking = F,
             styledMode = T
      ) %>% 
        hc_tooltip(
          useHTML = T,
          headerFormat = '<table>',
          pointFormat = paste('<tr><th colspan="2"><b>{point.name}</b></th></tr>',
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
    }
    hc %>%
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
      )
  })
  
  output$sentiment_of_speech_col_compare <- renderHighchart({
    data <- sentiment_of_speech_data() 
    hc <- highchart() %>% 
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
    if(length(input$words)){
      selection <- sentiment_of_speech_data_filtered()
      hc <- hc %>% 
        hc_add_series(
          type = "bar",
          stack = 3,
          name= "Selection",
          data = selection$sentiment
        ) 
    }
    return(hc)
  })
  
  output$sentiment_of_speech_sha_compare <- renderHighchart({
    data <- sentiment_of_speech_data()
    hc <- highchart() %>% 
      hc_add_series(
        name = "Total sentiment",
        type = "arearange",
        stack = 1,
        data = data,
        hcaes(x = year, low = sentiment, high = sentiment_pos, neg = sentiment_neg)
      ) %>% 
      hc_norevese() %>% 
      hc_tooltip(
        shared = T,
        headerFormat = "<b>{point.x}</b><br>",
        pointFormat = "Positive (high): {point.high}<br>Negative (difference): {point.neg}<br>Sentiment (low): {point.low}<br>"
      ) %>% 
      hc_yAxis(
        softMin = 0,
        startOnTick = T
      )
    # if(length(input$words)){
    #   selection <- sentiment_of_speech_data_filtered()
    #   hc <- hc %>% 
    #     hc_add_series(
    #       type = "arearange",
    #       name= "Selection",
    #       data = selection,
    #       hcaes(x = year, low = sentiment, high = sentiment_pos, neg = sentiment_neg)
    #     ) 
    # }
    return(hc)
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
    data <- sentiment_of_words_data()
    if("fwords" %in% colnames(data)){
      data <- data %>% 
        arrange(fwords, n_hword_total, polarity)
    } else{
      data <- data %>% 
        arrange(n_hword_total, polarity)
    }
    
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
  

  ## Sentiment valuebox's ---------------------------------------------------
  output$total_sum_sen <- renderValueBox({
    if(length(input$words) > 0){
      data <- sentiment_of_speech_data_filtered()
    } else{
      data <- sentiment_of_speech_data()
    }
    total_sum_sen <- sum(data$sentiment)
    valueBox(
      total_sum_sen, "Summed sentiment", icon = icon("equals"),
      color = "purple"
    )
  })
  
  output$total_pos_sen <- renderValueBox({
    if(length(input$words) > 0){
      data <- sentiment_of_speech_data_filtered()
    } else{
      data <- sentiment_of_speech_data()
    }
    total_pos_sen <- sum(data$sentiment_pos)
    valueBox(
      total_pos_sen, "Summed positive sentiment", icon = icon("plus"),
      color = "green"
    )
  })
  
  output$total_neg_sen <- renderValueBox({
    if(length(input$words) > 0){
      data <- sentiment_of_speech_data_filtered()
    } else{
      data <- sentiment_of_speech_data()
    }
    total_neg_sen <- sum(data$sentiment_neg)
    valueBox(
      total_neg_sen, "Summed negative sentiment", icon = icon("minus"),
      color = "red"
    )
  })
  
  output$num_pos_sen <- renderValueBox({
    if(length(input$words) > 0){
      data <- sentiment_of_speech_data_filtered()
      num_pos_sen <- round(sum(data$n_words_pos),2)
    } else{
      data <- sentiment_of_words_data()
      num_pos_sen <- sum(data[data$polarity > 0,]$n_hword_total)
    }
    valueBox(
      num_pos_sen, "Number of words that carried positive sentiment", icon = icon("plus-circle"),
      color = "green"
    )
  })
  
  output$total_num_wor <- renderValueBox({
    if(length(input$words) > 0){
      data <- sentiment_of_speech_data_filtered()
    } else{
      data <- sentiment_of_speech_data()
    }
    total_num_wor <- sum(data$n_words)
    valueBox(
      total_num_wor, "Number of words that carried sentiment", icon = icon("hashtag"),
      color = "blue"
    )
  })
  
  output$num_neg_sen <- renderValueBox({
    if(length(input$words) > 0){
      data <- sentiment_of_speech_data_filtered()
      num_neg_sen <- round(sum(data$n_words_neg),2)
    } else{
      data <- sentiment_of_words_data()
      num_neg_sen <- sum(data[data$polarity < 0,]$n_hword_total)
    }
    valueBox(
      num_neg_sen, "Number of words that had negative sentiment", icon = icon("minus-circle"),
      color = "red"
    )
  })
  
  output$mean_sum_sen <- renderValueBox({
    if(length(input$words) > 0){
      data <- sentiment_of_speech_data_filtered()
    } else{
      data <- sentiment_of_speech_data()
    }
    mean_sum_sen <- round(mean(data$sentiment),2)
    mean_sum_sen <- ifelse(is.nan(mean_sum_sen), 0, mean_sum_sen)
    valueBox(
      mean_sum_sen, "Average sentiment", icon = icon("equals"),
      color = "purple"
    )
  })
  
  output$mean_pos_sen <- renderValueBox({
    if(length(input$words) > 0){
      data <- sentiment_of_speech_data_filtered()
    } else{
      data <- sentiment_of_speech_data()
    }
    mean_pos_sen <- round(mean(data$sentiment_pos),2)
    mean_pos_sen <- ifelse(is.nan(mean_pos_sen), 0, mean_pos_sen)
    valueBox(
      mean_pos_sen, "Average positive sentiment", icon = icon("plus"),
      color = "green"
    )
  })
  
  output$mean_neg_sen <- renderValueBox({
    if(length(input$words) > 0){
      data <- sentiment_of_speech_data_filtered()
    } else{
      data <- sentiment_of_speech_data()
    }
    mean_neg_sen <- round(mean(data$sentiment_neg),2)
    mean_neg_sen <- ifelse(is.nan(mean_neg_sen), 0, mean_neg_sen)
    valueBox(
      mean_neg_sen, "Average negative sentiment", icon = icon("minus"),
      color = "red"
    )
  })
  
  output$mean_num_wor <- renderValueBox({
    if(length(input$words) > 0){
      data <- sentiment_of_speech_data_filtered()
    } else{
      data <- sentiment_of_speech_data()
    }
    mean_num_wor <- round(mean(data$n_words),2)
    mean_num_wor <- ifelse(is.nan(mean_num_wor), 0, mean_num_wor)
    valueBox(
      mean_num_wor, "Average number of words that carried sentiment", icon = icon("hashtag"),
      color = "blue"
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
      addPolygons(stroke = T, weight=0.2, color="black", smoothFactor = 0.3, fillOpacity = 1,
                  fillColor=~pal(n), label = ~ paste(ADMIN, "was said:", n, "times"), highlightOptions = list(weight = 0.7, fillOpacity = 0.9)) %>% 
      addLegend(pal = pal, values = ~n)
  })
}


