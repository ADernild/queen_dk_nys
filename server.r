server <- function(input, output, session) {

  # UI output -----------------------------------------------------------------
  ## Sidebar Menu -------------------------------------------------------------
  output$menu <- renderMenu({
      sidebarMenu(
        menuItem("Welcome and about", tabName = "index", icon = icon("home"), selected = T),
        menuItem("Topics", tabName = "tm", icon = icon("comment-dots")),
        menuItem("Sentiment", tabName = "sentiment", icon = icon("theater-masks")),
        menuItem("Map", tabName = "map", icon = icon("globe-europe")),
        menuItem("Word statistic", tabName = "stats", icon = icon("chart-pie")),
        div(id="sidebar-input",
            h3("Filters"),
            # checkboxGroupInput("re",
            #                    label = "Options",
            #                    choices = "Allow reactive choises",
            #                    selected = "Allow reactive choises"
            # ),
            radioButtons ("l",
                          label = "Language",
                          selected = languages[1],
                          choiceNames = c("Danish", "English"),
                          choiceValues = languages
            ),
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
    req(input$l)
    if(input$l == "DK"){
      year_min <- year_min
      year_max <- year_max
      years <- years
    } else if(input$l == "EN"){
      year_min <- year_min_en
      year_max <- year_max_en
      years <- years_en
    } else{
      warning("Invallid country setting")
      return(p("Filter could not load due to invallid country setting."))
    }
    if(input$yearopt == "Range"){
      sliderInput("year_r", "Years",
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
  
  # Date data --------------------------------------------------------------
  y <- reactive({
    req(input$yearopt)
    if(input$yearopt == "Range"){
      req(input$year_r)
      year <- input$year_r[1]:input$year_r[2]
    } else{
      req(input$year_si)
      year <- input$year_si
    }
    return(year)
  })
  
  # Index ------------------------------------------------------------------
  output$royall_beautyfication <- renderUI({
    req(input$l)
    req(input$words)
    if(input$l == "DK"
       && length(input$words) == 3
       && "danmark" %in% input$words
       && "dansk" %in% input$words
       && "dannebrog" %in% input$words){
      tags$link(rel = "stylesheet", type = "text/css", href = "royall_beautyfication.css") %>% 
        return()
    }
  })
  
  ## Covered speeches ------------------------------------------------------
  output$Covered_speech <- renderUI({
    req(input$l)
    if(input$l == "DK"){
      title <- "Hendes Majestæt Dronningens nytårstale"
      source <- source_year
    } else if(input$l == "EN"){
      source <- source_year_en
      title <- "Her Majesty The Queen’s New Year Address"
    } else{
      warning("Invallid country setting")
      return(p("Content could not load due to invallid country setting."))
    }
    
    req(y())
    source <- source %>% 
      filter(year %in% y())

    content <- tags$ul(
      lapply(1:nrow(source), function(i) {
        tags$li(a(href=source$urls[i],
                  target = "_blank",
                  paste(title, source$year[i])))
      })
    )
    return(content)
  })
    
  ## wiki_infobox ----------------------------------------------------------
  output$scrabing_info <- renderUI({
    req(input$l)
    if(input$l == "DK"){
      source <- "https://da.wikipedia.org/wiki/Margrethe_2."
      date <- "08/12/2021"
    } else if(input$l == "EN"){
      source <- "https://en.wikipedia.org/wiki/Margrethe_II_of_Denmark"
      date <- "12/12/2021"
    } else{
      warning("Invallid country setting")
      return(p("Content could not load due to invallid country setting."))
    }
    a(href=source,
      target= "_blank",
      paste("Info scarped from Wikipedia (", date, ").", sep = "")
    )
  })
  
  output$wiki_infobox <- renderUI({
    req(input$l)
    if(input$l == "DK"){
      includeHTML("www/queen_info_table.html")
    } else if(input$l == "EN"){
      includeHTML("www/queen_info_table_en.html")
    } else{
      warning("Invallid country setting")
      return(p("Content could not load due to invallid country setting."))
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
  ## sentiment data ---------------------------------------------------------
  sentiment_of_speech_data <- reactive({
    req(y())
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
    
    filter(data, year %in% y())
  })
  
  sentiment_of_words_data <- reactive({
    req(y())
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
    
    data <- data %>% filter(year %in% y())

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
    req(y())
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
    
    filter(data, year %in% y())
  })

  ## sentiment of speeches -------------------------------------------------
  ### Bubles ---------------------------------------------------------------
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
  
  ### Column compare -------------------------------------------------------
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
  
  ### Column compare -------------------------------------------------------
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
  
  ### Column average -------------------------------------------------------
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

  ## sentiment_of_words ----------------------------------------------------
  ### Sentiment comparison -------------------------------------------------
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
      hc_legend(
        reversed = T
      ) %>% 
      hc_xAxis(
        reversed = T
      )
  })

  ### Word comparison ------------------------------------------------------
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
      hc_legend(
        reversed = T
      ) %>% 
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
  
  # Map ---------------------------------------------------------------------
  mapData <- reactive({
    data <- poly_prep(geojson, countries, req(y()))
    return(data)
  })
  
  output$map <- renderLeaflet({
    data <- mapData()
    leaflet(data,
            options = leafletOptions(worldCopyJump = T,
                                     minZoom = 2,
                                     maxZoom = 4
                                     )
            ) %>%
      addTiles(options = providerTileOptions(
        worldCopyJump = T,
        minZoom = 2,
        maxZoom = 4)) %>% 
      addPolygons(stroke = T, weight=0.2, color="black", smoothFactor = 0.3, fillOpacity = 1,
                  fillColor=~pal(n), popup = ~paste("<b>", ADMIN, "</b>", "was said:", n, "times in total", "<br/>",
                                                     sapply(1:length(n_year), function(i) ifelse(length(n_year[[i]])>10,
                                                                                                 paste(n_year[[i]], "t. in:", year[[i]], collapse=", "),
                                                                                                 paste(n_year[[i]], "times in:", year[[i]], collapse="<br/>")),
                                                            simplify=T)),
                  popupOptions = labelOptions(textsize = "8px"),
                  highlightOptions = list(weight = 0.7, fillOpacity = 0.9),
                  layerId = data@data$ADMIN) %>% 
      addLegend(pal = pal, values = ~n)
  })
  
  output$n_hist <- renderHighchart({
    data <- mapData()
    df <- data.frame(year = unlist(data@data$year), n_year = unlist(data@data$n_year)) %>% 
      group_by(year) %>% 
      summarise(n_year = sum(n_year)) %>% 
      hchart("bar", hcaes(x="year", y="n_year")) %>% 
      hc_yAxis(title = list(text = "Mentions per year")) %>% 
      hc_xAxis(title = list(text = "Year"))
  })
  
  output$sentences <- renderUI({
    data <- mapData()
    sentences <- sample(unlist(data@data$sentence), size = 5)
    sentences[!is.na(sentences)] %>% 
      str_to_sentence() %>% 
      paste(collapse=". <br/>") %>% 
      HTML()
  })
  # Setview and zoom to clicked country
  observe({
    click <- input$map_shape_click
    proxy <- leafletProxy("map")
    if(is.null(click))
      return()
    proxy %>% setView(lng = click$lng, lat = click$lat, zoom = 4)
  })
  
  # Making visualization based on clicked country
  observeEvent(input$map_shape_click, {
    data <- mapData()
    click <- input$map_shape_click
    selected <- data@data[data@data$ADMIN == click$id,]
    
    if(!is.null(click$id)){
      output$n_hist <- renderHighchart({
        df <- tidyr::unnest(data@data, cols = c(year, n_year)) %>% 
          select(ADMIN, ISO_A2, year, n_year)
        df$ISO_A2[df$ADMIN != click$id] <- "*All"
        df <- df %>% 
          group_by(ISO_A2, year) %>% 
          summarise(n_year = sum(n_year)) %>% 
          hchart("bar", hcaes(x = "year", y = "n_year", group = "ISO_A2"),
                stacking = "normal") %>% 
          hc_yAxis(title = list(text = "Mentions per year")) %>% 
          hc_xAxis(title = list(text = "Year")) %>% 
          hc_title(text = click$id)
        })
      output$sent_box <- renderHighchart({
        df <- tidyr::unnest(data@data, cols=c(year, sentiment_year)) %>% 
          select(ADMIN, ISO_A2, year, sentiment_year)
        df$ISO_A2[df$ADMIN != click$id] <- "*All"
        df <- df %>% 
          group_by(ISO_A2, year) %>% 
          summarise(sentiment = mean(sentiment_year))
        dat <- data_to_boxplot(df, sentiment, ISO_A2, year)
        
        highchart() %>% 
          hc_xAxis(title = list(text = "Country"), type = "category") %>% 
          hc_yAxis(title = list(text = "Average sentence sentiment")) %>% 
          hc_title(text = paste("Average sentiment when", click$id, "is mentioned")) %>% 
          hc_add_series_list(dat)
      })
      output$sentences <- renderUI({
        sentences <- unlist(selected$sentence)[1:5]
        sentences[!is.na(sentences)] %>% 
          str_to_sentence() %>% 
          paste(collapse=". <br/>") %>% 
          HTML()
      })
      }
    })

  # Word statistics ---------------------------------------------------------
  ## Word data --------------------------------------------------------------
  speech_data <- reactive({
    req(y())
    data <- tokens %>%
      select(year, headword, n_hword_year, n_hword_total) %>% 
      distinct() %>% 
      group_by(headword, year) %>% 
      arrange(year, headword)
    
    return(data)
  })
  
  speech_data_word_filt <- reactive({
    data <- speech_data()
    
    if(length(input$words) > 0){
      data <- data %>% 
        filter(headword %in% input$words)
    }
    
    return(data)
  })
  
  speech_data_como_filt <- reactive({
    req(input$slider_word_ussage)
    data <- speech_data_word_filt()
    
    common_opt <- unique(arrange(data, desc(n_hword_total))$n_hword_total)[input$slider_word_ussage]
    
    data <- filter(data, n_hword_total >= common_opt)
    
    total <- data %>% 
      summarise(n_sel = sum(n_hword_year))
    
    data <- data %>% 
      left_join(total, by= c("year", "headword"))
    
    return(data)
  })
  
  speech_data_como_filt_max <- reactive({
    req(input$slider_word_ussage)
    data <- speech_data_word_filt()
    
    common_opt <- unique(arrange(data, desc(n_hword_total))$n_hword_total)[ifelse(input$slider_word_ussage>15,15,input$slider_word_ussage)]

    data <- filter(data, n_hword_total >= common_opt)
    
    total <- data %>% 
      summarise(n_sel = sum(n_hword_year))
    
    data <- data %>% 
      left_join(total, by= c("year", "headword"))
    
    return(data)
  })

  ## Word cloud ------------------------------------------------------------
  output$wordcloud <- renderWordcloud2({
    data <-  speech_data_como_filt() %>% 
      ungroup() %>% 
      select(headword, n_hword_total) %>% 
      distinct() %>% 
      arrange(headword) %>% 
      rename(word = headword, freq = n_hword_total)
    wordcloud2(data=data, size=1.6, color='random-dark')
  })
  
  ## Stream Graph ----------------------------------------------------------
  output$word_ussage_streamgraph <- renderHighchart({
    data <-  speech_data_como_filt_max()
    req(input$slider_word_ussage)
    
    hchart(data, "streamgraph", hcaes(year, n_hword_year, group = headword)) %>% 
        hc_yAxis(
          visible = F
        ) %>% 
        hc_norevese() %>% 
        hc_tooltip(
          shared = T
        )
  })
  
  ## Columns ---------------------------------------------------------------
  output$word_ussage_col <- renderHighchart({
    data <-  speech_data_como_filt()

    hc <- hchart(data, "column",
                 hcaes(x=year, y=n_hword_year,group = headword)) %>% 
      hc_plotOptions(
        series = list (
          stacking = 'normal'
        )
      ) %>% 
      hc_norevese() %>% 
      hc_tooltip(
        shared = T
      )
    return(hc)
  })
  
  ## Columns percent ------------------------------------------------------
  output$word_ussage_col_per <- renderHighchart({
    data <-  speech_data_como_filt()

    hc <- hchart(data, "column",
                 hcaes(x=year, y=n_hword_year,group = headword)) %>% 
      hc_plotOptions(
        series = list (
          stacking = 'percent'
        )
      ) %>% 
      hc_norevese() %>% 
      hc_tooltip(
        shared = T
      )
    return(hc)
  })
  
  ## Pie ------------------------------------------------------------------
  output$word_ussage_pie <- renderHighchart({
    #Todo: Add years
    data <-  speech_data_como_filt() %>% 
      ungroup() %>%
      select(headword, n_hword_total) %>%
      distinct() %>% 
      arrange(desc(n_hword_total))

    hc <- hchart(data, "pie", name="Frequency",
                 hcaes(y=n_hword_total, name = headword)) %>% 
      hc_plotOptions(
        series = list(
          colorByPoint = T
        ),
        pie = list(
          allowPointSelect = T,
          cursor = 'pointer',
          dataLabels = list(
            enabled = T,
            format = '<b>{point.name}</b>: {point.y}'
          )
        )
      ) %>% 
      hc_norevese() %>% 
      hc_tooltip(
        shared = T,
        headerFormat = "",
        pointFormat = "<span style=\"color: {point.color} \">\u25CF</span> <b>{point.name}</b><br>Frequency: {point.y}<br>Selection slice size: {point.percentage:.1f}%"
      )
      return(hc)
  })
  
  ## Scatter --------------------------------------------------------------
  output$word_ussage_scatter <- renderHighchart({
    data <-  speech_data_como_filt_max()
    
    hc <- hchart(data, "scatter",
                 hcaes(x=year, y=n_hword_year, total = n_hword_total, group = headword)) %>% 
      hc_norevese() %>% 
      hc_plotOptions(
        scatter = list(
          marker = list(
            radius = 3,
            states = list(
              hover = list(
                enabled = T
              )
            )
          )
        ),
        states = list(
          hover = list(
            marker = list(
              enabled = F
            )
          )
        )
      ) %>% 
      hc_xAxis(
        startOnTick = T,
        endOnTick = T,
        showLastLabel = T
      ) %>% 
      hc_tooltip(
        shared = T,
        headerFormat = "<span style=\"color: {point.color} \">\u25CF</span> <b>{point.series.name}</b><br>",
        pointFormat = "Year: {point.x}<br>Frequency: {point.y}<br>Total frequency: {point.total}"
      )
    return(hc)
  })
  
  ## Table ----------------------------------------------------------------
  output$word_ussage_tbl <- renderDT({
    warning(names(speech_data_como_filt()))
    speech_data_como_filt() %>%
      ungroup() %>% 
      transmute(`Year` = year, `Word` = headword, `Total frequency` = n_hword_total,
                `Frequency in year` = n_hword_year,
                `Frequency in selection in year` = n_sel)
  }, rownames = FALSE, filter = 'top',
  options = list(
    
    )
  )
}
