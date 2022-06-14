server <- function(input, output, session) {

  is_local <<- Sys.getenv('SHINY_PORT') == ""
  # Data --------------------------------------------------------------------
  tokens_data <- reactiveFileReader(100000, session, "data/tokens.rds", readRDS) # All tokens, filtered
  
  
  
  ## reactive data ----------------------------------------------------------

  # UI output -----------------------------------------------------------------
  ## Sidebar Menu -------------------------------------------------------------
  output$menu <- renderMenu({
      sidebarMenu(
        menuItem(span("Home", title="Start page"), tabName = "index", icon = shiny::icon("home", title="Start page"), selected = T),
        menuItem(span("Topics", title="Topics found, ready yo be analyzed"), tabName = "tm", icon = shiny::icon("comment-dots", title="Topics found, ready yo be analyzed")),
        menuItem(span("Sentiment", title="Sentiment analysis for speeches and word selections"), tabName = "sentiment", icon = shiny::icon("theater-masks", title="Sentiment analysis for speeches and word selections")),
        menuItem(span("Fyn", title="Map of locations"), tabName = "fyn", icon = shiny::icon("map", title="Map of locations")),
        # menuItem(span("Countries", title="Map of cointries mentioned during speeches"), tabName = "map", icon = shiny::icon("globe-europe", title="Map of cointries mentioned during speeches")),
        menuItem(span("Word statistics", title="Statistics for all words"), tabName = "stats", icon = shiny::icon("chart-pie", title="Statistics for all words")),
        menuItem(span("Data", title="Information about data sources and data subject"), tabName = "data", icon=shiny::icon("database", title="Information about data sources and data subject")),
        menuItem(span("How to operate", class="help-me", title="Help and how to operate dashboard"), tabName = "howto", icon=shiny::icon("question-circle", class="help-me", title="Help and how to operate dashboard")),
        div(id="sidebar-input",
            h3("Selection"),
            selectizeInput("id", label="UUID", choices = c(),
                           multiple = TRUE, options = list(maxOptions = length(article_lib$uuid))),
            selectizeInput("docs", label="Article name", choices = c(),
                           multiple = TRUE, options = list(maxOptions = length(article_lib$uuid))),
            div(class="hidden",
                numericInput("n_recent", "Hidden numeric input to filter articles", value = 200)),
            actionButton("sync", "Sync selection", title="Syncronize Artcile-names and UUID"),
            actionButton("clear_id", "Clear selection"),
            h3("Filters"),
            div(sliderInput("n_recent_filter", "N recent articles",
                         as.numeric(ifelse(length(article_lib$uuid)>200, 200, length(article_lib$uuid))),
                         min = 10, max = round_any(length(article_lib$uuid), 10, f = ceiling), step = 10, round = 10),
                         title = "Limit for how many articles to load at the same time."),
            selectizeInput("topic", label="Topic", choices = c(),
                           multiple = FALSE, options = list(maxOptions = length(topic_frame$topic))),
            selectizeInput("section", label="Section", choices = c(),
                           multiple = TRUE, options = list(maxOptions = length(sections))),
            selectizeInput("authors", label="Authors", choices = c(),
                           multiple = TRUE, options = list(maxOptions = length(authors))),
            selectizeInput("location", label="Location", choices = c(),
                           multiple = TRUE, options = list(maxOptions = length(authors))),
            textInput("user_id", label="User-id", placeholder = "Eg. 1234567891234-123"),
            # selectizeInput("words", label="Featured words", choices = c(),
            #                multiple = TRUE, options = list(maxOptions = length(words_tokens_all))),
            # actionButton("clear", "Clear featured words"),
            actionButton("topic_id_sync", "Apply filters", title="Update UUID filtered by set topic, section and authors"),
            actionButton("clear_filter", "Clear filters"),
            actionButton("clear_all", "Clear all")
          )
        )
  })

  ### Sidebar Menu Updates ----------------------------------------------------
  # updateSelectizeInput(
  #   session, 'words', choices = words_tokens_all, server = TRUE
  #   )
  
  updateSelectizeInput(
    session, 'id', choices = article_lib$uuid, selected = "", server = TRUE
  )

  updateSelectizeInput(
    session, 'docs', choices = named_id, selected = "", server = TRUE
  )

  updateSelectizeInput(
    session, 'topic', choices = topic_frame$topic, selected = "", server = TRUE
  )

  updateSelectizeInput(
    session, 'section', choices = sections, selected = "", server = TRUE
  )

  updateSelectizeInput(
    session, 'authors', choices = authors, selected = "", server = TRUE
  )

  updateSelectizeInput(
    session, 'location', choices = locations, selected = "", server = TRUE
  )
  
  ### Sidebar Menu Functionality ----------------------------------------------
  id_docs <- reactive({
    val <- unique(c(input$id, input$docs))
    if(length(val) == 0){
      val <- article_lib$uuid[1:as.numeric(ifelse(input$n_recent>length(article_lib$uuid), length(article_lib$uuid), input$n_recent))]
    }
    return(val)
  })
  
  topic_id <- reactive({
    topic_frame %>% 
      filter(topic %in% input$topic) %>%
      .$uuid %>% 
      unlist() %>% 
      return
  })
  
  section_id <- reactive({
    article_lib %>% 
      filter(section %in% input$section) %>%
      .$uuid %>% 
      return
  })
  
  authors_id <- reactive({
    if("Not set" %in% input$authors){
      val <- input$authors %>% 
        .[. != "Not set"] %>% 
        c("")
      
    } else{
      val <- input$authors
    }
    dis <- article_lib %>% 
      filter(any(match(str_split(authors, ", "), val))) %>%
      .$uuid %>% 
      return
  })
  
  location_id <- reactive({
    article_lib %>% 
      filter(location %in% input$location) %>%
      .$uuid %>% 
      return
  })

  article_lib_filt <- reactive({
    article_lib %>% 
      filter(uuid %in% id_docs()) %>% 
      return
  })
  
  ### Sidebar Menu Events -----------------------------------------------------
  observeEvent(input$sync, {
    val <- id_docs()
    updateSelectizeInput(
      session,
      'id',
      choices = article_lib$uuid,
      selected = val,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      'docs',
      choices = named_id,
      selected = val,
      server = TRUE
    )
  })
  
  observeEvent(input$topic_id_sync, {
    topics_dis <- unique(topic_id())
    section_dis <- unique(section_id())
    authors_dis <- unique(authors_id())
    locations_dis <- unique(location_id())
    topics <- topics_dis
    section <- section_dis
    authors <- authors_dis
    locations <- locations_dis
    user_id <- input$user_id
    
    if(length(topics)>0 || length(section)>0 || length(authors)>0 || length(locations)>0||user_id!=""){
      val <- article_lib$uuid
      if(length(topics)>0){
        val <- val[val %in% topics]
      }
      if(length(section)>0){
        val <- val[val %in% section]
      }
      if(length(authors)>0){
        val <- val[val %in% authors]
      }
      if(length(locations)>0){
        val <- val[val %in% locations]
      }
      if(user_id!=""){
        api_articles <- find_articles(user_id, session)
        # api_articles <- find_articles("1652689900547-757", session)
        if(is.atomic(api_articles) && !is.na(api_articles)){
          val <- val[val %in% api_articles]
        }
      }
      if(length(val)==0 || !match(val, article_lib$uuid)){
        showNotification("Your selection had no results.",
                         id="no_results_in_filter",
                         type="message",
                         duration=7,
                         closeButton=T,
                         session = session)
        val <- input$id # Keep current selection
      }
    } else{
      # No selection
      showNotification("No selection detected.",
                       id="no_filter_selection",
                       type="message",
                       duration=7,
                       closeButton=T,
                       session = session)
      val <- input$id # Keep current selection
    }
    
    updateSelectizeInput(
      session,
      'id',
      choices = article_lib$uuid,
      selected = val,
      server = TRUE
    )
    updateNumericInput(
      session,
      "n_recent",
      "Updated",
      input$n_recent_filter
    )
  })
  
  # observeEvent(input$clear, {
  #   updateSelectizeInput(
  #     session,
  #     'words',
  #     choices = words_tokens_all,
  #     selected = c(""),
  #     server = TRUE
  #   )
  # })
  
  observeEvent(input$clear_id, {
    updateSelectizeInput(
      session,
      'id',
      choices = article_lib$uuid,
      selected = c(""),
      server = TRUE
    )
    updateSelectizeInput(
      session,
      'docs',
      choices = named_id,
      selected = c(""),
      server = TRUE
    )
    updateNumericInput(
      session,
      "n_recent",
      "Updated",
      200
    )
  })
  
  observeEvent(input$clear_filter, {
    updateSliderInput(
      session,
      "n_recent_filter",
      value = as.numeric(ifelse(length(article_lib$uuid)>200, 200, length(article_lib$uuid))),
      min = 10,
      max = round_any(length(article_lib$uuid), 10, f = ceiling),
      step = 10
    )
    updateSelectizeInput(
      session,
      'section',
      choices = sections,
      selected = c(""),
      server = TRUE
    )
    updateSelectizeInput(
      session,
      'authors',
      choices = authors,
      selected = c(""),
      server = TRUE
    )
    updateSelectizeInput(
      session,
      'location',
      choices = locations,
      selected = c(""),
      server = TRUE
    )
    updateTextInput(
      session,
      "user_id",
      value = ""
    )
    # updateSelectizeInput(
    #   session,
    #   'words',
    #   choices = words_tokens_all,
    #   selected = c(""),
    #   server = TRUE
    # )
    updateSelectizeInput(
      session,
      'topic',
      choices = topic_frame$topic,
      selected = c(""),
      server = TRUE
    )
  })
  
  observeEvent(input$clear_all, {
    updateSelectizeInput(
      session,
      'id',
      choices = article_lib$uuid,
      selected = c(""),
      server = TRUE
    )
    updateSelectizeInput(
      session,
      'docs',
      choices = named_id,
      selected = c(""),
      server = TRUE
    )
    updateNumericInput(
      session,
      "n_recent",
      "Updated",
      200
    )
    updateSliderInput(
      session,
      "n_recent_filter",
      value = as.numeric(ifelse(length(article_lib$uuid)>200, 200, length(article_lib$uuid))),
      min = 10,
      max = round_any(length(article_lib$uuid), 10, f = ceiling),
      step = 10
    )
    updateSelectizeInput(
      session,
      'section',
      choices = sections,
      selected = c(""),
      server = TRUE
    )
    updateSelectizeInput(
      session,
      'authors',
      choices = authors,
      selected = c(""),
      server = TRUE
    )
    updateSelectizeInput(
      session,
      'location',
      choices = locations,
      selected = c(""),
      server = TRUE
    )
    updateTextInput(
      session,
      "user_id",
      value = ""
    )
    # updateSelectizeInput(
    #   session,
    #   'words',
    #   choices = words_tokens_all,
    #   selected = c(""),
    #   server = TRUE
    # )
    updateSelectizeInput(
      session,
      'topic',
      choices = topic_frame$topic,
      selected = c(""),
      server = TRUE
    )
  })
  
  # Home ------------------------------------------------------------------
  ## CSS ------------------------------------------------------------------
  # Nothing here yet
  
  ## Valuebox --------------------------------------------------------------
  ### Speech ---------------------------------------------------------------
  output$total_covered <- renderValueBox({
    covered <- length(id_docs())
    covered <- prettyNum(covered, big.mark=".", scientific=FALSE, decimal.mark= ",")
    valueBox(
      covered, "Articles covered", icon = icon("fal fa-newspaper", verify_fa = FALSE),
      color = "light-blue",
    )
  })
  
  output$total_sentences <- renderValueBox({
    if(input$topic != ""){
      val <- topic_frame %>% 
        filter(topic %in% input$topic) %>%
        .$doc_len
    } else{
      val <- n_unique_sentences
    }
    val <- prettyNum(val, big.mark=".", scientific=FALSE, decimal.mark= ",")
    valueBox(
      val, span("Unique sentences", title="In selected topic"), icon = icon("comments"),
      color = "light-blue"
    )
  })
  
  output$total_word <- renderValueBox({
    data <- speech_data_word_filt() %>% 
      ungroup() %>% 
      select(word, n_stem_total) %>% 
      summarise(n = sum(n_stem_total))
    total_words <- sum(data$n)
    total_words <- prettyNum(total_words, big.mark=".", scientific=FALSE, decimal.mark= ",")
    valueBox(
      total_words, span("Words analyzed", title="After stopwords are filtered."), icon = icon("hashtag"),
      color = "light-blue"
    )
  })
  
  output$total_word_unique <- renderValueBox({
    data <- speech_data_word_filt()$word %>% 
      unique()
    total_unique_words <- length(data)
    total_unique_words <- prettyNum(total_unique_words, big.mark=".", scientific=FALSE, decimal.mark= ",")
    valueBox(
      total_unique_words, span("Unique Words analyzed", title="After stopwords are filtered."), icon = icon("hashtag"),
      color = "light-blue"
    )
  })
  
  ### Topics ---------------------------------------------------------------
  output$total_amount_of_topics <- renderValueBox({
    data <- stm_models()$mod$settings$dim$K
    data <- prettyNum(data, big.mark=".", scientific=FALSE, decimal.mark= ",")
    valueBox(
      data, "Topics", icon = icon("comment-dots"),
      color = "yellow"
    )
  })
  
  # output$total_countries_mentioned <- renderValueBox({
  #   req(mapData())
  #   countries_mentioned <- length(mapData()$countries)
  #   valueBox(
  #     countries_mentioned, "Countries mentioned", icon = icon("globe-europe"),
  #     color = "blue"
  #   )
  # })
  
  # output$total_featured_words <- renderValueBox({
  #   n_featured_words <- length(input$words)
  #   n_featured_words <- prettyNum(n_featured_words, big.mark=".", scientific=FALSE, decimal.mark= ",")
  #   valueBox(
  #     n_featured_words, "Featured words", icon = icon("hashtag"),
  #     color = "light-blue"
  #   )
  # })
  
  ### Sentiment ------------------------------------------------------------
  output$total_sum_sen <- renderValueBox({
    # if(length(input$words) > 0){
    #   data <- sentiment_of_speech_data_filtered()
    # } else{
      data <- sentiment_of_speech_data()
    # }
    total_sum_sen <- sum(data$sentiment)
    total_sum_sen <- prettyNum(total_sum_sen, big.mark=".", scientific=FALSE, decimal.mark= ",")
    valueBox(
      total_sum_sen, "Summed sentiment", icon = icon("theater-masks"),
      color = "purple"
    )
  })
  
  output$total_pos_sen <- renderValueBox({
    # if(length(input$words) > 0){
    #   data <- sentiment_of_speech_data_filtered()
    # } else{
      data <- sentiment_of_speech_data()
    # }
    total_pos_sen <- sum(data$sentiment_pos)
    total_pos_sen <- prettyNum(total_pos_sen, big.mark=".", scientific=FALSE, decimal.mark= ",")
    valueBox(
      total_pos_sen, "Summed positive sentiment", icon = icon("smile"),
      color = "green"
    )
  })
  
  output$total_neg_sen <- renderValueBox({
    # if(length(input$words) > 0){
    #   data <- sentiment_of_speech_data_filtered()
    # } else{
      data <- sentiment_of_speech_data()
    # }
    total_neg_sen <- sum(data$sentiment_neg)
    total_neg_sen <- prettyNum(total_neg_sen, big.mark=".", scientific=FALSE, decimal.mark= ",")
    valueBox(
      total_neg_sen, "Summed negative sentiment", icon = icon("frown"),
      color = "red"
    )
  })
  
  output$total_num_wor <- renderValueBox({
    # if(length(input$words) > 0){
    #   data <- sentiment_of_speech_data_filtered()
    # } else{
      data <- sentiment_of_speech_data()
    # }
    total_num_wor <- sum(data$n_words)
    total_num_wor <- prettyNum(total_num_wor, big.mark=".", scientific=FALSE, decimal.mark= ",")
    valueBox(
      total_num_wor, "Number of words that carried sentiment", icon = icon("equals"),
      color = "purple"
    )
  })
  
  output$num_pos_sen <- renderValueBox({
    # if(length(input$words) > 0){
    #   data <- sentiment_of_speech_data_filtered()
    # } else{
      data <- sentiment_of_speech_data()
    # }
    num_pos_sen <- sum(data$n_pos)
    num_pos_sen <- prettyNum(num_pos_sen, big.mark=".", scientific=FALSE, decimal.mark= ",")
    valueBox(
      num_pos_sen, "Number of words that carried positive sentiment", icon = icon("plus-circle"),
      color = "green"
    )
  })
  
  output$num_neg_sen <- renderValueBox({
    # if(length(input$words) > 0){
    #   data <- sentiment_of_speech_data_filtered()
    # } else{
      data <- sentiment_of_speech_data()
    # }
    num_neg_sen <- sum(data$n_neg)
    num_neg_sen <- prettyNum(num_neg_sen, big.mark=".", scientific=FALSE, decimal.mark= ",")
    valueBox(
      num_neg_sen, "Number of words that had negative sentiment", icon = icon("minus-circle"),
      color = "red"
    )
  })
  
  output$mean_sum_sen <- renderValueBox({
    # if(length(input$words) > 0){
    #   data <- sentiment_of_speech_data_filtered()
    # } else{
      data <- sentiment_of_speech_data()
    # }
    mean_sum_sen <- round(mean(data$sentiment),2)
    mean_sum_sen <- ifelse(is.nan(mean_sum_sen), 0, mean_sum_sen)
    mean_sum_sen <- prettyNum(mean_sum_sen, big.mark=".", scientific=FALSE, decimal.mark= ",")
    valueBox(
      mean_sum_sen, "Average sentiment", icon = icon("equals"),
      color = "purple"
    )
  })
  
  output$mean_pos_sen <- renderValueBox({
    # if(length(input$words) > 0){
    #   data <- sentiment_of_speech_data_filtered()
    # } else{
      data <- sentiment_of_speech_data()
    # }
    mean_pos_sen <- round(mean(data$sentiment_pos),2)
    mean_pos_sen <- ifelse(is.nan(mean_pos_sen), 0, mean_pos_sen)
    mean_pos_sen <- prettyNum(mean_pos_sen, big.mark=".", scientific=FALSE, decimal.mark= ",")
    valueBox(
      mean_pos_sen, "Average positive sentiment", icon = icon("plus"),
      color = "green"
    )
  })
  
  output$mean_neg_sen <- renderValueBox({
    # if(length(input$words) > 0){
    #   data <- sentiment_of_speech_data_filtered()
    # } else{
      data <- sentiment_of_speech_data()
    # }
    mean_neg_sen <- round(mean(data$sentiment_neg),2)
    mean_neg_sen <- ifelse(is.nan(mean_neg_sen), 0, mean_neg_sen)
    mean_neg_sen <- prettyNum(mean_neg_sen, big.mark=".", scientific=FALSE, decimal.mark= ",")
    valueBox(
      mean_neg_sen, "Average negative sentiment", icon = icon("minus"),
      color = "red"
    )
  })
  
  output$mean_num_wor <- renderValueBox({
    # if(length(input$words) > 0){
    #   data <- sentiment_of_speech_data_filtered()
    # } else{
      data <- sentiment_of_speech_data()
    # }
    mean_num_wor <- round(mean(data$n_words),2)
    mean_num_wor <- ifelse(is.nan(mean_num_wor), 0, mean_num_wor)
    mean_num_wor <- prettyNum(mean_num_wor, big.mark=".", scientific=FALSE, decimal.mark= ",")
    valueBox(
      mean_num_wor, "Average number of words that carried sentiment", icon = icon("hashtag"),
      color = "purple"
    )
  })
  
  
  # topicVis ----------------------------------------------------------------
  stm_models <- reactive({
    data <- stm_model_da
    return(data)
  })
  
  output$topicVis <- renderVis({
             if(!is.null(input$nTerms)){
              with(stm_models(),
                    toLDAvisJson(mod, docs, R = input$nTerms))}
    })
  
  ## Show sentences based on topic clicked ----------------------------------
  
  observe({
    req(input$topicVis_topic_click)
    req(input$topis_sentence_slider)
    req(input$topic_r)
    slide_num <- input$topis_sentence_slider
    topic <- input$topicVis_topic_click
    output$topicText <- renderUI({
      if(slide_num>0){
        sentences <- unlist(thoughts$docs[topic])
        id <- unlist(thoughts$uuid[topic])
        df <- data.frame(sentences, id)
        df <- df[sample.int(nrow(df), ifelse(nrow(df)<slide_num, nrow(df), slide_num)),]
        df$sentences <- str_to_sentence(df$sentences) %>% 
          sample(slide_num)
        name <- article_lib %>% 
          filter(uuid %in% id) %>% 
          rowwise() %>% 
          mutate(title = ifelse(nchar(title)>50,
                 paste(strtrim(title, 47), "...", sep=""),
                 title)) %>% 
          .$title %>% 
          sample(slide_num)

        paste("<ul>", paste0("<li>", df$sentences, ".", " (", name, " | uuid: ", df$id, ")", "</li>", collapse = ""), "</ul>") %>%
        # paste("<ul>", paste0("<li>", df$sentences, ".", " (", df$years, ")", "</li>", collapse = ""), "</ul>") %>%
          HTML()
      } else{
        HTML("No senteces allowed")
      }
    })
      
    
    output$topics_sentce_title <-  renderText({
      paste("Sentences belonging to topic", topic)
    })
    
    output$topics_means_title <-  renderText({
      paste("Average sentiment in topic", topic)
    })

    if(input$topic_r == 1){
      golem::invoke_js(
        "getTopics",
        list(
          ok = "ok"
        )
      )
      golem::invoke_js(
        "getTopicsId",
        list(
          ok = "ok"
        )
      )
      
      topics <- unlist(str_split(input$tippertoppertopicspopper, ","))
      print(topics)
      
      topic_id <- as.numeric(input$topic_id)
      print(topic_id)
      
      chosen <- c()
      chosen <- c(unique(c(topics[topics %in% tokens_data()$stemmed],
                           unique(tokens_data()$stemmed[tokens_data()$word %in% topics]),
                           unique(tokens_data()$stemmed[tokens_data()$headword %in% topics])))) # Much faster
      
      # updateSelectizeInput(
      #   session,
      #   "words",
      #   "Featured words",
      #   choices = words_tokens_all,
      #   selected = chosen,
      #   server = TRUE
      # )
      
      updateSelectizeInput(
        session,
        'topic',
        choices = topic_frame$topic,
        selected = paste("Topic", topic_id),
        server = TRUE
      )
    }
    
    output$sent_topic <- renderHighchart({
      df <- thoughts
      polarity_topic <- as.numeric(unlist(df$polarity[topic]))
      polarity_rest <- as.numeric(unlist(df$polarity[-topic]))
      topic_ <- as.numeric(unlist(df$uuid[topic]))
      rest_ <- as.numeric(unlist(df$uuid[-topic]))
      dat <- data.frame(topic = c(rep(paste("Topic", topic), length(polarity_topic)), rep("Rest", length(polarity_rest))),
                        polarity = c(polarity_topic, polarity_rest),
                        id = c(topic_, rest_))
      
      dat <- data_to_boxplot(dat, polarity, group_var = topic, group_var2 = topic)
      
      highchart() %>%
        hc_xAxis(title = list(text = "Topic chosen vs. rest"), type = "category") %>%
        hc_yAxis(title = list(text = "Average sentence sentiment")) %>%
        hc_title(text = paste("Average sentiment in topic", topic)) %>%
        hc_add_series_list(dat) %>%
        hc_tooltip(
          headerFormat = ""
        ) %>% 
        hc_dualcol()
    })
  })
  
  output$sent_topic <- renderHighchart({
    df <- thoughts
    polarity_list <- df$polarity
    polarity <- unnest(data.frame(t(rbind(paste("Topic", 1:length(polarity_list)), data.frame(t(sapply(1:length(polarity_list), function(i) polarity_list[i][1])))))), cols=c(X1, X2))
    polarity$X1 <- factor(polarity$X1, levels = c(paste("Topic", 1:length(polarity_list))))
    dat <- data_to_boxplot(polarity, X2, group_var = X1, group_var2 = X1)
    
    highchart() %>%
      hc_xAxis(title = list(text = "Topics"), type = "category") %>%
      hc_yAxis(title = list(text = "Average sentence sentiment")) %>%
      #hc_title(text = paste("Average sentiment in topic", topic)) %>%
      hc_add_series_list(dat) %>%
      hc_tooltip(
        headerFormat = ""
      ) %>% 
      hc_multicol()
  })
  
  # observeEvent(input$topicVis_term_click, {
  #   req(input$topic_r)
  #   if(input$topic_r == 2){
  #     chosen <- input$words
  #     if(input$topicVis_term_click %in% tokens_data()$stemmed){
  #       chosen <- c(chosen, input$topicVis_term_click)
  #     } else if(input$topicVis_term_click %in% tokens_data()$word){
  #       word <- tokens_data()[tokens_data()$word == input$topicVis_term_click,]$stemmed[1]
  #       chosen <- c(chosen, word)
  #     } else if(input$topicVis_term_click %in% tokens_data()$headword){
  #       word <- tokens_data()[tokens_data()$headword == input$topicVis_term_click,]$stemmed[1]
  #       chosen <- c(chosen, word)
  #     }
  #   
  #     updateSelectizeInput(session,
  #                          "words",
  #                          "Featured words",
  #                          selected = chosen)
  #   }
  # })
  
  output$topics_sentce_title <-  renderText({
    "Sentences belonging to topics"
  })
  
  output$topics_means_title <-  renderText({
    "Average sentiment in topics"
  })
  
  # Sentiment ---------------------------------------------------------------
  ## sentiment data ---------------------------------------------------------
  sentiment_of_speech_data <- reactive({
    data <- sentiment %>%
      filter(uuid %in% id_docs()) %>% 
      mutate(sentiment = round(sentiment),
             sentiment_pos = round(sentiment_pos),
             sentiment_neg = round(sentiment_neg)) %>% 
      group_by(uuid) %>% 
      arrange(date_updated_at)
    return(data)
  })
  
  sentiment_of_words_data <- reactive({
    req(input$slider_sentiment_of_words_n_words)
    data <- tokens_data() %>%
      filter(polarity != 0) %>% 
      filter(uuid %in% id_docs()) %>% 
      group_by(stemmed) %>% 
      distinct(uuid, stemmed, .keep_all = TRUE)
    
    if("fwords" %in% colnames(data)){
      data <- data %>% 
        arrange(desc(fwords), desc(n_stem_total))
    } else{
      data <- data %>% 
        arrange(desc(n_stem_total))
    }
    
    
    first_n_words <- unique(data$stemmed)[1:input$slider_sentiment_of_words_n_words]
    
    data <- data %>% filter(stemmed %in% first_n_words)
    
    if("fwords" %in% colnames(data)){
      data %>% arrange(fwords, polarity, n_stem_total)
    } else{
      data %>% arrange(polarity, n_stem_total)
    }
    
    return(data)
  })
  
  # sentiment_of_speech_data_filtered <- reactive({
  #   req(input$words)
  #   
  #   data <- tokens_data() %>%
  #     rowwise() %>% 
  #     filter(uuid %in% id_docs()) %>% 
  #     mutate(polarity_pos = as.numeric(ifelse(polarity > 0, polarity, 0)),
  #            polarity_neg = as.numeric(ifelse(polarity < 0, polarity, 0)),
  #            n_in_pos = as.numeric(ifelse(polarity > 0, n_in, 0)),
  #            n_in_neg = as.numeric(ifelse(polarity < 0, n_in, 0))) %>% 
  #     group_by(uuid, stemmed)
  #   
  #   if(length(input$words) > 0 && cmatch(data$stemmed, input$words)){
  #     data <- data %>%
  #       filter(stemmed %in% input$words)
  #   }
  # 
  #   data <- data %>%
  #     summarise(sentiment = sum(n_in*polarity),
  #               sentiment_pos = sum(n_in*polarity_pos),
  #               sentiment_neg = sum(n_in*polarity_neg),
  #               average_sentiment = mean(n_in*polarity),
  #               n_pos = sum(n_in_pos),
  #               n_neg = sum(n_in_neg)
  #     ) %>% 
  #     mutate(sentiment = round(sentiment),
  #            sentiment_pos = round(sentiment_pos),
  #            sentiment_neg = round(sentiment_neg),
  #            n_words = n_pos+n_neg) %>% 
  #     arrange(date_updated_at)
  #   return(data)
  # })

  ## sentiment of speeches -------------------------------------------------
  ### Bubles ---------------------------------------------------------------
  output$sentiment_of_speech_bubles <- renderHighchart({
    data <- sentiment_of_speech_data()
    
    shiny::validate(
      need(nrow(data) != 0, "Dataset is empty")
    )
    
    if("fwords" %in% colnames(data)){
      data <- data %>% 
        arrange(fwords)
      hc <- hchart(data,
                   type="bubble",
                   hcaes(x = sentiment_pos,
                         y = sentiment_neg,
                         z = sentiment,
                         pn = n_words,
                         size = sentiment,
                         l = sentiment_label,
                         name = title,
                         group = fwords,
                         fwords = fwords),
                   showInLegend = F,
                   stickyTracking = F,
                   styledMode = T
      ) %>% 
        hc_tooltip(
          useHTML = T,
          headerFormat = '<table>',
          pointFormat = paste('<tr><th colspan="2"><b>{point.title}</b></th></tr>',
                              '<tr><th>UUID:</th><td>{point.uuid}</td></tr>',
                              '<tr><th>Featured words in article :</th><td>{point.fwords}</td></tr>',
                              '<tr><th>Overall sentiment:</th><td>{point.l}</td></tr>',
                              '<tr><th>Positive sentiment (x):</th><td>{point.x}</td></tr>',
                              '<tr><th>Negative sentiment (y):</th><td>{point.y}</td></tr>',
                              '<tr><th>Summed sentiment (size):</th><td>{point.z}</td></tr>',
                              '<tr><th>Words with polarity:</th><td>{point.pn}</td></tr>',
                              sep = ""),
          footerFormat = '</table>',
          followPointer = F,
          followTouchMove = F,
          snap = 100,
          hideDelay = 500
        )
      if(length(unique(data$fwords))>=2){
        hc <- hc %>% hc_dualcol_rev()
      } else{
        hc <- hc %>% hc_dualcol()
      }
    } else{
      hc <- hchart(data,
             type="bubble",
             hcaes(x = sentiment_pos,
                   y = sentiment_neg,
                   z = sentiment,
                   pn = n_words,
                   size = sentiment,
                   l = sentiment_label,
                   name = title,
                   group = uuid),
             showInLegend = F,
             stickyTracking = F,
             styledMode = T
      ) %>% 
        hc_tooltip(
          useHTML = T,
          headerFormat = '<table>',
          pointFormat = paste('<tr><th colspan="2"><b>{point.title}</b></th></tr>',
                              '<tr><th>UUID:</th><td>{point.uuid}</td></tr>',
                              '<tr><th>Overall sentiment:</th><td>{point.l}</td></tr>',
                              '<tr><th>Positive sentiment (x):</th><td>{point.x}</td></tr>',
                              '<tr><th>Negative sentiment (y):</th><td>{point.y}</td></tr>',
                              '<tr><th>Summed sentiment (size/saturation):</th><td>{point.z}</td></tr>',
                              '<tr><th>Words with polarity:</th><td>{point.pn}</td></tr>',
                              sep = ""),
          footerFormat = '</table>',
          followPointer = F,
          followTouchMove = F,
          snap = 100,
          hideDelay = 500
        ) %>% 
        hc_colorAxis(
          minColor = col_red_gradient[2],
          maxColor = col_red_gradient[1],
          floor = min(data$sentiment),
          ceiling = max(data$sentiment),
          min = min(data$sentiment),
          max = max(data$sentiment),
          startOnTick = T,
          endOnTick = T,
          dataClassColor = "category",
          title = "Sentiment"
        )
    }
    hc %>%
      hc_plotOptions(
        bubble = list(
          minSize = paste(7.5, "%", sep=""),
          maxSize = paste(15, "%", sep="")
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
        gridLineWidth = T,
        title = list(
          text = "Positive sentiment"
        )
      ) %>% 
      hc_xAxis(
        startOnTick = T,
        gridLineWidth = T,
        title = list(
          text = "Negative sentiment"
        )
      )
  })
  
  ### Column compare -------------------------------------------------------
  output$sentiment_of_speech_col_compare <- renderHighchart({
    data <- sentiment_of_speech_data() 
    
    shiny::validate(
      need(try(nrow(data) != 0), message = "Dataset is empty")
    )
    
    hc <- highchart() %>% 
      hc_add_series(
        type = "bar",
        stack = 1,
        name="Positive sentiment",
        data = data,
        hcaes(x = title, y = sentiment_pos)
      ) %>% 
      hc_add_series(
        type = "bar",
        stack = 1,
        name="Negative sentiment",
        data = data,
        hcaes(x = title, y = sentiment_neg)
      ) %>% 
      hc_add_series(
        type = "bar",
        stack = 2,
        name ="Summed sentiment",
        data = data,
        hcaes(x = title, y = sentiment)
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
        # endOnTick = T,
        # startOnTick = T,
        title = list(
          text = "ID"
        )
      ) %>% 
      hc_yAxis(
        startOnTick = T,
        title = list(
          text = "Sentiment"
        )
      ) %>% 
      hc_legend(
        reversed = T
      ) %>% 
      hc_tooltip(
        shared = T
      ) %>% 
      hc_quadcolsum()
    # if(length(input$words)>0){
    #   selection <- sentiment_of_speech_data_filtered() %>%
    #     select(sentiment, uuid, stemmed) %>%
    #     ungroup()
    #   for(word in unique(selection$stemmed)){
    #     for(uuid in unique(data$uuid)){
    #       if(!(word %in% selection[selection$uuid == uuid,]$stemmed)){
    #         selection <-  selection %>%
    #           add_row(stemmed = word, uuid=uuid, sentiment=0)
    #       }
    #     }
    #   }
    #   selection <-  selection %>%
    #     group_by(uuid) %>% 
    #     summarise(sentiment = sum(sentiment)) %>% 
    #     arrange(date_updated_at, sentiment)
    #   hc <- hc %>%
    #     hc_add_series(
    #       type = "bar",
    #       stack = 3,
    #       name= "Sentiment of selection",
    #       data = selection,
    #       hcaes(x = uuid, y = sentiment)
    #     )
    # }
    return(hc)
  })
  
  ### Shankey compare -------------------------------------------------------
  output$sentiment_of_speech_sha_compare <- renderHighchart({
    data <- sentiment_of_speech_data()
    
    shiny::validate(
      need(nrow(data) != 0, "Dataset is empty")
    )
    
    hc <- highchart() %>% 
      hc_add_series(
        name = "Positive sentiment",
        type = "line",
        data = data,
        hcaes(x = title, y = sentiment_pos)
        # showInLegend = F
      ) %>% 
      hc_add_series(
        name = "Total sentiment",
        type = "line",
        data = data,
        hcaes(x = title, y = sentiment)
        # showInLegend = F
      ) %>% 
      hc_add_series(
        name = "Negative sentiment",
        type = "line",
        data = data,
        hcaes(x = title, y = sentiment_neg)
        # showInLegend = F
      ) %>% 
      hc_add_series(
        name = "Sentiment Range",
        type = "arearange",
        data = data,
        hcaes(x = title, low = sentiment, high = sentiment_pos, neg = sentiment_neg)
        # enableMouseTracking =F
        # showInLegend = F
      ) %>% 
      hc_norevese() %>% 
      hc_tooltip(
        shared = T,
        headerFormat = "<b>{point.x}</b><br>"#,
        # pointFormat = "Positive (high): {point.high}<br>Negative (difference): {point.neg}<br>Sentiment (low): {point.low}<br>"
        # formatter = JS(paste0('function (){
        # return "<b>"+this.points[0].x+"</b><br>" +
        # "Positive (high): "+this.points[0].high+"<br>"+
        # "Negative (difference): "+this.points[0].y+"<br>"+
        # "Sentiment (low): "+this.points[0].low
        #                       }')
        # )
      ) %>%
      hc_yAxis(
        softMin = 0,
        startOnTick = T
      ) %>% 
      hc_xAxis(
        startOnTick = T,
        categories = data$title,
        title = list(
          text = "ID"
        )
      ) %>% 
      hc_yAxis(
        startOnTick = T,
        title = list(
          text = "Sentiment"
        )
      ) %>% 
      hc_plotOptions(
        arearange = list(
          marker = F
        )
      ) %>% 
      hc_fivecolsum()
    # if(length(input$words)>0){
    #   selection <- sentiment_of_speech_data_filtered() %>% 
    #     select(sentiment, uuid, title, stemmed, date_updated_at) %>% 
    #     ungroup()
    #   for(word in unique(selection$stemmed)){
    #     for(uuid in unique(data$uuid)){
    #       if(!(word %in% selection[selection$uuid == uuid,]$stemmed)){
    #         selection <-  selection %>%
    #           add_row(stemmed = word, title = title, uuid=uuid, sentiment=0, date_updated_at=date_updated_at)
    #       }
    #     }
    #   }
    #   selection <-  selection %>%
    #     group_by(uuid) %>% 
    #     summarise(sentiment = sum(sentiment)) %>% 
    #     arrange(date_updated_at, sentiment)
    #   hc <- hc %>%
    #     hc_add_series(
    #       type = "line",
    #       name= "Sentiment of selection",
    #       data = selection,
    #       hcaes(x = title, y = sentiment)
    #     )
    # }
    return(hc)
  })
  
  ### Column average -------------------------------------------------------
  output$sentiment_of_speech_avg <- renderHighchart({
    data <- sentiment_of_speech_data()
    
    shiny::validate(
      need(nrow(data) != 0, "Dataset is empty")
    )
    
    hchart(data,
           hcaes(x = title,
                 y = round(average_sentiment,2),
                 group = sentiment_label),
           type="column",
           colorByPoint = T,
           styledMode = T
    ) %>% 
      hc_norevese() %>% 
      hc_multicol()
  })

  ## sentiment_of_words ----------------------------------------------------
  ### Sentiment comparison -------------------------------------------------
  output$sentiment_of_words <- renderHighchart({
    data <- sentiment_of_words_data()
    
    shiny::validate(
      need(nrow(data) != 0, "Dataset is empty")
    )
    
    data <- data %>% 
      distinct(stemmed, .keep_all = TRUE) %>% 
      arrange(polarity, n_stem_total)
    
    hc <- hchart(data,
           hcaes(x = stemmed,
                 y = polarity,
                 group = sentiment_true),
           type="column") %>% 
      hc_tooltip(
        shared = TRUE,
        headerFormat = "<b>{point.key}</b><br>",
        pointFormat = "<span style=\"color: {point.color} \">\u25CF</span> {point.series.name}: {point.y}"
      ) %>% 
      hc_norevese() %>% 
      hc_legend(
        reversed = T
      ) %>% 
      hc_yAxis(
        startOnTick = T,
        tickInterval = 1,
        min = -5,
        max = 5,
        title = list(
          text = "Polarity"
        )
      ) %>% 
      hc_xAxis(
        reversed = T,
        title = F
      )
    if(length(unique(data$sentiment_true))<=2){
      hc <- hc %>% hc_dualcol_rev()
    } else if(length(unique(data$sentiment_true))==3){
      if("Positive | featured word" %in% unique(data$sentiment_true)){
        hc <- hc %>% hc_tricol_pos()
      } else{
        hc <- hc %>% hc_tricol_neg()
      }
    } else{
      hc <- hc %>% hc_quadcol_custom()
    }
    return(hc)
  })

  ### Word comparison ------------------------------------------------------
  output$sentiment_of_words_freq <- renderHighchart({
    data <- sentiment_of_words_data() 
    
    shiny::validate(
      need(nrow(data) != 0, "Dataset is empty")
    )
    
    # data <- data %>%
    #   select(!n_stem_total)
    if(length(input$id)>0 || length(input$article)>0){
      total_stem <- data %>%
        group_by(stemmed) %>%
        summarise(n_stem_total = sum(n_stem))
      
      data <- data %>% 
        mutate(n_stem_total = total_stem[min(which(stemmed == total_stem$stemmed)),]$n_stem_total)
    }
    
    data <- data %>% 
      distinct(stemmed, .keep_all = TRUE) %>% 
      mutate(polarity = as.numeric(polarity)*n_stem_total) %>% 
      arrange(n_stem_total, polarity)
    
    if("fwords" %in% colnames(data)){
      data <- data %>% 
        arrange(fwords, n_stem_total, polarity)
    } else{
      data <- data %>% 
        arrange(n_stem_total, polarity)
    }
    
    hc <- hchart(data,
           hcaes(x = stemmed,
                 y = n_stem_total,
                 group = sentiment_true),
           type="column") %>% 
      hc_tooltip(
        shared = TRUE,
        headerFormat = "<b>{point.key}</b><br>",
        pointFormat = "<span style=\"color: {point.color} \">\u25CF</span> Frequency: {point.y} - Total polarity: {point.polarity}."
      ) %>% 
      hc_norevese() %>% 
      hc_legend(
        reversed = T
      ) %>% 
      hc_yAxis(
        startOnTick = T,
        tickInterval = round_any(max(data$n_stem_total)/4,10),
        min = 0,
        max = max(data$n_stem_total),
        title = list(
          text = "Frequency of word"
        )
      ) %>% 
      hc_xAxis(
        reversed = T,
        title = F
      )
    if(length(unique(data$sentiment_true))<=2){
      hc <- hc %>% hc_dualcol_rev()
    } else if(length(unique(data$sentiment_true))==3){
      if("Positive | featured word" %in% unique(data$sentiment_true)){
        hc <- hc %>% hc_tricol_pos()
      } else{
        hc <- hc %>% hc_tricol_neg()
      }
    } else{
      hc <- hc %>% hc_quadcol_custom()
    }
    return(hc)
  })
  
  ## Aricle inspecter -------------------------------------------------------
  output$single_article_inspecter <- renderUI({
    shiny::validate(
      need(length(id_docs()) == 1, "No, or more than 1, article selected.")
    )
    val <- cleaned_sentences %>% 
      filter(uuid == id_docs()) %>% 
      .$content
    # print(val)
    return(HTML(val))
  })

  # Map ---------------------------------------------------------------------
  ## Map data ---------------------------------------------------------------
  mapData <- reactive({
    data <- poly_prep(geojson, countries)
    return(data)
  })

  ## Creating a map ---------------------------------------------------------
  output$map <- renderLeaflet({
    data <- mapData()
    shiny::validate(
      need(nrow(data) != 0, "Dataset is empty")
    )
    leaflet(data,
            options = leafletOptions(worldCopyJump = T,
                                     minZoom = 1.8,
                                     maxZoom = 4,
                                     zoomSnap = 0.2,
                                     )
            ) %>%
      addTiles(options = providerTileOptions(
        worldCopyJump = T,
        minZoom = 1.8,
        maxZoom = 4,
        zoomSnap = 0.2)) %>% 
      addPolygons(stroke = T, weight=0.2, color="black", smoothFactor = 0.3, fillOpacity = 1,
                  fillColor=~pal(n), popup = ~paste("<b>", ADMIN, "</b>", "was said:", n, "times in total", "<br/>",
                                                     sapply(1:length(n_), function(i) ifelse(length(n_[[i]])>10,
                                                                                                 paste(n_[[i]], "t. in:", uuid[[i]], collapse=", "),
                                                                                                 paste(n_[[i]], "times in:", uuid[[i]], collapse="<br/>")),
                                                            simplify=T)),
                  popupOptions = labelOptions(textsize = "8px"),
                  highlightOptions = list(weight = 0.7, fillOpacity = 0.9),
                  layerId = data@data$ADMIN) %>% 
      setView(0, 20, 1.8) %>% 
      addLegend(pal = pal, values = ~n, title="Mentions")
  })
  
  ## Bar chart --------------------------------------------------------------
  output$n_hist <- renderHighchart({
    data <- mapData()
    
    shiny::validate(
      need(nrow(data) != 0, "Dataset is empty")
    )
    
    df <- data.frame(uuid = unlist(data@data$uuid), n_ = unlist(data@data$n_)) %>% 
      group_by(uuid) %>% 
      summarise(n_ = sum(n_)) %>% 
      hchart("bar", name="All", hcaes(x="uuid", y="n_")) %>% 
      hc_yAxis(title = list(text = "Mentions per article")) %>% 
      hc_xAxis(title = list(text = "ID")) %>% 
      hc_tooltip(
        shared = TRUE,
        headerFormat = "<b>{point.key}</b>",
        pointFormat = "<br><span style=\"color: {point.color} \">\u25CF</span> Mentions in Article: {point.y}"
      ) %>% 
      hc_dualcol()
  })
  
  ## Sentences --------------------------------------------------------------
  output$sentences <- renderUI({
    data <- mapData()
    if(nrow(data)==0){
      return("")
    }
    
    data <- tidyr::unnest(tidyr::unnest(data@data, cols = c(uuid, sentence)), cols=c(sentence)) %>% 
      unique() %>% 
      select(sentence, uuid)
    sentences <- data[sample.int(nrow(data), size = 5),]
    # if(!is.null(input$words)){
    #        words <- str_split(sentences$sentence, " ")
    #        featured_word <- paste0("\\b", input$words, "\\b")
    #        test <- sapply(featured_word, function(x) paste0("<b>", str_sub(x, 3, -3), "</b>"), USE.NAMES = T)
    #        words <- lapply(words, function(x) str_replace_all(x, test))
    #        sentences$sentence <- lapply(words, function(x) paste(x, collapse=" "))
    #        sentences$sentence <- str_to_sentence(sentences$sentence)
    # }else{
      sentences$sentence <- str_to_sentence(sentences$sentence)
           # }
    paste("<ul>", paste0("<li>", sentences$sentence, ".", " (", sentences$uuid, ")", "</li>", collapse=""),"</ul>") %>% 
      HTML()
  })
  
  ## Setview and zoom to clicked country ------------------------------------
  observe({
    click <- input$map_shape_click
    proxy <- leafletProxy("map")
    if(is.null(click))
      return()
    proxy %>% setView(lng = click$lng, lat = click$lat, zoom = 4)
  })
  
  ## Making visualization based on clicked country --------------------------
  observe({
    req(input$map_shape_click)
    data <- mapData()
    click <- input$map_shape_click
    selected <- data@data[data@data$ADMIN == click$id,]
    
    if(!is.null(click$id)){
      output$n_hist <- renderHighchart({ # bar chart based on number of times a country is mentioned
        df <- tidyr::unnest(data@data, cols = c(uuid, n_)) %>% 
          select(ADMIN, ISO_A2, uuid, n_)
        df$ISO_A2[df$ADMIN != click$id] <- "*Other"
        df <- df %>% 
          group_by(ISO_A2, uuid) %>% 
          summarise(n_ = sum(n_)) %>% 
          hchart("bar", hcaes(x = "uuid", y = "n_", group = "ISO_A2"),
                stacking = "normal") %>% 
          hc_yAxis(title = list(text = "Mentions per Article")) %>% 
          hc_xAxis(title = list(text = "ID")) %>% 
          hc_title(text = click$id) %>% 
          hc_tooltip(
            shared = TRUE,
            headerFormat = "<b>{point.key}</b><br>Total mentions: {point.total}",
            pointFormat = "<br><span style=\"color: {point.color} \">\u25CF</span> Mentions in Article: {point.y}"
          ) %>% 
          hc_dualcol()
        }) 
      
      output$sent_box <- renderHighchart({# boxplot of average sentiment of sentences in which a country is mentioned
        df <- tidyr::unnest(data@data, cols=c(uuid, sentiment_)) %>% 
          select(ADMIN, ISO_A2, uuid, sentiment_)
        df$ISO_A2[df$ADMIN != click$id] <- "*Other"
        df <- df %>% 
          group_by(ISO_A2, uuid) %>% 
          summarise(sentiment = mean(sentiment_))
        dat <- data_to_boxplot(df, sentiment, ISO_A2, ISO_A2)
        
        highchart() %>% 
          hc_xAxis(title = list(text = "Country"), type = "category") %>% 
          hc_yAxis(title = list(text = "Average sentence sentiment")) %>% 
          hc_title(text = paste("Average sentiment when", click$id, "is mentioned")) %>% 
          hc_add_series_list(dat) %>% 
          hc_dualcol()
      })
      
      output$map_sentce_title <-  renderText({
        paste("Sentences mentioning", click$id)
      })
      
      output$sentences <- renderUI({ # Showing sentences of country mentioned
        req(input$map_sentence_slider)
        data <- tidyr::unnest(tidyr::unnest(selected, cols = c(uuid, sentence)), cols=c(sentence)) %>% 
          unique() %>% 
          select(sentence, uuid)
        slide_num <- input$map_sentence_slider
        sentences <- data[sample.int(nrow(data), size = ifelse(nrow(data) < slide_num, nrow(data), slide_num)),]
        # if(!is.null(input$words)){
        #   words <- str_split(sentences$sentence, " ")
        #   featured_word <- paste0("\\b", input$words, "\\b")
        #   test <- sapply(featured_word, function(x) paste0("<b>",str_sub(x, 3, -3), "</b>"), USE.NAMES = T)
        #   words <- lapply(words, function(x) str_replace_all(x, test))
        #   sentences$sentence <- lapply(words, function(x) paste(x, collapse=" "))
        #   sentences$sentence <- str_to_sentence(sentences$sentence)
        #   print(sentences$sentence)
        # }else{
          sentences$sentence <- str_to_sentence(sentences$sentence)
        # }
        
        paste("<ul>", paste0("<li>", sentences$sentence, ".", " (", sentences$uuid, ")", "</li>", collapse=""),"</ul>") %>% 
          HTML()
      })
      }
    })
  
  output$map_sentce_title <-  renderText({
    "Sentence(s) mentioning a country"
  })
  

  # Fyn Maps ----------------------------------------------------------------
  ## Fyn Maps data ----------------------------------------------------------
  fyn_map_data <- reactive({
    data <- article_lib %>%
      filter(uuid %in% id_docs()) %>%
      group_by(geocode, location) %>% 
      summarise(n = n()) %>%
      mutate(freq = round((n/sum(.$n))*100, 2)) %>% 
      ungroup()
    
    for(geocode_val in unique(article_lib$geocode)){
      if(!(geocode_val %in% data$geocode)){
        data <-  data %>%
          tibble::add_row(geocode = geocode_val,
                  location = article_lib[article_lib$geocode == geocode_val,]$location[1],
                  n = 0,
                  freq = 0)
      }
    }
    
    data <- data %>% 
      group_by(geocode, location) %>% 
      arrange(geocode)
    
    return(data)
  })
  
  ## Fyn Maps creation -----------------------------------------------------
  output$map_fyn_komunes <- renderLeaflet({
    # map_data <- data %>%
    map_data <- fyn_map_data() %>%
      filter(geocode %in% geodata_komunes$kode) %>%
      mutate(freq_fyn = round((n/sum(.$n))*100, 2),
             map_title = HTML(paste("<b>", location, "</b>","<br/>Articles: ", n, sep="")))
    
    # Get and set data
    geodata <- geodata_komunes
    geodata$n <- map_data$n
    geodata$freq <- map_data$freq
    geodata$freq_fyn <- map_data$freq_fyn
    
    crd <- data.frame(x = geodata$visueltcenter_x,
                      y = geodata$visueltcenter_y)
    
    # Set map options
    leaf_options <- leafletOptions(zoomControl = FALSE,
                                   worldCopyJump = F,
                                   minZoom = 8,
                                   maxZoom = 10,
                                   zoomSnap = 0.4
    )
    pal <- colorNumeric(
      palette = col_dual,
      domain = fyn_map_data()$n
    )
    
    # Create map
    leaflet(geodata,
            options = leaf_options
            ) %>%
      addTiles(options = leaf_options) %>%
      addPolygons(stroke = T,
                  weight=1,
                  color="#fff",
                  smoothFactor = 0.3,
                  fillOpacity = 1,
                  fillColor=~pal(n),
                  popup = ~paste("<b>", navn, "-komune</b>", " has: ", n, " articles - ", freq, "% of total - ", freq_fyn, "% on fyn", "<br/>", sep=""),
                  popupOptions = labelOptions(textsize = "8px"),
                  highlightOptions = list(weight = 0.7, fillOpacity = 0.9),
                  layerId = geodata_komunes$kode
                  ) %>%
      addLabelOnlyMarkers(lng = ~x, lat = ~y, data = crd,
                          label = map_data$map_title,
                          labelOptions = labelOptions(noHide = TRUE,
                                                      direction = 'center',
                                                      style = list(
                                                        "color" = "#fff",
                                                        "border" = "0",
                                                        "box-shadow" = "0",
                                                        "background-color" = "transparent",
                                                        "text-align" = "center",
                                                        "text-shaddow" = "-1px -1px 0 rgb(0 0 0 / 50%), 1px -1px 0 rgb(0 0 0 / 50%), -1px 1px 0 rgb(0 0 0 / 50%), 1px 1px 0 rgb(0 0 0 / 50%)"
                                                      ))
                          ) %>%
      # # setView(0, 20, 1.8) %>%
      addLegend(pal = pal, values = fyn_map_data()$n, title="#Articles") %>%
      addControl(paste("Articles with Fyn-komune", sep = ": ", sum(geodata$n)))
  })
  
  output$map_fyn_danmark <- renderLeaflet({
    # map_data <- data %>%
    map_data <- fyn_map_data() %>%
      filter(geocode == "+45")
    
    # Get and set data
    geodata <- geodata_denmark
    geodata$n <- map_data$n
    geodata$freq <- map_data$freq

    # Set map options
    leaf_options <- leafletOptions(zoomControl = FALSE,
                                   worldCopyJump = F,
                                   minZoom = 6,
                                   maxZoom = 8,
                                   zoomSnap = 0.4
    )
    pal <- colorNumeric(
      palette = col_dual,
      domain = fyn_map_data()$n
    )
    
    # Create map
    leaflet(geodata,
            options = leaf_options
            ) %>%
      addTiles(options = leaf_options) %>%
      addPolygons(stroke = T,
                  weight=1,
                  color="#fff",
                  smoothFactor = 0.3,
                  fillOpacity = 1,
                  fillColor=~pal(n),
                  label = ~HTML(paste("<b>Danmark</b>", " has: ", n, " articles - ", freq, "% of total", "<br/>", sep="")),
                  labelOptions = labelOptions(noHide = TRUE,
                                              direction = 'center',
                                              style = list(
                                                "color" = "#fff",
                                                "border" = "0",
                                                "box-shadow" = "0",
                                                "background-color" = "transparent",
                                                "text-align" = "center",
                                                "text-shaddow" = "-1px -1px 0 rgb(0 0 0 / 50%), 1px -1px 0 rgb(0 0 0 / 50%), -1px 1px 0 rgb(0 0 0 / 50%), 1px 1px 0 rgb(0 0 0 / 50%)"
                                              )),
                  popup = ~paste("<b>Danmark</b>", " has: ", n, " articles - ", freq, "% of total", "<br/>", sep=""),
                  popupOptions = labelOptions(textsize = "8px"),
                  highlightOptions = list(weight = 0.7, fillOpacity = 0.9),
                  layerId = geodata@data$name
                  ) %>%  
      # setView(0, 20, 1.8) %>%
      addControl(paste("Articles with Denmark", sep = ": ", geodata$n))
  })
  
  output$map_fyn_southjutland <- renderLeaflet({
    # map_data <- data %>%
    map_data <- fyn_map_data() %>%
      filter(geocode == "1083")
    
    # Get and set data
    geodata <- geodata_southjutland
    geodata$n <- map_data$n
    geodata$freq <- map_data$freq

    # Set map options
    leaf_options <- leafletOptions(zoomControl = FALSE,
                                   worldCopyJump = F,
                                   minZoom = 7,
                                   maxZoom = 9,
                                   zoomSnap = 0.4
    )
    pal <- colorNumeric(
      palette = col_dual,
      domain = fyn_map_data()$n
    )
    
    # Create map
    leaflet(geodata,
            options = leaf_options
            ) %>%
      addTiles(options = leaf_options) %>%
      addPolygons(stroke = T,
                  weight=1,
                  color="#fff",
                  smoothFactor = 0.3,
                  fillOpacity = 1,
                  fillColor=~pal(n),
                  label = ~HTML(paste("<b>", navn, "</b>", " has: ", n, " articles - ", freq, "% of total", "<br/>", sep="")),
                  labelOptions = labelOptions(noHide = TRUE,
                                              direction = 'center',
                                              style = list(
                                                "color" = "#fff",
                                                "border" = "0",
                                                "box-shadow" = "0",
                                                "background-color" = "transparent",
                                                "text-align" = "center",
                                                "text-shaddow" = "-1px -1px 0 rgb(0 0 0 / 50%), 1px -1px 0 rgb(0 0 0 / 50%), -1px 1px 0 rgb(0 0 0 / 50%), 1px 1px 0 rgb(0 0 0 / 50%)"
                                              )),
                  popup = ~paste("<b>", navn, "</b>", " has: ", n, " articles - ", freq, "% of total", "<br/>", sep=""),
                  popupOptions = labelOptions(textsize = "8px"),
                  highlightOptions = list(weight = 0.7, fillOpacity = 0.9),
                  layerId = geodata@data$name
                  ) %>% 
      # setView(0, 20, 1.8) %>%
      addControl(paste("Articles with Southern Jutland", sep = ": ", geodata$n))
  })
  
  output$map_fyn_fyn <- renderLeaflet({
    # map_data <- data %>%
    map_data <- fyn_map_data() %>%
      filter(geocode == "DK031")
    
    # Get and set data
    geodata <- geodata_fyn
    geodata$n <- map_data$n
    geodata$freq <- map_data$freq

    # Set map options
    leaf_options <- leafletOptions(zoomControl = FALSE,
                                   worldCopyJump = F,
                                   minZoom = 8,
                                   maxZoom = 10,
                                   zoomSnap = 0.4
    )
    pal <- colorNumeric(
      palette = col_dual,
      domain = fyn_map_data()$n
    )
    
    # Create map
    leaflet(geodata,
            options = leaf_options
            ) %>%
      addTiles(options = leaf_options) %>%
      addPolygons(stroke = T,
                  weight=1,
                  color="#fff",
                  smoothFactor = 0.3,
                  fillOpacity = 1,
                  fillColor=~pal(n),
                  label = ~HTML(paste("<b>", navn, "</b>", " has: ", n, " articles - ", freq, "% of total", "<br/>", sep="")),
                  labelOptions = labelOptions(noHide = TRUE,
                                              direction = 'center',
                                              style = list(
                                                "color" = "#fff",
                                                "border" = "0",
                                                "box-shadow" = "0",
                                                "background-color" = "transparent",
                                                "text-align" = "center",
                                                "text-shaddow" = "-1px -1px 0 rgb(0 0 0 / 50%), 1px -1px 0 rgb(0 0 0 / 50%), -1px 1px 0 rgb(0 0 0 / 50%), 1px 1px 0 rgb(0 0 0 / 50%)"
                                              )),
                  popup = ~paste("<b>", navn, "</b>", " has: ", n, " articles - ", freq, "% of total", "<br/>", sep=""),
                  popupOptions = labelOptions(textsize = "8px"),
                  highlightOptions = list(weight = 0.7, fillOpacity = 0.9),
                  layerId = geodata@data$name
                  ) %>% 
      # setView(0, 20, 1.8) %>%
      addControl(paste("Articles with Fyn", sep = ": ", geodata$n))
  })
  
  
  # Word statistics ---------------------------------------------------------
  ## Word data --------------------------------------------------------------
  speech_data <- reactive({
    data <- tokens_data() %>%
      filter(uuid %in% id_docs()) %>% 
      select(uuid, word, n_total, n_in, n_stem, n_stem_total, title, date_updated_at) %>% 
      distinct() %>% 
      group_by(word, uuid) %>% 
      arrange(date_updated_at, word)
    
    return(data)
  })
  
  speech_data_word_filt <- reactive({
    data <- speech_data()
    
    # if(length(input$words) > 0){
    #   data <- data %>% 
    #     filter(word %in% input$words)
    # }
    
    return(data)
  })
  
  speech_data_como_filt <- reactive({
    req(input$slider_word_ussage)
    data <- speech_data_word_filt()

    common_opt <- unique(arrange(data, desc(n_total))$n_total)[input$slider_word_ussage]

    if(!is.na(common_opt)){
      data <- filter(data, n_total >= common_opt)
    }
    
    total <- data %>% 
      summarise(n_sel = sum(n_in))
    
    data <- data %>% 
      left_join(total, by= c("uuid", "word"))
    
    return(data)
  })
  
  speech_data_como_filt_max <- reactive({
    req(input$slider_word_ussage)
    data <- speech_data_word_filt()
    
    common_opt <- unique(arrange(data, desc(n_total))$n_total)[ifelse(input$slider_word_ussage>20, 20, input$slider_word_ussage)]
    
    if(!is.na(common_opt)){
      data <- data %>% 
        filter(n_total >= common_opt)
    }
    
    total <- data %>% 
      summarise(n_sel = sum(n_in))
    
    data <- data %>% 
      left_join(total, by= c("uuid", "word"))
    
    return(data)
  })

  ## Word cloud ------------------------------------------------------------
  output$wordcloud <- renderWordcloud2({
    data <- speech_data_como_filt() 
    shiny::validate(
      need(nrow(data) != 0, "Dataset is empty")
    )
    data <- data %>% 
      ungroup() %>% 
      select(word, n_total) %>% 
      distinct() %>% 
      arrange(word) %>% 
      rename(word = word, freq = n_total)
    wordcloud2(data=data, size=1.6, color='random-dark')
  })
  
  ## Stream Graph ----------------------------------------------------------
  output$word_ussage_streamgraph <- renderHighchart({
    data <-  speech_data_como_filt_max()
    shiny::validate(
      need(nrow(data) != 0, "Dataset is empty")
    )

    data <-  data %>% 
      ungroup() %>% 
      select(uuid, title, n_in, word, date_updated_at)

    for(word in unique(data$word)){
      for(uuid in unique(data$uuid)){
        if(!(word %in% data[data$uuid == uuid,]$word)){
          data <-  data %>%
            add_row(word = word,
                    uuid=uuid,
                    date_updated_at=data[data$uuid == uuid,]$date_updated_at[[1]],
                    title=data[data$uuid == uuid,]$title[[1]],
                    n_in=0)
        }
      }
    }

    data <- data %>% 

      arrange(date_updated_at, desc(n_in), word)
    hchart(data, "streamgraph",
           hcaes(title,
                 n_in,
                 group = word)) %>% 
      hc_yAxis(
        visible = F
      ) %>% 
      hc_xAxis(
        title = list(
          text="ID"
        ),
        tickInterval=1
      ) %>% 
      hc_norevese() %>% 
      hc_tooltip(
        shared = T
      ) %>% 
      hc_multicol()
  })
  
  ## Columns ---------------------------------------------------------------
  output$word_ussage_col <- renderHighchart({
    data <-  speech_data_como_filt()
    shiny::validate(
      need(nrow(data) != 0, "Dataset is empty")
    )
    
    common_opt <- unique(arrange(data, desc(n_total))$n_total)[ifelse(input$slider_word_ussage>30, 30, input$slider_word_ussage)]
    
    if(!is.na(common_opt)){
      data <- filter(data, n_total >= common_opt)
    }
    
    hc <- hchart(data, "column",
                 hcaes(x=title,
                       y=n_in,
                       group = word)) %>% 
      hc_plotOptions(
        series = list (
          stacking = 'normal'
        )
      ) %>% 
      hc_norevese() %>% 
      hc_yAxis(
        title = list(
          text="Frequency"
        )
      ) %>% 
      hc_xAxis(
        # categories = min(data$uuid):max(data$uuid),
        title = list(
          text="ID"
        )
      ) %>% 
      hc_tooltip(
        shared = T,
        headerFormat = "<b>ID</b>: {point.x}<br>Total frequency: {point.total}",
        pointFormat = "<br><span style=\"color: {point.color} \">\u25CF</span> {point.series.name}: {point.y}"
      ) %>% 
      hc_multicol()
    return(hc)
  })
  
  ## Columns percent ------------------------------------------------------
  output$word_ussage_col_per <- renderHighchart({
    data <-  speech_data_como_filt()
    shiny::validate(
      need(nrow(data) != 0, "Dataset is empty")
    )
    
    common_opt <- unique(arrange(data, desc(n_total))$n_total)[ifelse(input$slider_word_ussage>30, 30, input$slider_word_ussage)]
    
    if(!is.na(common_opt)){
      data <- filter(data, n_total >= common_opt)
    }
    
    hc <- hchart(data, "column",
                 hcaes(x=title,
                       y=n_in,
                       group = word)) %>% 
      hc_plotOptions(
        series = list (
          stacking = 'percent'
        )
      ) %>% 
      hc_norevese() %>% 
      hc_tooltip(
        shared = T
      ) %>% 
      hc_multicol()
    return(hc)
  })
  
  ## Pie ------------------------------------------------------------------
  output$word_ussage_pie <- renderHighchart({
    data <-  speech_data_como_filt()
    shiny::validate(
      need(nrow(data) != 0, "Dataset is empty")
    )
    
    common_opt <- unique(arrange(data, desc(n_total))$n_total)[ifelse(input$slider_word_ussage>40, 40, input$slider_word_ussage)]
    
    if(!is.na(common_opt)){
      data <- filter(data, n_total >= common_opt)
    }
    
    data <- data %>% 
      ungroup() %>%
      select(word, n_in) %>%
      group_by(word) %>% 
      summarise(n_total = sum(n_in)) %>% 
      arrange(desc(n_total))

    hc <- hchart(data, "pie", name="Frequency",
                 hcaes(y=n_total, name = word)) %>% 
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
      hc_multicol %>% 
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
    shiny::validate(
      need(nrow(data) != 0, "Dataset is empty")
    )
    
    hc <- hchart(data, "scatter",
                 hcaes(x=title,
                       y=n_in,
                       total = n_total,
                       group = word)) %>% 
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
      hc_yAxis(
        title = list(
          text="Frequency"
        )
      ) %>% 
      hc_xAxis(
        startOnTick = T,
        endOnTick = T,
        showLastLabel = T,
        tickInterval=1,
        title = list(
          text="ID"
        )
      ) %>% 
      hc_tooltip(
        shared = T,
        headerFormat = "<span style=\"color: {point.color} \">\u25CF</span> <b>{point.series.name}</b><br>",
        pointFormat = "ID: {point.x}<br>Frequency: {point.y}<br>Total frequency: {point.total}"
      ) %>% 
      hc_multicol()
    return(hc)
  })
  
  ## Table ----------------------------------------------------------------
  output$word_ussage_tbl <- renderDT({ # unused
    speech_data_como_filt() %>%
      ungroup() %>%
      transmute(`uuid` = uuid, `Word` = word, `Total frequency` = n_total,
                `Frequency in year` = n_in,
                `Frequency in selection in year` = n_sel)
  }, rownames = FALSE, filter = 'top',
  options = list(

    )
  )
  

  # Data UI ----------------------------------------------------------------
  ## Covered speeches ------------------------------------------------------
  output$Covered <- renderUI({
    test <<-article_lib_filt()
    id <- article_lib_filt()$uuid
    title <- article_lib_filt()$title
    source <- article_lib_filt()$link

    shiny::validate(
      need(nrow(article_lib_filt()) != 0, "No avilable data")
    )
    
    content <- tags$ul(
      lapply(1:nrow(article_lib_filt()), function(i) {
        tags$li(a(href=source[i],
                  target = "_blank",
                  paste(title[i], 
                        " ",
                        source[i],
                        " (", id[i], ")",
                        sep=""
                        )
                  )
                )
      })
    )
    return(content)
  })
  
  output$update_data_local_warning <- renderUI({
    if(is_local){
      p("Notice: In local version, forced reset is not applied. Reload page by yourself.")
    }
  })
  
  observeEvent(input$refresh_data, {
    source("scripts/load_article_data.R")
    file.create("restart.txt")
  })
  
  observeEvent(input$update_core_data, {
    source("scripts/update_core_data.R")
    source("scripts/load_article_data.R")
    file.create("restart.txt")
  })
  
  observeEvent(input$update_non_stm_data, {
    source("scripts/update_non_topic_data.R")
    source("scripts/load_article_data.R")
    file.create("restart.txt")
  })

}
