server <- function(input, output) {

  # Sidebar Menu -------------------------------------------------------------
  output$menu <- renderMenu({
      sidebarMenu(
        menuItem("Welcome and about", tabName = "index", icon = icon("home")),
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

  # sentiment_of_words_data -------------------------------------------------
  sentiment_of_words_data <- reactive({
    data <- tokens %>%
      filter(polarity != 0) %>% 
      group_by(headword) %>% 
      distinct(headword, .keep_all = TRUE) %>%
      arrange(desc(n_hword_total)) %>% 
      head(input$slider_sentiment_of_words_n_words) %>% 
      arrange(polarity)
  })
  
  # sentiment_of_words ------------------------------------------------------
  output$sentiment_of_words <- renderHighchart({
    data <- sentiment_of_words_data()
    hchart(data,
           hcaes(x = headword, y = polarity, group = sentiment_true),
           type="column") %>% 
      hc_chart(
        inverted = FALSE
      ) %>% 
      hc_xAxis(
        reversed = TRUE
      ) %>% 
      hc_yAxis(
        min = -3,
        max =3
      ) %>% 
      hc_colorAxis(
        reversed = FALSE
      ) %>% 
      hc_legend(
        reversed = TRUE
      ) %>% 
      hc_tooltip(
        pointFormat = "<span style=\"color: {point.color} \">\u25CF</span> {point.series.name}: {point.y}",
        headerFormat = "<b>{point.key}</b><br>",
        shared = TRUE
      ) %>% 
      hc_plotOptions(
        series = list(
          reversed = FALSE
        )
      )
  })

  # wiki_infobox ------------------------------------------------------------
  output$wiki_infobox <- renderUI({
    includeHTML("www/queen_info_table.html")
  })
}
