server <- function(input, output) {
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
