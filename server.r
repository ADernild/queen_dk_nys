server <- function(input, output) {
  output$topicmodel <- renderVis({
    if(!is.null(input$nTerms)){
      ifelse(input$modelVis == "lda_model",
             with(lda_model,
                  createJSON(phi, theta, doc.length, vocab, term.frequency,
                             R = input$nTerms)),
             with(stm_model,
                  toLDAvisJson(mod, docs, R = input$nTerms)))
    }
  })
  
  sentiment_of_words_data <- reactive({
    data <- tokens %>%
      filter(polarity != 0) %>% 
      group_by(headword) %>% 
      distinct(headword, .keep_all = TRUE) %>%
      arrange(desc(n_hword_total)) %>% 
      head(input$slider_sentiment_of_words_n_words) %>% 
      arrange(polarity)
  })
  
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
      hc_plotOptions(
        series = list(
          reversed = FALSE
        )
      )
  })
}
