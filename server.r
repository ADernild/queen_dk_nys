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
}
