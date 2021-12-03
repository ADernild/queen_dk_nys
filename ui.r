ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  # Application title
  titlePanel("Queen speech central"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 420,
                  value = 69)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("distPlot1"),
      plotOutput("distPlot2"),
      plotOutput("distPlot3")
    )
  )
)