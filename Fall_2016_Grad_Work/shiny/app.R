library(shiny)
library(ggplot2)

library(rsconnect)
dataset <- read.csv('Shiny_book.csv')


ui <- shinyUI(pageWithSidebar(
  
  headerPanel("Drink Views"),
  
  sidebarPanel(
    
    sliderInput('sampleSize', 'Observed Sample Selection', min=1, max=nrow(dataset),
                value=min(1, nrow(dataset)), step=1, round=0),
    
    selectInput('x', 'X', names(dataset)),
    selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
    selectInput('color', 'Color', c('None', names(dataset))),
    
   
    
    selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
    selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
  ),
  
  mainPanel(
    plotOutput('plot')
  )
))

server <- shinyServer(function(input, output) {
  
  dataset <- reactive({
    Shiny_book[sample(nrow(Shiny_book), input$sampleSize),]
  })
  
  output$plot <- renderPlot({
    
    p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
 
    print(p)
    
  }, height=700)
  
})

shinyApp(ui = ui, server = server)
shiny::runApp()
deployApp(shinyApp())

