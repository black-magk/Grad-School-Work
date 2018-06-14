library(shiny)
library(ggplot2)

dataset = read.csv("Shiny_book.csv")

shinyUI(pageWithSidebar(
  
  headerPanel("Views"),
  
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