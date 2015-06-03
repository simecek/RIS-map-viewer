require(shiny)

source("plotgeno.R")
dataset <- readRDS("data/dataset.rds")
strains = readRDS("data/strains.rds")
panels <- readRDS("data/panels.rds")

get.strain <- function(input) {
  c(input$strain1, input$strain2, input$strain3,
    input$strain4, input$strain5, input$strain6)[which(panels==input$panel)]
}
get.compare <- function(input) {
  c(input$compare1, input$compare2, input$compare3,
    input$compare4, input$compare5, input$compare6)[which(panels==input$panel)]
}

shinyServer(function(input, output) {
   
  output$genoPlot <- renderPlot({
    
    sel.strain = get.strain(input)
    sel.compare = get.compare(input)    
    # make genotype plot
    if (sel.compare == "") {
      tmp <- subset(dataset, strain==sel.strain)
      plotgeno(tmp)
    } else {
      tmp <- subset(dataset, strain==sel.strain)
      tmp2 <- subset(dataset, strain==sel.compare)
      plot2geno(tmp2, tmp)
    }  
  })
  
  output$downloadStrain <- downloadHandler(
    
    # download genotype of the given strain
    
    filename = function() { paste(make.names(get.strain(input)), '.csv', sep='') },
    content = function(file) {
      write.csv(subset(dataset, strain%in%c(get.strain(input),get.compare(input))), file, row.names=FALSE)
    }
  )
  
  output$downloadPanel <- downloadHandler(
    
    # download genotype of the given panel
    
    filename = function() { paste(make.names(input$panel), '.csv', sep='') },
    content = function(file) {
      write.csv(subset(dataset, panel==input$panel), file, row.names=FALSE)
    }
  )
  
})
