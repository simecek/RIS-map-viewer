require(shiny)

source("plotgeno.R")
dataset <- readRDS("data/dataset.rds")
strains <- readRDS("data/strains.rds")
panels <- readRDS("data/panels.rds")
csvr.files <- c("AXB/BXA RIS" = "csvr/rqtl.axb.csv", 
                "BXD RIS"  = "csvr/rqtl.bxd.csv",
                "LXS RIS"  = "csvr/rqtl.lxs.csv",
                "B6.A CSS" = "csvr/rqtl.a.b6.csv",
                "B6.PWD CSS"  = "csvr/rqtl.pwd.b6.csv",
                "B6.MSM CSS" = "csvr/rqtl.msm.b6.csv")

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
      plotgeno(aconvert(tmp, input$assembly), chr.lengths(input$assembly))
    } else {
      tmp <- subset(dataset, strain==sel.strain)
      tmp2 <- subset(dataset, strain==sel.compare)
      plot2geno(aconvert(tmp2, input$assembly), aconvert(tmp, input$assembly), chr.lengths(input$assembly))
    }  
  })
  
  output$downloadStrain <- downloadHandler(
    
    # download genotype of the given strain
    
    filename = function() { paste(make.names(get.strain(input)), '.csv', sep='') },
    content = function(file) {
      if (input$dstrain == "list of intervals") {
        write.csv(aconvert(subset(dataset, strain%in%c(get.strain(input),get.compare(input))),input$assembly), file, row.names=FALSE)
      } else {
        tmp <- read.csv(csvr.files[input$panel], as.is=TRUE, check.names=FALSE)
        cols <- c(1:4, which(names(tmp)==input$strain1),ncol(tmp))
        write.csv(aconvert2(tmp[,cols],input$assembly), file, row.names=FALSE)
      }   
    }
  )
  
  output$downloadPanel <- downloadHandler(
    
    # download genotype of the given panel
    
    filename = function() { paste(make.names(input$panel), '.csv', sep='') },
    
    content = function(file) {
      if (input$dataset == "list of intervals") {
        write.csv(aconvert(subset(dataset, panel==input$panel),input$assembly), file, row.names=FALSE)
      } else {
        tmp <- read.csv(csvr.files[input$panel], as.is=TRUE, check.names=FALSE)
        write.csv(aconvert2(tmp,input$assembly), file, row.names=FALSE)
      }  
    }
  )
  
})
