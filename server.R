library(shiny)

source("plotgeno.R")

#dataset <-read.csv("imap.together.csv", as.is=TRUE)
#display <- read.csv("master.together.csv", as.is=TRUE)
#dataset$strain <- display$display[match(dataset$strain, make.names(display$name))]
#dataset$type.explained = NA
#dataset$type.explained[dataset$type %in% c("gain", "loss")] <- "CNV"
#dataset$type.explained[dataset$type =="CC"] <- "Contamination"
#dataset$type.explained[dataset$type =="AB"] <- "Heterozygous"
#dataset$type.explained[dataset$type =="AA" & dataset$panel != "LXS"] <- "C57BL/6J"
#dataset$type.explained[dataset$type =="AA" & dataset$panel == "LXS" ] <- "ILS"
#dataset$type.explained[dataset$type =="BB" & dataset$panel %in% c("AXB","A.B6") ] <- "A/J"
#dataset$type.explained[dataset$type =="BB" & dataset$panel == "BXD" ] <- "DBA/2J"
#dataset$type.explained[dataset$type =="BB" & dataset$panel == "LXS" ] <- "ISS"
#dataset$type.explained[dataset$type =="BB" & dataset$panel == "MSM.B6" ] <- "MSM/Ms"
#dataset$type.explained[dataset$type =="BB" & dataset$panel == "PWD.B6" ] <- "PWD/Ph"
#write.csv(dataset,"dataset.csv",row.names=FALSE)

dataset <-read.csv("dataset.csv", as.is=TRUE)

get.panel <- function(s) subset(dataset, strain==s)$panel[1]

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
   
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  output$genoPlot <- renderPlot({
        
    # make genotype plot
    if (input$compare == "") {
      tmp <- subset(dataset, strain==input$strain)
      plotgeno(tmp)
    } else {
      tmp <- subset(dataset, strain==input$strain)
      tmp2 <- subset(dataset, strain==input$compare)
      plot2geno(tmp2, tmp)
    }  
  })
  
  output$downloadStrain <- downloadHandler(
    
    # download genotype of the given strain
    
    filename = function() { paste(make.names(input$strain), '.csv', sep='') },
    content = function(file) {
      write.csv(subset(dataset, strain==input$strain), file, row.names=FALSE)
    }
  )
  
  output$downloadPanel <- downloadHandler(
    
    # download genotype of the given strain
    
    filename = function() { paste(make.names(get.panel(input$strain)), '.csv', sep='') },
    content = function(file) {
      write.csv(subset(dataset, panel==get.panel(input$strain)), file, row.names=FALSE)
    }
  )
  
})
