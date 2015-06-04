library(shiny)

panels <- readRDS("data/panels.rds")
strains<- readRDS("data/strains.rds")

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("RIS strains"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    selectInput(inputId = "panel", label = strong("Panel"), 
                choices = panels),
    helpText("RIS = recombinant inbred strains", br(), 
             "CSS = chromosome substitution strains"),
    
    
    conditionalPanel(
      condition = "input.panel == 'AXB/BXA RIS'",
      selectInput(inputId = "strain1", label = strong("Strain"), 
                  choices = strains[['AXB/BXA RIS']]),
      selectInput(inputId = "compare1", label = strong("In comparison to"), choices = c("",strains[['AXB/BXA RIS']]), selected = '')),
    conditionalPanel(
      condition = "input.panel == 'BXD RIS'",
      selectInput(inputId = "strain2", label = strong("Strain"), 
                  choices = strains[['BXD RIS']]),
      selectInput(inputId = "compare2", label = strong("In comparison to"), choices = c("",strains[['BXD RIS']]), selected = '')),
    conditionalPanel(
      condition = "input.panel == 'LXS RIS'",
      selectInput(inputId = "strain3", label = strong("Strain"), 
                  choices = strains[['LXS RIS']]),
      selectInput(inputId = "compare3", label = strong("In comparison to"), choices = c("",strains[['LXS RIS']]), selected = '')),
    conditionalPanel(
      condition = "input.panel == 'B6.A CSS'",
      selectInput(inputId = "strain4", label = strong("Strain"), 
                  choices = strains[['B6.A CSS']]),
      selectInput(inputId = "compare4", label = strong("In comparison to"), choices = c("",strains[['B6.A CSS']]), selected = '')),
    conditionalPanel(
      condition = "input.panel == 'B6.PWD CSS'",
      selectInput(inputId = "strain5", label = strong("Strain"), 
                  choices = strains[['B6.PWD CSS']]),
      selectInput(inputId = "compare5", label = strong("In comparison to"), choices = c("",strains[['B6.PWD CSS']]), selected = '')),
    conditionalPanel(
      condition = "input.panel == 'B6.MSM CSS'",
      selectInput(inputId = "strain6", label = strong("Strain"), 
                  choices = strains[['B6.MSM CSS']]),
      selectInput(inputId = "compare6", label = strong("In comparison to"), choices = c("",strains[['B6.MSM CSS']]), selected = '')),
    
    
    helpText(" "," "," "," "),
    div(HTML("<br><br><br><br>")),
    
    
    selectInput("dstrain", "Download the strain as",
                choices = c("list of intervals", "csvr format")),
    downloadButton('downloadStrain', 'Download '),
    selectInput("dataset", "Download the panel as",
                choices = c("list of intervals", "csvr format")),
    downloadButton('downloadPanel', 'Download '),
    
    div(HTML("<br><br>")),
    radioButtons("assembly", "Genome assembly", c("mm9", "mm10"), selected="mm9"),
    
    div("Developed by Petr Simecek, see ",
        a("Github", href="https://github.com/simecek/RIS-map-viewer"),
        " for R/shiny source code")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("genoPlot", width="800px", height="800px")
  )
))
