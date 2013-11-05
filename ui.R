library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("RIS strains"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    textInput("strain", "Strain", "BXD1"),
    textInput("compare", "In comparison to", ""),
    submitButton("Update View"),
    helpText(" "," "," "," "),
    div(HTML("<br><br><br><br>")),
    includeHTML("www/js/myselect2.js"),
    selectInput(inputId = "tool", label = "Tool:", choices = c("AXB1", "AXB2", "AXB3"), selected = 'Data view'),
    selectInput("dataset", "Download the strain as",
                choices = c("list of intervals", "smoothed genotype", "raw MDA data")),
    downloadButton('downloadStrain', 'Download '),
    selectInput("dataset", "Download the panel as",
                choices = c("list of intervals", "smoothed genotype", "raw MDA data")),
    downloadButton('downloadPanel', 'Download ')
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("genoPlot", width="800px", height="800px")
  )
))
