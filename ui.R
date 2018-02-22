library(shiny)
library(plotly)

shinyUI(fluidPage(

  titlePanel("N dim clustering"),

  sidebarPanel(
    fileInput('myfile','Upload csv'),
    checkboxInput('dt_range_flg','Use date range',value = FALSE),
    uiOutput("myDateRange"),
    checkboxInput('fil_flg','Use filter',value = FALSE),
    uiOutput("myOptions"),
    uiOutput("myColInputs"),
    uiOutput('myPrintf'),
    sliderInput('num_clusters', 'Cluster count', min = 1, max = 15, value = 5),
    actionButton("myRefreshButton","Refresh graph"),
    uiOutput("myDownloadOptions")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Scatter plot",plotlyOutput("myScatterPlot")),
      tabPanel("Cluster sizes",plotlyOutput("myClusterSizesPlot")),
      tabPanel("Summary of data",plotlyOutput("mySummaryPlot_1"),plotlyOutput("mySummaryPlot_2"))
      )
  )
  
  )
)
