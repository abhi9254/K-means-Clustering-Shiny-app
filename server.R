

shinyServer(function(input, output) {
  options(shiny.maxRequestSize=30*1024^2)
  inputfile <-reactive({input$myfile})
  
  mydata<-reactive(if(is.null(input$myfile)){return()}
    else {read.csv(inputfile()$datapath)})
  
  
  output$myDateRange<-renderUI({
    if(is.null(mydata()))
      return() 
    else
    {
    if(input$dt_range_flg==TRUE){  
      mycols<-names(mydata())
      tagList(selectInput('dt_range_col','Select date column',mycols,selected=mycols[3]),
              dateRangeInput('desired_dt_range','Date range (Inclusive)',format="yyyy-mm-dd",start ="2011-01-01",end="2011-01-01",
              startview = "decade", weekstart = 0, separator = " to ",width = "250" )
      )}
    }
  })
    
    output$myOptions <-renderUI({
    if(is.null(mydata()))
      return() 
    else
    { 
      if(input$fil_flg==TRUE){  
      mycols<-names(mydata())
      tagList(selectInput('fil_col1','Filter (Include rows)',mycols,selected=mycols[3]),
      radioButtons("rel1","Select relation",list("==","!=",">","<",">=","<="),selected=">",inline=TRUE),
      textInput("fil_val","Select a value",value ="",width = '200px'))
    }
    }})
  
  
  output$myColInputs <-renderUI({
    if(is.null(mydata()))
      return() 
    else
    { mycols<-names(mydata())
      tagList(selectInput('xcol','X variable',mycols,selected=mycols[3]),
              selectInput('ycol','Y variable',mycols,selected=mycols[4]))
    }
  })

  
#  Just a printf  
#  output$myPrintf<-renderUI({
#    selectInput('printf_1','Just printing',as.character(input$desired_dt_range[1]))
#  })

  
#Filter on data
  
   myActivedata<-reactive({
  
  #  Date range filter not working properly
  #  if(input$dt_range_flg==TRUE && input$dt_range[1]!='2011-01-01' && input$dt_range[2]!='2011-01-01') 
  #    subset(mydata(),mydata()[input$dt_range_col] >= input$desired_dt_range[1] ) #&& mydata()[input$dt_range_col] <= input$desired_dt_range[2]))
  #  else
  #    mydata()
  # })
    
     
     
    if(input$fil_flg==TRUE && nchar(input$fil_val)>0)
    { 
    switch(input$rel1,  
    '>'=subset(mydata(),mydata()[input$fil_col1] > input$fil_val),
    '<'=subset(mydata(),mydata()[input$fil_col1] < input$fil_val),
    '=='=subset(mydata(),mydata()[input$fil_col1] == input$fil_val),
    '!='=subset(mydata(),mydata()[input$fil_col1] != input$fil_val),
    '<='=subset(mydata(),mydata()[input$fil_col1] <= input$fil_val),
    '>='=subset(mydata(),mydata()[input$fil_col1] >= input$fil_val)
    )}
    else 
    mydata()
    })
    
   
   
  x<-reactive({myActivedata()[c(input$xcol)]})
  y<-reactive({myActivedata()[c(input$ycol)]})
  
  selectedData <- reactive({myActivedata()[c(input$xcol, input$ycol)]})
  km <- reactive({ set.seed(500)
        kmeans(selectedData(), input$num_clusters)})
  
  clusters<-reactive({km()$cluster})
  uniq_clusters<-reactive({unique(clusters())})
  
  
  
  output$myDownloadOptions<-renderUI({
    if(input$myRefreshButton==0)
      return()
    else
      tagList(
        selectInput("selectedDataset","Download results",paste("Cluster",uniq_clusters(),sep = " ")),
        downloadButton("downloadData","Download"))
    })
  
  output$myScatterPlot <- renderPlotly({
    if(input$myRefreshButton==0)
      return()
    
    else{
    x<-x()[,1]
    y<-y()[,1]
    
    plot_ly(myActivedata(),x=x,y=y,text =paste("Cluster :",km()$cluster,sep=""),
            type="scatter", mode="markers", color = clusters() )
    layout( dragmode =  "select",
            xaxis = list(title = input$xcol),
            yaxis = list(title = input$ycol))}
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$selectedDataset, '.csv', sep='') },
    content = function(file) {
      mydataset <-myActivedata()
      mydataset$cluster<-paste("Cluster",km()$cluster,sep=" ")
      mydataset2 <-subset(mydataset,mydataset$cluster==input$selectedDataset,select = c(1))
      write.csv(mydataset2, file,row.names=FALSE)
    },
    contentType="text/plain"
  )
  
  
  output$myClusterSizesPlot <- renderPlotly({
    if(input$myRefreshButton==0)
      return()
    
    else{
      Size<-km()$size
      Tot_size=NROW(myActivedata())
      Per_size=round((Size/Tot_size)*100,2)
      cluster_col<-as.numeric(rownames(km()$centers))
      plot_ly(x=km()$centers[,1],y=km()$centers[,2],type='scatter',size=Size,color=cluster_col,text=paste("Cluster :",cluster_col," (",Per_size,"%)",sep=""),mode='markers' )
      layout(
        xaxis = list(title = input$xcol),
        yaxis = list(title = input$ycol)
      )
      }
  })
      
  
  output$mySummaryPlot_1 <-renderPlotly({
    if(input$myRefreshButton==0)
      return()
    else{
      agg <-aggregate(myActivedata(), by=round(myActivedata()[c(input$xcol)]), FUN=length)
      plot_ly(x=agg[,1],y=agg[,2] ,type = "bar")
      layout(xaxis = list(title = input$xcol),
             yaxis = list(title= "Count"))
    }
  })
  
  
  output$mySummaryPlot_2 <-renderPlotly({
    if(input$myRefreshButton==0)
      return()
    else{
      agg2 <-aggregate(myActivedata(), by=round(myActivedata()[c(input$ycol)]), FUN=length)
      plot_ly(x=agg2[,1],y=agg2[,2] ,type = "bar")
      layout(xaxis = list(title = input$ycol),
             yaxis = list(title= "Count"))
    }
  })
  
  
})