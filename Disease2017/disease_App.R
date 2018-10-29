if (!require("shiny")) install.packages("shiny")
if (!require("shinyShortcut")) install.packages("shinyShortcut")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("maptools")) install.packages("maptools")
if (!require("rgeos")) install.packages("rgeos")
if (!require("Cairo")) install.packages("Cairo")
if (!require("ggmap")) install.packages("ggmap")
if (!require("scales")) install.packages("scales")
if (!require("RColorBrewer")) install.packages("RColorBrewer")

setwd("C:\\Disease2017\\IND_adm")
shinyApp(
  ui = tagList(
    shinythemes::themeSelector(),
    navbarPage(
      
      "Statewise Visualization of various diseases in 2017",
      tabPanel("Disease counts displayed in the Map of India",
               sidebarPanel(
                 fileInput("file1", "Choose the data file",
                           multiple = FALSE),
                 actionButton("action", "ANALYZE")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Diabetes",h4("Statewise Distribution of Diabetes cases in 2017"),plotOutput("plot1",width = "100%")),
                   tabPanel("Asthma",h4("Statewise Distribution of Asthma cases in 2017"),plotOutput("plot2",width = "100%")),
                   tabPanel("Disability",h4("Statewise Distribution of Disability cases in 2017"),plotOutput("plot3",width = "100%")),
                   tabPanel("Chronic Illness",h4("Statewise Distribution of Chronic Illness cases in 2017"),plotOutput("plot4",width = "100%")),
                   tabPanel("Diarrhoea",h4("Statewise Distribution of Diarrhoea cases in 2017"),plotOutput("plot5",width = "100%")),
                   tabPanel("Respiratory",h4("Statewise Distribution of Respiratory cases in 2017"),plotOutput("plot6",width = "100%")),
                   tabPanel("Fever",h4("Statewise Distribution of Fever cases in 2017"),plotOutput("plot7",width = "100%")),
                   tabPanel("Tuberculosis",h4("Statewise Distribution of Tuberculosis cases in 2017"),plotOutput("plot8",width = "100%"))
                 )
               )
      )
    )
  ),
  server = function(input, output,session) {
    x = reactive({
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      disease_data = read.csv(inFile$datapath, header = TRUE)
      
    })
    #Diabetes
    observeEvent(input$action,{output$plot1 = renderPlot({
      disease_data = x()
      ##create (or input) data to plot on map
      states.shp <- readShapeSpatial("IND_adm1.shp")
      num.states<-length(states.shp$NAME_1)
      mydata<-data.frame(NAME_1=states.shp$NAME_1, id=states.shp$ID_1, disease=disease_data$Diabetes)
      
      #fortify shape file to get into dataframe 
      states.shp.f <- fortify(states.shp, region = "ID_1")
      
      #merge with coefficients and reorder
      merge.shp.coef<-merge(states.shp.f, mydata, by="id", all.x=TRUE)
      final.plot<-merge.shp.coef[order(merge.shp.coef$order), ]
      disease_data = as.data.frame(disease_data)
      
      ggplot() +
        geom_polygon(data = final.plot, 
                     aes(x = long, y = lat, group = group, fill = disease), 
                     color = "white", size = 0.25) + 
        coord_map() +
        guides(fill=guide_legend(title="Number of Recorded cases"))  + 
        geom_label(size=3,aes(label = mydata$disease, x = disease_data$long, y = disease_data$lat)) +
        
        theme(legend.position = 'left',
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    }, height = 600, width = 800 )
    })
    #Asthma
    observeEvent(input$action,{output$plot2 = renderPlot({
      disease_data = x()
      # inFile <- input$file1
      # 
      # if (is.null(inFile))
      #   return(NULL)
      # disease_data = read.csv(inFile$datapath, header = TRUE)
      ##create (or input) data to plot on map
      states.shp <- readShapeSpatial("IND_adm1.shp")
      num.states<-length(states.shp$NAME_1)
      mydata<-data.frame(NAME_1=states.shp$NAME_1, id=states.shp$ID_1, disease=disease_data$Asthma)
      
      #fortify shape file to get into dataframe 
      states.shp.f <- fortify(states.shp, region = "ID_1")
      
      #merge with coefficients and reorder
      merge.shp.coef<-merge(states.shp.f, mydata, by="id", all.x=TRUE)
      final.plot<-merge.shp.coef[order(merge.shp.coef$order), ]
      disease_data = as.data.frame(disease_data)
      
      ggplot() +
        geom_polygon(data = final.plot, 
                     aes(x = long, y = lat, group = group, fill = disease), 
                     color = "white", size = 0.25) + 
        coord_map() +
        guides(fill=guide_legend(title="Number of Recorded cases"))  + 
        geom_label(size=3,aes(label = mydata$disease, x = disease_data$long, y = disease_data$lat)) +
        theme(legend.position = 'left',
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    }, height = 600, width = 800 )
    })
    #Disability
    observeEvent(input$action,{output$plot3 = renderPlot({
      disease_data = x()
      # inFile <- input$file1
      # 
      # if (is.null(inFile))
      #   return(NULL)
      # disease_data = read.csv(inFile$datapath, header = TRUE)
      ##create (or input) data to plot on map
      states.shp <- readShapeSpatial("IND_adm1.shp")
      num.states<-length(states.shp$NAME_1)
      mydata<-data.frame(NAME_1=states.shp$NAME_1, id=states.shp$ID_1, disease=disease_data$Disability)
      
      #fortify shape file to get into dataframe 
      states.shp.f <- fortify(states.shp, region = "ID_1")
      
      #merge with coefficients and reorder
      merge.shp.coef<-merge(states.shp.f, mydata, by="id", all.x=TRUE)
      final.plot<-merge.shp.coef[order(merge.shp.coef$order), ]
      disease_data = as.data.frame(disease_data)
      
      ggplot() +
        geom_polygon(data = final.plot, 
                     aes(x = long, y = lat, group = group, fill = disease), 
                     color = "white", size = 0.25) + 
        coord_map() +
        guides(fill=guide_legend(title="Number of Recorded cases"))  + 
        geom_label(size=3,aes(label = mydata$disease, x = disease_data$long, y = disease_data$lat)) +
        theme(legend.position = 'left',
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    }, height = 600, width = 800 )
    })
    #Chronic_Illness
    observeEvent(input$action,{output$plot4 = renderPlot({
      disease_data = x()
      # inFile <- input$file1
      # 
      # if (is.null(inFile))
      #   return(NULL)
      # disease_data = read.csv(inFile$datapath, header = TRUE)
      ##create (or input) data to plot on map
      states.shp <- readShapeSpatial("IND_adm1.shp")
      num.states<-length(states.shp$NAME_1)
      mydata<-data.frame(NAME_1=states.shp$NAME_1, id=states.shp$ID_1, disease=disease_data$Chronic_Illness)
      
      #fortify shape file to get into dataframe 
      states.shp.f <- fortify(states.shp, region = "ID_1")
      
      #merge with coefficients and reorder
      merge.shp.coef<-merge(states.shp.f, mydata, by="id", all.x=TRUE)
      final.plot<-merge.shp.coef[order(merge.shp.coef$order), ]
      disease_data = as.data.frame(disease_data)
      
      ggplot() +
        geom_polygon(data = final.plot, 
                     aes(x = long, y = lat, group = group, fill = disease), 
                     color = "white", size = 0.25) + 
        coord_map() +
        guides(fill=guide_legend(title="Number of Recorded cases"))  + 
        geom_label(size=3,aes(label = mydata$disease, x = disease_data$long, y = disease_data$lat)) +
        theme(legend.position = 'left',
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    }, height = 600, width = 800 )
    })
    #Diarrhoea
    observeEvent(input$action,{output$plot5 = renderPlot({
      disease_data = x()
      # inFile <- input$file1
      # 
      # if (is.null(inFile))
      #   return(NULL)
      # disease_data = read.csv(inFile$datapath, header = TRUE)
      ##create (or input) data to plot on map
      states.shp <- readShapeSpatial("IND_adm1.shp")
      num.states<-length(states.shp$NAME_1)
      mydata<-data.frame(NAME_1=states.shp$NAME_1, id=states.shp$ID_1, disease=disease_data$Diarrhoea)
      
      #fortify shape file to get into dataframe 
      states.shp.f <- fortify(states.shp, region = "ID_1")
      
      #merge with coefficients and reorder
      merge.shp.coef<-merge(states.shp.f, mydata, by="id", all.x=TRUE)
      final.plot<-merge.shp.coef[order(merge.shp.coef$order), ]
      disease_data = as.data.frame(disease_data)
      
      ggplot() +
        geom_polygon(data = final.plot, 
                     aes(x = long, y = lat, group = group, fill = disease), 
                     color = "white", size = 0.25) + 
        coord_map() +
        guides(fill=guide_legend(title="Number of Recorded cases"))  + 
        geom_label(size=3,aes(label = mydata$disease, x = disease_data$long, y = disease_data$lat)) +
        theme(legend.position = 'left',
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    }, height = 600, width = 800)
    })
    #Respiratory
    observeEvent(input$action,{output$plot6 = renderPlot({
      disease_data = x()
      # inFile <- input$file1
      # 
      # if (is.null(inFile))
      #   return(NULL)
      # disease_data = read.csv(inFile$datapath, header = TRUE)
      ##create (or input) data to plot on map
      states.shp <- readShapeSpatial("IND_adm1.shp")
      num.states<-length(states.shp$NAME_1)
      mydata<-data.frame(NAME_1=states.shp$NAME_1, id=states.shp$ID_1, disease=disease_data$Respiratory)
      
      #fortify shape file to get into dataframe 
      states.shp.f <- fortify(states.shp, region = "ID_1")
      
      #merge with coefficients and reorder
      merge.shp.coef<-merge(states.shp.f, mydata, by="id", all.x=TRUE)
      final.plot<-merge.shp.coef[order(merge.shp.coef$order), ]
      disease_data = as.data.frame(disease_data)
      
      ggplot() +
        geom_polygon(data = final.plot, 
                     aes(x = long, y = lat, group = group, fill = disease), 
                     color = "white", size = 0.25) + 
        coord_map() +
        guides(fill=guide_legend(title="Number of Recorded cases"))  + 
        geom_label(size=3,aes(label = mydata$disease, x = disease_data$long, y = disease_data$lat)) +
        theme(legend.position = 'left',
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    }, height = 600, width = 800)
    })
    #Fever
    observeEvent(input$action,{output$plot7 = renderPlot({
      disease_data = x()
      # inFile <- input$file1
      # 
      # if (is.null(inFile))
      #   return(NULL)
      # disease_data = read.csv(inFile$datapath, header = TRUE)
      ##create (or input) data to plot on map
      states.shp <- readShapeSpatial("IND_adm1.shp")
      num.states<-length(states.shp$NAME_1)
      mydata<-data.frame(NAME_1=states.shp$NAME_1, id=states.shp$ID_1, disease=disease_data$Fever)
      
      #fortify shape file to get into dataframe 
      states.shp.f <- fortify(states.shp, region = "ID_1")
      
      #merge with coefficients and reorder
      merge.shp.coef<-merge(states.shp.f, mydata, by="id", all.x=TRUE)
      final.plot<-merge.shp.coef[order(merge.shp.coef$order), ]
      disease_data = as.data.frame(disease_data)
      
      ggplot() +
        geom_polygon(data = final.plot, 
                     aes(x = long, y = lat, group = group, fill = disease), 
                     color = "white", size = 0.25) + 
        coord_map() +
        guides(fill=guide_legend(title="Number of Recorded cases"))  + 
        geom_label(size=3,aes(label = mydata$disease, x = disease_data$long, y = disease_data$lat)) +
        theme(legend.position = 'left',
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    }, height = 600, width = 800)
    })
    #Tuberculosis
    observeEvent(input$action,{output$plot8 = renderPlot({
      disease_data = x()
      # inFile <- input$file1
      # 
      # if (is.null(inFile))
      #   return(NULL)
      # disease_data = read.csv(inFile$datapath, header = TRUE)
      ##create (or input) data to plot on map
      states.shp <- readShapeSpatial("IND_adm1.shp")
      num.states<-length(states.shp$NAME_1)
      mydata<-data.frame(NAME_1=states.shp$NAME_1, id=states.shp$ID_1, disease=disease_data$Tuberculosis)
      
      #fortify shape file to get into dataframe 
      states.shp.f <- fortify(states.shp, region = "ID_1")
      
      #merge with coefficients and reorder
      merge.shp.coef<-merge(states.shp.f, mydata, by="id", all.x=TRUE)
      final.plot<-merge.shp.coef[order(merge.shp.coef$order), ]
      disease_data = as.data.frame(disease_data)
      
      ggplot() +
        geom_polygon(data = final.plot, 
                     aes(x = long, y = lat, group = group, fill = disease), 
                     color = "white", size = 0.25) + 
        coord_map() +
        guides(fill=guide_legend(title="Number of Recorded cases"))  + 
        geom_label(size=3,aes(label = mydata$disease, x = disease_data$long, y = disease_data$lat)) +
        theme(legend.position = 'left',
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    }, height = 600, width = 800)
    })
  })