library(shiny)
library(shinyShortcut)
library(shinythemes)
library(qdapRegex)
library(dplyr)
library(reshape2)
library(sqldf)
library(readxl)
library(sentiment)
library(stringr)
library(textstem)
library(ggplot2)
library(dygraphs)
library(networkD3)
shinyApp(
  ui = tagList(
    shinythemes::themeSelector(),
    navbarPage(
      
      "Sentiment Analyser",
      tabPanel("Main Analysis",
               sidebarPanel(
                 fileInput("file1", "Choose the Excel data file",
                           multiple = FALSE),
                 actionButton("action", "Energise!")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Sentiments",h4("Polarities"),dataTableOutput("table1")),
                   tabPanel("Sentiments Overall View",h4("Sentiment Plot"),plotOutput("plot1")),
                   tabPanel("Sentiment Trend over Time",h4("Sentiment Trend"),dygraphOutput("plot2")),
                   tabPanel("Influencer Analysis",h4("Influential users"),simpleNetworkOutput("plot3"))
                 )
               )
      )
    )
  ),
  server = function(input, output,session) {
    x = reactive({
      d1 = read_excel(input$file1$datapath,sheet=input$file1$sheetName)
      d1$reviewText <- gsub("http.*","",  d1$reviewText)
      d1$reviewText = gsub("@\\w+ *", "", d1$reviewText)
      d1$reviewText = gsub("<\\w+ *", "", d1$reviewText)
      d1$reviewText =  gsub("[[:punct:]]","", d1$reviewText)
      tweets = d1$reviewText
      tweets = as.data.frame(tweets)
      r1 = tweets
      r1$CleanText <- ""
      
      
      # Text preprocessing function
      Clean_String <- function(string){
        #symbol replace
        temp = str_replace_all(string, "[^[:alnum:]]", " ")
        # Lowercase
        temp <- tolower(string)
        # Remove everything that is not a number or letter 
        temp <- str_replace_all(temp,"[^a-zA-Z\\s]", " ")
        # Remove stopwords
        temp <- removeWords(temp, stopwords('en'))
        # Shrink down to just one white space
        temp <- str_replace_all(temp,"[\\s]+", " ")
        # Split the string into words
        #temp <- str_split(temp, " ")[[1]]
        temp <- stem_words(temp)
        # Get rid of trailing "" if necessary
        indexes <- which(temp == "")
        if(length(indexes) > 0){temp <- temp[-indexes]}
        # Get unique words
        return(paste(unique(temp),collapse = ' '))
      }  
      
      #Clean all the texts row-wise
      
      for(i in 1:NROW(r1))
      {
        r1$CleanText[i] <- Clean_String(r1$text[i])
      }  
      textdata = r1[c("tweets")]
      sentiment_scores = classify_polarity(r1)
      Sentiment = as.data.frame(sentiment_scores[,3:4])
      final_result = cbind(d1,Sentiment)
    })
    observeEvent(input$action,{output$table1 <- renderDataTable({
      final_result=x()
      return(final_result)
    })})
    observeEvent(input$action,{output$plot1 = renderPlot({
      final_result=x()
      ggplot(final_result, aes(BEST_FIT, fill = BEST_FIT ) ) + geom_bar()
    })})
    observeEvent(input$action,{output$plot2 = renderDygraph({
      final_result=x()
      final_result$Months = month(final_result$reviewTime)
      final_result$Years = year(final_result$reviewTime)
      fr_agg = data.frame(cbind(final_result$Years,final_result$Months,as.character(final_result$BEST_FIT)))
      colnames(fr_agg)=c("Years","Months","BEST_FIT")
      fr_agg = data.frame(dcast(fr_agg,(Years+Months ~ BEST_FIT)))
      neutral_ts = ts(as.numeric(as.character(fr_agg$neutral)),start=min(final_result$Years),frequency = 12)
      positive_ts = ts(as.numeric(as.character(fr_agg$positive)),start=min(final_result$Years),frequency = 12)
      negative_ts = ts(as.numeric(as.character(fr_agg$negative)),start=min(final_result$Years),frequency = 12)
      sentiment_trend = cbind(positive_ts,negative_ts,neutral_ts)
      dygraph(sentiment_trend, main = "Sentiment Trend over Time") %>% 
        dySeries("positive_ts",label = "Positive") %>%
        dySeries("negative_ts",label = "Negative") %>%
        dySeries("neutral_ts",label = "Neutral") %>%
        dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
        dyOptions(fillGraph = TRUE, fillAlpha = 0.4)%>%
        dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))%>% dyRangeSelector()
      
    })})
    observeEvent(input$action,{output$plot3 = renderSimpleNetwork({
      d1=x()
      d1$helpful_positive=as.numeric(rm_between(d1$helpful,"[",",",extract = TRUE))
      d1$total_views=as.numeric(rm_between(d1$helpful,", ","]",extract = TRUE))
      d1$overall_helpful=d1$helpful_positive / d1$total_views
      d1$overall_helpful = replace(d1$overall_helpful, is.na(d1$overall_helpful), 0)
      book_summary = data.frame(d1%>%group_by(asin)%>%summarise(helpfulness=mean(overall_helpful)))
      main_inf = data.frame(sqldf("select * from book_summary where helpfulness>0.75"))
      final = merge.data.frame(d1,main_inf,by="asin",all.y = TRUE)
      final = final[order(final$total_views,decreasing = TRUE),]
      final = final[,-13]
      final = final[1:20,]
      Source = data.frame(cbind(final$reviewerID,final$asin,final$overall_helpful))
      colnames(Source) = c("reviewerID","asin","overall_helpful")
      simpleNetwork(Source,Source = "reviewerID",Target = "asin",width = 720, height = 720)
    })})
})