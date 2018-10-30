if (!require("shiny")) install.packages("shiny")
if (!require("dplyr")) install.packages("dplyr")
if (!require("shinyShortcut")) install.packages("shinyShortcut")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("glmnet")) install.packages("glmnet")
if (!require("reshape2")) install.packages("reshape2")
if (!require("caret")) install.packages("caret")
if (!require("randomForest")) install.packages("randomForest")
if (!require("tree")) install.packages("tree")
if (!require("boot")) install.packages("boot")
if (!require("corrplot")) install.packages("corrplot")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("rpart")) install.packages("rpart")
if (!require("rpart.plot")) install.packages("rpart.plot")
if (!require("gridExtra")) install.packages("gridExtra")


#setwd("C:/PIMA")
shinyApp(
  ui = tagList(
    shinythemes::themeSelector(),
    navbarPage(
      "PIMA Diabetes Analysis",
      tabPanel("About PIMA",
       
        sidebarPanel(
          h4("Prediction of Diabetes in PIMA Women "),
          br(),
          h5("Add the PIMA dataset in the next tab and Analyze the dataset"),width = 2
        ),
        mainPanel(
          h4("Background of study"),
          p("Diabetes mellitus is a group of metabolic disorders where the blood sugar levels are higher than normal for prolonged periods of time [1]. Diabetes is caused either due to the insufficient production of insulin in the body or due to improper response of the body's cells to Insulin. The former cause of Diabetes is also called Type 1 DM or Insulin-dependent Diabetes mellitus and the latter is known as Type 2 DM or Non-Insulin Dependent DM. Gestational Diabetes is a third type of Diabetes where women not suffering from DM develop high sugar levels during pregnancy. In the United States, 30.3 million Americans were recorded as suffering from Diabetes with 1.5 million being diagnosed with Diabetes every year. Total cost of diagnosed Diabetes in the US in 2017 was $327 billion [2]. Diabetes is especially hard on women as it can affect both the mother and their unborn children during pregnancy. Women with Diabetes have a higher likelihood at having a heart attack, miscarriages or babies born with birth defects [3]."),
          h4("Motivation and Goal of study"),
          p("Due to increasing incidence rate of diabetes and prediabetes, it is a pressing issue in the health care industry to rightly identify the factors that contribute to the occurrence of Diabetes in people, more so, in Women. From secondary research, factors such as BMI, Blood Pressure, Cholesterol and Glucose levels are important factors that cause Diabetes. In Women, Pregnancy seems to be an additional factor. According to the World Health Organization, people with 2-hour post-load plasma glucose levels at least 200 mg/dl (11.1 mmol/l) at any survey examination were diagnosed with Diabetes [5]. To validate the above hypotheses, identify additional risk factors and build tools that can predict the occurrence of Diabetes, particularly in women, the Pima Indians' Diabetes dataset was chosen."),
          h4("About the Dataset"),
          p("The diabetes data containing information about PIMA Indian females, near Phoenix, Arizona has been under continuous study since 1965 due to the high incidence rate of Diabetes in PIMA females. The dataset was originally published by the National Institute of Diabetes and Digestive and Kidney Diseases, consisting of diagnostic measurements pertaining to females of age greater than 20. It contains information of 768 females, of which 268 females were diagnosed with Diabetes. Information available includes 8 variables, such as, Age, Number of Pregnancies, Glucose, Insulin, etc. More detailed description about the variables is listed in the table below. The response variable in the dataset is a binary classifier, Outcome, that indicates if the person was diagnosed with Diabetes or not."),
          width = 10
        )
      ),
      tabPanel("Data Exploration",
               sidebarPanel(
                 fileInput("file1", "Choose the data file",
                           multiple = FALSE),
                 actionButton("action", "ANALYZE"),
                 width = 2
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("PIMA Data",h4("   "),dataTableOutput("table1")),
                   tabPanel("Response Variable",plotOutput("plot1")),
                   tabPanel("Predictor Variables",h4("  "),plotOutput("plot2"),plotOutput("plot2a")),
                   tabPanel("Bivariate Associations",plotOutput("plot3")),
                   tabPanel("Correlations between Predictor variables",plotOutput("plot4"))
                  ),
                 width = 10
               )
      ),
      tabPanel("Data Modelling",
               sidebarPanel(
                 h5("Click on each tab to see the results"),width = 2
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Logistic Regression",h4("Logistic Regression"),p(" "),verbatimTextOutput("plot5")),
                   tabPanel("Logistic Regression - Parameter Tuning",h4("Logistic Regression - Parameter Tuning"),p(" "),plotOutput("plot6")),
                   tabPanel("Classification Tree",h4("Classification Tree"),p(" "),plotOutput("plot7")),
                   tabPanel("Classification Tree - Parameter Tuning",h4("Classification Tree - Parameter Tuning"),p(" "),plotOutput("plot8"))
                 ),
                 width = 10
               )
      ),
      tabPanel("Model Performance Evaluation",
               sidebarPanel(
                 h5("Click on each tab to compare the results"),width = 2
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Logistic Regression",h4(" Logistic Regression Performance"),p(" "),verbatimTextOutput("plot9")),
                   tabPanel("Logit Model",h4("Prediction"),p(" "),plotOutput("plot10")),
                   tabPanel("Classification Tree",h4("Classification Tree Performance"),p(" "),plotOutput("plot11")),
                   tabPanel("CART Model",h4("Confusion Matrix and Statistics"),p(" "),plotOutput("plot12"))
                 ),
                 width = 10
               )
      ),
      tabPanel("Conclusion",
               sidebarPanel(h5(" "),width = 2),
               mainPanel(h5("Conclusion"),p("he PIMA Indian Women's Database was analyzed and explored in detail. The patterns identified using Data exploration methods were validated using the modeling techniques employed. Classification models such as Logistic Regression, Classification Trees, Random Forest and SVM were built and evaluated to identify best model to predict the occurrence of Diabetes in PIMA Indian women. From the cross-validated performance measure of sensitivity, the Logistic Regression model was concluded as the best performing model."),width = 9)
        ,width = 10
      )
    )
  ),
  server = function(input, output,session) {
    x = reactive({
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      data = read.csv(inFile$datapath, header = TRUE)
      return(data)
    })
    observeEvent(input$action,{output$table1 <- renderDataTable({
      data = x()

    })})
    
    #Response variable
    observeEvent(input$action,{output$plot1 = renderPlot({

      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      data = read.csv(inFile$datapath, header = TRUE)
      
      data$Outcome <- factor(data$Outcome)
      
      # 1. Outcome
      ggplot(data,aes(Outcome,fill = Outcome)) +
        geom_bar() + 
        ggtitle("\n\nDistribution of Outcome variable\n")
    },height = 900)
    })
    observeEvent(input$action,{output$plot2 = renderPlot({
      
      #  Pregnancies
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      data = read.csv(inFile$datapath, header = TRUE)
      p1 <- ggplot(data, aes(x = Outcome, y = Pregnancies,fill = Outcome)) +
        geom_boxplot() +
        theme(legend.position = "bottom") +
        ggtitle("Number of pregnancies Vs Diabetes")
      
      p2 <- ggplot(data,aes(x = Pregnancies,fill = factor(Outcome))) + 
        geom_bar(position = "Dodge") + 
        scale_x_continuous(limits = c(0,16)) +
        theme(legend.position = "bottom") +
        labs(title = "Pregnancies Vs Outcome")
      
      # 2. Glucose
      p3 <- ggplot(data, aes(x = Glucose, color = Outcome, fill = Outcome)) +
        geom_density(alpha = 0.8) +
        theme(legend.position = "bottom") +
        labs(x = "Glucose", y = "Density", title = "Density plot of glucose")
      
      p4 <- ggplot(data, aes(x = Outcome, y = Glucose,fill = Outcome)) +
        geom_boxplot() +
        theme(legend.position = "bottom") +
        ggtitle("Variation of glucose in women Vs Diabetes")
      
      # 3. Blood Pressure
      p5 <- ggplot(data, aes(x = BloodPressure, color = Outcome, fill = Outcome)) +
        geom_density(alpha = 0.8) +
        theme(legend.position = "bottom") +
        labs(x = "Blood pressure", y = "Density", title = "Density plot of Blood pressure")
      
      p6 <- ggplot(data, aes(x = Outcome, y = BloodPressure,fill = Outcome)) +
        geom_boxplot() +
        theme(legend.position = "bottom") +
        ggtitle("Variation of blood pressure in women Vs Diabetes")
      
      # 4. Skin Thickness
      p7 <- ggplot(data, aes(x = SkinThickness, color = Outcome, fill = Outcome)) +
        geom_density(alpha = 0.8) +
        theme(legend.position = "bottom") +
        labs(x = "Skin thickness", y = "Density", title = "Density plot of skin thickness")
      
      p8 <- ggplot(data, aes(x = Outcome, y = SkinThickness,fill = Outcome)) +
        geom_boxplot() +
        theme(legend.position = "bottom") +
        ggtitle("Variation of skin thickness Vs Diabetes")
      
      # 5. Insulin
      p9 <- ggplot(data, aes(x = Outcome, y = Insulin,fill = Outcome)) +
        geom_boxplot() + 
        theme(legend.position = "bottom") +
        ggtitle("Variation of Insulin content Vs Diabetes")
      
      p10 <- ggplot(data, aes(Insulin, fill = Outcome)) +
        geom_histogram(binwidth=10) +
        theme(legend.position = "bottom") +
        ggtitle("Variation of Insulin content Vs Diabetes")
      
      # 6. BMI
      p11 <- ggplot(data, aes(BMI, fill = Outcome)) +
        geom_histogram() +
        theme(legend.position = "bottom") +
        ggtitle("Variation of BMI of women Vs Diabetes")
      
      p12 <- ggplot(data, aes(x = Outcome, y = BMI,fill = Outcome)) +
        geom_boxplot(binwidth = 5) +
        theme(legend.position = "bottom") +
        ggtitle("Variation of BMI of women Vs Diabetes")
      
      # 7. DPF
      p13 <- ggplot(data, aes(x = Outcome, y = DiabetesPedigreeFunction,fill = Outcome)) +
        geom_boxplot() +
        theme(legend.position = "bottom") +
        ggtitle("Variation of DPF of women Vs Diabetes")
      
      
      p14 <- ggplot(data, aes(DiabetesPedigreeFunction,fill = Outcome)) +
        geom_histogram() +
        theme(legend.position = "bottom") +
        ggtitle("Variation of DPF Vs Diabetes")
      
      # 8. Age
      p15 <- ggplot(data, aes(Age, fill = Outcome)) +
        geom_histogram(binwidth = 5) +
        theme(legend.position = "bottom") +
        ggtitle("Variation of Age of women Vs Diabetes")
      
      p16 <- ggplot(data, aes(x = Outcome, y = Age,fill = Outcome)) +
        geom_boxplot() +
        theme(legend.position = "bottom") +
        ggtitle("Variation of Age of women Vs Diabetes")
      
        gridExtra::grid.arrange(p1, p2,p3, p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16, ncol = 4, nrow = 4)
      }, height = 1000)
    })
    observeEvent(input$action,{output$plot3 = renderPlot({
      data = x()
      p1 <- ggplot(data, aes(x = Age, y = Pregnancies)) +
        geom_point(aes(color=Outcome)) + 
        theme(legend.position = "bottom") +
        ggtitle("Relationship of Pregnancies with Age Vs Diabetes")
      
      p2 <- ggplot(data,aes(x=Insulin,y=Glucose))+
        geom_point(aes(color=Outcome))+
        theme(legend.position = "bottom") +
        ggtitle("Relationship of Insulin with Glucose Vs Diabetes")
      
      gridExtra::grid.arrange(p1, p2, ncol = 2)
      
    })
    })
    observeEvent(input$action,{output$plot4 = renderPlot({
      data = x()
      corMat = cor (data[, -9])
      diag (corMat) = 0 #Remove self correlations
      corrplot.mixed(corMat,tl.pos = "lt")
    })
    })
    #Logistic Regression
    observeEvent(input$action,{output$plot5 = renderPrint({
      data = x()
      # Create training and testing set
      set.seed(12345)
      ratio = sample(1:nrow(data), size = 0.20*nrow(data))
      test.data = data[ratio,] #Test dataset 20% of total
      train.data = data[-ratio,] #Train dataset 80% of total
      
      model.glm <- glm(Outcome~.,data=train.data,family = binomial)
      step_model <- step(model.glm)
      
      # Final model using Logistic Regression
      model.new <- glm(formula = Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction, family = binomial, data = train.data)
      
      summary(model.new)
      
      return(train.data)
    })
    })
    #  Logistic Regression - Parameter Tuning
    observeEvent(input$action,{output$plot6 = renderPlot({
      data = x()
      
      set.seed(12345)
      ratio = sample(1:nrow(data), size = 0.20*nrow(data))
      test.data = data[ratio,] #Test dataset 20% of total
      train.data = data[-ratio,] #Train dataset 80% of total
      
      model.glm <- glm(Outcome~.,data=train.data,family = binomial)
      step_model <- step(model.glm)
      
      # Final model using Logistic Regression
      model.new <- glm(formula = Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction, family = binomial, data = train.data)
      
      # CV to choose cut-off probability
      searchgrid = seq(0.01, 0.6, 0.02)
      result = cbind(searchgrid, NA)
      cost1 <- function(r, pi) {
        weight1 = 2
        weight0 = 1
        c1 = (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0 (False Negative)
        c0 = (r == 0) & (pi > pcut)  #logical vector - true if actual 0 but predict 1 (False Positive)
        return(mean(weight1 * c1 + weight0 * c0))
      }
      for (i in 1:length(searchgrid)) {
        pcut <- result[i, 1]
        result[i, 2] <- cv.glm(data = train.data, glmfit = model.new, cost = cost1, 
                               K = 3)$delta[2]
      }
      plot(result, ylab = "CV Cost",main = "Optimal cut-off probability identification")
      
    })
    })
    #Classification Tree
    observeEvent(input$action,{output$plot7 = renderPlot({
      data = x()
      
      set.seed(12345)
      ratio = sample(1:nrow(data), size = 0.20*nrow(data))
      test.data = data[ratio,] #Test dataset 20% of total
      train.data = data[-ratio,] #Train dataset 80% of total
      
      tree.model <- rpart(Outcome~., data=train.data, method="class")
      rpart.plot(tree.model)
      
    })
    })
    #Classification Tree - Parameter Tuning
    observeEvent(input$action,{output$plot8 = renderPlot({
      data = x()
      set.seed(12345)
      ratio = sample(1:nrow(data), size = 0.20*nrow(data))
      test.data = data[ratio,] #Test dataset 20% of total
      train.data = data[-ratio,] #Train dataset 80% of total
      
      tree.model <- rpart(Outcome~., data=train.data, method="class")
      plotcp(tree.model)
      # Optimal cut-off value
      # Pruning the tree
      tree.model<- rpart(Outcome~., data=train.data, method="class",cp=0.015)
      rpart.plot(tree.model)
    })
  })
})