library(stats)
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plotly))
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(DT)
library(DBI)
library("writexl") #to export dataframe as excel file

library(reshape2)
library(visdat)
library(funModeling)
suppressPackageStartupMessages(library(dplyr))
library(shinyWidgets)
library(corrplot)
library('shinyalert')
library(clusterSim)
library("readxl")
library(shinyjs)
library(stringr)
library(dbplyr)
library(htmltools)
library(ggplot2)
library(scales)
library(gridExtra)
library(shinyscreenshot)
library(ggcorrplot)
library("caTools")
library(Gmisc)
library(funModeling) 
library(tidyverse) 
library(caret)
library(e1071 )
library(rpart)
library(rattle )
#library(Hmisc)

options(shiny.maxRequestSize=300*1024^2)
server<-function(input,output,session){
  
  credit_fraud_data <-NULL
  credit_fraud_data_initial <-NULL
  credit_fraud_data <-NULL
  employee_attrition_data <-NULL
  bank_marketing_data <-NULL
  generalErrorMessage <- NULL
  credit_fraud_num_columns_list <- c()
  credit_fraud_categorial_columns_list <- c()
  bank_marketing_num_columns_list <- c()
  bank_marketing_categorial_columns_list <- c()
  employee_attrition_num_columns_list <- c()
  employee_attrition_categorial_columns_list <- c()
  #Déséquilibre
  balance_level = NULL
  ajusted_data <- NULL
  occ0 = 0
  occ1 = 0
  difference = 0
  balance_value = 0
  sliderValue = 0
  train_set <- NULL
  test_set <- NULL
  targetValues <- NULL
  errorMessage <- NULL
  
  #function to fill a choosed select
  fillSelect <- function(selectName, listValues, placeHolderMessage) {
    if (!is.null(selectName) && selectName != "") {
      if (!is.null(listValues) && length(listValues) > 0) {
        updateSelectizeInput(session, selectName, choices=sort(unique(listValues)), 
                             selected=listValues[[1]], options = list(placeholder=placeHolderMessage)
        )
      }else{
        #shinyalert("Oops!", "Les données à passer au select sont vides!", type = "error")
      }
    }else{
      #shinyalert("Oops!", "Veuillez renseigner le nom du select!", type = "error")
    }
    #return(dataFrame)
  }
  
  #get data uploaded from credit_fraud file
  data_credit_fraud<- eventReactive(input$input_credit_fraud, {
    #Read uploaded dataset
    inFile <- input$input_credit_fraud
    if (is.null(inFile)) return(NULL)
    
    if(stringr::str_ends(input$input_credit_fraud$datapath, "csv")) {
      credit_fraud_data <<- read.csv(input$input_credit_fraud$datapath,header = TRUE, sep=",", stringsAsFactors = FALSE, na.strings = c("","NA"))
      if (is.null(credit_fraud_data) || length(colnames(credit_fraud_data)) == 1) {
        credit_fraud_data <<- read.csv(input$input_credit_fraud$datapath,header = TRUE, sep=";", stringsAsFactors = FALSE, na.strings = c("","NA"))
      }
      if (is.null(credit_fraud_data)) {
        credit_fraud_data <<- read.csv(input$input_credit_fraud$datapath,header = TRUE, sep=";", stringsAsFactors = FALSE, na.strings = c("","NA"))
      }
    } else if (stringr::str_ends(input$input_credit_fraud$datapath, "(xlsx|xls)")) {
      sheet1 <- readxl::read_excel(input$input_credit_fraud$datapath, 1 , na = c("N/A", "n/a"))
      #sheet2 <- readxl::read_excel(input$input_credit_fraud$datapath, 2, na = c("N/A", "n/a"))
      credit_fraud_data <<- data.frame(sheet1, stringsAsFactors = FALSE)
    }
    credit_fraud_data_initial <<- credit_fraud_data
    
    if (!is.null(credit_fraud_data)) {
      #fillSelect("outcome", colnames(credit_fraud_data), "Choisir")
      #fillSelect("variables", colnames(credit_fraud_data), "Choisir")
      fillSelect("columns_select_target", colnames(credit_fraud_data), "Choisir target")
      fillSelect("columns_select_list", colnames(credit_fraud_data), "Sélectionner")
      #Générer une liste afin que l'utilisateur puisse choisir les variables 
      observe({
        v<-array(names(credit_fraud_data))
        updateSelectInput(
          session, 
          "variables", 
          choices = v ,
          selected = v[[1]]
        )
      })
      
      #Générer une liste afin que l'utilisateur puisse choisir les predicteurs 
      observe({
        data = credit_fraud_data
        n = c()
        for ( i in 1:ncol(credit_fraud_data)){
          if(is.integer(data[,i]) || is.numeric(data[,i])){
            n = c(n,i)
          }
        }
        data = data[,n]
        v<-array(names(data))
        updateSelectInput(
          session, 
          "outcome", 
          choices = v ,
          selected = v[1]
        )
      })
      
      output$df_status <- renderPrint({
        df_status(credit_fraud_data)
      })
      
      output$df_freq<-renderPlot({
        freq(credit_fraud_data)
      })
      
      output$df_plot_num<-renderPlot({
        plot_num(credit_fraud_data)
      })
    }
    credit_fraud_data 
  })
  
  output$table_credit_fraud<-DT::renderDataTable({
    tmp.dat <- data_credit_fraud()
    DT::datatable(
      tmp.dat, extensions = 'Buttons',
      options = list(dom = 'Blfrtip', scrollX = TRUE, buttons = 
       list(list(
          extend = "collection",
          text = 'Show All',
          action = DT::JS("function ( e, dt, node, config ) {
          dt.page.len(-1);
          dt.ajax.reload();
        }")
     ),
     'copy', 'csv', list(
       extend = 'excel',
       filename = 'fichier de chargement credit_fraud',
       title = NULL,
       pageLength = 15,
       exportOptions = list(columns = c(1:length(credit_fraud_data)))
     ),list(extend = 'colvis', columns = c(1:length(colnames(credit_fraud_data)))))),filter='top'
    )
  })
  
  #permet de vérifier si on a choisi au moins deux prédicteurs
  Ok <-  eventReactive(input$demarrage,{
    if(length(input$variables)<2){
      shinyalert("Oops!", "On a besoin de plus deux predicteurs pour que tout fonctionne.", type = "error")
      return (FALSE)
    }else{
      return(TRUE)
    }
  })
  
  checkParams <- function(){
    if(length(input$variables)<2){
      shinyalert("Oops!", "On a besoin de plus deux predicteur pour que tout fonctionne.", type = "error")
      return (FALSE)
    }else{
      return(TRUE)
    }
  }
  
  
  makeReactiveBinding("Slider1")
  
  observeEvent(input$Slider1, {
    sliderValue <- input$Slider1
    if (sliderValue != 0 && !is.null(credit_fraud_data)) {
      output$cntTest <- renderText({
        paste("% Train :", sliderValue)
      })
      
      output$cntTrain <- renderText({
        paste("% Test :", 100 - sliderValue)
      })
      # if(!is.null(credit_fraud_data)){
      #   set.seed(123)
      #   splitSlider = sliderValue / 100
      #   split = sample.split(credit_fraud_data, SplitRatio = splitSlider)
      #   train_set <<- subset(credit_fraud_data, split == TRUE)
      #   print(nrow(train_set))
      #   
      #   test_set <<- subset(credit_fraud_data, split == FALSE)
      #   print(nrow(test_set))
      # }
    }
    
  })
  
  observeEvent(input$demarrage, {
    if (!is.null(credit_fraud_data)) {
      if(input$Slider1 != 0){
        set.seed(123)
        splitSlider = sliderValue / 100
        split = sample.split(credit_fraud_data, SplitRatio = splitSlider)
        train_set <<- subset(credit_fraud_data, split == TRUE)
        test_set <<- subset(credit_fraud_data, split == FALSE)
      }else{
        shinyalert("Veuillez régler la taille du train et du test set", "Attention veuillez configurer les paramètres de train et de test !", "warning")
      }
    }else{
      shinyalert("Veuillez charger un jeu de données", "Attention veuillez charger votre jeu données !", "error")
    }
  })
  
  #####################Logistic regression###################
  
  lg <- eventReactive(input$demarrage,{
    ok = checkParams()
    if(ok==TRUE){
      parameter1 = paste(input$outcome ,"~")
      
      parameter1 = paste(parameter1 , input$variables[1])
      
      for (i in 1:length(input$variables)){
        parameter1 = paste(parameter1,"+" , input$variables[i])
      }
      print(parameter1)
      parameter3 = "binomial"
      parameter <- list(formula= parameter1, data= train_set, family = parameter3, na.action=na.omit)
      shinyalert("Modèle réussi", "Entrainement et test réussis !", "success")
      fastDoCall(glm, parameter, quote = FALSE)
    }else {
      return(NULL)
    }
  })
  
  output$logistic_regression <- renderPrint({
    ok = Ok()
    if(ok==TRUE){
      summary(lg())
    } 
  })
  ################################Matrice de confusion###########################################
  output$tablelg <- renderPrint({
    ok = Ok()
    if(ok==TRUE){
      y <- factor(as.integer(test_set[,input$outcome]),levels=0:1)
      pred <- predict(lg(),test_set, type = "response")
      pred1 <- integer(length =length(pred))
      pred1[pred > 0.5] <- 1
      pred1 <- as.factor(pred1) 
      confusionMatrix(pred1,y)
    }
    
    
  })
  ################################### Score de Prediction ############################"
 
   output$scorelg <- renderPrint({
    ok = Ok()
    if(ok==TRUE){
      y <- factor(as.integer(test_set[,input$outcome]),levels=0:1)
      pred <- predict(lg(), test_set, type = "response")
      pred1 <- integer(length =length(pred))
      pred1[pred > 0.5] <-1
      pred1 <- as.factor(pred1) 
      table = table(pred1,y)
      precision <- posPredValue(pred1, y, positive="1")
      recall <- sensitivity(pred1, y, positive="1")
      F1 <- (2 * precision * recall) / (precision + recall)
      sprintf("Precision: %s  |   Recall: %s    |     F-score: %s", precision, recall, F1)
    }})
   
   ############################## Courbe ROC###########################################
   
   output$lgRoc <- renderPlot({
     ok = Ok()
     if(ok==TRUE){
       y <- factor(as.integer(test_set[,input$outcome]),levels=0:1)
       pred <- predict(lg(), test_set ,type = "response")
       pred1 <- prediction(pred,y)
       perf <- performance(pred1,"tpr","fpr")
       plot(perf)
     }})
   
   
   ########################## AUC ###############################################
   output$auclg <- renderPrint({
     ok = Ok()
     if(ok==TRUE){
       y <- factor(as.integer(test_set[,input$outcome]),levels=0:1)
       pred <- predict(lg(), test_set,type = "response")
       pred1 <- prediction(pred,y)
       auc_ROCR <- performance(pred1, measure = "auc")
       auc_ROCR <- auc_ROCR@y.values[[1]]
       sprintf("AUC: %s", auc_ROCR)}
   })
   
       ###################### Arbre de Décision ########################################"
   Svm <- eventReactive(input$demarrage,{
     ok = Ok()
     
     if(ok==TRUE){
       svm(train_set[,input$variables],as.integer(train_set[,input$outcome]), data= train_set,
           kernel = 'linear',na.action=na.omit, scale=F)}
   })
   
   output$SVM  <- renderPrint({
     ok = Ok()
     if(ok==TRUE){
       svm = Svm()
       summary(svm)
     }
   })
   
   output$tablesvm <- renderPrint({
     ok = Ok()
     if(ok==TRUE){
       y <- factor(as.integer(test_set[,input$outcome]),levels=0:1)
       pred <- predict(Svm(), test_set[,input$variables], type = "response")
       pred1 <- integer(length =length(pred))
       pred1[pred > 0.5] <-1
       pred1 <- as.factor(pred1) 
       confusionMatrix(pred1,y)
     }
  })
   
   output$scoresvm <- renderPrint({
     ok = Ok()
     if(ok==TRUE){
       y <- factor(as.integer(test_set[,input$outcome]),levels=0:1)
       pred <- predict(Svm(), test_set[,input$variables], type = "response")
       pred1 <- integer(length =length(pred))
       pred1[pred > 0.5] <-1
       pred1 <- as.factor(pred1) 
       table = table(pred1,y)
       precision <- posPredValue(pred1, y, positive="1")
       recall <- sensitivity(pred1, y, positive="1")
       F1 <- (2 * precision * recall) / (precision + recall)
       sprintf("Precision: %s  |   Recall: %s    |     F-score: %s", precision, recall, F1)
     }
   })
   
   
   
   output$svmRoc <- renderPlot({
     ok = Ok()
     if(ok==TRUE){
       y <- factor(as.integer(test_set[,input$outcome]),levels=0:1)
       pred <- predict(Svm(), test_set[,input$variables],type = "response")
       pred1 <- prediction(pred,y)
       perf <- performance(pred1,"tpr","fpr")
       plot(perf)
     }
   })
   
   output$aucsvm <- renderPrint({
     ok = Ok()
     if(ok==TRUE){
       y <- factor(as.integer(test_set[,input$outcome]),levels=0:1)
       pred <- predict(Svm(), test_set[,input$variables],type = "response")
       pred1 <- prediction(pred,y)
       auc_ROCR <- performance(pred1, measure = "auc")
       auc_ROCR <- auc_ROCR@y.values[[1]]
       sprintf("AUC: %s", auc_ROCR) 
     }
   })
   
  ################################""" #Arbre de decision##############################################
   Ad <- eventReactive(input$demarrage,{
     ok = Ok()
     if(ok==TRUE){
       parameter1 = paste(input$outcome ,"~")
       parameter1 = paste(parameter1 , input$variables[1])
       for (i in 2:length(input$variables)){
         parameter1 = paste(parameter1,"+" , input$variables[i])
       }
       parameter <- list(formula= parameter1, data= train_set, na.action=na.omit)
       fastDoCall(rpart, parameter,quote = FALSE)
       }
   })
   
   output$AD <- renderPrint({
     ok = Ok()
     if(ok==TRUE){
       summary(Ad())
     }
   })
     
   
   output$tablead <- renderPrint({
     ok = Ok()
     if(ok==TRUE){
       y <- factor(as.integer(test_set[,input$outcome]),levels=0:1)
       ad=Ad()
       pred <- predict(ad,newdata=test_set[,input$variables],type='matrix',na.action=na.omit)
       pred1 <- integer(length =length(pred))
       pred1[pred > 0.5] <-1
       pred1 <- as.factor(pred1)
       confusionMatrix(pred1,y)}
   })
   
   
   output$ad <- renderPlot({
     ok = Ok()
     if(ok==TRUE){
       rpart_model= Ad()
       plot(rpart_model)
       text(rpart_model)
       fancyRpartPlot(rpart_model)}
   })
   
   
   
   output$scoread <- renderPrint({
     ok = Ok()
     if(ok==TRUE){
       y <- factor(as.integer(test_set[,input$outcome]),levels=0:1)
       pred <- predict(Ad(), test_set[,input$variables], type = "matrix")
       pred1 <- integer(length =length(pred))
       pred1[pred > 0.5] <-1
       pred1 <- as.factor(pred1) 
        precision <- posPredValue(pred1, y, positive="1")
       recall <- sensitivity(pred1, y, positive="1")
       F1 <- (2 * precision * recall) / (precision + recall)
       sprintf("Precision: %s  |   Recall: %s    |     F-score: %s", precision, recall, F1)
      }
   })
         
   
   

  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##################### Over Simpling / Under Simpling ###################
  
  #-----------------------------------------------#
  #--------------DEBUT TRAITEMENTS DATA A&R-------#
  #-----------------------------------------------#
  
  observeEvent(input$columns_select_target, {
    if (!is.null(credit_fraud_data)) {
      val <- unique(credit_fraud_data[c(input$columns_select_target)])
      if (nrow(val) > 2) {
        errorMessage <<- "Il y a trop de valeurs distinctes dans cette colonne"
      }else{
        if (nrow(val) == 2) {
          targetValues <<- c(val[1, input$columns_select_target], val[2, input$columns_select_target])
          errorMessage <<- NULL
        }else{
          errorMessage <<- "Cette colonne n'a pas assez de données"
        }
      }
    }
  })
  
  #Drop all NA values
  observeEvent(input$dropAll, {
    if (!is.null(credit_fraud_data)) {
      
      #get all not empty values
      credit_fraud_data <<- na.omit(credit_fraud_data)
      
      #sync data
      data_credit_fraud<- eventReactive(input$input_credit_fraud, {
        credit_fraud_data
      })
      
      #displaying on the screen
      output$table_credit_fraud<-DT::renderDataTable({
        tmp.dat <- data_credit_fraud()
        DT::datatable(
          tmp.dat, extensions = 'Buttons',
          options = list(dom = 'Blfrtip', scrollX = TRUE, buttons = 
           list(list(
              extend = "collection",
              text = 'Show All',
              action = DT::JS("function ( e, dt, node, config ) {
              dt.page.len(-1);
              dt.ajax.reload();
          }")
         ),
         'copy', 'csv', list(
           extend = 'excel',
           filename = 'fichier de chargement credit_fraud',
           title = NULL,
           pageLength = 15,
           exportOptions = list(columns = c(1:length(credit_fraud_data)))
         ),list(extend = 'colvis', columns = c(1:length(colnames(credit_fraud_data)))))),filter='top'
        )
      })
      shinyalert("Effectué avec succès !", "Vous pouvez voir les modifications dans l'onglet jeu de donées", type = "success")
    }else{
      shinyalert("Oops!", "Veuillez charger un dataset", type = "error")
    }
  })
  
  #Replace all NA values
  observeEvent(input$replaceAll, {
    if (!is.null(credit_fraud_data)) {
      
      #get all cells where empty values are
      tabNa<-which(is.na(credit_fraud_data),arr.ind=TRUE)
      
      if (length(tabNa) > 0) {
        
        #replace all other lines
        for (i in 1:dim(credit_fraud_data)[2]){
          if ( is.na(credit_fraud_data[1,i]))
          {
            j<-2
            while(is.na(credit_fraud_data[j,i])){
              j<-j+1
            }
            credit_fraud_data[1,i]<<-credit_fraud_data[j,i]
          }
        }
        
        #replace all other lines
        for (i in 1:(length(tabNa)/2)){
          ligne<-tabNa[i,1]
          colonne<-tabNa[i,2]
          if(ligne > 1){
            credit_fraud_data[ligne,colonne]<<-credit_fraud_data[ligne-1,colonne]
          }
        }
        
        #sync data
        data_credit_fraud<- eventReactive(input$input_credit_fraud, {
          credit_fraud_data
        })
        
        #displaying on the screen
        output$table_credit_fraud<-DT::renderDataTable({
          tmp.dat <- data_credit_fraud()
          DT::datatable(
            tmp.dat, extensions = 'Buttons',
            options = list(dom = 'Blfrtip', scrollX = TRUE, buttons = 
             list(list(
                extend = "collection",
                text = 'Show All',
                action = DT::JS("function ( e, dt, node, config ) {
                dt.page.len(-1);
                dt.ajax.reload();
            }")
           ),
           'copy', 'csv', list(
             extend = 'excel',
             filename = 'fichier de chargement credit_fraud',
             title = NULL,
             pageLength = 15,
             exportOptions = list(columns = c(1:length(credit_fraud_data)))
           ),list(extend = 'colvis', columns = c(1:length(colnames(credit_fraud_data)))))),filter='top'
          )
        })
        print(dim(credit_fraud_data))
        shinyalert("Effectué avec succès!", "Vous pouvez voir les modifications dans l'onglet jeu de données", type = "success")
      }else{
        shinyalert("Information!", "Il y a pas de valeurs manquantes dans ce jeu de données", type = "warning")
      }
      
    }else{
      shinyalert("Oops!", "Veuillez d'abord charger un dataset", type = "error")
    }
    
  })
  
  #reset dataset
  observeEvent(input$resetAll, {
    if (!is.null(credit_fraud_data)) {
      
      credit_fraud_data <<- credit_fraud_data_initial
      
      
      fillSelect("columns_select_list", colnames(credit_fraud_data_initial), "Sélectionner")
      
      #sync data
      data_credit_fraud<- eventReactive(input$input_credit_fraud, {
        credit_fraud_data
      })
      
      #displaying on the screen
      output$table_credit_fraud<-DT::renderDataTable({
        tmp.dat <- data_credit_fraud()
        DT::datatable(
          tmp.dat, extensions = 'Buttons',
          options = list(dom = 'Blfrtip', scrollX = TRUE, buttons = 
           list(list(
              extend = "collection",
              text = 'Show All',
              action = DT::JS("function ( e, dt, node, config ) {
              dt.page.len(-1);
              dt.ajax.reload();
          }")
         ),
         'copy', 'csv', list(
           extend = 'excel',
           filename = 'fichier de chargement credit_fraud',
           title = NULL,
           pageLength = 15,
           exportOptions = list(columns = c(1:length(credit_fraud_data)))
         ),list(extend = 'colvis', columns = c(1:length(colnames(credit_fraud_data)))))),filter='top'
        )
      })
    }
  })
  
  observeEvent(input$remove_all, {
    updateSelectizeInput(session,"columns_select",choices=sort(unique(categorial_columns_list)), 
     selected=NULL, options = list(placeholder="Please Select at Least One Column")
    )
  })
  
  makeReactiveBinding("balance_level")
  
  observeEvent(input$balance_level, {
    balance_level <<- input$balance_level
    balance_level <<- as.integer(balance_level)
  })
  
  
  #Balance data
  observeEvent(input$balance_add, {
    
    print(targetValues)
    
    if (!is.null(credit_fraud_data) && is.null(errorMessage)) {
      
      balanced_dataset <- NULL
      
      #Count occurence
      occ0<<-sum(credit_fraud_data[input$columns_select_target] == targetValues[1])
      
      occ1<<-sum(credit_fraud_data[input$columns_select_target] == targetValues[2])
      
      
      # get only rows that have 0 on target column
      dataset_zero_values_on_target <- filter(credit_fraud_data, input$columns_select_target == targetValues[1])
      
      # get only rows that have 1 on target column
      dataset_one_values_on_target <- filter(credit_fraud_data, input$columns_select_target == targetValues[2])
      
      if (occ1 < occ0) {
        difference <<- occ0 - occ1
      }else{
        difference <<- occ1 - occ0
      }
      print("------ occurence de 1 ------")
      print(occ1)
      
      print("------ occurence de 0 ------")
      print(occ0)
      
      print("----- difference d'occurence de 1 et 0 -----")
      print(difference)
      
      #apply value on slider input
      balance_value = as.integer((balance_level * difference) / 100)  #this is the number of rows we are going to add into datased to make it balanced
      
      print("----- nombre de lignes à ajouter -----")
      print(balance_value)
      
      #if we have more 0 than 1
      if (occ1 < occ0) {
        ajusted_data <<- dataset_one_values_on_target[sample(nrow(dataset_one_values_on_target), balance_value, replace = TRUE, prob = NULL), ]
      }else{ #if not
        ajusted_data <<- dataset_zero_values_on_target[sample(nrow(dataset_zero_values_on_target), balance_value), ]
      }
      balanced_dataset <- rbind(credit_fraud_data, ajusted_data)
      print(dim(balanced_dataset))
      
      print("***** Voir l'équilibre *****")
      print(sum(balanced_dataset[input$columns_select_target] == targetValues[1]))
      print(sum(balanced_dataset[input$columns_select_target] == targetValues[2]))
      
      credit_fraud_data <<- balanced_dataset
      
      #Refreshing view
      #sync data
      data_credit_fraud<- eventReactive(input$input_credit_fraud, {
        credit_fraud_data
      })
      
      #displaying on the screen
      output$table_credit_fraud<-DT::renderDataTable({
        tmp.dat <- data_credit_fraud()
        DT::datatable(
          tmp.dat, extensions = 'Buttons',
          options = list(dom = 'Blfrtip', scrollX = TRUE, buttons = 
           list(list(
              extend = "collection",
              text = 'Show All',
              action = DT::JS("function ( e, dt, node, config ) {
              dt.page.len(-1);
              dt.ajax.reload();
          }")
         ),
         'copy', 'csv', list(
           extend = 'excel',
           filename = 'fichier de chargement credit_fraud',
           title = NULL,
           pageLength = 15,
           exportOptions = list(columns = c(1:length(credit_fraud_data)))
         ),list(extend = 'colvis', columns = c(1:length(colnames(credit_fraud_data)))))),filter='top'
        )
      })
      shinyalert("Ajout de données réussi!", "Vous pouvez voir les modifications dans l'onglet jeu de données", type = "success")
      
      
    }else{
      shinyalert("Oops!", errorMessage, type = "error")
    }
  })
  
  
  #Déséquilibre
  observeEvent(input$balance_delete, {
    if (!is.null(credit_fraud_data) && is.null(errorMessage)) {
  
      #Count occurence
      occ0<<-sum(credit_fraud_data[input$columns_select_target] == targetValues[1])
      occ1<<-sum(credit_fraud_data[input$columns_select_target] == targetValues[2])
      
      
      # get only rows that have 0 on target column
      dataset_zero_values_on_target <- filter(credit_fraud_data, input$columns_select_target == targetValues[1])
      
      # get only rows that have 1 on target column
      dataset_one_values_on_target <- filter(credit_fraud_data, input$columns_select_target == targetValues[2])
      
      print("dim")
      print(dim(dataset_one_values_on_target)[1])
      
      if (occ1 < occ0) {
        difference <<- occ0 - occ1
      }else{
        difference <<- occ1 - occ0
      }
      print("------ occurence de 1 ------")
      print(occ1)
      
      print("------ occurence de 0 ------")
      print(occ0)
      
      print("----- difference d'occurence de 1 et 0 -----")
      print(difference)
      
      #apply value on slider input
      balance_value = as.integer((balance_level * difference) / 100)  #this is the number of rows we are going to add into datased to make it balanced
      
      print("----- nombre de lignes à supprimer -----")
      print(balance_value)
      
      #if we have more 0 than 1
      if (occ1 < occ0) {
        ajusted_data <<- tail(dataset_zero_values_on_target, n=dim(dataset_zero_values_on_target)[1]-balance_value)
        balanced_dataset <- rbind(dataset_one_values_on_target, ajusted_data)
      }else{ #if not
        ajusted_data <<- tail(dataset_one_values_on_target, n=dim(dataset_one_values_on_target)[1]-balance_value)
        balanced_dataset <- rbind(dataset_zero_values_on_target, ajusted_data)
      }
      print(dim(balanced_dataset))
      credit_fraud_data <<- balanced_dataset
      
      print("***** Voir l'équilibre *****")
      print(sum(balanced_dataset[input$columns_select_target] == targetValues[1]))
      print(sum(balanced_dataset[input$columns_select_target] == targetValues[2]))
      
      #Refreshing view
      data<- eventReactive(input$input_credit_fraud, {
        credit_fraud_data
      })
      
      #displaying on the screen
      output$table<-DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat, 
        options = list(scrollX = TRUE),filter='top')
      })
      shinyalert("Suppression de données réussi!", "Vous pouvez voir les modifications dans l'onglet jeu de donées", type = "success")
      
    }else{
      shinyalert("Oops!", errorMessage, type = "error")
    }
  })
  
  observeEvent(input$cancelColumn, {
    if (!is.null(credit_fraud_data_initial)) {
      credit_fraud_data <<- credit_fraud_data_initial
      fillSelect("columns_select_list", colnames(credit_fraud_data_initial), "Sélectionner")
      #syncing data
      data<- eventReactive(input$input_credit_fraud, {
        credit_fraud_data_initial
      })
      
      #displaying on the screen
      output$table<-DT::renderDataTable({
        tmp.dat <- data()
        DT::datatable(tmp.dat, 
        options = list(scrollX = TRUE),filter='top')
      })
    }else{
      shinyalert("Oops!", "Veuillez gérer les valeurs manquantes", type = "error")
    }
  })
  
  observeEvent(input$dropColumn, {
    if (!is.null(credit_fraud_data)) {
      credit_fraud_data <<- credit_fraud_data[ , !names(credit_fraud_data) %in% c(input$columns_select_list)]
      fillSelect("columns_select_list", colnames(credit_fraud_data), "Sélectionner")
      print(credit_fraud_data)
      output$table_credit_fraud<-DT::renderDataTable({
        tmp.dat <- credit_fraud_data
        DT::datatable(
          tmp.dat, extensions = 'Buttons',
          options = list(dom = 'Blfrtip', scrollX = TRUE, buttons = 
          list(list(
            extend = "collection",
            text = 'Show All',
            action = DT::JS("function ( e, dt, node, config ) {
            dt.page.len(-1);
            dt.ajax.reload();
        }")
         ),
         'copy', 'csv', list(
           extend = 'excel',
           filename = 'fichier de chargement credit_fraud',
           title = NULL,
           pageLength = 15,
           exportOptions = list(columns = c(1:length(credit_fraud_data)))
         ),list(extend = 'colvis', columns = c(1:length(colnames(credit_fraud_data)))))),filter='top'
        )
      })
    }else{
      shinyalert("Oops!", "Veuillez gérer les valeurs manquantes", type = "error")
    }
  })
  #-----------------------------------------------#
  #--------------FIN TRAITEMENTS DATA A&R---------#
  #-----------------------------------------------#
  
}

