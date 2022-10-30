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
      if (is.null(credit_fraud_data)) {
        credit_fraud_data <<- read.csv(input$input_credit_fraud$datapath,header = TRUE, sep=";", stringsAsFactors = FALSE, na.strings = c("","NA"))
      }
    } else if (stringr::str_ends(input$input_credit_fraud$datapath, "(xlsx|xls)")) {
      sheet1 <- readxl::read_excel(input$input_credit_fraud$datapath, 1 , na = c("N/A", "n/a"))
      #sheet2 <- readxl::read_excel(input$input_credit_fraud$datapath, 2, na = c("N/A", "n/a"))
      credit_fraud_data <<- data.frame(sheet1, stringsAsFactors = FALSE)
      credit_fraud_data_initial <<- credit_fraud_data
    }
    
    if (!is.null(credit_fraud_data)) {
      #fillSelect("outcome", colnames(credit_fraud_data), "Choisir")
      #fillSelect("variables", colnames(credit_fraud_data), "Choisir")
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
  
  # lg <- eventReactive(input$demarrage,{
  #   print("call lg")
  #   print(train_set)
  #   ok = Ok()
  #   if(ok==TRUE){
  #     parameter1 = paste(input$outcome ,"~")
  #     parameter1 = paste(parameter1 , input$variables[1])
  #     
  #     for (i in 2:length(input$variables)){
  #       parameter1 = paste(parameter1,"+" , input$variables[i])
  #     }
  #     
  #     parameter3 = "binomial"
  #     parameter <- list(formula= parameter1, data= train_set,family=parameter3, na.action=na.omit)
  #     
  #     fastDoCall(glm, parameter,quote = FALSE)
  #     
  #     
  #   }else {
  #     
  #     return(NULL)
  #     
  #   }
  # })
  
  lg <- eventReactive(input$demarrage,{
    ok = checkParams()
    if(ok==TRUE){
      parameter1 = paste(input$outcome ,"~")
      
      parameter1 = paste(parameter1 , input$variables[1])
      
      for (i in 1:length(input$variables)){
        parameter1 = paste(parameter1,"+" , input$variables[i])
      }
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
  
}