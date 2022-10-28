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

ui <- dashboardPage(skin="green",
  dashboardHeader(title="TP Accompagnement R"),
  dashboardSidebar
    (title = "Menu",
       sidebarMenu(
         #menuItem("Gérer les origines", tabName = "raw", icon = icon("table")),
         menuItem("Analyse exploratoire", tabName = "dash", icon = icon("fas fa-chart-bar")),
         menuItem("Charger les fichiers", tabName = "app", icon = icon("chart-pie")),
         
         fileInput(inputId = "input_credit_fraud", label = "Charger un dataset",
           multiple = TRUE,
           accept = c("text/csv",".xlsx",".xls",".tsv",
                      "text/comma-separated-values,text/plain",
                      ".csv",
                      '.xlsx'),
           placeholder = "Sélectionner un fichier"
         )
         
         
         #menuItem("Education", tabName = "education", icon = icon("mortar-board")),
         #menuItem("Gender", tabName = "gender", icon = icon("intersex")),
         #menuItem("Job-Satisfaction", tabName = "satisfaction", icon = icon("smile-o")),
         #menuItem("year_company", tabName = "year", icon = icon("handshake-o"))
         
       )
    ),
  dashboardBody(
    tags$head(tags$style(HTML('
        /* logo */
        .skin-green .main-header .logo {
            background-color: #073d60;
            color: #fff;
            border-bottom: 0 solid transparent;
        }
        .skin-blue .main-header .logo {
                              background-color: #f4b943;
        }
        .skin-green .main-header .navbar {
            background-color: #03787c;
        }
        .skin-green .left-side, .skin-green .main-sidebar, .skin-green .wrapper {
            background-color: #073d60;
        }
        .skin-green .sidebar-menu>li.active>a, .skin-green .sidebar-menu>li:hover>a {
            color: #fff;
            background: #03787c;
            border-left-color: #00a65a;
        }
        div#output_signataire {
            color: red;
            text-align: center;
        }'
    ))
    ),
    tabItems(
      
      
      
      
      #################################
      # TAB POUR ANALYSE EXPLORATOIRE #
      #################################
      
      #Display datasets
      tabItem(tabName = "dash",
        fluidRow(
          tabsetPanel(
            tabPanel(
              "Chargement des jeux de données",
              fluidRow(
                box(width = 12,
                  wellPanel(
                    #Display datasets
                    tabItem(tabName = "stock",
                      fluidRow(
                        tabsetPanel(
                          fluidRow(width = 12,
                            box(width = 12, style="position: relative; margin: 16px;",
                              tabItem(tabName = "CF",
                                fluidRow(
                                  tabsetPanel(
                                    tabPanel(
                                      "Jeu de données",
                                      fluidRow(
                                        box(width = 12,dataTableOutput("table_credit_fraud"),title="Dataset",
                                        footer="Jeu de données",
                                        collapsible = T)
                                      )
                                    ),
                                    tabPanel(
                                      "Analyse exploratoire",
                                      fluidRow(
                                        box(width = 12,dataTableOutput("table_credit_fraud_analyse"),
                                          title="Analyse exploratoire des données",
                                          br(),
                                          tags$p(strong("quelles sont les dimensions du jeu de données, existe-t’il des valeurs manquantes ou des attributs constants?")),
                                          br(),
                                          fluidRow(
                                            column(8,
                                             tags$table(border = 1, width= "100%",
                                              tags$thead(
                                                tags$tr(style="line-height: 3;",
                                                  tags$td(align = "center", strong("Dimensions"), style="width: 10%;"),
                                                  tags$td(align = "center", strong("Valeurs manquantes en colonnes"), style="width: 20%;"),
                                                  tags$td(align = "center", strong("Valuers manquantes en lignes"), style="width: 20%;"),
                                                  tags$td(align = "center", strong("Nombre d'attributs constants"), style="width: 20%;"),
                                                  tags$td(align = "center", strong("Commentaire"), style="width: 30%;")
                                                )
                                              ), 
                                            tags$tbody(
                                              tags$tr(
                                                tags$td(align = "center", textOutput("output_dimension_credit")),
                                                tags$td(align = "center", textOutput("output_na_colonnes_credit")),
                                                tags$td(align = "center", textOutput("output_na_lignes_credit")),
                                                tags$td(align = "center", textOutput("output_attributs_contants_credit")),
                                                tags$td(align = "center", "Nous verrons comment gérer ces valeurs dans la partie reservée à l'échantillonage des données")
                                                #tags$td(align = "center", textOutput("output_commentaire_credit"))
                                              )
                                            )
                                           )   
                                          ),
                                          column(4)
                                        ),
                                        br(),
                                        tags$p(strong("affichez à l’aide un graphe adapté la proportion d’individus qui ont churné")),
                                        
                                        #afficher le pie
                                        br(),
                                        column(12,mainPanel(plotOutput("pie_credit_card"), style="")),
                                        
                                        br(),
                                        tags$p(strong("pour chaque variable catégorielle, affichez à l’aide un graphe adapté la proportion de churn vs. non churn")),
                                        
                                        fluidRow(
                                          box(width = 6, title = "Choisir la colonne cible", column(12,
                                            fluidRow(
                                              #Liste des colonnes du dataset
                                              column(6,
                                               selectizeInput( inputId = "cat_columns_select_credit_fraud",
                                                 label = NULL,
                                                 choices = NULL, 
                                                 multiple = FALSE,
                                                 selected = NULL
                                               )
                                              ),
                                              column(6,
                                                # Bouton pour déselectionner
                                                actionButton("remove_selected_cat_credit_card" ,"Annuler", icon("trash"),
                                                style = "color: #FFFFFF; background-color: #CA001B; border_color: #CA001B"))
                                              ),
                                            )
                                          ),
                                          #ici opérations sur les deux jeux de données
                                          box(width = 6, title = textOutput("output_title_cat_credit"),  column(12, 
                                            # Partie resultats
                                              column(12,mainPanel(plotOutput("plot_zone_cat_credit_card"), style="heigth: auto;"))
                                            )
                                          )
                                        ),
                                        
                                        br(),
                                        tags$p(strong("pour chaque variable numérique, affichez séparemment à l’aide un graphe adapté (eg.histogramme) les valeurs pour les populations churn & non churn")),
                                        
                                        fluidRow(
                                          box(width = 6, title = "Choisir la colonne cible", column(12,
                                            fluidRow(
                                              #Liste des colonnes du dataset
                                              column(6,
                                               selectizeInput( inputId = "num_columns_select_credit_fraud",
                                                 label = NULL,
                                                 choices = NULL, 
                                                 multiple = FALSE,
                                                 selected = NULL
                                               )
                                              ),
                                              column(6,
                                                     # Bouton pour déselectionner
                                                actionButton("remove_selected_num_credit_card" ,"Annuler", icon("trash"),
                                                style = "color: #FFFFFF; background-color: #CA001B; border_color: #CA001B"))
                                              ),
                                            )
                                          ),
                                          #ici opérations sur les deux jeux de données
                                          box(width = 6, title = textOutput("output_title_num_credit"),  column(12, 
                                              # Partie resultats
                                              column(12,mainPanel(plotOutput("plot_zone_num_credit_card"), style="heigth: auto;"))
                                            )
                                          )
                                        ),
                                        
                                        br(),
                                        tags$p(strong("affichez la matrice de corrélation des attributs")),
                                        fluidRow(
                                          box(width = 12, title = "Matrice de correlation", style="heigth: auto;", column(12,
                                              column(12,mainPanel(plotOutput("plot_zone_corr_credit_card"), style="heigth: auto;"))
                                            )
                                          )
                                        ),
                                        collapsible = T)
                                      )
                                    ),
                                    tabPanel(
                                      "Prédiction de churn",
                                      fluidRow(
                                        box(width = 12,dataTableOutput("table_credit_fraud_churn"), title="Prédiction de churn", collapsible = T)
                                      )
                                    ),
                                    tabPanel(
                                      "Sur-échantillonage et sous-échantillonage",
                                      fluidRow(
                                        box(width = 12,dataTableOutput("table_credit_fraud_echantillonage"), title="Sur-échantillonage et sous-échantillonage",
                                        #-----------------------------------------------#
                                        #--------------DEBUT AJOUT BOUTTONS A&R---------#
                                        #-----------------------------------------------#
                                        wellPanel(
                                          fluidRow(
                                            box(width = 7,title="Déséquilibre des classes", column(12,
                                               sliderInput("balance_level", "Régler le problème de déséquilibre à combien de %:",
                                               min = 0, max = 100,
                                               value = 0, step = 5,
                                               animate = animationOptions(interval = 300, loop = TRUE)),
                                               
                                               # Bouton pour gérer les valeurs manquantes
                                               actionButton("balance_add" ,"Ajouter Classe Minoritaire", icon("plus"),
                                                class = "btn btn-sm btn-success"),
                                               
                                               # Bouton pour gérer les valeurs manquantes
                                               actionButton("balance_delete" ,"Diminuer Classe Majoritaire", icon("minus"),
                                                class = "btn btn-sm btn-danger")
                                              )
                                            ),
                                            box(width = 5 , title="Gérer les valeurs manquantes", column(12,
                                              # Bouton pour gérer les valeurs manquantes
                                              actionButton("dropAll" ,"Drop All NA", icon("trash"),
                                                          class = "btn btn-sm btn-success"),
                                             
                                              actionButton("replaceAll" ,"Replace All NA", icon("plus"),
                                                          class = "btn btn-sm btn-primary"),
                                             
                                              actionButton("resetAll" ,"Reset", icon("sync"),
                                                          class = "btn btn-sm btn-danger")
                                              )
                                            )
                                          )
                                        ),
                                        #-----------------------------------------------#
                                        #--------------FIN AJOUT BOUTTONS A&R-----------#
                                        #-----------------------------------------------#
                                        collapsible = T)
                                      )
                                    )
                                  )
                                )
                              ),
                            collapsible = T),
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            tabPanel(
              "Implémentation des differents modèles",
              fluidRow(
                tabsetPanel(
                  fluidRow(width = 12,
                   box(width = 12, style="position: relative; margin: 16px;",
                     tabItem(tabName = "CF",
                       fluidRow(
                         tabsetPanel(
                           tabPanel(
                             "Arbre de décision",
                             fluidRow(
                               box(width = 12,dataTableOutput("table_modele"), title="Modèle Arbre de décision",
                                 wellPanel(
                                   fluidRow(width=12,
                                    box(width = 6,
                                        column(8),
                                        column(4)
                                    ),
                                    box(width = 6,
                                        column(8),
                                        column(4)
                                    ),
                                    box(width = 6,
                                        column(8),
                                        column(4)
                                    ),
                                    box(width = 6,
                                        column(8),
                                        column(4)
                                    )
                                   )
                                 ),
                                 collapsible = T
                               )
                             )
                           ),
                           tabPanel(
                             "Régression Logistique",
                             fluidRow(
                               box(width = 12,dataTableOutput("table_regression"), title="Modèle Régression Logistique",
                                   wellPanel(),
                                   collapsible = T
                               )
                             )
                           ),
                           tabPanel(
                             "Support Vector Machine (sans kernel)",
                             fluidRow(
                               box(width = 12,dataTableOutput("table_svm_sans"), title="Modèle Support Vector Machine (sans kernel)",
                                 wellPanel(),
                                 collapsible = T
                                )
                             )
                           ),
                           tabPanel(
                             "Support Vector Machine (avec kernel)",
                             fluidRow(
                               box(width = 12,dataTableOutput("table_svm_avec"), title="Modèle Support Vector Machine (avec kernel)",
                                   wellPanel(),
                                   collapsible = T
                               )
                             )
                           )
                         )
                       )
                     ),
                     collapsible = T),
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)


#------DEBUT DU SERVER-------
#update upload params
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
  
  #function to fill a choosed select
  fillSelect <- function(selectName, listValues, placeHolderMessage) {
    if (!is.null(selectName) && selectName != "") {
      if (!is.null(listValues) && length(listValues) > 0) {
        updateSelectizeInput(session, selectName, choices=sort(unique(listValues)), 
          selected=NULL, options = list(placeholder=placeHolderMessage)
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
    print("cliqcked")
    #Read uploaded dataset
    inFile <- input$input_credit_fraud
    if (is.null(inFile)) return(NULL)
    
    if(stringr::str_ends(input$input_credit_fraud$datapath, "csv")) {
      credit_fraud_data <<- read.csv(input$input_credit_fraud$datapath,header = TRUE, sep=",", stringsAsFactors = FALSE, na.strings = c("","NA"))
    } else if (stringr::str_ends(input$input_credit_fraud$datapath, "(xlsx|xls)")) {
        sheet1 <- readxl::read_excel(input$input_credit_fraud$datapath, 1 , na = c("N/A", "n/a"))
        #sheet2 <- readxl::read_excel(input$input_credit_fraud$datapath, 2, na = c("N/A", "n/a"))
        credit_fraud_data <<- data.frame(sheet1, stringsAsFactors = FALSE)
        credit_fraud_data_initial <<- credit_fraud_data
    }
    
    if (!is.null(credit_fraud_data)) {
      
      dim <- getDataSetDim(credit_fraud_data)
      #display value in the text zone that contains a dimension!
      output$output_dimension_credit <- renderText({
        dim
      })
      
      result <- getEmptyValues(credit_fraud_data)
      
      output$output_na_colonnes_credit <- renderText({
        result
      })
      
      output$output_na_lignes_credit <- renderText({
        result
      })
      
      attrs <- getConstantsAttributes(credit_fraud_data)
      
      output$output_attributs_contants_credit <- renderText({
        attrs
      })
      
      output$pie_credit_card<-renderPlot({
        plotPie(credit_fraud_data, "Class", c(1, 0), c("Churn", "Non churn"))
      })
      
      credit_fraud_num_columns_list <<- getColumnsByType(credit_fraud_data, "n", "Class")
      
      credit_fraud_categorial_columns_list <<- getColumnsByType(credit_fraud_data, "c", "Class")
      
      # print(credit_fraud_num_columns_list)
      # 
      # print(credit_fraud_categorial_columns_list)
      
      if (!is.null(credit_fraud_num_columns_list)) {
        fillSelect("num_columns_select_credit_fraud", credit_fraud_num_columns_list, "Veuillez choisir un champ")
      }
      
      if (!is.null(credit_fraud_categorial_columns_list)) {
        fillSelect("cat_columns_select_credit_fraud", credit_fraud_categorial_columns_list, "Veuillez choisir un champ")
      }
      
      output$plot_zone_corr_credit_card<-renderPlot({
        computeCorr(credit_fraud_data)
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
  
  
  
}
#------FIN DU SERVER-------

shinyApp(ui,server)
