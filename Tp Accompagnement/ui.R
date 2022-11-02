library(shinydashboard)
ui <- dashboardPage(skin="green",
  dashboardHeader(title="TP Accompagnement R"),
  dashboardSidebar
    (title = "Menu",
       sidebarMenu(
         #menuItem("Gérer les origines", tabName = "raw", icon = icon("table")),
         menuItem("Analyse exploratoire", tabName = "dash", icon = icon("fas fa-chart-bar")),
         #menuItem("Charger les fichiers", tabName = "app", icon = icon("chart-pie")),
         
         fileInput(inputId = "input_credit_fraud", label = "Charger un dataset",
           multiple = TRUE,
           accept = c("text/csv",".xlsx",".xls",".tsv",
                      "text/comma-separated-values,text/plain",
                      ".csv",
                      '.xlsx'),
           placeholder = "Sélectionner un fichier"
         ),
         conditionalPanel(condition = "input.tabselected==2",
                          
          #Choix des variables
          selectInput(
            "variables", 
            h4("Choisissez les variables du dataset"), 
            choices = NULL,
            selected = "Chargement en cours" ,
            multiple = TRUE 
          ),
          
          
          #Choix du prédicteur
          selectInput(
            "outcome", 
            h4("Choisissez le predicteur du dataset"), 
            choices = c("Chargement en cours"),
            selectize = TRUE 
          ),
          
          sliderInput(
            "Slider1",
            label = h4("% Du Train Set"),
            min = 0,
            max = 100,
            value = 0
          ),
          textOutput("cntTrain"),
          textOutput("cntTest"),
          tags$hr(),
          actionButton(inputId = "demarrage", label = "Demarrer l'entrainement")                
                          
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
          tabsetPanel(type="tabs", id="tabselected",
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
                                          tags$p(strong("Analyse visuelle des variables du jeu de données")),
                                          br(),
                                          fluidRow(
                                            box(width = 12, title="Cette étude nous donne aussi l'importance jouée par une varialbe, nous pouvons décider de la supprimer ou non, de modifier son type, etc...",
                                              wellPanel(
                                                fluidRow(width=12,
                                                 # box(width = 12,
                                                 #   column(12,
                                                 #      mainPanel(verbatimTextOutput("df_freq"), style="")
                                                 #    )
                                                 # ),
                                                 box(width = 6,
                                                   column(12,
                                                      mainPanel(verbatimTextOutput("df_status"), style="")
                                                   )
                                                 ),
                                                 # box(width = 6,
                                                 #   column(12,
                                                 #      mainPanel(plotOutput("df_plot_num"), style="")
                                                 #   )
                                                 # )
                                                )
                                              ),
                                              collapsible = T
                                            )
                                          ),
                                          br(),
                                          tags$h3(strong("Veuillez renseigner la colonne target au niveau de l'échantillonnage afin de mettre à jour les graphiques"), style="color: red;"),
                                          
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
                                        
                                        br(),
                                        tags$h4(strong(textOutput("error_target")), style="color: green;"),
                                        
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
                                          box(width = 6, title = "Cas quantitatif continu", column(12,
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
                                        
                                        fluidRow(
                                          box(width = 6, title = "Cas quantitatif discret", column(12,
                                              fluidRow(
                                                #Liste des colonnes du dataset
                                                column(6,
                                                 selectizeInput( inputId = "quant_columns_select_credit_fraud",
                                                   label = NULL,
                                                   choices = NULL, 
                                                   multiple = FALSE,
                                                   selected = NULL
                                                 )
                                                ),
                                                column(6,
                                                 # Bouton pour déselectionner
                                                  actionButton("remove_selected_quant_credit_card" ,"Annuler", icon("trash"),
                                                  style = "color: #FFFFFF; background-color: #CA001B; border_color: #CA001B"))
                                              )
                                            )
                                          ),
                                          #ici opérations sur les deux jeux de données
                                          box(width = 6, title = textOutput("output_title_quant_credit"),  column(12, 
                                              # Partie resultats
                                              column(12,mainPanel(plotOutput("plot_zone_quant_credit_card"), style="heigth: auto;"))
                                            )
                                          )
                                        ),
                                        
                                        br(),
                                        tags$p(strong("affichez la matrice de corrélation des attributs")),
                                        fluidRow(
                                          box(width = 12, title = "Matrice de correlation", style="heigth: auto;", column(12,
                                              br(),
                                              tags$h4(strong(textOutput("error_correlation")), style="color: red;"),
                                              mainPanel(plotOutput("plot_zone_corr_credit_card"), style="heigth: auto;")
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
                                            box(width = 2 , title="Définir la colonne Target",
                                              column(12,
                                               selectizeInput( inputId = "columns_select_target",
                                                 label = NULL,
                                                 choices = NULL, 
                                                 multiple = FALSE,
                                                 selected = NULL
                                               )
                                              )
                                            ),
                                            box(width = 4, title="Déséquilibre des classes", column(12,
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
                                            box(width = 3 , title="Opérations sur les colonnes",
                                              column(6,
                                               selectizeInput(inputId = "columns_select_type",
                                                 label = NULL,
                                                 choices = c("integer", "character"), 
                                                 multiple = FALSE,
                                                 selected = 1
                                               )
                                              ),
                                              column(6,
                                               selectizeInput(inputId = "columns_select_list",
                                                label = NULL,
                                                choices = NULL, 
                                                multiple = TRUE,
                                                selected = NULL
                                               )
                                              ),
                                              column(4,
                                              # Bouton pour gérer les valeurs manquantes
                                              actionButton("changeType" ,"Change Type", icon("edit"),
                                                class = "btn btn-sm btn-warning")
                                              ),
                                              column(4,
                                               # Bouton pour gérer les valeurs manquantes
                                               actionButton("dropColumn" ,"Drop Columns(s)", icon("trash"),
                                                class = "btn btn-sm btn-danger")
                                              ),
                                              column(4,
                                                 # Bouton pour gérer les valeurs manquantes
                                                 actionButton("cancelColumn" ,"Cancel", icon("reset"),
                                                  class = "btn btn-sm btn-primary pull-right")
                                              )
                                            ),
                                            box(width = 3 , title="Gérer les valeurs manquantes", column(12,
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
                                        column(12, title="Caractéristiques du modèle",
                                               verbatimTextOutput("AD"),),
                                        column(4)
                                    ),
                                    box(width = 6,
                                        column(12, title="Matrice de Confusion",
                                               verbatimTextOutput("tablead"),),
                                        column(4)
                                    ),
                                    box(width = 6,
                                        column(12, title="Arbre de décision",
                                               plotOutput("ad",width = "100%", height = "500px"),),
                                        column(4)
                                    ),
                                   
                                    box(width = 6,
                                        column(12, title ="Score",
                                               verbatimTextOutput("scoread")),
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
                                   wellPanel(
                                     fluidRow(width=12,
                                      box(width = 6,
                                        column(12, title="Caractéristiques du modèle",
                                          verbatimTextOutput("logistic_regression"),
                                        )
                                      ),
                                      box(width = 6, 
                                          column(12,title="Matrice de Confusion",
                                                 verbatimTextOutput("tablelg"),),
                                          column(4)
                                      ),
                                      box(width = 6,
                                          column(12, title ="Scores de la Prédiction",
                                                 verbatimTextOutput("scorelg"),),
                                          column(4)
                                      ),
                                      box(width = 6,
                                          column(12, title ="Courbe ROC",
                                                 plotOutput("lgRoc",width = "100%", height = "500px"),),
                                          column(4)
                                      ),
                                      box(width = 6,
                                          column(12, title ="AUC",
                                                 verbatimTextOutput("auclg"),),
                                          column(4)
                                      )
                                     )
                                   ),
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
                                   wellPanel(
                                     
                                     fluidRow(width=12,
                                      box(width = 6,
                                          column(12, title="Caractéristiques du modèle",
                                                 verbatimTextOutput("SVM"),),
                                          column(4)
                                      ),
                                      box(width = 6,
                                          column(12,title="Matrice de Confusion",
                                                 verbatimTextOutput("tablesvm"),),
                                          column(4)
                                      ),
                                      box(width = 6,
                                          column(12,title="Score de prédiction",
                                                 verbatimTextOutput("scoresvm"),),
                                          column(4)
                                      ),
                                      box(width = 6,
                                          column(12, title ="Courbe ROC",
                                                 plotOutput("svmRoc",width = "100%", height = "500px"),),
                                          column(4)
                                      ),
                                      box(width = 6,
                                          column(12, title ="AUC",
                                                 verbatimTextOutput("aucsvm")),
                                          column(4)
                                      )
                                     )
                                     
                                   ),
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
              ),
              value=2
            )
          )
        )
      )
    )
  )
)
