# --------------------------------------------------------------------------- #
#                                                                             #
#               MODULE DE POUR INTERROGATION DES META-DONNEES                 #
#                   & GENERATION DICTIONNAIRE DE VARIABLE                     #
#                                                                             #
#                                 SHINY APP                                   #
#                                                                             #
# --------------------------------------------------------------------------- #

# Chargement des packages
library(shiny)
library(RPostgres)
library(DBI)
library(markdown)
library(sqldf)
library(tidyr)
library(ggplot2)
library(dplyr)
library(forcats)
library(openxlsx)
library(DT)
library(shinythemes)
library(shinyjs)
library(RColorBrewer)
library(stringr)


# Ouverture en lecture
db <- 'bdsig'
host_db <- 'postsig'
db_port <- '5434'
db_user <- 'sig_consult'
db_password <- 'lecture'

con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db,
                 port=db_port, user=db_user, password=db_password)


# Ouverture en Ecriture
db <- 'bdsig'
host_db <- 'postsig'
db_port <- '5434'
db_user <- 'sig'
db_password <- 'tetelle'

writing <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db,
                     port=db_port, user=db_user, password=db_password)


# ------------------------ CREATION SHINY APP ------------------------------- #



# --- PARTIE UI ------------------------------------

ui <- navbarPage("SIG - Méta-Données",windowTitle = "MétaDonnées SIG", collapsible = TRUE,
                 theme = shinytheme("cosmo"),
                 
                 
                 # Premier onglet supérieur
                 tabPanel(
                   "Interroger les méta-données",
                   
                   # Titre du panel N°1
                   titlePanel("Module de d'interrogation des variables en base"),hr(),
                   
                   # Création double fenêtre (sidebar + mainpanel)
                   sidebarLayout(
                     # Champ latéral gauche (Sidebar)
                     sidebarPanel(
                       fluidRow(
                         column(
                           width = 6,
                           
                           # Tous les schémas
                           radioButtons(
                             inputId = "select_schema",
                             label = "Schéma de données",
                             inline = FALSE,
                             choices = c("Tous les schémas", "Un ou plusieurs schéma(s)")
                           ),
                           
                           # sélection du shéma
                           conditionalPanel(
                             condition = "input.select_schema == 'Un ou plusieurs schéma(s)'",
                             selectInput(
                               "schema_bdd",
                               label = "Sélection de schéma(s) de données",
                               multiple = TRUE,
                               choices = dbGetQuery(con, "SELECT DISTINCT table_schema FROM information_schema.columns")
                             )
                           ),
                           
                         ),
                         
                         column(
                           width = 6,
                           
                           # Toutes les tables du schéma
                           conditionalPanel(
                             condition = "input.select_schema == 'Un ou plusieurs schéma(s)'",
                             radioButtons(
                               "select_table",
                               label = "Table de données",
                               choices = c("Toutes les tables du schéma", "Une ou plusieurs table(s)")
                             )
                           ),
                           
                           
                           # Sélection conditionnel de la table parmi les schméa sélectionnée (partie en server)
                           conditionalPanel(
                             condition = "input.select_table == 'Une ou plusieurs table(s)'",
                             selectInput(
                               "table_bdd",
                               label = "Sélection de table(s) de données",
                               multiple = TRUE,
                               choices = dbGetQuery(con,
                                                    "SELECT DISTINCT table_name FROM information_schema.columns"
                               )
                             )
                           ),
                           
                         )
                         
                         
                       ),
                       
                       
                       # Sélection conditionnelle des variables parmi les schéma et tables choisies + export dictionnaire de variables
                       hr(),
                       tags$b("Générer un dictionnaire des variables"),
                       br(), 
                       em("Pour permettre la sélection choisir au moins un schéma et une table."),
                       br(),
                       conditionalPanel(condition = "input.select_table == 'Une ou plusieurs table(s)'", 
                                        br(),
                                        em("Les variables sont choisies parmi les tables et schémas sélectionnés. Elles seront inclues dans le dictionnaire de variables généré au format .xlsx et téléchargeable ci-dessous."),
                                        br(),br(),
                                        selectInput("var_bdd", label = "Variables d'intérêt", choices = dbGetQuery(writing, "SELECT DISTINCT column_name FROM information_schema.columns"), multiple = TRUE),
                                        downloadButton("download_dic_var", label = "Générer le dictionnaire (.xlsx)"))
                       
                       
                     ),
                     
                     # Fenêtre principale (mainpanel)
                     mainPanel(
                       
                       # Affichage du commentaire de la table
                       fluidRow(
                         tags$b("Commentaires renseignés pour cette table de données : "), 
                         br(),
                         tags$em("Affiche <NA> si aucun commentaire n'a été renseigné"),
                         br()
                       ),

                       fluidRow(width = 8, offset = 2, align = "center",
                         h4(textOutput("com_table")),
                         hr()
                       ),
                       
                       # Visualisation des attributs
                       dataTableOutput("visualisation")
                       
                       )
                     
                     
                   )
                 ),
                 
                 
                 tabPanel(
                   "Aide à la saisie des méta-données",
                   
                   # Titre de la fenêtre
                   titlePanel("Renseigner des libellés depuis un fichier Excel"), hr(), 
                   
                   # Création d'un affichage avec menu latéral gauche
                   sidebarLayout(
                     
                     # Menu de gauche
                     sidebarPanel(
                       
                       # Titre
                       tags$h4(tags$b("Importer un fichier de libellés")),
                       hr(), 

                       # Type de fichier considéré
                       radioButtons("type_file", label = "Type de fichier", choices = c('Fichier Excel', 'Autre fichier')),
                       
                       # Paramètres généraux
                       tags$b("Libellé des colonnes"),
                       checkboxInput(inputId = 'header', label = 'Libellés en première ligne', value = FALSE),
                       
                       # Liste de paramètres (autre fichier)
                       conditionalPanel(condition = "input.type_file == 'Autre fichier'",radioButtons(inputId = 'sep', label = 'Séparateur de données', choices = c("Virgule"=',',"Point-Virgule"=';',"Tabulation"='\t', "Espace"=''), selected = ';')),
                       
                       # Module de chargement des données - FICHIER EXCEL
                       fluidRow(
                         column(width = 9,
                                conditionalPanel(condition = "input.type_file == 'Fichier Excel'", fileInput("file", ""))
                         ),
                         column(width = 3,
                                conditionalPanel(condition = "input.type_file == 'Fichier Excel'", numericInput("sheet_num", label = "N° feuille", value = 1, min = 1)),
                         )
                       ),

                       
                       # Module de chargement de données - autre fichier
                       conditionalPanel(condition = "input.type_file == 'Autre fichier'", fileInput("file", "")), 
                       
                       # Sélection des colonnes pour correspondance variable / libellé
                       hr(),
                       tags$h4(tags$b("Ajout dans la table d'attributs")), hr(),
                       
                       column(width = 5,
                              selectInput("var_id", label = "Colonne des variables", choices = "", multiple = FALSE),
                              selectInput("label_id", label = "Colonne des libellés", choices = "", multiple = FALSE)
                              ),
                       
                       column(width = 2),
                       
                       column(width = 5, 
                              selectInput("schema_id", label = "Schéma de référence", choices = dbGetQuery(con, "SELECT DISTINCT table_schema FROM information_schema.columns"), multiple = FALSE),
                              selectInput("table_id", label = "Table de référence", choices = "", multiple = FALSE)
                              ),


                       hr(),
                       br(),
                       actionButton("add", label = "Metre à jour la table d'attributs"),
                       conditionalPanel(
                         condition = "input.var_id != ''",
                         hr(),
                         tags$h4(tags$b("Détection des problèmes de correspondance")),
                         em("Variable issue du fichier Excel n'exitant pas dans la base de données du SIG. Si aucune ligne n'est présentée, toutes les variables du fichier Excel existent dans la base de données."),
                         DTOutput("tab_verif")   
                       )
                       
                     ),
                     
                     # Fenêtre principale (droite)
                     mainPanel(
                       
                       # Affichage de la table importée
                       tableOutput("table")                   
                     )
                   )
                       
                   
                 )
                 

)


# --- PARTIE SERVER -------------------------------

server <- function(input, output, session){
  
  
  # SELECTION DE TABLE CONDITIONNELLEMENT au schéma choisi
  observe({
    x <- dbGetQuery(writing, "SELECT table_name, table_schema from information_schema.columns") %>%
      filter(table_schema %in% (input$schema_bdd))
    
    # Can also set the label and select items
    updateSelectInput(session, "table_bdd",
                      label = "Sélection de table(s) de données",
                      choices = x$table_name
    )
  })
  
  
  # SELECTION DES VARIABLES CONDITIONNELLEMENT AUX SCHEMAS ET TABLES CHOISI(E)S
  observe({
    x <- dbGetQuery(writing, "SELECT table_name, table_schema, column_name from information_schema.columns") %>%
      filter(table_schema %in% (input$schema_bdd), table_name %in% (input$table_bdd))
    
    # Can also set the label and select items
    updateSelectInput(session, "var_bdd",
                      label = "Variables d'intérêt",
                      choices = x$column_name
    )
  })
  
  
  # Création de la table d'attributs
  tab_attrib <- reactive({
    
    # Récupération de la table d'attributs
    temp <- dbGetQuery(writing, "SELECT col_description((table_schema||'.'||table_name)::regclass::oid, ordinal_position) as column_comment, * from information_schema.columns")  
    
    
    # Sélection de schéma (si option choisie)
    if(input$select_schema == "Un ou plusieurs schéma(s)"){
      temp <- temp %>% filter(table_schema %in% (input$schema_bdd))
      
      # Selection de table (si option chosie)
      if(input$select_table == "Une ou plusieurs table(s)"){
        temp <- temp %>% filter(table_name %in% (input$table_bdd))
      }
      
    }
    
    # Libellé plus propres + ordre des variables
    temp %>%
      select(column_name, column_comment, table_name, table_schema) %>%
      rename("Schéma" = table_schema, "Table" = table_name, "Variable (nom)" = column_name, "libellé / Descrptif court" = column_comment)
    
  })
  
  
  # Liste de tables pour le schéma sélectionné
  output$visualisation <- renderDataTable(
    DT::datatable(
      tab_attrib(),
      options = list(pageLength = 15, scrollX = TRUE),
      filter = "top",
      rownames = FALSE)
  )
  
  # Commentaire de la table (première table du premier schéma)
  com_obj <- reactive({
    
    if(length(input$schema_bdd) == 1 & length(input$table_bdd) == 1){
      
      # Création requête pour la table concernée
      req_com_table <- paste0("SELECT obj_description('", input$schema_bdd, ".", input$table_bdd, "'::regclass)")
      
      # Récupération du champ commentaire sous forme de table
      com <- dbGetQuery(con, req_com_table[1])
      
      # Sortie du texte
      # cat(com[1,1])
      com[1,1]
      
    }else{
      
      if(length(input$schema_bdd) < 1 | length(input$table_bdd) < 1){
        com <- "--- Sélectionner un schéma et une table de données ---"
      }else{
        com <- "--- Ne sélectionner qu'un seul schéma et une seule table pour afficher le commentaire ---"
      }

      
    }

    
  })
  
  # Sortie du texte
  output$com_table <- renderText(com_obj())
  
  # Sélecteur des variables d'intérêt (pour export)
  tab_dictionnaire <- reactive({
    
    # Ensemble des attributs + sélection des schéma et tables d'intérêt
    dbGetQuery(writing, "SELECT col_description((table_schema||'.'||table_name)::regclass::oid, ordinal_position) as column_comment, * from information_schema.columns")  %>%
      filter(table_schema %in% (input$schema_bdd), table_name %in% (input$table_bdd), column_name %in% (input$var_bdd)) %>%
      select(column_name, column_comment, table_name, table_schema) %>%
      rename("Variable" = column_name, "Libellé / Descriptif" = column_comment, "Table de données" = table_name, "Shéma de données" = table_schema)
    
  })
  
  # Sortie du dictionnaire de variables
  output$download_dic_var <- downloadHandler(
    filename = paste("Dictionnaire_variables", Sys.Date(),".xlsx", sep=""),
    content = function(file) {
      write.xlsx(tab_dictionnaire(), file, rowNames = FALSE, sep=";")
    }
  )
  
  
  # IMPORT FICHIER - Transformation du fichier en jeu de données
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()
    }else{
      if(input$type_file == 'Fichier Excel'){
        
        # if(is.null(input$sheet_names)){return()}else{read.xlsx(xlsxFile = file1$datapath, sheet = input$sheet_names, colNames = input$header)}
        read.xlsx(xlsxFile = file1$datapath, sheet = input$sheet_num, colNames = input$header)
        
      }else{
        read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
      }
      
    } 
    
  })
  
  # IMPORT FICHIER - Affichage table de données
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  
  # IMPORT - Sélectionner le libellé de colonne "variable"
  observe({
    x <- data() 
    y <- names(x)
    
    # Can also set the label and select items
    updateSelectInput(session, "var_id",
                      label = "Colonne des variables",
                      choices = y
    )
  })
  
  # IMPORT - Sélectionner le libellé de colonne "variable"
  observe({
    x <- data() 
    y <- names(x)
    
    # Can also set the label and select items
    updateSelectInput(session, "label_id",
                      label = "Colonne des libellés",
                      choices = y
    )
  })
  
  # IMPORT - Choix de la table (selon schéma sélectionné)
  observe({
    x <- dbGetQuery(writing, "SELECT table_name, table_schema from information_schema.columns") %>%
      filter(table_schema %in% (input$schema_id))
    
    # Can also set the label and select items
    updateSelectInput(session, "table_id",
                      label = "Table de référence",
                      choices = x$table_name
    )
  })
  
  # Lancement des requêtes SQL à l'activation du boutton
  observeEvent(input$add, {
    
    # Récupération de la table de données
    data <- data()
    
    # Liste des variables en base
    liste_var_bd <- data.frame(var = names(dbGetQuery(con, paste0("SELECT * FROM ", input$schema_id, ".", input$table_id))))
    
    # Création d'une table avec deux variables (variable et libellé)
    temp  <- data %>% select(input$var_id, input$label_id) 
    
    # Remplacement des guillemets 
    temp[,2] <- str_replace_all(temp[,2], "'", "-")
    temp[,2] <- str_replace_all(temp[,2], '"', "-")
    
    # Création de la requête
    temp$requete <- paste0("COMMENT ON COLUMN ", input$schema_id, ".", input$table_id, ".", temp[,1], " IS '", temp[,2], "'")
    
    # Conservation uniquement des variables existant en BDD
    temp$var <- temp[,1]
    temp2 <- inner_join(temp, liste_var_bd, by = "var") 
    
    # Initialisation du compteur
    i <- 1
    
    # Lancement de la boucle sur toutes les lignes
    while(i <= nrow(temp2)){
      
      dbGetQuery(writing, temp2[i,"requete"])
      
      i <- i+1
      
    }
    
    # affichage notification
    showNotification("Intégration des méta-données réalisée", type = "message")
    
  })
  
  output$tab_verif <- renderDataTable(options = list(paging = FALSE, searching = FALSE), {
    
    # Récupération de la table de données
    data <- data()
    
    # LIste des variables en base
    liste_var_bd <- data.frame(var = names(dbGetQuery(con, paste0("SELECT * FROM ", input$schema_id, ".", input$table_id))))
    liste_var_bd$dispo <- "Existe en BDSIG"
    
    # Création d'une table avec deux variables (variable et libellé)
    temp  <- data %>%
      select(input$var_id, input$label_id)
    
    temp$var <- temp[,1]
    temp$indispo <- "N'existe pas dans la table choisie de la BDSIG (sera retirée lors de l'intégration des libellés)"
    
    left_join(temp, liste_var_bd, by = "var") %>%
      filter(is.na(dispo)) %>%
      select(var, indispo) %>%
      rename(Variable = var, "Disponibilité en BDD" = indispo)

  })
  
  
  
}


# --- CREATION APP -------------------------------

shinyApp(ui = ui, server = server)

