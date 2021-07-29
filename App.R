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
                   titlePanel("Module de recherche des variables en base"),hr(),
                   
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
                               choices = dbGetQuery(
                                 con,
                                 "SELECT DISTINCT table_schema FROM information_schema.columns"
                               )
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
                               label = "Tables de données",
                               choices = c("Toutes les tables du schéma", "Une ou plusieurs table(s)")
                             )
                           ),
                           
                           # sélection de la table
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
                       
                       
                       # Sélection de variables et export dictionnaire de variables
                       hr(),
                       tags$b("Générer une dictionnaire de variables"),
                       br(), 
                       em("Pour permettre la sélection choisir au moins un schéma et une table."),
                       br(),
                       conditionalPanel(condition = "input.select_table == 'Une ou plusieurs table(s)'", 
                                        em("Le thésaurus contient toutes les variables existantes, mais seules celles des schéma et table choisies peuvent être sélectionnées."), br(),br(),
                                        selectInput("var_bdd", label = "Variables d'intérêt", choices = dbGetQuery(writing, "SELECT DISTINCT column_name FROM information_schema.columns"), multiple = TRUE),
                                        downloadButton("download_dic_var", label = "Générer le dictionnaire (.xlsx)"))
                       
                       
                     ),
                     
                     # Fenêtre principale (mainpanel)
                     mainPanel(dataTableOutput("visualisation"))
                     
                     
                   )
                 ),
                 
                 
                 tabPanel(
                   "Aide à la saisie des méta-données",
                   
                   # Titre de la fenêtre
                   titlePanel("Générateur de SQL - Renseignement Méta-Données"), hr(), 
                   
                 ),
                 
                 tabPanel("Export dictionnaire de variables",
                          
                          # Titre
                          titlePanel("Module d'édition de dictionnaire de variables"), hr()
                 )
)


# --- PARTIE SERVER -------------------------------

server <- function(input, output, session){
  
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
      options = list(pageLength = 20, scrollX = TRUE),
      filter = "top",
      rownames = FALSE)
  )
  
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
  
}


# --- CREATION APP -------------------------------

shinyApp(ui = ui, server = server)

