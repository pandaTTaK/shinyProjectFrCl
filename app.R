
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)
library(RMySQL)
library(glue)

con <- dbConnect(MySQL(), host="localhost", user="root", password="fWpouIc+Pa5@tZ2", 
                 dbname="evaluation")


noms <- tbl(con,"reponse")%>%
  filter(question_id == 2) %>%
  collect()

formations <- tbl(con,"choix")%>%
  filter(question_id == 14) %>%
  collect()

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Evaluation CEFIM"),
  
  
  
  sidebarLayout(
    sidebarPanel(
    
    selectInput("noms",
                "Nom:",
                c(unique((noms$texte)))),
    
    
    selectInput("form",
                "Formations:",
                c(unique((formations$libelle)))),
               
    
    
    numericInput("bins",
                 "Nombre de barres :",
                 value = 30,
                 min = 1,
                 max = 50),  
      
    submitButton ( text  =  "Rechercher" , icon  =  NULL , width  =  NULL )
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("histo"),
      dataTableOutput("table"),
      dataTableOutput("df"),
      dataTableOutput("scoreetu"),
      dataTableOutput("questetu_form"),
      dataTableOutput("promo_quest")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  onStop(function(){
    dbDisconnect(con)
    print("Je suis déconnecté")
  })
  
  # Filter data based on selections
  
  df <- reactive({
    formations <- tbl(con,"choix")%>%
      filter(question_id == 14) %>%
      collect()
  })
  
  
  dfnoms <- reactive({
    noms <- tbl(con,"reponse")%>%
      filter(question_id == 2) %>%
      collect()
  })
  
  dfnom_quest_scores <- reactive({
    nom_quest_scores <- tbl(con,sql("SELECT session.id,
				reponse_nom.texte AS nom,
                reponse_prenom.texte AS prenom,
                question.libelle, reponse.score
            FROM session
            INNER JOIN reponse
                ON reponse.session_id = session.id
            INNER JOIN question
                ON question.id = reponse.question_id
            INNER JOIN reponse AS reponse_nom
                ON reponse_nom.session_id = session.id
                AND reponse_nom.question_id = 2
            INNER JOIN reponse AS reponse_prenom
                ON reponse_prenom.session_id = session.id
                AND reponse_prenom.question_id = 3
            WHERE question.type = 'score'")) %>%
      collect()
  })
    
    dfformation_etudiant <- reactive({
      formation_etudiant <- tbl(con,sql("SELECT session.id,
        choix_formation.libelle AS Formation,
	              reponse_nom.texte AS nom,
	               reponse_prenom.texte AS prenom
          FROM session
          INNER JOIN reponse AS reponse_nom
              ON reponse_nom.session_id = session.id
              AND reponse_nom.question_id = 2
          INNER JOIN reponse AS reponse_prenom
              ON reponse_prenom.session_id = session.id
              AND reponse_prenom.question_id = 3
          INNER JOIN choix AS choix_formation
            	ON reponse_nom.session_id = session.id")) %>%
              #AND choix_formation.libelle = 'AEC'"
      collect()
  })
    
    df_questetu_form <- reactive({
      req <- glue_sql("SELECT reponse_nom.texte AS Nom,
                        reponse_formation.session_id,
                        page.titre, question.libelle, reponse.score, reponse.texte
                    FROM choix
                    INNER JOIN reponse as reponse_formation
                    	ON reponse_formation.choix_id = choix.id
                    	AND reponse_formation.question_id = 14
                    INNER JOIN reponse
                    	ON reponse.session_id = reponse_formation.session_id
                    INNER JOIN reponse AS reponse_nom
                        ON reponse_nom.session_id = reponse_formation.session_id
                        AND reponse_nom.texte = {input$noms}
                    INNER JOIN question
                    	ON question.id = reponse.question_id
                    INNER JOIN page
                    	ON page.id = question.page_id
                    WHERE choix.libelle = {input$form}", .con = con)
                 
      questetu_form <- tbl(con, sql(req)) %>%
        collect()
    })  
    
    
    df_promo_quest <- reactive({
      req2 <- glue_sql("SELECT question.id, question.libelle FROM choix c 
                    inner join page_condition pgc on c.id = pgc.choix_id 
                    inner join question on question.page_id = pgc.page_id
                    where c.libelle = {input$form}", .con = con)
      
      promo_quest <- tbl(con, sql(req2)) %>%
        collect()
    })  
  output$histo <- renderPlot({

    df() %>%
      ggplot() +
      geom_histogram(aes(x = libelle), stat = "count")
  })

  output$df <- renderDataTable({
    
    
     dfformation_etudiant() #%>%  
    #   filter(libelle == input$form) %>%
    #   datatable(
    #     extensions = 'Buttons', options = list(
    #       dom = 'Bfrtip',
    #       buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    #     )
    #   )
  })
  
  
  output$scoreetu <- renderDataTable({
    
    dfnom_quest_scores() %>%  
      filter(nom == input$noms) %>%
      datatable(
        extensions = 'Buttons', options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        )
      )
  })
  
  output$questetu_form <- renderDataTable({
    
    df_questetu_form() %>%
      datatable(
        extensions = 'Buttons', options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        )
      )
  })
  
  output$promo_quest <- renderDataTable({
    
    df_promo_quest() %>%
      datatable(
        extensions = 'Buttons', options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        )
      )
  })
  
  
  output$noms <- renderText ({
    noms <- tbl(con,"reponse")%>%
      filter(question_id == 2) %>%
      collect()
  })
  
  output$prenoms <- renderText ({
    prenoms <- tbl(con,"reponse")%>%
      filter(question_id == 3) %>%
      collect()
  })
  
  output$formations <- renderText ({
    formations <- tbl(con,"choix")%>%
      filter(question_id == 14) %>%
      collect()
  })
  
  observeEvent(c(input$prenoms,input$prenoms), {
    nomsetu <- dfnoms()$texte %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "noms", choices = nomsetu)
    
  })
  
  observeEvent(dfformation_etudiant(), {
    libelles <- dfformation_etudiant()$libelle %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "form", choices = libelles)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
