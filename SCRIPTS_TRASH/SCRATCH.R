# PACKAGES
library(shiny)

# WD
setwd("C:/Users/p_a_8/OneDrive/Bureau/M2 MODE/COURS/Semestre 3/FACE/COLLECTION")


# UI ----------------------------------------------------------------------
ui <- fluidPage(
  # TITRE
  titlePanel(img(src = "logo_rennes1.png", height = 140*.9, width = 300*.9)),
  
  # LAYOUT OF INTERFACE 
  sidebarLayout(
    
    # Section of input controls (e.g. slider)
    sidebarPanel("sidebar panel",
                 
                 # Widget : actionButton
                 selectInput(
                   inputId = "myactionbutton",  # Name R of the widget
                   label = "user_name_actionButton",
                   choices = c("Choix 1", "Choix 2", "Choix 3"),
                   selected = "Choix 1"
                   ),
                 
                 # Widget : slider
                 sliderInput(
                   inputId = "myslider",
                   label = "Slide Away",
                   min = 0,
                   max = 1,
                   step = 0.1,
                   value = 0.5
                             )
                 
                 ),
    
    # Section of output of the shiny application
    mainPanel(h1("MainPanel"),
              br(),  # Saut de ligne
              h2("Description"),
              p("Projet de M2 MODE : dynamique adaptative"),
              
              # Reactive texte
              textOutput(outputId = "reactive_text")
              ),
    
    # Position of the sidebarPanel
    position = "left"
    # Layout arguments : https://shiny.posit.co/r/articles/build/layout-guide/
    
  )
)


# SERVER ------------------------------------------------------------------
server <- function(input,  # stocke les valeurs / arguments des widgets (e.g. input$myslider)
                   output  # output d'une fonction render* from *Output
                   ) 
  {
  
  # CODE R 'fixe' A NE RUN QU'UNE SEULE FOIS
  iris <- iris
  
  
  # CODE R POTENTIELLEMENT REACTIF
  # /!\ Tout ce qu'il a dedans est re-run à chaque manipulation de widget
  
  output$reactive_text <- renderText(
    {
          paste("C'est le texte : ",
                input$myslider,
                " et ",
                input$myactionbutton
                )
    }
  )
}


# APPLICATION -------------------------------------------------------------
shinyApp(ui = ui, server = server)

