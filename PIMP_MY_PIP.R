library(shiny)
library(latex2exp)
library(shinythemes)

# WD
setwd("C:/Users/p_a_8/OneDrive/Bureau/M2 MODE/COURS/Semestre 3/FACE/COLLECTION")

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  
  # THEME : sélectionnable dans uen liste déroulante
  # shinythemes::themeSelector(),
  
  # TITRE
  titlePanel(img(src = "logo_rennes1.png", height = 140*.9, width = 300*.9)),
  
  # LAYOUT OF INTERFACE 
  sidebarLayout(
    # Section of input controls (e.g. slider)
    sidebarPanel(h3("Paramètres de la fonction de ressource K(x)", align = "center"),  # Titre du sidebarPanel
                 
                 withMathJax(), # Permet utilisation LaTeX
                 
                 #  FONCTION K 
                 # Widget_slider : K_0
                 sliderInput(
                   inputId = "k0",
                   label = "$$K_0$$",
                   min = .5,
                   max = 1.5,
                   step = 0.1,
                   value = 1
                 ),
                 
                 # Widget_slider : x_0
                 sliderInput(
                   inputId = "x0",
                   label = "$$x_0$$",
                   min = .9,
                   max = 1.1,
                   step = 0.1,
                   value = 1
                 ),
                 
                 # Widget_slider : lambda
                 sliderInput(
                   inputId = "lambda",
                   label = "$$\\lambda$$",
                   min = 0,
                   max = 5,
                   step = 0.1,
                   value = 1
                 ),
                 
                 #  FONCTION A 
                 # Widget_slider : X_1
                 # sliderInput(
                 #   inputId = "x1",
                 #   label = "$$x_1$$",
                 #   min = 0,
                 #   max = 2,
                 #   step = 0.1,
                 #   value = 1
                 # ),
                 # 
                 # # Widget_slider : X_2
                 # sliderInput(
                 #   inputId = "x2",
                 #   label = "$$x_2$$",
                 #   min = 0,
                 #   max = 2,
                 #   step = 0.1,
                 #   value = 1
                 # ),
                 
                 # Widget_slider : lambda
                 sliderInput(
                   inputId = "sigma",
                   label = "$$\\sigma$$",
                   min = 0,
                   max = 1,
                   step = 0.01,
                   value = .5
                 )
                 
    ),
    
    # Section of output of the shiny application
    mainPanel(
      # AJOUT LIGNE
      fluidRow(
        # /!\ Somme des colonnes doit faire 12
        column(width = 6,  # Largeur (12 maximum)
               wellPanel(p("Column width 2", align = "center")),  # En-tête de même largeur
               plotOutput(outputId = "plot_K")  # Placement du plot
               ),
        
        column(width = 6,
               wellPanel(p("Column width 2", align = "center")),
               plotOutput(outputId = "plot_A")
               )
      ),
      
    fluidRow(width = 12, 
             wellPanel(p("dazdsvfdsf", align = "center")),
             plotOutput(outputId = "pip_plot")
             )
    )
    
    # Layout arguments : https://shiny.posit.co/r/articles/build/layout-guide/
  )
)

# SERVER ------------------------------------------------------------------
server <- function(input,  # stocke les valeurs / arguments des widgets (e.g. input$myslider)
                   output  # output d'une fonction render* from *Output
) {
  
  source("FONCTIONS/FONCTIONS.R")
  
  # CODE R POTENTIELLEMENT REACTIF
  # /!\ Tout ce qu'il a dedans est re-run à chaque manipulation de widget
  
  # PLOT FONCTION K
  output$plot_K <- renderPlot({
    
    x <- seq(0, 2, length.out = 100)
    plot(
      x = x, 
      y = k_f(x = x, k0 = input$k0, x0 = input$x0, lambda = input$lambda),
      lwd = 1.5,
      ylim = c(0, input$k0),
      xlim = c(0, 2),
      type = "l",
      xlab = "Valeur du trait",
      ylab = "K(x)",
      cex.lab = 1.5
    )
    
  })
  
  # PLOT FONCTION A
  output$plot_A <- renderPlot({
    
    # Définition des valeurs de traits
    x1 <- seq(0, 2, length.out = 100)
    x2 <- seq(2, 0, length.out = 100)
    
    d = x1 - x2
    
    plot(
      a_f(x1 = x1, x2 = x2, sigma = input$sigma) ~ d, 
      lwd = 1.5,
      ylim = c(0, 1),
      xlim = c(min(d), max(d)),
      type = "l",
      xlab = TeX("$x_1 - x_2$"),
      ylab = TeX("$a(x_1 - x_2)$"),
      cex.lab = 1.5
    )
    
  })
}

# APPLICATION -------------------------------------------------------------
shinyApp(ui = ui, server = server)
