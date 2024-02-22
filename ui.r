
library(shiny)
library(shinydashboard)
library(factoextra)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(DT)

source("team.r")
source("global_function.r")
shinyUI(dashboardPage(
  dashboardHeader(title = "ACP"),
  dashboardSidebar(br(),
                   sidebarMenu(
                     menuItem(
                       icon = icon("table"),
                       "Make ACP" ,
                       tabName = "makeACP",
                       badgeLabel = "principal componnents analyis"
                     ),
                     menuItem(
                       icon = icon("table"),
                       "Make AFC",
                       tabName = "makeAFC",
                       badgeLabel = "factor analysis of the componnents"
                     ),
                     menuItem(icon = icon("clipboard"), "About App", tabName = "about"),
                     menuItem(icon = icon("users"), "Our Team", tabName = "team")
                   )),
  dashboardBody(tabItems(
    tabItem(tabName = "team", team),
    tabItem(
      tabName = "about",
      h1(
        "ANalyse is an application who permit to make ACP or AFC of one dataset"
      )
    ),
    tabItem(
      tabName = "makeACP",
      h1("Welcome to acp make"),
      fluidRow(
        box(
          title = " ",
          color = "blue",
          fileInput(inputId = "userData", label = "Load your data"),
          radioButtons(
            inputId = "sep",
            label = "Choose your separator",
            choices = c(
              comma = ",",
              semicolon = ";",
              space = " ",
              tab = "\t"
            ),
            selected = ";",
            inline = TRUE
          ),
          ##fin des radios bouttons
          div(
            div(
              numericInput(
                "nrow",
                label = "Numbers of rows",
                min = 2,
                value = 2
              ),
              style="display:inline-block"
            ),
            div(
              numericInput(
                "ncol",
                label = "Numbers of columns",
                min = 2,
                value = 2
              ),
              style="display:inline-block"
            )
           
          )
          
        )
      ),
      br(),
      br(),
      br(),
      h2("Your ouput"),
      
      
      navbarPage(title = "PLOT RESULT"),
      fluidRow(
        box(
          title = "Mains axis",
          plotOutput("mainAxisPlot"),
          width = 4,
          solidHeader = T,
          status = "success",
          actionButton("mainAxisC", "See in big"),
        ),
        box(
          title = "Representation qualities",
          plotOutput("representaionQualities"),
          width = 4,
          solidHeader = T ,
          status = "info"
        ),
        box(
          title = "COntibutions",
          plotOutput("contirbutionPlot"),
          width = 4,
          solidHeader = T,
          status = "danger"
        )
      ),
      navbarPage(title = "Techniqual information"),
      fluidRow(
        # box(
        #   title = "Data like you load",
        #   tableOutput("likeYouLoad"),
        #   width = 3,
        #   background = "blue"
        # ),
        box(
          title = "Data with exclude column",
          tableOutput("afterYouLoad"),
          width = 3,
          background = "black"
        ),
        box(
          title = "Variance Covariance",
          tableOutput("varianceCovariance"),
          width = 3,
          background = "orange"
        ),
        box(
          title = "Correlation Matrix",
          tableOutput("correlationMatrix"),
          width = 3,
          background = "green"
        ),
        box(title = "Eings vectors",
            tableOutput("eingsVectors"),
            h4("Eings values"),
            tableOutput("eingsValues"),
            width = 3)
      )
      
    ),
    tabItem(tabName = "makeAFC", h1("Welcome to AFC make"))
  ), ),
))
