library(shiny)
library(shinydashboard)
library(factoextra)
library(corrplot)
source("team.r")
source("new_acp.R")
source("global_function.r")

maindata = matrix(c(1, 2, 3, 3, 23, 4, 5, 3, 4, 5, 6, 7, 4, 56, 7, 8),
                  ncol = 4,
                  byrow = T / F)
maindata = as.matrix(maindata)
shinyServer(function(input, output) {
  dataAsvec = reactive({
    getvector(input$userData,
              nr = toNum(input$nrow),
              nc = toNum(input$ncol),
              sep = input$sep)
  })
  ##on va faire une fonction reactive pour lire les donnees
  acp_result = reactive({
    dataFile = input$userData
    if (is.null(dataFile))
      return ()
    ACP(dataAsvec(), diag(rep(1 / toNum(input$nrow), toNum(input$nrow))
))
  })
  ##
  
  output$likeYouLoad = renderTable({
    if (is.null(input$userData)) {
      return ()
    }
    input$userData
  }, bordered = TRUE)
  
  output$afterYouLoad = renderTable({
    if (is.null(input$userData))
      return ()
    dataAsvec()
  }, bordered = TRUE)
  
  #definition de la matrice de variance covariance
  output$varianceCovariance = renderTable({
    if (is.null(input$userData))
      return()
    acp_result()$variance_covariance
  }, bordered = T)
  
  output$correlationMatrix = renderTable({
    if(is.null(input$userData)) return()
    cor(dataAsvec())
  }, bordered = T)
  ##sortie des vectreurs propres
  output$eingsVectors=renderTable(
    acp_result()$vecteurs_propres
  )
  ##sortie des valeurs propres
  output$eingsValues=renderTable(
    acp_result()$valeurs_propres
  )
  #definition de la sortie des plots
  
  output$contirbutionPlot = renderPlot({
    if (is.null(input$userData))
      return()
    result = acp_result()
    hist(result$contributions, col = "orange", xlab = "Variable")
  })
  
  #correlationd
  output$representaionQualities = renderPlot({
    if (is.null(input$userData))
      return()
    result = acp_result()
    corrplot(result$qualities, title = "Representation qualities", is.corr = FALSE)
  })
  #maintenan les moustaches
  
  output$mainAxisPlot = renderPlot({
    if (is.null(input$userData))
      return()
    result = acp_result()
    corrplot(
      result$mainAxis,
      col = "green",
      is.corr = FALSE,
      title = "Contribution of variable to main Axis"
    )
    
  })
})