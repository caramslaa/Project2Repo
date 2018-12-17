install.packages("rpart") 
install.packages("rpart.plot")
library(dplyr)
library(ggplot2)
library(shiny)
library(readr)
library(tree)
library(randomForest)
library(rpart)
library(rpart.plot)

shinyServer(function(input, output, session) {
  
  #obtains the dataset
  protData <- reactive({
    pdbdf <- read_csv("https://raw.github.ncsu.edu/caramsla/Project2Repository/master/pdbdf.csv?token=AAAsN_rpt5sHVQxAeZ5Xfs9iUGTtkpQrks5cF88TwA%3D%3D")%>% 
    filter(classification == input$classification) %>% 
      filter(structureMolecularWeight  < input$MW) %>%
        filter(resolution <= input$MR)
  })
  #dataset2
  protDataP3 <- reactive({
    pdbDF1 <- read_csv("https://raw.github.ncsu.edu/caramsla/Project2Repository/master/pdbdf.csv?token=AAAsN_rpt5sHVQxAeZ5Xfs9iUGTtkpQrks5cF88TwA%3D%3D")
    pdbDF1$classification <- as.factor(pdbDF1$classification)
    pdbDF1 <- pdbDF1[!is.na(pdbDF1$structureMolecularWeight), ]
    pdbDF1 <- pdbDF1[!is.na(pdbDF1$phValue), ]
    pdbDF1 <- pdbDF1[!is.na(pdbDF1$densityPercentSol),]
    pdbDF1 <- pdbDF1[!is.na(pdbDF1$densityMatthews),]
    pdbDF1 <- pdbDF1[!is.na(pdbDF1$residueCount),]
    pdbDF1
  })

  
  #talks about the mean of the enzyme type selected
  output$means <- renderUI({
    paste("The mean of the enzyme type is",mean(protData() %>% filter(classification == input$classification)
                                                %>% .$structureMolecularWeight)," amu.")
  })
  output$text9 <- renderUI({
    paste("Adjustments to this input appear in the Scatter Plot Tab")
  })
  # redundant graph generation for the purpose of downloading
  plotuno <- reactive({
    ggplot(protData(), aes(x = structureMolecularWeight)) +geom_histogram(color="black", fill="white")
  })
  
  plotdos <- reactive({
    p <- ggplot(protData(), aes(x = resolution, y = residueCount)) + geom_point()
  })
  #original graphs
  output$plot1 <- renderPlot({
    ggplot(protData(), aes(x = structureMolecularWeight)) +geom_histogram(color="black", fill="white")
  })
  output$plot2 <- renderPlot({
    ggplot(protData(), aes(x = protData()$resolution, y = protData()$residueCount)) + geom_point()
  })
  # again, part of the redundancy
  output$hist <- renderPlot({
    print(plotuno())
  })
  
  output$scatter <- renderPlot({
    print(plotdos())
  })
  
  output$plot6 <- renderPlot({
    protdata <- protDataP3()
    hierClust <- hclust(dist(data.frame(protdata$structureMolecularWeight, protdata$densityPercentSol, protdata$phValue,
                                         protdata$densityMatthews,protdata$residueCount)), method = input$method2 )
    hierClust2 <- cutree(hierClust, k=input$k)
    ggplot(data = protdata, aes(x=get(input$xvar2), y=get(input$yvar2))) + geom_point(aes(col=as.factor(hierClust2))) + labs(color = "Clusters")
    
  })
  # most of what's contained in the information tab
  output$infoTab <- renderUI({
    withMathJax(
      helpText('This Shiny App is based on data from the Protein Data Bank, 
               an archive for thousands of Protein structures. This particular
               dataset contains information about four of the most common classes
               of enzymes found in the human body, but the records are not limited 
               to human proteins. Proteins are built out of 20 amino acids that 
               fold into recognizable structures and catalyze reactions. When providing 
               input for the maximum structure molecular 
               weight, the residue count found on the scatterplot y-axis will be affected 
               proportionally because the two have a nearly linear relationship. One of 
               the variables included is the Matthews density value, which is the density 
               of the crystal expressed as the ratio of the volume of the asymmetric unit 
               to the molecular mass of a monomer of the structure. I was able to find a 
               probability expression relating the Matthews coefficient with five empirical
               parameters.'),
      helpText('$$(P0,A,V,w,s)$$'),
      helpText('$$P(V_m) = P0 + A\\cdot\\ e^{-e^{-z}-z\\cdot\\ s + 1}$$'),
      helpText('$$z = \\left(\\frac{V_m- V}{w}\\right)$$'),
      helpText('Instructions: Visit the "Residues and Resolution Scatter Plot" tab and click on
               a point! This will reveal information about the point in the "Point Info" tab.')
      )
  })
  # a URL
  url <- a("RCSB PDB", href="https://www.rcsb.org/")
  output$tab <- renderUI({
    tagList("URL link:", url)
  })
  
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
  # nearpoints function to return points from what has been clicked on
  clickData <- reactive({
    clickdf <- nearPoints(protData(), input$plot_click, x = "resolution", y = "residueCount")
  })
  
  #download the data you've selected from the sidebar
  output$downloadData <- downloadHandler(filename = function() {paste("CRAPP", ".csv", sep = "")},
                                         content = function(file) {
                                           write.csv(protData(), file, row.names = FALSE)
                                         }
  )
  #download what you clicked on
  output$downloadClick <- downloadHandler(filename = function() {paste("CRAPPclick", ".csv", sep = "")},
                                         content = function(file) {
                                           write.csv(clickData(), file, row.names = FALSE)
                                         }
  )
  #download the plots
  output$downloadPlot1 <- downloadHandler(
    filename = function() { paste("plottwo", '.png', sep='') },
    content = function(file) {
      ggsave(file,plot = plotuno())
    }
  )
  
  output$downloadPlot2 <- downloadHandler(
    filename = function() { paste("plottwo", '.png', sep='') },
    content = function(file) {
      ggsave(file,plot = plotdos())
    }
  )
  #Table tab generators
  output$table <- renderTable({protData()})
  output$table2 <- renderTable({clickData()})
  
  
  #generate tree plots
  output$plot4 <-
    renderPlot({
    protdata <- protDataP3()
    if(input$treetype == "Classification - Protein Class")
    {
      classtree <- rpart(classification ~ structureMolecularWeight + phValue + densityPercentSol + densityMatthews,
                         method = input$method,data=protdata)
      rpart.plot(classtree,box.palette="RdBu", shadow.col="gray", nn=TRUE) 
    }
    else if(input$treetype == "Regression - Matthews")
    {
      regtree <- rpart(densityMatthews ~ structureMolecularWeight + phValue + densityPercentSol, method = input$method, data = protdata)
      rpart.plot(regtree,box.palette="RdBu", shadow.col="gray", nn=TRUE)
    }
    else if(input$treetype == "Regression - Residue Count")
    {
      regtree <- rpart(residueCount ~ structureMolecularWeight + phValue + densityPercentSol + densityMatthews, method = input$method, data = protdata)
      rpart.plot(regtree,box.palette="RdBu", shadow.col="gray", nn=TRUE)
      
    }
  })
  
  output$plot3 <- renderPlot({
    protdata <- protDataP3()
    a <- ggplot(protdata, aes(x = get(input$xvar), y = get(input$yvar)))
    a + geom_point() + xlab(input$xvar) + ylab(input$yvar) + geom_smooth(method = "lm")
  })
  
  output$regpreview <- renderText({
    protdata <- protDataP3()
    fit <- lm(structureMolecularWeight ~ residueCount, data = protdata)
    fitPred <- predict(fit, newdata = data.frame(residueCount = input$variableinput))
    suppressWarnings(paste("The predicted variable of",input$yvar, "for the given",input$xvar,"is",fitPred, sep = " "))
  })
})
