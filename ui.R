library(shiny)

proteins <- c("HYDROLASE","TRANSFERASE","OXIDOREDUCTASE","LYASE")

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Protein Database Information for Common Enzymes"),

  sidebarLayout(
    sidebarPanel(
      uiOutput('equation'),
      selectizeInput("classification", "Classification", selected = "HYDROLASE", choices = proteins),
      numericInput("MW", "Maximum Molecular Weight", value = 200000),
      uiOutput("means"),
      br(),
      numericInput("MR","Maximum Allowable Resolution",value = 5),
      uiOutput("text9")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram",
                 downloadButton("downloadPlot1", "Download Histogram"),
                 plotOutput("plot1")),
        tabPanel("Information", HTML("<strong>Thanks for stopping by!</strong>"), 
                 uiOutput("infoTab"),uiOutput("tab")),
        tabPanel("Residues and Resolution Scatter Plot",
                 downloadButton("downloadPlot2", "Download Scatter"),
                 plotOutput("plot2", click = "plot_click"),
                 verbatimTextOutput("info"),
                 verbatimTextOutput("click_info")),
        tabPanel("Specified Data Table", downloadButton("downloadData", "Download specified Data"),
                 tableOutput("table")),
        tabPanel("Point Info", downloadButton("downloadClick", "Download Point Data"),
                 tableOutput("table2")),
        tabPanel("Supervised Learning -- Tree", 
                 selectizeInput("treetype", "treetype", choices = c("Classification - Protein Class", "Regression - Matthews","Regression - Residue Count")),
                 selectizeInput("method","method", choices = c("class","anova","poisson")),
                 plotOutput("plot4")),
        tabPanel("Simple Linear Regression", selectizeInput("xvar","X Variable",
                        choices = c("structureMolecularWeight","phValue","densityPercentSol","densityMatthews","residueCount")),
                 selectizeInput("yvar", "Y Variable",
                        choices = c("residueCount","phValue","densityPercentSol","densityMatthews","structureMolecularWeight")),
                 plotOutput("plot3"), numericInput("variableinput","Variable Input",value = 10),uiOutput("regpreview")),
        tabPanel("Exploratory Cluster Analysis",
                 selectizeInput("xvar2","X Variable", choices = c("densityMatthews","phValue","structureMolecularWeight","densityPercentSol","residueCount")),
                 selectizeInput("yvar2", "Y Variable", choices = c("densityPercentSol","phValue","densityMatthews","residueCount","structureMolecularWeight")),
                 selectizeInput("method2","method", choices = c("complete","single","median","mcquitty")),
                 numericInput("k","k",value = 5), plotOutput("plot6"))
      )
      
    )
  )
))
#save
