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
                 tableOutput("table2"))
      )
      
    )
  )
))
