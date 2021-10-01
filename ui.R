
file.accept <- c("text/csv", "text/comma-separated-values,text/plain", ".csv")

shinyUI(fluidPage(
  titlePanel("Web Structural Equation Models"),
  
  
  tabsetPanel(type = "tabs",
              tabPanel("Data",
                       fileInput("file", "CSV file", accept = file.accept),
                       tags$hr(),
                       
                       tags$head(
                         tags$link(rel = "stylesheet", type = "text/css", href = "my.css")
                       ),
                       
                       DT::dataTableOutput("data")),
              
              tabPanel("Variable",
                       
                       fluidRow(
                         column(4, 
                                h4("Numbers and factors used for explanatory variables"),
                                htmlOutput("vec.obj")),
                         
                         #http://stla.github.io/stlapblog/posts/shiny_editTable.html
                         column(4, 
                                h4("Latent variable"),
                                rhandsontable::rHandsontableOutput("latent.variable")),
                         
                         column(4, 
                                h4("Explanatory variable"),
                                htmlOutput("vec.last"))
                       ),
                       
                       tags$hr(),
                       tags$head(
                         tags$link(rel = "stylesheet", type = "text/css", href = "my.css")
                       ),
                       DT::dataTableOutput("last.data")
              ),
              

              
              tabPanel("Model",
                       rhandsontable::rHandsontableOutput("model"),
                       tags$hr(),
                       textOutput("imp.fit.text"),
                       
                       tabsetPanel(type = "tabs",
                                   tabPanel("Model", verbatimTextOutput("model.text")),
                                   tabPanel("Summary", 
                                            verbatimTextOutput("summary"),
                                            verbatimTextOutput("model.fit"),
                                            verbatimTextOutput("model.modification")),
                                   tabPanel("Plot", plotOutput("semplot")),
                                   tabPanel("Graph",
                                            
                                            DiagrammeR::grVizOutput("plot"))
                                   
                                   
                                   )
                      ),
              
              
              tabPanel("Setting",
                       h4("Data scaling"),
                       checkboxInput("data.scale", label = "Scale", value = TRUE),
                       
                       h4("SEM"),
                       selectInput("estimator", label = "Estimator", 
                                   choices = c("ML", "MLR", "WLS", "DWLS")))
              
              )
  )

)
