require(shiny)
require(shinythemes)
require(knitr)
#require(combn)
#setwd("C:/Users/Federico/AppData/Local/Architect-0.9.3/workspace-0.9.3/looming")
#runApp("shinyLoom")

shinyUI(fluidPage(theme = shinytheme("spacelab"), 
        titlePanel(h1("Looming Patterns")),
        title = "Looming Patterns",
        
        sidebarLayout(
            sidebarPanel(
                fluidRow(
                    column(6, 
                        numericInput(inputId = "nHeddles", 
                            label = "Numero licci (heddles)", 
                            value = 3, min = 2, max = 8, step = 1), 
                        numericInput(inputId = "warpingLength", 
                            label = "Numero fili ordito", 
                            value = 10, min = 10, max = 120, step = 5) 
                    ),
                    column(6, 
                        numericInput(inputId = "patternLength", 
                            label = "Lunghezza patterns", 
                            value = 3, min = 2, max = 10, step = 1), 
                        numericInput(inputId = "maxPlots", 
                            label = "Patterns da visualizzare", 
                            value = 6, min = 3, max = 60, step = 3) 
                    ), 
                    column(width = 9, 
                        h4(paste0("Click on the right to generate a PDF with ",
                                "all patterns."), 
                            align = "left") 
                    ), 
                    column(width = 3, 
                        downloadButton("downloadReport", label = "Download") 
                    )
                )# END - fluidRow
            ),# END - sidebarPanel 
            
            mainPanel(
                tabsetPanel(id = "inTabs", 
                    tabPanel(title = "Settings", 
                        fluidRow(
                            h2("Selezione Armatura Licci", align = "left"),
                            uiOutput("warping1"), 
                            uiOutput("warping2"), 
                            uiOutput("warping3"), 
                            uiOutput("warping4"), 
                            uiOutput("warping5"), 
                            uiOutput("warping6"), 
                            uiOutput("warping7"), 
                            uiOutput("warping8")
                        )# END - fluidRow
                    ), # END - tabPanel
                    tabPanel(title = "Results",
                        fluidRow(
#                            verbatimTextOutput("seeWarping"),
                            plotOutput("seeWarping1_3"), br(),
                            plotOutput("seeWarping4_6"), br(),
                            plotOutput("seeWarping7_9")
                        )# END - fluidRow
                    )# END - tabPanel
                )# END - tabsetPanel
            )
        )# END - sidebarLayout
    
    )# END - fluidPage
)# END - shinyUI



