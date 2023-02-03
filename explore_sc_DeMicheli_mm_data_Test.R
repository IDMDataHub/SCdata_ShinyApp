#!/usr/bin/env R

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinycssloaders)) # Adds spinner icon to loading outputs.
suppressPackageStartupMessages(library(shinydashboard))
library(shiny)
library(FactoMineR)
#library(cerebroApp) #launchCerebro()

##> TO ADD check-box with multiple choices 
#remotes::install_github("dreamRs/shinyWidgets")
library(shinyWidgets) 

### Read DeMicheli-mm data : 


## User Interface: controls the layout and appearance of your app.
my_ui <- fluidPage(
          titlePanel("Explore Single-Cell Datasets"),
          sidebarLayout(position="left",
                sidebarPanel(
                        h4("DeMicheli et al.,single-cell data on mouse (Cell Reports 2020)."),
                        br(),
                        pickerInput(inputId="cell_types",
                                    label = "Select Cell-type(s) to analyse",
                                    choices = c("Anti-inflammatory_macrophages","B_T_NK_cells",
                                                "Endothelial","FAPs","Mature_skeletal_muscle","Monocytes_Macrophages_Platelets",
                                                "MuSCs_and_progenitors","Neural_Glial_Schwann_cells","Pro-inflammatory_macrophages",
                                                "Resident_Macrophages_APCs","Smooth_muscle_cells","Tenocytes"),
                                     options = list(`actions-box` = TRUE),
                                     multiple = TRUE ),
                        br(),
                        textInput(inputId="genename",label="Gene Symbol") #,
                        #actionButton("run", label="Run Analysis")
                  ),
                 mainPanel(
                    h4("~~Results~~"),
                    br(),
                    h6("The histogram with the gene expression in the cell-types you chose: "),
                    plotOutput(outputId="histogram"),
                    textOutput("msg")
                )
         )
)



myserver <- function(input, output,session) {
  
    demicheli_mm <- readRDS("single_cell_DB/SC_DeMicheli_all_Genes_mouse.rds")

    observeEvent(input$genename,{ 
     
      if (input$genename %in%  demicheli_mm$GeneName) {
        
          tab2plot <- demicheli_mm %>%
                      filter(GeneName == input$genename ) %>%
                      filter(CellType %in% input$cell_types )

          output$histogram <- renderPlot({

              p <- ggplot(tab2plot , aes(x=TimePoint, y=MeanExpr, fill=TimePoint)) +
                          geom_bar(stat="identity", width=0.9, position="dodge" ) +
                          geom_errorbar(aes(ymin=MeanExpr-SEM, ymax=MeanExpr+SEM,color=TimePoint),
                                            position="dodge", width=0.9, size=0.3 )
              p2 <- p + facet_wrap(~ CellType ) + ggtitle(input$genename) + theme_minimal()
              p2
          })
        
          msg <- print("Bingo! The Gene exists in the dataset!")
        } else {
         msg <- print("Oops ! Error! The gene you chose is not present in the dataset!Please verify the name or try an alias.")
       }
      output$msg  <- renderText({ msg })
  })
    
}                       
                        
# Run the app ----
shinyApp(ui = my_ui, server = myserver)
