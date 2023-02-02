#!/usr/bin/env R

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinycssloaders)) # Adds spinner icon to loading outputs.
suppressPackageStartupMessages(library(shinydashboard))
library(shiny)
library(FactoMineR)

#library(cerebroApp) #launchCerebro()

# # User Interface: controls the layout and appearance of your app.
my_ui <- fluidPage(
          titlePanel("Explore Single-Cell Datasets"),
          
          sidebarLayout(position="left",
                sidebarPanel(
                    selectInput("which_dataset","Choose a Dataset",
                                choices=c("De Micheli - human" = "demicheli_hs",
                                          "De Micheli - mouse" = "demicheli_mm",
                                          "Oprescu - mouse"    = "oprescu_mm"), 
                                multiple = FALSE),
                   selectInput("cell_types","Cell Types", 
                                choices=c(c("FBN1+ MFAP5+ CD55+ Fibroblasts","DCN+ GSN+ MYOC+ Fibroblasts","COL1A1+ Fibroblasts"),
                                          c("Endothelial","FAPs","Mature_skeletal_muscle"),
                                          c("Endothelial_cells","FAPs","Skeletal_muscle") ) )
                ),
                mainPanel(
                  h4("~~Results~~"),
                  br()
                )
            )
)


myserver <- function(input, output, session) {

  ##> TO ADD check-box with multiple choices 
  ##> check out : https://github.com/dreamRs/shinyWidgets
  
  CellTypes <- reactive({case_when(
                        input$which_dataset=="demicheli_hs" ~c("FBN1+ MFAP5+ CD55+ Fibroblasts","DCN+ GSN+ MYOC+ Fibroblasts","COL1A1+ Fibroblasts"),
                        input$which_dataset=="demicheli_mm" ~c("Endothelial","FAPs","Mature_skeletal_muscle"),
                        input$which_dataset=="oprescu_mm"   ~c("Endothelial_cells","FAPs","Skeletal_muscle"))
    }) 
  
  
  observeEvent(input$which_dataset, { updateSelectInput(session,"cell_types", 
                                                        label="Cell Types", choices=CellTypes(), selected=NULL)})
 
  observe( print(CellTypes() ))
  
  # db_choice <-  c("De Micheli- human" = "demicheli_hs",
  #                 "De Micheli - mouse" = "demicheli_mm",
  #                 "Oprescu - mouse"  = "oprescu_mm")
  # 
  # output$db_choice <- db_choice
                        
}                       
                        


# Run the app ----
shinyApp(ui = my_ui, server = myserver)
