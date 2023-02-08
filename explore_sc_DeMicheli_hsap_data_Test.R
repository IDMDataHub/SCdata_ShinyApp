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

### Read DeMicheli-human data : 


## User Interface: controls the layout and appearance of your app.
my_ui <- fluidPage(
  titlePanel("Explore Single-Cell Datasets"),
  sidebarLayout(position="left",
                sidebarPanel(
                  h4("DeMicheli et al.,single-cell data on human (Skeletal Muscle J. 2020 )."),
                  br(),
                  ## Multiple choice of cell-types of which to see expression
                  pickerInput(inputId="cell_types",
                              label = "Select Cell-type(s) to analyse",
                              choices = c("FBN1+ MFAP5+ CD55+ Fibroblasts",
                                          "DCN+ GSN+ MYOC+ Fibroblasts",
                                          "COL1A1+ Fibroblasts",
                                          "ACTA1+ Mature skeletal muscle",
                                          "CLDN5+ PECAM1+ Endothelial",
                                          "ICAM1+ SELE+ VCAM1+ Endothelial",
                                          "PAX7+ DLK1+ MuSCs and progenitors",
                                          "PAX7low MYF5+ MuSCs and progenitors",
                                          "S100A9+ LYZ+ Inflammatory macrophages",
                                          "C1QA+ CD74+ Macrophages"),
                              options = list(`actions-box` = TRUE),
                              multiple = TRUE ),
                  br(),
                  ## Gene by User Input 
                  textInput(inputId="genename",label="Gene Symbol")
                ),
                mainPanel(
                  h4("~~Results~~"),
                  br(),
                  textOutput("msg"),
                  h5("The histogram with the gene expression in the cell-types you chose: "),
                  plotOutput(outputId="histogram"),
                  downloadButton("savePlot", label="Save image"),
                  br() ,
                  #> show data.frame for the chosen gene
                  tableOutput("my_table"),
                  downloadButton("downloadData", "Download Table"),
                  br()
                )
  )
)


myserver <- function(input, output,session) {
  
  demicheli_mm <- readRDS("single_cell_DB/SC_DeMicheli_all_Genes_human.rds")
  
  observeEvent(input$genename, { 
    
    if (input$genename  %in%  demicheli_mm$GeneName) {
      
        tab2plot <- demicheli_mm %>%
                    filter(GeneName == input$genename ) %>%
                    filter(CellType %in% input$cell_types )
        
        
        #output$histogram <- renderPlot({
          
          p <- ggplot(tab2plot , aes(x=PatientID, y=MeanExpr, fill=PatientID)) +
            geom_bar(stat="identity", width=0.9, position="dodge" ) +
            geom_errorbar(aes(ymin=MeanExpr-SEM, ymax=MeanExpr+SEM, color=PatientID),
                          position="dodge", width=0.9, size=0.3 )
          p2 <- p + 
                facet_wrap(~ CellType) + 
                ggtitle(input$genename) + 
                theme(axis.text.x = element_text(angle = 45,vjust=0.5),
                      axis.title.x = element_text(vjust= -2)) 
                     # distance of "PatientID" title from graph
        plotOutput <- p2
        #})
        
        output$histogram <- renderPlot({ plotOutput })
        
        ## Capture the Save Image Button :
        output$savePlot <- downloadHandler(filename = "my_Gene_x_CellTypes.png",
                                           content = function(file){
                                             ggsave(file, plot=plotOutput )
                                           })
        
        msg <- print("Bingo! The Gene exists in the dataset!")
        
        
        ## Show & Download Table 
        mytable <- reactive({ tab2plot  })
        output$my_table <- renderTable({mytable() })
        
        ## Download Gene-Table
        output$downloadData <- downloadHandler(
                              filename = "Gene_x_Cell_types_table.tsv",
                              content = function(file) {
                                  write_delim(mytable(), file, col_names=T,delim="\t")
                                })
        
        
        
    } else {
      msg <- print("Oops ! Error! The gene you chose is not present in the dataset!Please verify the name or try an alias.")
    }
    output$msg  <- renderText({ msg })
  
  })

}


# Run the app ----
shinyApp(ui = my_ui, server = myserver)
