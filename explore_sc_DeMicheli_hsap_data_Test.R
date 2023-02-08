#!/usr/bin/env R

### Check if necessary packages are installed: 
pckg_need <- c("shiny","shinydashboard","tidyverse","shinyWidgets")

if ( any(! pckg_need %in% installed.packages()) ) {
  tobeinst <- pckg_need[which(! pckg_need %in% installed.packages() )]
  
  for (pkg in tobeinst){
    install.packages(pkg);
    remotes::install_github("dreamRs/shinyWidgets")
  }
}


## If all installed, Load :
if ( all(pckg_need %in% installed.packages()) ){
  library(shiny)
  suppressPackageStartupMessages(library(shinydashboard))
  suppressPackageStartupMessages(library(tidyverse))
  library(shinyWidgets)
  
}

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
                  h5("> The histogram with the gene expression in the cell-types you chose: "),
                  plotOutput(outputId="histogram"),
                  downloadButton("savePlot", label="Save image"),
                  br(),
                  #> show data.frame for the chosen gene
                  h5("> The table with the gene expression is as follows: "),
                  tableOutput("my_table"),
                  downloadButton("downloadData", "Download Table"),
                  br(),
                  h5("--------------------------------------------------------")
                )
  )
)


myserver <- function(input, output,session) {
  
  demicheli_hs <- readRDS("single_cell_DB/SC_DeMicheli_all_Genes_human.rds")
  
  observeEvent(input$genename, { 
    
    if (input$genename  %in%  demicheli_hs$GeneName) {
      
        tab2plot <- demicheli_hs %>%
                    filter(GeneName == input$genename ) %>%
                    filter(CellType %in% input$cell_types )
       
          p <- ggplot(tab2plot , aes(x=PatientID, y=MeanExpr, fill=PatientID)) +
            geom_bar(stat="identity", width=0.9, position="dodge" ) +
            geom_errorbar(aes(ymin=MeanExpr-SEM, ymax=MeanExpr+SEM, color=PatientID),
                          position="dodge", width=0.9, size=0.3 )
          
          p.theme <- p + 
                  facet_wrap(~ CellType) + 
                  ggtitle(input$genename) + 
                  theme(axis.text.x = element_text(angle = 45,vjust=0.5),
                        axis.title.x = element_text(vjust= -2)) 
        plotOutput <- p.theme
       
        
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
