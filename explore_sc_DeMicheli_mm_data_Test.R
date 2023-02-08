#!/usr/bin/env R

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinycssloaders)) # Adds spinner icon to loading outputs.
suppressPackageStartupMessages(library(shinydashboard))
library(shiny)
library(FactoMineR)

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
                        ## Multiple choice of cell-types of which to see expression
                        pickerInput(inputId="cell_types",
                                    label = "Select Cell-type(s) to analyse",
                                    choices = c("Anti-inflammatory_macrophages","B_T_NK_cells",
                                                "Endothelial","FAPs","Mature_skeletal_muscle",
                                                "Monocytes_Macrophages_Platelets",
                                                "MuSCs_and_progenitors","Neural_Glial_Schwann_cells",
                                                "Pro-inflammatory_macrophages",
                                                "Resident_Macrophages_APCs",
                                                "Smooth_muscle_cells","Tenocytes"),
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
                    plotOutput(outputId="histo_ct"),
                    downloadButton("savePlot_1", label="Save image"),
                    br(),
                    h5(">The histogram with the gene expression per Timepoint : "),
                    plotOutput(outputId="histo_tm"),
                    downloadButton("savePlot_2", label="Save image"),

                    br() ,
                    #> show data.frame for the chosen gene
                    tableOutput("my_table"),
                    downloadButton("downloadData", "Download Table"),
                    br(),
                    h6("------------------------------------------------------")
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


              p1 <- ggplot(tab2plot , aes(x=TimePoint, y=MeanExpr, fill=TimePoint)) +
                          geom_bar(stat="identity", width=0.9, position="dodge" ) +
                          geom_errorbar(aes(ymin=MeanExpr-SEM, ymax=MeanExpr+SEM,color=TimePoint),
                                            position="dodge", width=0.9, size=0.3 )
              p1.theme <- p1 + facet_wrap(~ CellType) + ggtitle(input$genename) + theme_minimal()
              plotOutput1 <- p1.theme


              output$histo_ct <- renderPlot({ plotOutput1 })

              ## Save Image Button :
              output$savePlot_1 <- downloadHandler(filename = "my_Gene_x_CellTypes.png",
                                         content = function(file){
                                         ggsave(file, plot=plotOutput1 )
                                       })
              #---------- plot-2 ---------------------#
               p2 <- ggplot(tab2plot , aes(x=CellType, y=MeanExpr, fill=CellType)) +
                     geom_bar(stat="identity", width=0.9, position="dodge" ) +
                     geom_errorbar(aes(ymin=MeanExpr-SEM, ymax=MeanExpr+SEM, color=CellType),
                                   position="dodge", width=0.9, size=0.3 )
               p2.theme <- p2 +
                 facet_wrap(~ TimePoint) +
                 ggtitle(input$genename) +
                 theme(axis.text.x = element_text(angle = 45,vjust=0.5),
                       axis.title.x = element_text(vjust= -2))

               plotOutput2 <- p2.theme

               output$histo_tm <- renderPlot({ plotOutput2 })

               ## Save Image Button :
               output$savePlot_2 <- downloadHandler(filename = "my_Gene_x_TimePoint.png",
                                                     content = function(file){
                                                     ggsave(file, plot=plotOutput2 )
                                                  })
                msg <- print("Bingo! The Gene exists in the dataset!")
                ## Show & Download Table
                mytable <- reactive({ tab2plot  })
                output$my_table <- renderTable({mytable() })

                ## Download Gene-Table
                output$downloadData <- downloadHandler(filename = "Gene_x_Cell_types_table.tsv",
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
