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

### Read Oprescu-mouse data :


## User Interface: controls the layout and appearance of your app.
my_ui <- fluidPage(
  titlePanel("Explore Single-Cell Datasets"),
  sidebarLayout(position="left",
                sidebarPanel(
                  h4("Oprescu et al.,single-cell data on mouse (iScience 23,100993,2020)."),
                  br(),
                  ## Multiple choice of cell-types of which to see expression
                  pickerInput(inputId="cell_types",
                              label = "Select Cell-type(s) to analyse",
                              choices = c("APC", "Endothelial_cells", "FAPs","Fibroblasts",
                                          "M1","M2", "Monocytes","MuSC","Myeloid_cells",
                                          "Neutrophils", "Pericytes","ProliferatingIC",
                                          "Skeletal_muscle", "Tcells","Tenocytes" ),
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
                  h5(">The histogram with the gene expression per Cell-type,as you chose: "),

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

  oprescu_mm <- readRDS("single_cell_DB/SC_Oprescu_all_Genes_mouse.rds")

  observeEvent(input$genename, {

    if (input$genename  %in%  oprescu_mm$GeneName) {

      tab2plot <- oprescu_mm %>%
        filter(GeneName == input$genename ) %>%
        filter(CellType %in% input$cell_types )

      tab2plot$TimePoint <- fct_relevel(tab2plot$TimePoint, c("Non_Inj","D0_5" ,"D2" ,"D3_5" ,"D5","D10","D21"))

      #---------- plot-1 ---------------------#
      p1 <- ggplot(tab2plot , aes(x=as.factor(TimePoint), y=MeanExpr, fill=TimePoint)) +
            geom_bar(stat="identity", width=0.9, position="dodge" ) +
            geom_errorbar(aes(ymin=MeanExpr-SEM, ymax=MeanExpr+SEM, color=TimePoint),
                          position="dodge", width=0.9, size=0.3 )
      p1.theme <- p1 +
        facet_wrap(~ CellType) +
        ggtitle(input$genename) +
        theme(axis.text.x = element_text(angle = 45,vjust=0.5),
              axis.title.x = element_text(vjust= -2))

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
