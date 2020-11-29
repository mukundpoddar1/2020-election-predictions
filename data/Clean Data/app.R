#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(forcats)
library(dslabs)
library(corrplot)

#final datasets for 2016 and 2020
dataset_2016 <- read.csv("../merged_final_2016.csv")
dataset_2020 <- read.csv("../merged_final_2020.csv")

#create list of countries for drop-down selection
variable_choices <- setNames(names(dataset),names(dataset))

ui <- fluidPage(
        #second tab for scatter plot
            # Application title
            titlePanel("EDA"),
            
            # first row: interactive elements
            fluidRow(
                # first column: explanatory text
                column(3,
                       p("explore the relationship between 2 variables in the election dataset"),
                       br()
                ), # end of first column
                
                # 2nd column: dropdown menu to select variable 1
                column(3,
                       selectInput(inputId = "variable_1", label = "Choose a variable:",
                                   choices = variable_choices)
                ), # end of second column 
                # 3nd column: dropdown menu to select variable 2
                column(3,
                       selectInput(inputId = "variable_2", label = "Choose a variable:",
                                   choices = variable_choices)
                ), # end of third column
                
            ), # end of fluidRow
            
            # plot output and text output
            fluidRow(
                column(6,
                       plotOutput("scatterPlot")
                ), # end of column 1
                column(6,
                       plotOutput("scatterPlot2")
                ), # end of column 1
            ), #end of fluidRow
            
                fluidRow(
                # 5th column: input up to 10 predictors
                column(7,
                    selectizeInput(inputId = "corr", label = "Select up to 10 variables for correlation plot",
                                   choices = variable_choices,options=list(maxItems=10),
                                   selected = "unemployment")
                ), #end column
            ), #end fluidRow
            
            fluidRow(
                column(9,
                       plotOutput("Corr_matrix")
                ),
            ),
            fluidRow(
                column(9,
                       plotOutput("Corr_matrix_2")
                ),
            ),
            fluidRow(
                # another column, select variable to see distribution
                column(3,
                       selectInput(inputId = "hist_selection", label = "Choose a variable:",
                                   choices = variable_choices,
                                   selected = "unemployment")
                ) #end of column
            ), # end of fluidRow 
            fluidRow(
                column(6,
                       plotOutput("hist")
                ),
                column(6,
                       plotOutput("hist_2")
                ),
            )
) #end of fluidPage

server <- function(input, output) {
    #scatterplot plot for 2 variables for 2016 and 2020
    output$scatterPlot <- renderPlot({
        ggplot(dataset_2016, aes_string(x = input$variable_1, y = input$variable_2)) +
            geom_point()+ggtitle("2016 scatterplot")
    }) # end of renderPlot
    
    output$scatterPlot2 <- renderPlot({
        ggplot(dataset_2020, aes_string(x = input$variable_1, y = input$variable_2)) +
            geom_point()+ggtitle("2020 scatterplot")
    }) # end of renderPlot

    output$Corr_matrix <- renderPlot({
        corrplot(cor(select(dataset_2016,contains(input$corr)),
                     use = "pairwise.complete.obs"),method="number",title="2016 Correlation Matrix",mar=c(0,0,2,0))
    })
    
    output$Corr_matrix_2 <- renderPlot({
        corrplot(cor(select(dataset_2020,contains(input$corr)),
                     use = "pairwise.complete.obs"),method="number",title="2020 Correlation Matrix",mar=c(0,0,2,0))
    })
    
    output$hist <-renderPlot({
        ggplot(dataset_2016, aes_string(input$hist_selection))+geom_histogram()+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("2016 histogram")
    })
    
    output$hist_2 <-renderPlot({
        ggplot(dataset_2020, aes_string(input$hist_selection))+geom_histogram()+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("2020 histogram")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)