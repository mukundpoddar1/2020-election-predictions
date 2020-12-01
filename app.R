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
library(usmap)

#final datasets for 2016 and 2020
dataset_2016 <- read.csv("data/merged_final_2016.csv")
dataset_2020 <- read.csv("data/merged_final_2020.csv")

#create list of countries for drop-down selection
variable_choices <- setNames(names(dataset_2016),names(dataset_2016))

# Create list of radio button choices
map_choices <- c("State" = "states", "County" = "counties")

# Function to calculate State categories from county percentages
get_county <- function(df) {
    df %>% 
        mutate(outcome = ifelse(democrats_pct > republicans_pct, "Democrat", "Republican")) %>% 
        distinct(fips, outcome) %>% return()
}


get_state <- function(df) {
    df %>% 
        mutate(state_fips = str_sub(str_sub(paste0("0",fips),-5), 1,2),
               dem_pop = democrats_pct*popestimate,
               rep_pop = republicans_pct*popestimate) %>%
        group_by(state_fips) %>% 
        summarize(
            total_dems = sum(dem_pop, na.rm = T),
            total_reps = sum(rep_pop, na.rm = T),
            outcome = ifelse(total_dems > total_reps, "Democrat", "Republican"),
            state_fips = as.numeric(state_fips)
        ) %>% 
        ungroup() %>% 
        distinct(state_fips, outcome) %>% rename(fips = state_fips) %>% return()
        
}

# colors for correlation matrix
col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
                           "cyan", "#007FFF", "blue","#00007F"))

ui <- navbarPage( title = "Can we predict the US Presidential Elections?",
                  tabPanel("EDA",
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
                  ), # end of EDA tab panel
                  
                tabPanel("Results",
                         fluidRow(
                             radioButtons("type","Electoral College or County-Level Results", choices = map_choices, selected = "states")
                             ), # end fluidRow
                         fluidRow(h3("Actual Results")),
                         fluidRow(
                             column(6,
                                    wellPanel("2016",
                                        plotOutput("actual_2016")
                                        ) # end wellPanel
                                    ),
                             column(6,
                                    wellPanel("2020",
                                              plotOutput("actual_2020")
                                              )
                                    ) # end column
                         ), # end fluidRow
                         fluidRow(h3("Predicted Results"),
                                  selectInput("model", "Choose a model", choices = c("Logistic Regression", "Random Forest", "XGBoost", "Support Vector Machines"), selected = "Logistic Regression")
                                  ),
                         fluidRow(
                             column(6,
                                    wellPanel("2016",
                                              plotOutput("predict_2016")
                                    ) # end wellPanel
                             ),
                             column(6,
                                    wellPanel("2020",
                                              plotOutput("predict_2020")
                                    )
                             ) # end column
                         ) # end fluidRow
                         ) # end tabPanel
            
            
) #end of navbarPage

server <- function(input, output) {
    

# EDA TAB ---------------------------------------------------------------


    #scatterplot plot for 2 variables for 2016 and 2020
    output$scatterPlot <- renderPlot({
        ggplot(dataset_2016, aes_string(x = input$variable_1, y = input$variable_2)) +
            geom_point(colour="red")+ggtitle("2016 scatterplot")
    }) # end of renderPlot
    
    output$scatterPlot2 <- renderPlot({
        ggplot(dataset_2020, aes_string(x = input$variable_1, y = input$variable_2)) +
            geom_point(colour="blue")+ggtitle("2020 scatterplot")
    }) # end of renderPlot

    output$Corr_matrix <- renderPlot({
        corrplot(cor(select(dataset_2016,contains(input$corr)),
                     use = "pairwise.complete.obs"),method="number",title="2016 Correlation Matrix",mar=c(0,0,2,0),col=col4(10))
    })
    
    output$Corr_matrix_2 <- renderPlot({
        corrplot(cor(select(dataset_2020,contains(input$corr)),
                     use = "pairwise.complete.obs"),method="number",title="2020 Correlation Matrix",mar=c(0,0,2,0),col=col4(10))
    })
    
    output$hist <-renderPlot({
        ggplot(dataset_2016, aes_string(input$hist_selection))+geom_histogram(fill="red")+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("2016 histogram")
    })
    
    output$hist_2 <-renderPlot({
        ggplot(dataset_2020, aes_string(input$hist_selection))+geom_histogram(fill="blue")+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("2020 histogram")
    })
    

# MAP TAB ---------------------------------------------------------------
    ## Get data sets
    actual_2016 <- reactive({
        if(input$type == "states") {
            get_state(dataset_2016)
        } else {
            get_county(dataset_2016)
        }
    })
    
    actual_2020 <- reactive({
        if(input$type == "states") {
            get_state(dataset_2020)
        } else {
            get_county(dataset_2020)
        }
    })
    
    # Create base map plots
    actual_maps_2016 <- reactive(plot_usmap(regions = input$type, data = actual_2016(), values = "outcome"))
    actual_maps_2020 <- reactive(plot_usmap(regions = input$type, data = actual_2020(), values = "outcome"))
    

    ## Plots
    output$actual_2016 <- renderPlot({
            actual_maps_2016() +
            theme_bw()+
            scale_fill_manual(name = "Winner", values = c("blue", "red")) +
            theme(panel.grid.major = element_blank(),
                  panel.background = element_blank(),
                  axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank())
    })
    
    output$actual_2020 <- renderPlot({
            actual_maps_2020() +
            theme_bw()+
            scale_fill_manual(name = "Winner", values = c("blue", "red")) +
            theme(panel.grid.major = element_blank(),
                  panel.background = element_blank(),
                  axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank())
    })
    
    ## Plots (replace with predicted data)
    output$predict_2016 <- renderPlot({
        actual_maps_2016() +
            theme_bw()+
            scale_fill_manual(name = "Winner", values = c("blue", "red")) +
            theme(panel.grid.major = element_blank(),
                  panel.background = element_blank(),
                  axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank())
    })
    
    output$predict_2020 <- renderPlot({
        actual_maps_2020() +
            theme_bw()+
            scale_fill_manual(name = "Winner", values = c("blue", "red")) +
            theme(panel.grid.major = element_blank(),
                  panel.background = element_blank(),
                  axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank())
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)