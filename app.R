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

#read in dataset for 2016 election returns
election_returns_2016 <- read.csv("data/Source Data/election_returns_2012_2016.csv")

#read in 2020 election returns % Biden, % Trump, population
election_returns_2020<- read.csv("data/Clean Data/election_results_2020_population_percent.csv")

#read in covid results
covid_data <- read.csv("data/COVID/covid_data.csv")

# Load in predictions
electoral_preds <- readRDS("data/electoral_college_predictions.rds")

# Load in raw predictions
load("data/raw_predictions.rda")
rm(mlr_model, svm_model, xgb_model)

# Region information
geocodes <- readxl::read_excel("data/Source Data/state-geocodes-v2017.xlsx", skip= 5)
# Create division field
geocodes$division <- ifelse(str_detect(geocodes$Name, "Division"), geocodes$Name, NA)
geocodes$region <- ifelse(str_detect(geocodes$Name, "Region"), geocodes$Name, NA)

geocodes <- geocodes %>% 
    fill(division, .direction = "down")

geocodes <- geocodes %>% 
    fill(region, .direction = "down")


#Dataset for voter turnout/population to assess proxy of population for voter turnout (which we mutiply by the ratio of democrats/republican votes for each county)
turnout_pop_2016 <- election_returns_2016 %>% filter(year==2016) %>%
    select(FIPS, totalvotes) %>%
    left_join(dataset_2016 %>% select(fips, popestimate),by=c("FIPS"="fips")) %>%
    mutate(turnout_population_ratio = totalvotes/popestimate)

#reformat 2020 election returns to get democratic/republic vote ratio by state
election_returns_totals_2020 <- election_returns_2020 %>% select(state, COUNTY, BIDEN, TRUMP, POPULATION) %>% mutate(biden_votes = BIDEN*POPULATION, trump_votes =TRUMP*POPULATION)
state_ratio_2020 <- election_returns_totals_2020 %>% group_by(state) %>% summarise(vote_ratio = sum(biden_votes,na.rm = TRUE)/sum(trump_votes,na.rm = TRUE), .groups = 'drop')

#manually calculate RAtio for  
#Taken from: https://www.nytimes.com/interactive/2020/11/03/us/elections/results-president.html
arizona_ratio <- 49.36/49.06 
colorado_ratio <- 55.4/41.9
new_mexico_ratio <- 54.3/43.5
wyoming_ratio <- 69.9/26.6
alaska_ratio <- 52.8/42.8

state_ratio_2020$vote_ratio[which(state_ratio_2020$state=="Arizona")] <- arizona_ratio
state_ratio_2020$vote_ratio[which(state_ratio_2020$state=="Colorado")] <- colorado_ratio
state_ratio_2020$vote_ratio[which(state_ratio_2020$state=="New Mexico")] <- new_mexico_ratio
state_ratio_2020$vote_ratio[which(state_ratio_2020$state=="Wyoming")] <- wyoming_ratio
state_ratio_2020 <- rbind(state_ratio_2020, data.frame(state="Alaska", vote_ratio=alaska_ratio))

#roll up covid data to state level
state_covid <- covid_data %>% group_by(state) %>% 
    summarise(total_population = sum(population), total_cases = sum(X..total.covid.cases), cases_per_100k = total_cases/total_population*100000, .groups='drop')

#combine 2020 election returns and covid data to compare how elections went with covid case counts for each state
vote_ratio_covid <- state_ratio_2020 %>% left_join(state_covid %>% select(state, cases_per_100k), by = "state")
vote_ratio_covid$vote_color <- ifelse(vote_ratio_covid$vote_ratio>1,"blue","red")
vote_ratio_covid <- vote_ratio_covid[complete.cases(vote_ratio_covid),]  #DC was removed

#create list of countries for drop-down selection
var_names <- c("FIPS", "Campaign Funds (Democrat)", "Campaign Funds (Republican)",
               "Population estimate", "Net Migration", "Race (White)", 
               "Race (Black)", "Race (Hispanic)", "Race (Asian)",
               "Age (0-19 years)", "Age (20-39 years)", "Age (40-59 years)",
               "Age (60-79 years)", "Age (80 years or older)", "Consumer Spending (Financial Services and Insurance)",
               "Consumer Spending (Gasoline and other Energy Goods)", 
               "Consumer Spending (Health Care)", "Consumer Spending (Other Nondurable Goods)",
               "Consumer Spending (Personal Consumption Expenditures)",
               "Consumer Spending (Food)", "Consumer Spending (Household)",
               "Consumer Spending (Non Profit)", "Consumer Spending (Nondurable Goods)",
               "Consumer Spending (Durable Goods)", "Consumer Spending (Goods, Clothing, Footwear)",
               "Consumer Spending (Services)", "Consumer Spending (Recreation)",
               "Consumer Spending (Transportation)", "Raw GDP", "GDP change",
               "Unemployment", "Mean Poll Outcome (Democrats)", "Median Poll Outcome (Democrats)",
               "Standard Deviation Poll Outcome (Democrats)", "Mean Poll Outcome (Republicans)", 
               "Median Poll Outcome (Republicans)", "Standard Deviation Poll Outcome (Republicans)",
               "Election Returns Consistency (Democrats)", "Election Returns Consistency (Republicans)", "Ratio of % Votes for Democrats to % Votes for Republicans") 
variable_choices <- setNames(names(dataset_2016), var_names)

# Create list of radio button choices
map_choices <- c("State" = "states", "County" = "counties")

# Model choices
model_choices = c("Multiple Logistic Regression" = "mlr", "Support Vector Machine" = "svm", "XGBoost" = "xgb")

# Function to calculate State categories from county percentages
get_county <- function(df, model_pred) {
    df %>% mutate(outcome = ifelse(df[model_pred] > 1, "Democrat", "Republican")) %>% 
        distinct(fips, outcome) %>% return()
}


get_state <- function(df) {
    df %>% 
        mutate(
            outcome = ifelse(state_win == 1, "Democrat", "Republican")
        ) %>% 
        distinct(state_fips, outcome) %>% 
        rename(fips = state_fips) %>% return()
        
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
                                      p("Explore the relationship between 2 variables in our election dataset"),
                                      br()
                               ), # end of first column
                               
                               # 2nd column: dropdown menu to select variable 1
                               column(3,
                                      selectInput(inputId = "variable_1", label = "Choose a variable:",
                                                  choices = variable_choices, selected = "race_black")
                               ), # end of second column 
                               # 3nd column: dropdown menu to select variable 2
                               column(3,
                                      selectInput(inputId = "variable_2", label = "Choose a variable:",
                                                  choices = variable_choices, selected = "dem_rep_ratio")
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
                                      plotOutput("hist") #histogram to see distribution of selected variable in 2016
                               ),
                               column(6,
                                      plotOutput("hist_2") #histogram to see distribution of selected variable in 2020
                               ),
                           ),
                           fluidRow(
                               column(8,
                                      plotOutput("hist_3") #histogram to see distribution of voter turnout/population for 2016
                               ),
                           )
                  ), # end of EDA tab panel
                  
                  # Extra viz tab (we can move this around)
                  tabPanel("Viz",
                           fluidRow(
                               column(3,
                                      selectInput(inputId = "variable_box", label = "Choose a variable:",
                                                  choices = variable_choices, selected = "race_black")
                               )
                           ),
                           fluidRow(
                               column(1),
                               column(10,
                                      wellPanel("2016",
                                                plotOutput("boxplot_16")
                                                )
                                      ),
                               column(1)
                           ),
                           fluidRow(
                               column(1),
                               column(10,
                                      wellPanel("2020",
                                                plotOutput("boxplot_20")
                                                )
                               ),
                               column(1)
                           )
                           
                           ),
                  
                tabPanel("Results",
                         fluidRow(
                             radioButtons("type","Electoral College or County-Level Results", choices = map_choices, selected = "states"),selectInput("model", "Choose a model", choices = model_choices, selected = "Multiple Linear Regression")
                             
                             ), # end fluidRow
                         fluidRow(
                             column(1),
                             column(5,
                                    wellPanel("Actual Results 2020",
                                              plotOutput("actual_2020")
                                              )
                                    ), # end column
                             column(5,
                                    wellPanel("Predicted Results 2020",
                                              plotOutput("predict_2020")
                                    ) # end wellPanel
                             ),
                             column(1)
                             ), # end fluidRow
                         fluidRow(
                            column(10,
                                   wellPanel("Electoral Votes by Party",
                                             tableOutput("electoral_votes")
                                )
                            )
                        ),
                        fluidRow(
                            column(7,
                                   wellPanel("2020 Democratic %/Republican % Ratio vs Covid Cases Per 100K (click on point to see state info)",
                                             plotOutput("covid_elections",
                                                        click="scatterPlot_click"
                                                        )
                                             ), # end wellPanel
                            ) #end column
                        ), #end fluidRow
                        #create point click to get more info on a point
                        fluidRow(
                            column(width=6,
                                   h4("Points near click"),
                                   verbatimTextOutput("click_info")
                            ) #end of column  
                        ) #end of fluidRow
                        
                ) # end tabPanel
            
            
) #end of navbarPage

server <- function(input, output) {
    

# EDA TAB ---------------------------------------------------------------


    #scatterplot plot for 2 variables for 2016 and 2020
    output$scatterPlot <- renderPlot({
        ggplot(dataset_2016, aes_string(x = input$variable_1, y = input$variable_2)) +
            geom_point(colour="red")+
            xlab(names(variable_choices)[which(variable_choices == input$variable_1)]) +
            ylab(names(variable_choices)[which(variable_choices == input$variable_2)]) +
            ggtitle("2016 scatterplot") +
            theme_bw()
    }) # end of renderPlot
    
    output$scatterPlot2 <- renderPlot({
        ggplot(dataset_2020, aes_string(x = input$variable_1, y = input$variable_2)) +
            xlab(names(variable_choices)[which(variable_choices == input$variable_1)]) +
            ylab(names(variable_choices)[which(variable_choices == input$variable_2)]) +
            geom_point(colour="blue")+
            ggtitle("2020 scatterplot") +
            theme_bw()
    }) # end of renderPlot

    output$Corr_matrix <- renderPlot({
        corrplot(cor(select(dataset_2016,contains(input$corr)),
                     use = "pairwise.complete.obs"),method="number",title="2016 Correlation Matrix",mar=c(0,0,2,0),col=col4(10),number.cex=0.50)
    })
    
    output$Corr_matrix_2 <- renderPlot({
        corrplot(cor(select(dataset_2020,contains(input$corr)),
                     use = "pairwise.complete.obs"),method="number",title="2020 Correlation Matrix",mar=c(0,0,2,0),col=col4(10),number.cex=0.50)
    })
    
    output$hist <-renderPlot({
        ggplot(dataset_2016, aes_string(input$hist_selection))+
            geom_histogram(fill="red", color = "grey")+
            xlab(names(variable_choices)[which(variable_choices == input$hist_selection)]) +
            ylab("Count") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            ggtitle("2016 histogram")
    })
    
    output$hist_2 <-renderPlot({
        ggplot(dataset_2020, aes_string(input$hist_selection))+
            geom_histogram(fill="blue", color = "grey")+
            xlab(names(variable_choices)[which(variable_choices == input$hist_selection)]) +
            ylab("Count") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            ggtitle("2020 histogram")
    })

    output$hist_3 <-renderPlot({
        ggplot(turnout_pop_2016, aes(turnout_population_ratio))+
            geom_histogram(fill="green", color = "grey")+
            xlab("Voter Turnout/Population") +
            ylab("Count") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            ggtitle("Distribution of 2016 Voter Turnout/Population by County")
    })
    

# MAP TAB ---------------------------------------------------------------
    actual_2020 <- reactive({
        if(input$type == "states") {
            get_state(electoral_preds[["actual"]])
        } else {
            get_county(test, "dem_rep_ratio")
        }
    })
    
    
    ## Get data sets
    pred_2020 <- reactive({
        if(input$type == "states") {
            get_state(electoral_preds[[input$model]])
        } else {
            get_county(test, input$model)
        }
    })
    
    # Create base map plots
    actual_maps_2020 <- reactive(plot_usmap(regions = input$type, data = actual_2020(), values = "outcome"))
    pred_maps_2020 <- reactive(plot_usmap(regions = input$type, data = pred_2020(), values = "outcome"))
    

    ## Plots
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
    
    
    output$predict_2020 <- renderPlot({
        pred_maps_2020() +
            theme_bw()+
            scale_fill_manual(name = "Winner", values = c("blue", "red")) +
            theme(panel.grid.major = element_blank(),
                  panel.background = element_blank(),
                  axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank())
    })
    
    output$covid_elections <- renderPlot({
        ggplot(vote_ratio_covid, aes(x=cases_per_100k, y=vote_ratio))+
            geom_point(aes(colour=vote_color), size = 4)+scale_color_manual(values = c("blue", "red"))+
            xlab("State") +
            ylab("% dem/% rep votes ratio") +
            ylim(0,3)+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            theme(legend.position = "none")
    })
    
    #shows state, dem/rep ratio, and covid cases / 100K for all points near the click
    output$click_info <- renderPrint({
        nearPoints(vote_ratio_covid %>% .[,c("state","cases_per_100k","vote_ratio")],
                   input$scatterPlot_click, addDist = FALSE)
    }) #end of renderPrint
    
    output$electoral_votes <- renderTable({
        
        model_electoral_votes <- colSums(electoral_preds[[input$model]][c("dem_electoral_votes","rep_electoral_votes")])
        actual_electoral_votes <- colSums(electoral_preds[["actual"]][c("dem_electoral_votes","rep_electoral_votes")])
        data.frame(model=c(input$model,"actual"),
                   `Total Democratic Electoral Votes`=c(model_electoral_votes["dem_electoral_votes"],
                                                        actual_electoral_votes["dem_electoral_votes"]),
                   `Total Republican Electoral Votes`=c(model_electoral_votes["rep_electoral_votes"],
                                                        actual_electoral_votes["rep_electoral_votes"])
        )
    })
    
    # Box plots
    output$boxplot_16 <- renderPlot({
        # Add on region
        e_16 <- dataset_2016 %>% 
            mutate(state_fips = str_sub(fips, 1,2)) %>% 
            left_join(geocodes %>% select(`State (FIPS)`, region), by = c("state_fips" = "State (FIPS)"))
        
        # Plot
        e_16 %>% 
            ggplot(aes_string(x = "state_fips", y = input$variable_box, fill = "region")) +
            scale_y_log10() +
            geom_boxplot() +
            xlab("State") +
            ylab(names(variable_choices)[which(variable_choices == input$bariable_box)]) + 
            theme_bw() +
            theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
        
    })
    
    output$boxplot_20 <- renderPlot({
        # Add on region
        e_20 <- dataset_2020 %>% 
            mutate(state_fips = str_sub(fips, 1,2)) %>% 
            left_join(geocodes %>% select(`State (FIPS)`, region), by = c("state_fips" = "State (FIPS)"))
        
        # Plot
        e_20 %>% 
            ggplot(aes_string(x = "state_fips", y = input$variable_box, fill = "region")) +
            scale_y_log10() +
            geom_boxplot() +
            xlab("State") +
            ylab(names(variable_choices)[which(variable_choices == input$variable_box)]) + 
            theme_bw() +
            theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
        
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)