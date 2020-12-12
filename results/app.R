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
dataset_2016 <- read.csv("../data/merged_final_2016.csv")
dataset_2020 <- read.csv("../data/merged_final_2020.csv")

#read in dataset for 2016 election returns
election_returns_2016 <- read.csv("../data/Source Data/election_returns_2012_2016.csv")

#read in 2020 election returns % Biden, % Trump, population
election_returns_2020<- read.csv("../data/Clean Data/election_results_2020_population_percent.csv")

#read in covid results
covid_data <- read.csv("../data/COVID/covid_data.csv")

#load covid data
state_covid_data <- readRDS("../data/COVID/state_us_jhu_clean.rds")

#pull covid data for the day before elections
nov_2nd_covid_data<- state_covid_data %>% filter(report_date=="2020-11-02") 

# Load in predictions
electoral_preds <- readRDS("../data/electoral_college_predictions.rds")

# Load in raw predictions
test <- readRDS("../data/test_with_rf.rds")

# Load in electoral college resutlts, RMSE, and R2 calculated from scripts/ml_models.rmd
load("r2.rda")
load("rmse_df.rda")
load("electoral_results_df.rda")

# Region information
geocodes <- readxl::read_excel("../data/Source Data/state-geocodes-v2017.xlsx", skip= 5)
# Create division field
geocodes$division <- ifelse(str_detect(geocodes$Name, "Division"), geocodes$Name, NA)
geocodes$region <- ifelse(str_detect(geocodes$Name, "Region"), geocodes$Name, NA)

geocodes <- geocodes %>% 
    fill(division, .direction = "down")

geocodes <- geocodes %>% 
    fill(region, .direction = "down")


#Dataset for voter turnout/population to assess proxy of population for voter turnout (which we multiply by the ratio of democrats/republican votes for each county)
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

#rollup state population from first covid_data file
state_population <- covid_data %>% group_by(state) %>% summarise(total_population=sum(population), .groups="drop")

#roll up covid data to state level
nov_2nd_covid_data_per_capita <- nov_2nd_covid_data %>% 
    left_join(state_population, by="state") %>%
    mutate(cases_per_100k = confirmed_cases/total_population*100000)

#combine 2020 election returns and covid data to compare how elections went with covid case counts for each state
vote_ratio_covid <- state_ratio_2020 %>% left_join(nov_2nd_covid_data_per_capita %>% select(state, cases_per_100k), by = "state")
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
model_choices = c("Multiple Logistic Regression" = "mlr", "Support Vector Machine" = "svm", "XGBoost" = "xgb", "Random forest" = "rf")

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
                tabPanel("Results",
                         fluidRow(
                             radioButtons("type","Electoral College or County-Level Results", choices = map_choices, selected = "states"),selectInput("model", "Choose a model to display predicted results", choices = model_choices, selected = "Multiple Linear Regression")
                             
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
                            column(6,
                                   wellPanel("Electoral Votes by Party",
                                             tableOutput("electoral_votes")
                                   ) #end wellPanel
                            ), #end column
                            column(3,
                                   wellPanel("RMSE of County Dem/Rep vote ratio",
                                             tableOutput("rmse_ratio")
                                    ) #end wellPanel
                            ), #end column
                            column(3,
                                   wellPanel("R^2 of County Dem/Rep vote ratio",
                                             tableOutput("r2_table")
                                   ) #end wellPanel
                            ) #end column
                        )
                ), # end tabPanel
                tabPanel("COVID-19 trends vs Election results",
                         fluidRow(
                             column(7,
                                    wellPanel("Since our models couldn't be trained on COVID data, we wanted to see if there
                                              a relationship between COVID-19 case counts on Nov 2nd (the day before the election)
                                              and how people ended up voting. Looking at the trend between the ratio of vote percentages
                                              for Biden vs Trump and COVID-19 cases, the states that voted Biden tended to have
                                              lower case counts in general vs the states that voted majority for Trump.",
                                              plotOutput("covid_elections",
                                                         click="scatterPlot_click"
                                              )
                                    ), # end wellPanel
                             ) #end column
                         ), #end fluidRow
                         
                         #create point click to get more info on a point
                         fluidRow(
                             column(width=10,
                                    h4("Points near click"),
                                    verbatimTextOutput("click_info")
                             ) #end of column  
                         ) #end of fluidRow
                ) #end tabPanel
) #end of navbarPage

server <- function(input, output) {
    
# MAP RESULTS TAB ---------------------------------------------------------------
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
            xlab("Number of COVID-19 Cases on Nov 2, 2020 by State") +
            ylab("% dem/% rep votes ratio") +
            ylim(0,3)+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            theme(legend.position = "none")+
            ggtitle("2020 % Votes Biden / % Votes Trump Ratio vs Covid Cases Per 100K \n(click on point to see state info)")+
            theme(plot.title = element_text(hjust = 0.5))
    })
    
    #shows state, dem/rep ratio, and covid cases / 100K for all points near the click
    output$click_info <- renderPrint({
        nearPoints(vote_ratio_covid %>% .[,c("state","cases_per_100k","vote_ratio")],
                   input$scatterPlot_click, addDist = FALSE)
    }) #end of renderPrint
    
    output$electoral_votes <- renderTable({
        electoral_results_df %>% arrange(Models)
    })
    
    output$rmse_ratio <- renderTable({
        rmse_df %>% arrange(Root.MSE)
    })
    
    output$r2_table <- renderTable({
        r2 %>% arrange(R.Squared)
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)