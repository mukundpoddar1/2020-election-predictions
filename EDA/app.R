#########################
# Title: BST 260 Project
# Purpose: Exploratory Data Analysis - Shiny app
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

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
# Region information
geocodes <- readxl::read_excel("../data/Source Data/state-geocodes-v2017.xlsx", skip= 5)

# Create division field
geocodes$division <- ifelse(str_detect(geocodes$Name, "Division"), geocodes$Name, NA)
geocodes$region <- ifelse(str_detect(geocodes$Name, "Region"), geocodes$Name, NA)

# Fill in division and region fields down to create full columns
geocodes <- geocodes %>% 
  fill(division, .direction = "down")

geocodes <- geocodes %>% 
  fill(region, .direction = "down")

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

# colors for correlation matrix
col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
                           "cyan", "#007FFF", "blue","#00007F"))

ui <- navbarPage(title = "Exploratory Analysis",
  ## Scatter Plot tab
  tabPanel("Scatter Plots",
           fluidRow(
             h4("Choose two variables two explore their relationship in our datasets"),
             splitLayout(
               verticalLayout(
                 selectInput(inputId = "variable_1", label = "Choose a variable:",
                             choices = variable_choices, selected = "race_black"),
                 column(12, align = "center", h5("2016 Data")),
                 plotOutput("scatterPlot")
               ),
               verticalLayout(
                 selectInput(inputId = "variable_2", label = "Choose a variable:",
                             choices = variable_choices, selected = "dem_rep_ratio"),
                 column(12, align = "center", h5("2020 Data")),
                 plotOutput("scatterPlot2")
               )

             ) # end of splitLayout
           ) # end of fluidRow
           ), # end of tabPanel: scatter plot  
  
  ## Correlation tab
  tabPanel("Correlation",
           fluidRow(
             h4("Choose variables to explore correlation in our datasets"),
             selectizeInput(inputId = "corr", label = "Select up to 10 variables for correlation plot",
                            choices = variable_choices,options=list(maxItems=10),
                            selected = "unemployment")
           ), # end of fluidRow
           fluidRow(
             splitLayout(
               verticalLayout(
                 column(12, align ="center", h5("2016 Corelation Matrix")),
                 plotOutput("Corr_matrix")
               ),
               verticalLayout(
                 column(12, align ="center", h5("2020 Corelation Matrix")),
                 plotOutput("Corr_matrix_2")
               )
             ) # end of splitLayout
           ) #end of fluidRow
           ), # end of tabPanel: correlation 
  
  ## Distributions tab
  tabPanel("Distributions",
           fluidRow(
             selectInput(inputId = "hist_selection", label = "Choose a variable:",
                         choices = variable_choices,
                         selected = "unemployment")
           ), # end of fluidRow
           fluidRow(
             splitLayout(
               verticalLayout(
                 column(12, align ="center", h5("2016 Histogram")),
                 plotOutput("hist")
               ),
               verticalLayout(
                 column(12, align ="center", h5("2020 Histogram")),
                 plotOutput("hist_2")
               )
             ) # end of splitLayout
           ) #end of fluidRow
           ), # end of tabPanel: distributions
  
  ## State Distributions tab
  tabPanel("State Distributions",
           fluidRow(
             selectInput(inputId = "variable_box", label = "Choose a variable:",
                         choices = variable_choices, selected = "race_black")
           ), # end of fluidRow
           fluidRow(
             verticalLayout(
               column(12, align ="center", h5("2016 Distribution by State")),
               plotOutput("boxplot_16"),
               column(12, align ="center", h5("2020 Distribution by State")),
               plotOutput("boxplot_20")
             ) # end of verticalLayout
           ) #end of fluidRow
           ) # end of tabPanel: state distributions
) #end  End of navbarPage

server <- function(input, output) {
  
  #### Scatter Plot
  #scatterplot plot for 2 variables for 2016 and 2020
  output$scatterPlot <- renderPlot({
    ggplot(dataset_2016, aes_string(x = input$variable_1, y = input$variable_2)) +
      geom_point(colour="red")+
      xlab(names(variable_choices)[which(variable_choices == input$variable_1)]) +
      ylab(names(variable_choices)[which(variable_choices == input$variable_2)]) +
      # ggtitle("2016 scatterplot") +
      theme_bw()
  }) # end of renderPlot
  
  output$scatterPlot2 <- renderPlot({
    ggplot(dataset_2020, aes_string(x = input$variable_1, y = input$variable_2)) +
      xlab(names(variable_choices)[which(variable_choices == input$variable_1)]) +
      ylab(names(variable_choices)[which(variable_choices == input$variable_2)]) +
      geom_point(colour="blue")+
      # ggtitle("2020 scatterplot") +
      theme_bw()
  }) # end of renderPlot
  
  #### Correlation
  output$Corr_matrix <- renderPlot({
    corrplot(cor(select(dataset_2016,contains(input$corr)),
                 use = "pairwise.complete.obs"),method="number",mar=c(0,0,2,0),col=col4(10),number.cex=0.70)
  })
  
  output$Corr_matrix_2 <- renderPlot({
    corrplot(cor(select(dataset_2020,contains(input$corr)),
                 use = "pairwise.complete.obs"),method="number",mar=c(0,0,2,0),col=col4(10),number.cex=0.70)
  })
  
  
  #### Variable Distributions 
  output$hist <-renderPlot({
    ggplot(dataset_2016, aes_string(input$hist_selection))+
      geom_histogram(fill="red", color = "grey")+
      xlab(names(variable_choices)[which(variable_choices == input$hist_selection)]) +
      ylab("Count") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$hist_2 <-renderPlot({
    ggplot(dataset_2020, aes_string(input$hist_selection))+
      geom_histogram(fill="blue", color = "grey")+
      xlab(names(variable_choices)[which(variable_choices == input$hist_selection)]) +
      ylab("Count") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  #### State Distributions
  # Box plots
  output$boxplot_16 <- renderPlot({
    # Add on region
    e_16 <- dataset_2016 %>% 
      mutate(state_fips = str_sub(fips, 1,2)) %>% 
      left_join(geocodes %>% select(`State (FIPS)`, region, Name), by = c("state_fips" = "State (FIPS)"))
    
    # Plot
    e_16 %>% 
      ggplot(aes_string(x = "Name", y = input$variable_box, fill = "region")) +
      scale_y_log10() +
      geom_boxplot() +
      xlab("State") +
      ylab(names(variable_choices)[which(variable_choices == input$variable_box)]) + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
  })
  
  output$boxplot_20 <- renderPlot({
    # Add on region
    e_20 <- dataset_2020 %>% 
      mutate(state_fips = str_sub(fips, 1,2)) %>% 
      left_join(geocodes %>% select(`State (FIPS)`, region, Name), by = c("state_fips" = "State (FIPS)"))
    
    # Plot
    e_20 %>% 
      ggplot(aes_string(x = "Name", y = input$variable_box, fill = "region")) +
      scale_y_log10() +
      geom_boxplot() +
      xlab("State") +
      ylab(names(variable_choices)[which(variable_choices == input$variable_box)]) + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)