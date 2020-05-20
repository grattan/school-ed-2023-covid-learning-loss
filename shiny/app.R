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
library(data.table)
ntiles_lost <- c(1,2,3)
data <- read_csv("data.csv")




# Define UI for application - input variables
ui <- fluidPage(
    # Application title
    titlePanel("Lost learning due to COVID 19"),
    numericInput("working_life", "Working life (years)", 37),
    numericInput("age_start", "Age to start work", 30),
    numericInput("percent_of_year_employed", "Percent of the year usually spent employed during working life", 1,step = .1),
    numericInput("amount_of_time_lost", "Learning lost (number of years)", .2 ,step = .1),
    numericInput("income_lost_per_year", "Income lost for ever year of learning lost", .1,step = .1),
    numericInput("discount_rate", "Discount rate", 1.04,step = .005),
    numericInput("cpi_inflation", "Inflation (CPI)", 1.025,step = .005),
    numericInput("wage_inflation", "Wage growth (nominal)", 1.035,step = .005),
    checkboxGroupInput("ntiles_lost", "Which deciles affected?", seq(1,10),selected = ntiles_lost),
    tableOutput("text_output"),
    textOutput("text")
    )


# Define how those inputs are converted to outputs
server <- function(input, output) {
    
    output$text_output <- renderTable(data %>% 
        #Find the year that people start work
        mutate(year_start = (input$age_start-age)+2020,
               year_retire = year_start + input$working_life) %>% 
        #Filter only to those who fall behind (usually the bottom 3 deciles), and look at the years in which they will be working. 
     filter(n_tile %in% input$ntiles_lost,
           year >= year_start,
           year <  year_retire)  %>%
        mutate(wage_index            = if_else( year == 0,1,1 * input$wage_inflation^(year_index)), # A wage index - goes up every year with wages
               cpi_index             = 1/if_else( year == 0,1,1 * input$cpi_inflation^(year_index)), # A CPI index - goes down every year with CPI
               discount_rate_index   = 1/if_else( year == 0,1,1 * input$discount_rate^(year_index)), # A discount rate index - goes down every year wtih discount rates
               wage_nominal          = wage_index * wage_original_2020,
               wage_discounted       = wage_nominal*cpi_index*discount_rate_index,
               #Compare the wage discounted to wages - discounted for your decile
               wage_discounted_ntile = wage_discounted * ratio_to_av,
               n_in_each_ntile = n/10,
               #Find your income pre covid. 
               income_pp         = wage_discounted_ntile*52*input$percent_of_year_employed,
               #Find out your post covid income. 
               income_lost_pp    = income_pp * input$amount_of_time_lost * input$income_lost_per_year,
               income_total      = n_in_each_ntile*income_pp,
               income_lost_total = n_in_each_ntile*income_lost_pp) %>%
        group_by(state,age,n) %>%
        summarise_at(vars(income_pp,
                          income_lost_pp,
                          income_total,
                          income_lost_total),
                     funs(sum)) %>%
         group_by(state) %>%
         summarise(`Lifetime income - per person (thousand)` = mean(income_pp)/1000,
                   `Income lost due to covid - per person (thousand)` = mean(income_lost_pp)/1000,
                   `Total lifetime income of all people in the model (bn)` = sum(income_total)/10^9,
                   `Income lost due to covid - total (bn)` = sum(income_lost_total)/10^9,
                   `Number of students (1,000)` = sum(n)/1000) %>% 
          mutate_each(funs(prettyNum(., big.mark=",",digits = 2)))
    )
    
    output$text <- renderText("All values expressed as NPV in $2020. We first take the total average weekly earnings from the ABS labour survey November 2019. 
                               We then inflate that by wages to get average earnings in each year. 
                              For each decile of child we then find the ratio of the average income within that decile of workers to the average of all workers within that state. 
                              So for instance, in NSW the average worker in the 1st decile earns 15% of what the average worker in NSW earns.
                              We multiply the average income, by that child's decile's ratio and by the chances they will be employed to get their total income. 
                              The figure is then multiplied by a CPI deflator and a NPV deflator to get the NPV of their earnings in that year.")
}

# Run the application 
shinyApp(ui = ui, server = server)
