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




# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Lost learning due to COVID 19"),
    numericInput("working_life", "Working life (years)", 37),
    numericInput("age_start", "Age to start work", 30),
    numericInput("percent_of_year_employed", "Percent of the year usually spent employed during working life", .9,step = .1),
    numericInput("amount_of_time_lost", "Learning lost (number of years)", .2 ,step = .1),
    numericInput("income_lost_per_year", "Income lost for ever year of learning lost", .1,step = .1),
    numericInput("discount_rate", "Discount rate", 1.03,step = .005),
    numericInput("cpi_inflation", "Inflation (CPI)", 1.025,step = .005),
    numericInput("wage_inflation", "Wage growth (nominal)", 1.035,step = .005),
    checkboxGroupInput("ntiles_lost", "Which deciles affected?", seq(1,10),selected = c(1,2,3)),
    tableOutput("text_output"),
    textOutput("text")
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$text_output <- renderTable(data %>% 
        mutate(year_start = (input$age_start-age)+2020,
               year_retire = year_start + input$working_life) %>% 
     filter(n_tile %in% input$ntiles_lost,
           year >= year_start,
           year <  year_retire)  %>%
        mutate(wage_index            = if_else( year == 0,1,1 * input$wage_inflation^(year_index)),
               cpi_index             = 1/if_else( year == 0,1,1 * input$cpi_inflation^(year_index)),
               discount_rate_index   = 1/if_else( year == 0,1,1 * input$discount_rate^(year_index)),
               wage_nominal          = wage_index * wage_original_2020,
               wage_discounted       = wage_nominal*cpi_index*discount_rate_index,
               wage_discounted_ntile = wage_discounted * ratio_to_av,
               n_in_each_ntile = n/10,
               income_pp         = wage_discounted_ntile*52*input$percent_of_year_employed,
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
          mutate_each(funs(prettyNum(., big.mark=",",digits = 1)))
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
