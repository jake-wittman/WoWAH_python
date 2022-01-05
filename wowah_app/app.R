#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(dtplyr)
library(stringr)
library(magrittr)
library(lubridate)
library(shinythemes)
library(tsibble)
library(fable)
library(feasts)
library(forecast)
source('functions.R')

auction_dt <- fread(here::here('data/latest_month_auctions.csv'))

# Change dates to date time
auction_dt[, date_time := paste0(substr(date_time, 1, 13), ":00:00")]
auction_dt[, date_time := as_datetime(date_time)]
auction_dt[, day := wday(date_time, label = TRUE)]
auction_dt[, hour := hour(date_time)]
auction_dt[, original_cost_g := cost_g]
setkey(auction_dt, name, date_time) # Might make later operations faster

auction_lazy <- lazy_dt(auction_dt) # Allows me to use dplyr syntax for a very minor speed penalty

run_test <- FALSE
if (run_test == TRUE) {
    input <- NA
    input$item <- 'Widowbloom'
test <- auction_lazy %>%
    filter(name %in% input$item) %>%
    group_by(name, date_time) %>%
    slice_min(cost_g, with_ties = FALSE) %>%
    as_tibble() %>%
    as_tsibble(key = name, index = date_time, regular = TRUE) %>%
    fill_gaps() # makes implicit gaps explicit

# Fill NAs with previous values
test <- test %>%
    mutate(fill_prev_value_cost = cost_g) %>%
    fill(fill_prev_value_cost, .direction = 'down')
outliers_and_missing <- tsoutliers(test$cost_g)
test$interp_cost_g <- test$cost_g
test$interp_cost_g[outliers_and_missing$index] <- outliers_and_missing$replacements

ts_cost <- msts(test[[input$fill_NA]], seasonal.periods = c(48, 168))

}
# Define UI
ui <- fluidPage(
    theme = shinytheme('darkly'),
    # Application title
    titlePanel("Malfurion - NA Auction House Data"),
    
    # Tab panel
    sidebarLayout(
        sidebarPanel(
            # Sidebar
            selectInput(
                "item",
                "Item:",
                choices = sort(unique(auction_dt$name)),
                selected = 'Widowbloom',
                selectize = TRUE,
                multiple = FALSE
            ),
            radioButtons(
                'fill_NA',
                label = 'How should missing values be filled?',
                choices = list('Last value carried forward' = 'fill_prev_value_cost'),
                selected = NULL
            ),
            checkboxInput('rm_outliers',
                          label = 'Remove outliers?',
                          value = FALSE),
            checkboxGroupInput(
                'seasonal_periods',
                'Which seasonal periods should be used?',
                choices = list('Daily' = 24,
                               'Weekly' = 168)
            )
        ),
        mainPanel(tabsetPanel(
            tabPanel(
                'Auction House Data',
                fluidRow(# Main panel
                    column(6,
                           plotOutput("min_price_plot")),
                    column(6,
                           plotOutput("heat_map"))),
                br(),
                br(),
                # Second row
                fluidRow(column(12,
                                plotOutput('stl_decomp')))
            ),
            tabPanel('Forecasts',
                     verbatimTextOutput('Forecasts will go here eventually')),
            tabPanel('Exploration',
                     verbatimTextOutput('Some other data exploration will eventually go here'))
        ))
    )
)


# Define server logic 
server <- function(input, output) {
    
    # Get the data down to just the item(s) I'm interested in
    # Right now, I'm restricting it to a single item but I may build it up
    # to allow for multiples
    item_data <- reactive({
        auction_lazy %>% filter(name %in% input$item) %>% as_tibble()
    })
    
    # Slice the data down to just the minimum value at each time point
    min_item_data <- reactive({
        item_data() %>% 
            group_by(name, date_time) %>% 
            slice_min(cost_g, with_ties = FALSE) 
    })
    
    # Fill in implicit time series missing values to explicit
    # Eventually will offer other ways for filling in missing value, but
    # for now it's just carry forward.
    item_ts <- reactive({
        item_ts_df <- min_item_data() %>% 
            as_tibble() %>%
            as_tsibble(key = name, index = date_time) %>%
            fill_gaps() %>%
            mutate(fill_prev_value_cost = original_cost_g) %>%
            fill(fill_prev_value_cost, .direction = 'down') %>% 
            mutate(cost_g = fill_prev_value_cost)
        
        if (input$rm_outliers == TRUE) {
            # Remove outliers if requested
            outliers_and_missing <- tsoutliers(item_ts_df$cost_g)
            item_ts_df$cost_g[outliers_and_missing$index] <- outliers_and_missing$replacements
        }
        
        else if (input$rm_outliers == FALSE) {
            # Don't remove outliers, reset cost to original
            item_ts_df$cost_g <- item_ts_df[[input$fill_NA]]
        }
        item_ts_df
    })
    
    output$min_price_plot <- renderPlot({
        item_ts() %>%  
            ggplot(aes(x = date_time, y = cost_g, colour = name)) +
            geom_point()
    })
    
    output$heat_map <- renderPlot({
        item_ts() %>% 
            group_by(name, day, hour) %>% 
            summarize(avg_min_cost = mean(cost_g, na.rm = TRUE)) %>% 
            ggplot(aes(x = hour, y = day, fill = avg_min_cost)) +
            geom_tile()
    })
    
    output$stl_decomp <- renderPlot({
        if (is.null(input$seasonal_periods)) {
            ts_cost <- ts(item_ts()$cost_g)
        } else {
            ts_cost <- msts(item_ts()$cost_g, seasonal.periods = as.numeric(input$seasonal_periods)) 
        }
        
        mstl(ts_cost) %>% autoplot()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
