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
library(magrittr)
library(lubridate)
library(shinythemes)

auction_data <- fread(here::here('data/latest_month_auctions.csv'))
# Change dates to date time
auction_data[, date_time := as_datetime(date_time)]
auction_data[, day := wday(date_time, label = TRUE)]
auction_data[, hour := hour(date_time)]

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme('darkly'),
    # Application title
    titlePanel("Title goes here"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("item",
                        "Item:",
                        choices = sort(unique(auction_data$name)),
                        selected = 'Widowbloom',
                        multiple = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("min_price_plot"),
           plotOutput("heat_map")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$min_price_plot <- renderPlot({
        auction_data %>% 
            filter(name %in% input$item) %>% 
            group_by(name, date_time) %>% 
            filter(cost_g == min(cost_g)) %>% 
            ggplot(aes(x = date_time, y = cost_g, colour = name)) +
            geom_point()
    })
    
    output$heat_map <- renderPlot({
        auction_data %>% 
            filter(name %in% input$item) %>% 
            group_by(name, date_time) %>% 
            filter(cost_g == min(cost_g, na.rm = TRUE)) %>% 
            ungroup() %>% 
            group_by(name, day, hour) %>% 
            summarize(avg_min_cost = mean(cost_g, na.rm = TRUE)) %>% 
            ggplot(aes(x = hour, y = day, fill = avg_min_cost)) +
            geom_tile()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
