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
library(DT)
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
library(patchwork)
library(glue)
source('functions.R')

# Set up some categories for different types of mats
sl_herbs <- c('Rising Glory',
              'Widowbloom',
              'Marrowroot',
              "Vigil's Torch",
              'Death Blossom',
              'Nightshade')
sl_ore <- c('Laestrite Ore',
            'Elethium Ore',
            'Solenium Ore',
            'Oxxein Ore',
            'Phaedrum Ore',
            'Sinvyr Ore')
sl_gems <- c('Angerseye',
             'Oriblase',
             'Umbryl')
sl_leather <- c('Desolate Leather',
                'Callous Hide',
                'Pallid Bone',
                'Gaunt Sinew',
                'Heavy Desolate Leather',
                'Heavy Callous Hide')
sl_cloth <- c('Shrouded Cloth',
              'Lightless Silk')
sl_enchant <- c('Soul Dust',
                'Sacred Shard',
                'Eternal Crystal')

auction_dt <- fread(here::here('data/latest_month_auctions.csv'))

# Change dates to date time
auction_dt[, date_time := paste0(substr(date_time, 1, 13), ":00:00")]
auction_dt[, date_time := as_datetime(date_time)]
auction_dt[, day := wday(date_time, label = TRUE)]
auction_dt[, hour := hour(date_time)]
auction_dt[, original_cost_g := cost_g]
setkey(auction_dt, name, date_time) # Might make later operations faster

auction_lazy <- lazy_dt(auction_dt) # Allows me to use dplyr syntax for a very minor speed penalty
auction_dt <- auction_lazy %>% 
    mutate(item_class = case_when(
        name %in% sl_ore ~ 'Ore',
        name %in% sl_enchant ~ 'Enchanting',
        name %in% sl_gems ~ 'Gems',
        name %in% sl_leather ~ 'Leather',
        name %in% sl_cloth ~ 'Cloth',
        name %in% sl_herbs ~ 'Herbs'
    )) %>% 
    as.data.table()
auction_lazy <- lazy_dt(auction_dt)
train_models <- list()
model_tests <- list()
full_models <- list()

item_choices <- c('SL Herbs', 'SL Ore', 'SL Leather', 
                  'SL Gems', 'SL Cloth', sort(unique(auction_dt$name)))
run_test <- FALSE
if (run_test == TRUE) {
    # Code in this conditional for testing stuff before actually using it
    input <- NA
    input$item <- c(sl_herbs, sl_ore)
    input$fill_NA <- 'fill_prev_value_cost'
    input$test_size <- 168
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
        fill(fill_prev_value_cost, .direction = 'downup')
    outliers_and_missing <- tsoutliers(test$cost_g)
    test$interp_cost_g <- test$cost_g
    test$interp_cost_g[outliers_and_missing$index] <- outliers_and_missing$replacements
    
    ts_cost <- msts(test[[input$fill_NA]], seasonal.periods = c(48, 168))
    # Simple exponential smooth - essentially carries forward last observation
    # although it's a weight average, with weights controlled by alpha
    # Not suitable for time series with a trend or seasonality
    fit <- test %>% 
        model(ETS(fill_prev_value_cost ~ error('A') + trend('N') + season('N')))
    fc <- fit %>% 
        forecast(h = 50)
    fc %>% 
        autoplot(test) +
        geom_line(aes(y = .fitted), col = '#D55E00', data = augment(fit)) +
        guides(colour = 'none')
    
    # Holt's method for trend / damped holt's method
    test %>% 
        model(
            "Holt's method" = ETS(fill_prev_value_cost ~ error('A') + trend('A') + season('N')),
            "Damped Holt's method" = ETS(fill_prev_value_cost ~ error('A') + trend('Ad') + season('N'))
        ) %>% 
        forecast(h = 50) %>% 
        autoplot(test, level = NULL)
    
    # Add seasonality
    fit <- test %>% 
        model(additive = ETS(fill_prev_value_cost ~ error('A') + trend('A') + season('A')),
              multiplicative = ETS(fill_prev_value_cost ~ error('M') + trend('A') + season('M')))
    fc <- fit %>% forecast(h = 50)
    fc %>% 
        autoplot(test)
    
    # Compare methods so far
    fit <- test %>% 
        model(
           mean = MEAN(fill_prev_value_cost),
           naive = NAIVE(fill_prev_value_cost),
           snaive = SNAIVE(fill_prev_value_cost ~ lag('day')),
           drift = RW(fill_prev_value_cost ~ drift()),
           "SES" = ETS(fill_prev_value_cost ~ error('A') + trend('N') + season('N')),
           "Holt's method" = ETS(fill_prev_value_cost ~ error('A') + trend('A') + season('N')),
           "Damped Holt's method" = ETS(fill_prev_value_cost ~ error('A') + trend('Ad') + season('N')),
           additive = ETS(fill_prev_value_cost ~ error('A') + trend('A') + season('A')),
           multiplicative = ETS(fill_prev_value_cost ~ error('M') + trend('A') + season('M')),
           'Damped multiplicative' = ETS(fill_prev_value_cost ~ error('M') + trend('Ad') + season('M')),
            arima = ARIMA(fill_prev_value_cost),
            search_arima = ARIMA(fill_prev_value_cost, stepwise = FALSE)
           #dyn_regression = ARIMA(fill_prev_value_cost ~ day + hour)
        )
    fc <- fit %>% forecast(h = 168)
    fc %>% 
        autoplot(test) +
        facet_wrap(~.model) +
        theme(legend.position = 'none')
    # Compare fits
    test %>% 
        # I think .init is essentially the size of the training set (so, 168 hours = 1 week)
        stretch_tsibble(.init = 168) %>% 
        model(
            mean = MEAN(fill_prev_value_cost),
            naive = NAIVE(fill_prev_value_cost),
            snaive = SNAIVE(fill_prev_value_cost ~ lag('day')),
            drift = RW(fill_prev_value_cost ~ drift()),
            "SES" = ETS(fill_prev_value_cost ~ error('A') + trend('N') + season('N')),
            "Holt's method" = ETS(fill_prev_value_cost ~ error('A') + trend('A') + season('N')),
            "Damped Holt's method" = ETS(fill_prev_value_cost ~ error('A') + trend('Ad') + season('N')),
            additive = ETS(fill_prev_value_cost ~ error('A') + trend('A') + season('A')),
            multiplicative = ETS(fill_prev_value_cost ~ error('M') + trend('A') + season('M')),
            'Damped multiplicative' = ETS(fill_prev_value_cost ~ error('M') + trend('Ad') + season('M')),
            arima = ARIMA(fill_prev_value_cost),
            search_arima = ARIMA(fill_prev_value_cost, stepwise = FALSE)
            #dyn_regression = ARIMA(fill_prev_value_cost ~ day + hour)
        ) %>% 
        forecast(h = 5) %>%
        accuracy(test) %>% 
        arrange(MAE)
    train_models <- list()
    test_test_ts <- test %>% 
        slice(n() - input$test_size:0)
    train_ts <- anti_join(test, test_test_ts, "date_time")
    train_models$train_arima <- train_ts %>%
        model(ARIMA(cost_g))
    model_eval$arima <- bind_rows(
        train_models$train_arima %>% accuracy(),
        train_models$train_arima %>% forecast(h = input$test_size) %>% 
            accuracy(test)
    )
    train_models$train_drift <- train_ts %>%
        model(RW(cost_g ~ drift()))
    model_eval$drift <- bind_rows(
        train_models$train_drift %>% accuracy(),
        train_models$train_drift %>% forecast(h = input$test_size) %>% 
            accuracy(test)
    )
    
    # Figuring out how to do combinations of ETS
    input$ets_error <- c('A', 'M')
    input$ets_trend <- c('N', 'Ad')
    input$ets_season <- c('N', 'M', 'A')
    ets_models <- crossing(input$ets_error, input$ets_season, input$ets_trend, .name_repair = 'universal')
    ets_models <- ets_models %>% 
        mutate(full_model_name = paste(input.ets_error, input.ets_season, input.ets_trend, sep = ","))
    model_formulas <- glue_data(ets_models,
                                "fill_prev_value_cost ~ error('{input.ets_error}') + trend('{input.ets_trend}') + season('{input.ets_error}')")
    model_formulas <- purrr::map(model_formulas, ~as.formula(.x))
    names(model_formulas) <- ets_models$full_model_name
    fit_ets_models <- map2(model_formulas, names(model_formulas), function(.x, .y) {
        out <- train_ts %>%
            model(ETS(.x))
        names(out)[names(out) == 'ETS(.x)'] <- paste0('ETS(', .y, ')')
        out
 
    }) 
    map(fit_ets_models, function(.x) {
        bind_rows(.x %>% accuracy(),
                  .x %>% forecast(h = input$test_size) %>% accuracy(test))
    }) %>% 
        bind_rows()
    
    #NNet
    train_ts %>% 
        model(NNETAR(fill_prev_value_cost)) %>% 
        forecast(h = 168) %>% 
        accuracy(test)
}
# Define UI
ui <- fluidPage(
    theme = shinytheme('darkly'),
    # Application title
    titlePanel("Malfurion - NA Auction House Data"),

# UI ----------------------------------------------------------------------

    
    # Tab panel
    sidebarLayout(
        sidebarPanel(
            # Sidebar
            selectInput(
                "item",
                "Item:",
                choices = item_choices,
                selected = 'Widowbloom',
                selectize = TRUE,
                multiple = TRUE
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
            ),
            sliderInput('plot_daterange',
                        label = 'Date range for plots',
                        min = min(auction_dt$date_time),
                        max = max(auction_dt$date_time),
                        value = c(min(auction_dt$date_time),
                                  max(auction_dt$date_time)),
                        timeFormat = "%m-%d-%y %H:%M")
        ),
        mainPanel(tabsetPanel(

# AH panel ----------------------------------------------------------------

            
            tabPanel(
                'Auction House Data',
                fluidRow(# Main panel
                    column(12,
                           plotOutput("min_price_plot"))),
                fluidRow(
                    column(12,
                           plotOutput("heat_map"))),
                br(),
                br(),
                # Second row
                fluidRow(column(12,
                                plotOutput('stl_decomp'))),
                br(),
                br(),
                fluidRow(column(6, plotOutput('acf')),
                         column(6, plotOutput('pacf'))),
                br(),
                br(),
                fluidRow(column(12, DTOutput('acf_table'))),
                br(),
                br(),
                fluidRow(column(12, DTOutput('train_ts')))
            ),

# Model panel -------------------------------------------------------------

            
            tabPanel('Models',
                     fluidRow(column(12,
                                     checkboxGroupInput('models',
                                                        label = 'Which models should be fit for forecasting?',
                                                        choices = list(
                                                            'ARIMA' = 'arima',
                                                            'ETS' = 'ets',
                                                            'Drift' = 'drift',
                                                            'Naive' = 'naive',
                                                            'Seasonal Naive' = 'snaive',
                                                            'Mean' = 'mean',
                                                            'Neural Net' = 'nnet'
                                                        ),
                                                        inline = TRUE))),
                     conditionalPanel(condition = "input.models.includes('ets')",
                                      fluidRow(h3('ETS Model Options'),
                                               p("Select error, trend, and seasonality for the ETS models.
                                                 Can select more than one option, but all possible combinations will
                                                 be run which may increase computation.")
                                               ),
                                      fluidRow(
                                          column(
                                              3,
                                              checkboxGroupInput(
                                                  'ets_error',
                                                  label = 'Error for ETS',
                                                  choices = list('Additive' = 'A',
                                                                 'Multiplicative' = 'M'),
                                                  selected = 'A'
                                              )
                                          ),
                                          
                                          column(
                                              3,
                                              checkboxGroupInput(
                                                  'ets_trend',
                                                  label = 'Trend for ETS',
                                                  choices = list(
                                                      'None' = 'N',
                                                      'Additive' = 'A',
                                                      'Additive Damped' = 'Ad'
                                                  ),
                                                  selected = 'N'
                                              )
                                          ),
                                          
                                          column(
                                              3,
                                              checkboxGroupInput(
                                                  'ets_season',
                                                  label = 'Season for ETS',
                                                  choices = list(
                                                      'None' = 'N',
                                                      'Additive' = 'A',
                                                      'Multiplicative' = 'M'
                                                  ),
                                                  selected = 'N'
                                              )
                                          )
                                      )),
                     conditionalPanel(condition = "input.models.includes('nnet')",
                                      fluidRow(p('Neural net models may increase computational time significantly.
                                                  Consider reducing the forecast test horizon.'))),
                     fluidRow(column(6, numericInput('test_size',
                                                      label = 'How many hourly observations should be used for test set?',
                                                      min = 1,
                                                      step = 1,
                                                      value = 168)),
                              column(6, checkboxInput('facet_train_plots',
                                                      label = 'Facet forecast plots by model?',
                                                      value = FALSE))),
                     fluidRow(column(12, actionButton('fit_model', 'Fit models!'))),
                     br(),
                     fluidRow(column(12, DTOutput('model_test_df'))),
                     br(),
                     fluidRow(column(12, plotOutput('train_model_plots'))),
                     br(),
                     fluidRow(column(12, DTOutput('model_train_df')))
                     ),
            tabPanel('Exploration',
                     verbatimTextOutput('Some other data exploration will eventually go here'))
        ))
    )
)


# Server ------------------------------------------------------------------


# Define server logic 
server <- function(input, output, session) {
    # Event reactive to allow group selection of SL crafting mats
    observeEvent(input$item, {
        if ("SL Herbs" %in% input$item) {
            updateSelectInput(session,
                                 "item",
                                 selected = c(input$item[!input$item %in% "SL Herbs"],
                                              sl_herbs))
            
        } else if ("SL Ore" %in% input$item) {
            updateSelectInput(session,
                                 "item",
                                 selected = c(input$item[!input$item %in% "SL Ore"],
                                              sl_ore))
        } else if ("SL Leather" %in% input$item) {
            updateSelectInput(session,
                                 "item",
                                 selected = c(input$item[!input$item %in% "SL Leather"],
                                              sl_leather))
        } else if ("SL Cloth" %in% input$item) {
            updateSelectInput(session,
                                 "item",
                                 selected = c(input$item[!input$item %in% "SL Cloth"],
                                              sl_cloth))
        } else if ("SL Gems" %in% input$item) {
            updateSelectInput(session,
                                 "item",
                                 selected = c(input$item[!input$item %in% "SL Gems"],
                                              sl_gems))
        } else if ("SL Enchant" %in% input$item) {
            updateSelectInput(session,
                                 "item",
                                 selected = c(input$item[!input$item %in% "SL Enchant"],
                                              sl_enchant))
        }
    })
    
    
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
            fill(fill_prev_value_cost, .direction = 'downup') %>% 
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
    
    # Get the last m observations to use as a test set. Defaults to 168 (1 week)
    train_ts <- eventReactive(input$fit_model, {
        item_ts() %>% 
            slice(n() - input$test_size:0) %>% 
            anti_join(item_ts(), ., by = 'date_time')
    })

    output$train_ts <- renderDT(train_ts())
    
    # Data plots --------------------------------------------------------------
    output$min_price_plot <- renderPlot({
        item_ts() %>%  
            filter(date_time <= input$plot_daterange[2] & date_time >= input$plot_daterange[1]) %>% 
            ggplot(aes(x = date_time, y = cost_g, colour = name)) +
            geom_point() +
            labs(x = 'Date', y = 'Minimum buyout (gold)',
                 title = paste(unique(item_ts()$name))) +
            theme(legend.position = "bottom") +
            scale_color_brewer(type = "qual", palette = 'Dark2')
    })
    
    output$heat_map <- renderPlot({
        heat_map_dat <- item_ts() %>% 
            filter(!is.na(day)) %>% 
            mutate(day = reorder(day, desc(day))) %>% 
            group_by(name, day, hour) %>% 
            summarize(avg_min_cost = mean(cost_g, na.rm = TRUE)) 
            
        if (length(input$item) > 1) {
            heat_map_dat %>% 
                ungroup() %>% 
                #group_by(name) %>% 
                group_split(name) %>% 
                map(function(.x) {
                    ggplot(.x, aes(x = hour, y = day, fill = avg_min_cost)) +
                        labs(x = 'Time of Day', y = 'Day', fill = 'Average minimum \nbuyout (gold)')+
                        geom_tile() +
                        labs(title = unique(.x$name)) +
                        theme(legend.position = 'bottom')
                }) %>% 
                wrap_plots()
        } else {
            heat_map_dat %>% 
                ggplot(aes(x = hour, y = day, fill = avg_min_cost)) +
                labs(x = 'Time of Day', y = 'Day', fill = 'Average minimum \nbuyout (gold)')+
                geom_tile() +
                theme(legend.position = 'bottom')
        }
    })
    
    output$stl_decomp <- renderPlot({
        if (is.null(input$seasonal_periods)) {
            ts_cost <- ts(item_ts()$cost_g)
        } else {
            ts_cost <- msts(item_ts()$cost_g, seasonal.periods = as.numeric(input$seasonal_periods)) 
        }
        
        mstl(ts_cost) %>% autoplot()
    })
    
    output$acf <- renderPlot({
        ACF(item_ts(), cost_g) %>%
            autoplot() +
            labs(caption =
            'The ACF will drop to zero relatively quickly for stationary time series,
            while non-stationary time series will have an ACF that decreases slowly.
            Non-stationary series also tend too have values of r1 that are positive and large.
            (From Hyndaman Forecasting textbook)')
    })
    
    output$pacf <- renderPlot({
        PACF(item_ts(), cost_g) %>%
            autoplot() +
            labs(caption = 'If data follow an ARIMA(p, d, 0) model the ACF will exponentially
                 decay or be sinusoidal and there will be a significant spike in the PACF plot at
                 lag p but non beyond lag p. If data follow an ARIMA(0, d, q) model, the conditions
                 for each plot are reversed and instead of lag p, lag q.')
    })
    
    output$acf_table <- renderDT({
        features(item_ts(), cost_g, feat_acf)
    })
    

# Train and test models ----------------------------------------------------------
    notify <- function(msg, id = NULL) {
        showNotification(msg, id = id, duration = NULL, closeButton = FALSE)
    }

    fit_train_models <- eventReactive(input$fit_model, {
        # Notification that model fitting has begun, to be removed when it finishes
        id <- notify('Fitting models...')
        on.exit(removeNotification(id), add = TRUE)
        
        # Model fitting code goes here
        if ('arima' %in% input$models) {
            notify('Fitting ARIMA', id = id)
            # Fit model to training data for evaluation
            train_models$train_arima <- train_ts() %>%
                model(ARIMA(cost_g))
        }
        if ('ets' %in% input$models) {
            notify('Fitting ETS', id = id)
            # Get all combination of ETS models
            ets_models <- crossing(input$ets_error, input$ets_season, input$ets_trend, 
                                   .name_repair = 'universal')
            # Set up names for the list of formulas
            ets_models <- ets_models %>% 
                mutate(full_model_name = paste(input.ets_error, input.ets_season, input.ets_trend, sep = ","))
            model_formulas <- glue_data(ets_models,
                                        "cost_g ~ error('{input.ets_error}') + trend('{input.ets_trend}') + season('{input.ets_error}')")
            model_formulas <- purrr::map(model_formulas, ~as.formula(.x))
            names(model_formulas) <- ets_models$full_model_name
            # Fit models
            train_models$train_ets <- map2(model_formulas, names(model_formulas), function(.x, .y) {
                out <- train_ts() %>%
                    model(ETS(.x))
                names(out)[names(out) == 'ETS(.x)'] <- paste0('ETS(', .y, ')')
                out
                
            }) %>% 
                reduce(left_join, by = 'name')

        }
        if ('drift' %in% input$models) {
            notify('Fitting drift', id = id)
            train_models$train_drift <- train_ts() %>%
                model(RW(cost_g ~ drift()))
            
        }
        
        if ('naive' %in% input$models) {
            notify('Fitting naive', id = id)
            train_models$train_naive <- train_ts() %>% 
                model(NAIVE(cost_g))
        }

        if ('snaive' %in% input$models){
            notify('Fitting seasaonal naive', id = id)
            train_models$train_snaive <- train_ts() %>%
                model(SNAIVE(cost_g ~ lag('day')))

        }
        if ('mean' %in% input$models) {
            notify('Fitting mean', id = id)
            train_models$train_mean <- train_ts() %>%
                model(MEAN(cost_g))
        }
        if ('nnet' %in% input$models) {
            notify('Fitting neural net', id = id)
            train_models$train_nnet <- train_ts() %>% 
                model(NNETAR(cost_g))
        }
        
        reduce(train_models, left_join, by = 'name')
    
    })
    
    output$model_train_df <- renderDT({fit_train_models() %>% 
            accuracy() %>% 
            arrange(RMSE) %>% 
            mutate(across(where(is.numeric), ~round(.x, 4)))})
    
    output$model_test_df <- renderDT({fit_train_models() %>% 
            forecast(h = input$test_size) %>% 
            accuracy(item_ts()) %>% 
            arrange(RMSE) %>% 
            mutate(across(where(is.numeric), ~round(.x, 4)))})
    
    
    output$train_model_plots <- renderPlot({
        train_mod_plots <- fit_train_models() %>% 
            forecast(h = input$test_size) %>%   
            filter(date_time <= input$plot_daterange[2] & date_time >= input$plot_daterange[1]) %>% 
            autoplot(item_ts())
        if (input$facet_train_plots == TRUE) {
            train_mod_plots + facet_wrap(~.model)
        } else {
            train_mod_plots
        }
        })
    

    # Forecast ---------------------------------------------------------------

    
}

# Run the application 
shinyApp(ui = ui, server = server)
