
library(shiny)
library(tidyverse)
library(ggExtra)
library(leaflet)


analysis_veg <- read.csv("https://s3.us-east-2.amazonaws.com/nanfang/analysis_veg.csv") %>% as.tibble()
tb <- read.csv("https://s3.us-east-2.amazonaws.com/nanfang/tb.csv") %>% as.tibble() %>% select(-X)
tb$lubridate <- lubridate::ymd_hms(tb$lubridate)

shinyServer(function(input, output) {
  
  output$relation_plot <- renderPlot({
    p <- ggplot(tb, aes(WTMP, ATMP)) + geom_point() +
      theme_classic() + labs(x = 'Sea temperature (C)', y = 'Air temperature (C)') +
      ggtitle('1988 - 2017 Southern Bering Sea Buoy Data')
    plot_ATMP_WTMP = ggExtra::ggMarginal(p, type = "histogram")
    plot_ATMP_WTMP
  })
  
  output$buoy_map <- renderLeaflet({
    m <- leaflet() %>% addTiles() %>% setView(177.738, 57.026, zoom = 4) %>% 
      addMarkers(lng=177.738, lat=57.026, popup="The Buoy (ID:46035)") 
    m
  })
  
  output$statistics_plot1 <- renderPlot({
    plot_ATMP_Yearly <- tb %>% 
      group_by(t = (lubridate::year(tb$lubridate) - 1987)) %>% 
      summarise(ATMP = mean(ATMP, na.rm = TRUE)) %>% 
      ggplot(aes(x = t, y = ATMP)) +
      geom_point() + labs(x = 'Year in sequence from 1988', y = 'Air temperature (C)') +
      ggtitle('Annual Air Temperature in Southern Bering Sea') +
      geom_smooth(method = lm)
    plot_ATMP_Yearly
  })  
  
  output$statistics_plot2 <- renderPlot({
    plot_WTMP_yearly <- tb %>% 
      group_by(t = (lubridate::year(tb$lubridate) - 1987)) %>% 
      summarise(WTMP = mean(WTMP, na.rm = TRUE)) %>% 
      ggplot(aes(x = t, y = WTMP)) +
      geom_point() + labs(x = 'Year in sequence from 1988', y = 'Sea temperature (C)') +
      ggtitle('Annual Sea Temperature in Southern Bering Sea') +
      geom_smooth(method = lm)
    plot_WTMP_yearly
  })  
  
  output$statistics_plot3 <- renderPlot({
    tb2 <- tb %>% 
      group_by(t = (lubridate::year(tb$lubridate) - 1988) * 12 + lubridate::month(tb$lubridate)) %>% 
      summarise(ATMP = mean(ATMP, na.rm = TRUE))
    
    tb2$sin <- sin((2 * pi / 12 ) * tb2$t)
    tb2$cos <- cos((2 * pi / 12 ) * tb2$t)
    
    model_ATMP_monthly <- lm(ATMP ~ sin + cos + t, tb2)
    model_ATMP_monthly_result <- function(t) {
      model_ATMP_monthly$coefficients[1] +
        model_ATMP_monthly$coefficients[2] * sin((2 * pi / 12 ) * t) +
        model_ATMP_monthly$coefficients[3] * cos((2 * pi / 12 ) * t) +
        model_ATMP_monthly$coefficients[4] * t
    }
    
    plot_ATMP_monthly <- ggplot(tb2, aes(x = t, y = ATMP)) +
      geom_point() + labs(x = 'Months in sequence from 1988', y = 'Air temperature (C)') +
      ggtitle('Monthly Air Temperature in Southern Bering Sea') +
      stat_function(fun = model_ATMP_monthly_result, n = 1000, colour = "red")
    plot_ATMP_monthly
  })   
  
  output$statistics_plot4 <- renderPlot({
    tb2 <- tb %>%
      group_by(t = (lubridate::year(tb$lubridate) - 1988) * 12 + lubridate::month(tb$lubridate)) %>%
      summarise(WTMP = mean(WTMP, na.rm = TRUE))
    
    tb2$sin <- sin((2 * pi / 12 ) * tb2$t)
    tb2$cos <- cos((2 * pi / 12 ) * tb2$t)
    
    model_WTMP_monthly <- lm(WTMP ~ sin + cos + t, tb2)
    model_WTMP_monthly_result <- function(t) {
      model_WTMP_monthly$coefficients[1] +
        model_WTMP_monthly$coefficients[2] * sin((2 * pi / 12 ) * t) +
        model_WTMP_monthly$coefficients[3] * cos((2 * pi / 12 ) * t) +
        model_WTMP_monthly$coefficients[4] * t
    }
    
    plot_WTMP_monthly <- ggplot(tb2, aes(x = t, y = WTMP)) +
      geom_point() + labs(x = 'Months in sequence from 1988', y = 'Sea temperature (C)') +
      ggtitle('Monthly Sea Temperature in Southern Bering Sea') +
      stat_function(fun = model_WTMP_monthly_result, n = 1000, colour = "red")
    plot_WTMP_monthly    
  })    
  
  output$statistics_plot5 <- renderPlot({
    tb_12 <- tb %>% filter(lubridate::hour(tb$lubridate) == 12)
    tb2 <-
      bind_cols(t = as.integer(as.numeric(tb_12$lubridate) / (3600 * 24) - 6574), ATMP = tb_12$ATMP)
    
    tb2$sin <- sin((2 * pi / 365.25) * tb2$t)
    tb2$cos <- cos((2 * pi / 365.25) * tb2$t)
    
    model_ATMP_Daily_12 <- lm(ATMP ~ sin + cos + t, tb2)
    
    model_ATMP_Daily_12_result <- function(t) {
      model_ATMP_Daily_12$coefficients[1] +
        model_ATMP_Daily_12$coefficients[2] * sin((2 * pi / 365.25) * t) +
        model_ATMP_Daily_12$coefficients[3] * cos((2 * pi / 365.25) * t) +
        model_ATMP_Daily_12$coefficients[4] * t
    }
    
    plot_ATMP_Daily_12 <- ggplot(tb2, aes(x = t, y = ATMP)) +
      geom_point() + labs(x = 'Days in sequence from 1988', y = 'Air temperature (C)') +
      ggtitle('Daily Air Temperature in Southern Bering Sea') +
      stat_function(fun = model_ATMP_Daily_12_result, n = 1000, colour = "red")
    plot_ATMP_Daily_12
  })    

  output$statistics_plot6 <- renderPlot({
    tb_12 <- tb %>% filter(lubridate::hour(tb$lubridate) == 12)
    tb2 <- bind_cols(t = as.integer(as.numeric(tb_12$lubridate) / (3600 * 24) - 6574), WTMP = tb_12$WTMP)
    
    tb2$sin <- sin((2 * pi / 365.25 ) * tb2$t)
    tb2$cos <- cos((2 * pi / 365.25 ) * tb2$t)
    model_WTMP_Daily_12 <- lm(WTMP ~ sin + cos + t, tb2)
    
    
    model_WTMP_Daily_12_result <- function(t) {
      model_WTMP_Daily_12$coefficients[1] +
        model_WTMP_Daily_12$coefficients[2] * sin((2 * pi / 365.25 ) * t) +
        model_WTMP_Daily_12$coefficients[3] * cos((2 * pi / 365.25 ) * t) +
        model_WTMP_Daily_12$coefficients[4] * t
    }
    
    plot_WTMP_Daily_12 <- ggplot(tb2, aes(x = t, y = WTMP)) +
      geom_point() + labs(x = 'Days in sequence from 1988', y = 'Sea temperature (C)') +
      ggtitle('Daily Sea Temperature in Southern Bering Sea') +
      stat_function(fun = model_WTMP_Daily_12_result, n = 1000, colour = "red") 
    plot_WTMP_Daily_12   
  })     
          
  output$statistics_plot7 <- renderPlot({
    tb_12 <- tb %>% filter(lubridate::hour(tb$lubridate) == 0)
    
    tb2 <- bind_cols(t = as.integer(as.numeric(tb_12$lubridate) / (3600 * 24) - 6574), ATMP = tb_12$ATMP)
    
    tb2$sin <- sin((2 * pi / 365.25 ) * tb2$t)
    tb2$cos <- cos((2 * pi / 365.25 ) * tb2$t)
    model_ATMP_Daily_00 <- lm(ATMP ~ sin + cos + t, tb2)
    
    model_ATMP_Daily_00_result <- function(t) {
      model_ATMP_Daily_00$coefficients[1] +
        model_ATMP_Daily_00$coefficients[2] * sin((2 * pi / 365.25 ) * t) +
        model_ATMP_Daily_00$coefficients[3] * cos((2 * pi / 365.25 ) * t) +
        model_ATMP_Daily_00$coefficients[4] * t
    }
    
    plot_ATMP_Daily_00 <- ggplot(tb2, aes(x = t, y = ATMP)) +
      geom_point() + labs(x = 'Days in sequence from 1988', y = 'Air temperature (C)') +
      ggtitle('Daily Air Temperature in Southern Bering Sea') +
      stat_function(fun = model_ATMP_Daily_00_result, n = 1000, colour = "red")
    plot_ATMP_Daily_00    
  })  
      
  output$statistics_plot8 <- renderPlot({
    tb_12 <- tb %>% filter(lubridate::hour(tb$lubridate) == 0)
    
    tb2 <- bind_cols(t = as.integer(as.numeric(tb_12$lubridate) / (3600 * 24) - 6574), WTMP = tb_12$WTMP)
    
    tb2$sin <- sin((2 * pi / 365.25 ) * tb2$t)
    tb2$cos <- cos((2 * pi / 365.25 ) * tb2$t)
    model_WTMP_Daily_00 <- lm(WTMP ~ sin + cos + t, tb2)
    
    model_WTMP_Daily_00_result <- function(t) {
      model_WTMP_Daily_00$coefficients[1] +
        model_WTMP_Daily_00$coefficients[2] * sin((2 * pi / 365.25 ) * t) +
        model_WTMP_Daily_00$coefficients[3] * cos((2 * pi / 365.25 ) * t) +
        model_WTMP_Daily_00$coefficients[4] * t
    }
    
    plot_WTMP_Daily_00 <- ggplot(tb2, aes(x = t, y = WTMP)) +
      geom_point() + labs(x = 'Days in sequence from 1988', y = 'Sea temperature (C)') +
      ggtitle('Daily Sea Temperature in Southern Bering Sea') +
      stat_function(fun = model_WTMP_Daily_00_result, n = 1000, colour = "red") 
    plot_WTMP_Daily_00 
  })  
  
  output$statistics_plot9 <- renderPlot({
    tb2 <-
      bind_cols(t = as.integer(as.numeric(tb$lubridate) / 3600 - 157777), ATMP = tb$ATMP)
    
    tb2$sin <- sin((2 * pi / 8766) * tb2$t)
    tb2$cos <- cos((2 * pi / 8766) * tb2$t)
    model_ATMP_hourly <- lm(ATMP ~ sin + cos + t, tb2)
    
    
    model_ATMP_hourly_result <- function(t) {
      model_ATMP_hourly$coefficients[1] +
        model_ATMP_hourly$coefficients[2] * sin((2 * pi / 8766) * t) +
        model_ATMP_hourly$coefficients[3] * cos((2 * pi / 8766) * t) +
        model_ATMP_hourly$coefficients[4] * t
    }
    plot_ATMP_hourly <- ggplot(tb2, aes(x = t, y = ATMP)) +
      geom_point() + labs(x = 'Hours in sequence from 1988', y = 'Air temperature (C)') +
      ggtitle('Hourly Air Temperature in Southern Bering Sea') +
      stat_function(fun = model_ATMP_hourly_result, n = 1000, colour = "red")
    plot_ATMP_hourly
  })  
  
  output$statistics_plot10 <- renderPlot({
    tb2 <- bind_cols(t = as.integer(as.numeric(tb$lubridate) / 3600 - 157785), WTMP = tb$WTMP)
    
    tb2$sin <- sin((2 * pi / 8766 ) * tb2$t)
    tb2$cos <- cos((2 * pi / 8766 ) * tb2$t)
    
    model_WTMP_hourly <- lm(WTMP ~ sin + cos + t, tb2)
    
    model_WTMP_hourly_result <- function(t) {
      model_WTMP_hourly$coefficients[1] +
        model_WTMP_hourly$coefficients[2] * sin((2 * pi / 8766 ) * t) +
        model_WTMP_hourly$coefficients[3] * cos((2 * pi / 8766 ) * t) +
        model_WTMP_hourly$coefficients[4] * t
    }
    
    
    plot_WTMP_hourly <- ggplot(tb2, aes(x = t, y = WTMP)) +
      geom_point() + labs(x = 'Hours in sequence from 1988', y = 'Sea temperature (C)') +
      ggtitle('Hourly Sea Temperature in Southern Bering Sea') +
      stat_function(fun = model_WTMP_hourly_result, n = 1000, colour = "red")
    plot_WTMP_hourly 
  })   
   
  output$veg_plot <- renderPlot({
    selectedGraph <- analysis_veg %>% filter(Year == input$year, Vegetable == input$vegetable)
    ggplot(selectedGraph, aes(log10(LD50), log10(A), label = selectedGraph$Chemical)) + 
      geom_point() + geom_text(hjust = 0, nudge_x = 0.05) +
      labs( x = 'Chemical toxicity meansured by log10(LD50[mg/kg])',
            y = 'Total log10(mass[lbs]) of chemical applied to planted acres') +
      ggtitle(paste(input$vegetable, input$year, sep = ''))
  })
})
