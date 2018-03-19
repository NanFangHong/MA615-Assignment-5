library(shiny)
library(shinydashboard)
library(leaflet)

dashboardPage(
  dashboardHeader(title = 'Tidyverse Final Project'),
  dashboardSidebar(
    sidebarMenu(menuItem("Part 1 Global Warming", tabName = "part1", icon = icon("heart"),
                         menuSubItem("Air-Sea Temperature", tabName = "AirSea"),
                         menuSubItem("30 Years Trend", tabName = "Trend"),
                         menuSubItem("30 Years Trend (Daily)", tabName = "Trend2"),
                         menuSubItem("30 Years Trend (Hourly)", tabName = "Trend3")),
                menuItem("Part 2 Pesticide Warning", icon = icon("heart"), tabName = "part2")
    
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "AirSea",
              
              fluidRow(
                column(width = 6, 
                box(title = "Scatter Plot with Histograms", status = "primary", width = NULL, 
                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                     "Loading..."),
                    
                    plotOutput("relation_plot"),
                    htmlOutput("printrelation"),
                    box(title = "Statistics", status = "success", width = NULL, 
                        withMathJax(),
                        #replace \ with \\ to run in this scenario
                        helpText('$$ T_{Air} = 1.31*T_{Sea} -3.44 $$
                                 $$ p < 2.2 * 10^{-16}$$
                                 Air temperature and sea temperature are strongly related.')
                        ))
                    
                    ),
                column(width = 6,
                box(title = "Buoy Location", status = "warning", width = NULL, 
                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                     "Loading..."),
                    leafletOutput("buoy_map")
                ),
                box(title = "Buoy Picture", status = "danger", width = NULL,
                    img(src='http://www.ndbc.noaa.gov/images/stations/3mfoam.jpg',width="100%"))
              ))
      ),
      tabItem(tabName = "Trend",
              fluidRow(
                tabBox(
                title = "Yearly mean",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", width = 6,
                tabPanel("Air Temperature", conditionalPanel(condition="$('html').hasClass('shiny-busy')","Loading..."),
                         plotOutput("statistics_plot1"),
                         
                         box(title = "Statistics", status = "success", width = NULL, 
                             withMathJax(),
                             #replace \ with \\ to run in this scenario
                             helpText('$$ T_{Air} = 0.06*t_{yearly} - 2.20 $$
$$ p = 0.07$$
                                      
                                      If do annual mean, the trend disappear because of missing data.'))),
                         tabPanel("Sea Temperature", conditionalPanel(condition="$('html').hasClass('shiny-busy')","Loading..."),
                         plotOutput("statistics_plot2"),
                         box(title = "Statistics", status = "success", width = NULL, 
                             withMathJax(),
                             #replace \ with \\ to run in this scenario
                             helpText('$$ T_{Sea} = 0.05*t_{yearly} + 4.23 $$
                                      $$ p = 0.09$$
                                      If do annual mean, the trend disappear because of missing data.')))),
                tabBox(
                  title = "Monthly mean",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset2", width = 6, 
                  tabPanel("Air Temperature", conditionalPanel(condition="$('html').hasClass('shiny-busy')","Loading..."),
                           plotOutput("statistics_plot3"),
                           box(title = "Statistics", status = "success", width = NULL, 
                               withMathJax(),
                               #replace \ with \\ to run in this scenario
                               helpText('$$ T_{Air}= 2.50 -4.58*\\sin(\\frac{2\\pi}{12}\\cdot t)-2.63*\\cos(\\frac{2\\pi}{12}\\cdot t) +0.004*t$$
  $$ p = 4.04* 10^{-9}$$
                                        
                                        Monthly Linear Regression shows Strong warming trend of air temperature.'))),
                  tabPanel("Sea Temperature", conditionalPanel(condition="$('html').hasClass('shiny-busy')","Loading..."),
                           plotOutput("statistics_plot4"),
                           box(title = "Statistics", status = "success", width = NULL, 
                               withMathJax(),
                               #replace \ with \\ to run in this scenario
                               helpText('$$ T_{Sea}= 4.38 -3.62*\\sin(\\frac{2\\pi}{12}\\cdot t)-1.23*\\cos(\\frac{2\\pi}{12}\\cdot t) +0.004*t$$
  $$ p = 4.06* 10^{-13}$$
                                        
                                        Monthly Linear Regression shows Strong warming trend of sea temperature.')))))),
      tabItem(tabName = "Trend2",
              fluidRow(
                tabBox(
                  title = "Daily 12 o'clock",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset3", width = 6,
                  tabPanel("Air Temperature", conditionalPanel(condition="$('html').hasClass('shiny-busy')","Loading..."),
                           plotOutput("statistics_plot5"),
                           box(title = "Statistics", status = "success", width = NULL, 
                               withMathJax(),
                               #replace \ with \\ to run in this scenario
                               helpText('$$ T_{Air}= 2.49 -3.75*\\sin(\\frac{2\\pi}{365}\\cdot t)-3.78*\\cos(\\frac{2\\pi}{365}\\cdot t) +1.43 * 10^{-4}*t$$
  $$ p < 2 * 10^{-16}$$
                                        
                                        Daily Linear Regression shows Strong warming trend of air temperature.'))),
                  tabPanel("Sea Temperature", conditionalPanel(condition="$('html').hasClass('shiny-busy')","Loading..."),
                           plotOutput("statistics_plot6"),
                           box(title = "Statistics", status = "success", width = NULL, 
                               withMathJax(),
                               #replace \ with \\ to run in this scenario
                               helpText('$$ T_{Sea}= 4.42 -3.20*\\sin(\\frac{2\\pi}{365}\\cdot t)-2.20*\\cos(\\frac{2\\pi}{365}\\cdot t) +1.32 * 10^{-4}*t$$
  $$ p < 2 * 10^{-16}$$
                                        
                                        Daily Linear Regression shows Strong warming trend of sea temperature.')))),
                tabBox(
                  title = "Daily 0 o'clock",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset4", width = 6,
                  tabPanel("Air Temperature", conditionalPanel(condition="$('html').hasClass('shiny-busy')","Loading..."),
                           plotOutput("statistics_plot7"),
                           box(title = "Statistics", status = "success", width = NULL, 
                               withMathJax(),
                               #replace \ with \\ to run in this scenario
                               helpText('$$ T_{Air}= 2.71 -3.78*\\sin(\\frac{2\\pi}{365}\\cdot t)-3.85*\\cos(\\frac{2\\pi}{365}\\cdot t) +1.41 * 10^{-4}*t$$
  $$ p < 2 * 10^{-16}$$
                                        
                                        Daily Linear Regression shows Strong warming trend of air temperature. No difference from 12:00 data.'))),
                  tabPanel("Sea Temperature", conditionalPanel(condition="$('html').hasClass('shiny-busy')","Loading..."),
                           plotOutput("statistics_plot8"),
                           box(title = "Statistics", status = "success", width = NULL, 
                               withMathJax(),
                               #replace \ with \\ to run in this scenario
                               helpText('$$ T_{Sea}= 4.46 -3.22*\\sin(\\frac{2\\pi}{365}\\cdot t)-2.20*\\cos(\\frac{2\\pi}{365}\\cdot t) +1.36 * 10^{-4}*t$$
  $$ p < 2 * 10^{-16}$$
                                        
                                        Daily Linear Regression shows Strong warming trend of sea temperature. No difference from 12:00 data.')))))
              ),
      tabItem(tabName = "Trend3",
              fluidRow(
                tabBox(
                  title = "Hourly Since 1988", 
                  id = "tabset5", width = 12, 
                  tabPanel("Air Temperature", conditionalPanel(condition="$('html').hasClass('shiny-busy')","Loading..."),
                           plotOutput("statistics_plot9"),
                           box(title = "Statistics", status = "success", width = NULL, 
                               withMathJax(),
                               #replace \ with \\ to run in this scenario
                               helpText('$$ T_{Air}= 2.56 -3.79*\\sin(\\frac{2\\pi}{8766}\\cdot t)-3.81*\\cos(\\frac{2\\pi}{8766}\\cdot t) +5.99 * 10^{-6}*t$$
  $$ p < 2 * 10^{-16}$$
                                        
                                        Hourly Linear Regression shows Strong warming trend of air temperature.'))),
                  tabPanel("Sea Temperature", conditionalPanel(condition="$('html').hasClass('shiny-busy')","Loading..."),
                           plotOutput("statistics_plot10"),
                           box(title = "Statistics", status = "success", width = NULL, 
                               withMathJax(),
                               #replace \ with \\ to run in this scenario
                               helpText('$$ T_{Sea}= 4.44 -3.21*\\sin(\\frac{2\\pi}{8766}\\cdot t)-2.21*\\cos(\\frac{2\\pi}{8766}\\cdot t) +5.48 * 10^{-6}*t$$
  $$ p < 2 * 10^{-16}$$
                                        
                                        Hourly Linear Regression shows Strong warming trend of sea temperature.'))))
              )
      ),     
      tabItem(tabName = "part2",
              fluidRow(
                box(width = 9, title = "Scatter Plot", status = "primary",
                    plotOutput("veg_plot"),
                    box(title = "Warning", status = "warning", width = NULL, 
                        'Those pesticide with small LD50 but large amount of application should be watched out.'
                    )
                ),
                box(width = 3,
                  title = "Control Panel", status = "success",
                    selectInput("year", "Year:", choices=c(2006, 2010, 2014, 2016)),
                    selectInput("vegetable", "Vegetable:", choices= c('BROCCOLI ',
                                                                      'CAULIFLOWER '))
                )
              )
      )
    )    

    
  )
)


