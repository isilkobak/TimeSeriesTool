###### TIME SERIES FORECASTING WITH SHINY ###### 
# load required packages
require(shiny)
require(shinyjs)
require(magrittr) ## dplyr imports %>% but not %$%
## require(readr)
## require(tidyr)
## require(dplyr)
## require(ggplot2)
require(tidyverse)
require(ggrepel) ## for repelling texts/labels with geom_text or _label
require(jpeg) ## background image
require(grid) ## background image
require(forecast)
require(gridExtra)
require(forecastHybrid)
require(fpp)
library(zoo)
library(xts)
library(ggplot2)
library(reshape2)
library(devtools)
library(data.table)

library(DT)
library(plotly)
library(dygraphs)
library(dplyr)
require(magrittr)

#set theme for ggplot2 graphics
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top",
                     axis.text.x = element_text(angle = 45, hjust = 1, vjust =1.25))


# Choices for drop-downs

myWeekends <- c("yes","no")

myDummy <- c("yes","no")

csvDummy <- read.csv("timeseries.csv")

############################################################
## shiny user interface function
############################################################


shinyUI(
  
  
  
  navbarPage(
    
    
    title="Time Series Forecasting",
    theme="bootstrap.css",
    inverse=TRUE,
    
    
    ##### FORECASTING TAB  #####                
    tabPanel("Time Series Forecasting", icon = icon("line-chart"),
             
             fluidPage(
               
               titlePanel('Time Series Forecasting', windowTitle='Workload Forecast'),
               
               fluidRow(
                 sidebarPanel(
                   fileInput("file1", "Upload your CSV file",
                             accept = c(
                               "text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
                   ),
                   h5("Try it out - download a dummy ", a("CSV with three daily time series", href=csvDummy, target="_blank")),
                   h5('You can find many ', a("more time series here", href='https://datamarket.com/data/list/?q=interval:day%20provider:tsdl', target="_blank")),
                   tags$hr(),
                   #selectInput(inputId="i_task_select", "Select Item",'',''),
                   uiOutput("selectComp"),
                   uiOutput("reacTimeIntervalUI"),
                   radioButtons(inputId="frequency",
                                label="Zaman Birimi",
                                choices= c("Hafta" = "Hafta", "Ay"="Ay"),
                                selected="Ay",
                                inline=TRUE),
                   uiOutput("reacProductListUI"),
                   radioButtons(inputId = "sortProductList", label=NA,
                                choices = c("Ad", 
                                            "S"="Siparis Miktari"),
                                selected="Ad", inline=TRUE),
                   # sliderInput(inputId="i_forecast_n","Forecast periods",value=30,min=2,max=120,step=1),
                 #  sliderInput(inputId = "i_forecast_n",
                 #              label = "Tahmin Zaman Araligi",
                 #             min = TimeInterval$min,
                 #             max = TimeInterval$max,
                 ##             ##value = c(TimeInterval$mid, TimeInterval$max),
                 #            value = c(TimeInterval$min, TimeInterval$max),
                 #             step = 1, sep ="", dragRange = TRUE),
                   radioButtons(inputId="i_weekends", "Include weekends", myWeekends, selected = myWeekends[2], inline = T),
                   radioButtons(inputId="i_dummy", "Adjust forecast for Events", myDummy, selected = myDummy[2], inline = T),
                   actionButton(inputId="goButton", "Start forecasting!"),
                   br(),
                   br(),
                   downloadButton('downloader', 'Download forecasts'),
                   width=3),
                 
  
                 column(7,
                        ## To have the plots fill the browser page
                        tabsetPanel(id = "theTabs",
                                    tabPanel("Zaman serileri",
                                             downloadButton("downloadPlot.timeSeries", "Grafiği indir"),
                                             plotOutput("timeSeries"),
                                             value="zamanSerisi"
                                    ), tabPanel("Yıllık Sipariş",
                                                downloadButton("downloadPlot.yearPlot", "Grafiği indir"),
                                                plotOutput("yearPlot"),
                                                value="yıllık"
                                    ),
                            
                                    tabPanel("Sezon ve Trend",
                                             downloadButton("downloadPlot.stl", "Grafiği indir"),                                     
                                             plotOutput("stlPlot"),
                                             value="stl"),

                                    tabPanel("Tahmin",
                                        
                                             downloadButton("downloadPlot.forecast", "Grafiği indir"),
                                             plotOutput("forecast"),
                                             value="tahmin"),                            
                                    tabPanel("Analiz",
                                             div(class="analiz",
                                                 div(h4("Artıkların analizi"),
                                                     tableOutput("accuracy")),
                                                 div(h4("Box test (H0: artıklar=beyaz gürültü)"),
                                                     tableOutput("BoxTestResult"))
                                             ),
                                             downloadButton("downloadPlot.resids", "Grafiği indir"),
                                             plotOutput("artiklar"),
                                             value="analiz"),

                                    
                                    tabPanel("Guvenlik Stogu",
                                             tableOutput("sstockTable"), 
                                             value="sstockTab"
                                             
                                    )
                        )
                 ),
                 column(2,
                        uiOutput("sideBarMenuRight")
                 ))
               
               
             #  fluidRow(
              #   column(width=6,
               #         h4("Forecast Values with Upper and Lower 95% Confidence Intervals"),
                #        DT::dataTableOutput("time_series_table")),
                 #column(width=6,
                  #      h4("Performance Metrics"),
                   #     DT::dataTableOutput("performance_metrics"))
               #)

               
             )
    )
  )
)