#TLC FastDash---------------------------------------------------------------------------------------------------------------------
#author:fausto Lopez
#purpose: quick simple accessible dashboard for public metrics

#load required libraries
#libs = c('data.table','shiny','shinydashboard','scales','lubridate'
#         ,'DT','Hmisc', 'fasttime','zoo','ggplot2','plotly')
#lapply(libs, require, character.only = T)

library(data.table)
library(shiny)
library(shinydashboard)
library(scales)
library(DT)
library(Hmisc)
library(zoo)
library(plotly)

#source data-----------------------------------------------------------------------------------------------------------------------------

#yellow = fread("http://www.nyc.gov/html/tlc/downloads/csv/data_reports_monthly_indicators_yellow.csv")
#green = fread("http://www.nyc.gov/html/tlc/downloads/csv/data_reports_monthly_indicators_shl.csv")

industry_metrics = fread("https://www1.nyc.gov/assets/tlc/downloads/csv/data_reports_monthly_indicators.csv")

pal = c("black", "maroon", "blue", "orange", "dark green", "gold")
names(industry_metrics) = tolower(gsub(" ","_", names(industry_metrics)))
industry_metrics = setDT(data.frame(lapply(industry_metrics, function(x) {
  gsub("%","",gsub(",", "", x),x)
})))[
  ,percent_of_trips_paid_with_credit_card:=as.numeric(
    as.character(
      percent_of_trips_paid_with_credit_card
    )
  )/100][
    ,month_year:=`month.year`][
      ,`month.year`:=NULL]

#pull columns
cols = c("trips_per_day", "farebox_per_day", "trips_per_day_shared"
         , "unique_drivers", "unique_vehicles", "vehicles_per_day"
         , "percent_of_trips_paid_with_credit_card")

#turn columns numeric lapply being finicky in shiny 
#observe({
# industry_metrics[,(cols):= lapply(.SD, as.character), .SDcols = cols]
#})
#observe({
# industry_metrics[,(cols):= lapply(.SD, as.numeric), .SDcols = cols]
#})

#going old school
industry_metrics$trips_per_day=as.numeric(as.character(industry_metrics$trips_per_day))
industry_metrics$farebox_per_day=as.numeric(as.character(industry_metrics$farebox_per_day))
industry_metrics$trips_per_day_shared=as.numeric(as.character(industry_metrics$trips_per_day_shared))
industry_metrics$unique_drivers=as.numeric(as.character(industry_metrics$unique_drivers))
industry_metrics$unique_vehicles=as.numeric(as.character(industry_metrics$unique_vehicles))
industry_metrics$vehicles_per_day=as.numeric(as.character(industry_metrics$vehicles_per_day))
industry_metrics$percent_of_trips_paid_with_credit_card=as.numeric(as.character(industry_metrics$percent_of_trips_paid_with_credit_card))
industry_metrics$avg_days_vehicles_on_road = as.numeric(as.character(industry_metrics$avg_days_vehicles_on_road))
industry_metrics$avg_hours_per_day_per_vehicle = as.numeric(as.character(industry_metrics$avg_hours_per_day_per_vehicle))
industry_metrics$avg_days_drivers_on_road   = as.numeric(as.character(industry_metrics$avg_days_drivers_on_road))        
industry_metrics$avg_hours_per_day_per_driver = as.numeric(as.character(industry_metrics$avg_hours_per_day_per_driver))          
industry_metrics$avg_minutes_per_trip = as.numeric(as.character(industry_metrics$avg_minutes_per_trip))


industry_metrics[,month_year1:=as.yearmon(month_year)][
  ,days:= monthDays(as.Date(month_year1))][
    ,trips_per_month:= trips_per_day*days][
      ,month_date:= as.Date(paste(month_year,"-28",sep=""))][
        ,farebox_per_month:= farebox_per_day * days][
          ,farebox_per_month:= farebox_per_month][
            ,week:=strftime(month_date, format="%W")][
              ,week:= as.factor(week)][
                ,trips_per_week:=trips_per_day * 7][
                  ,year:=format(as.yearmon(month_year), "%Y")][
                    ,farebox_per_week:= farebox_per_day * 7][
                      ,license_class:= as.factor(license_class)][
                        ,total_trips_per_day:=trips_per_day + trips_per_day_shared][
                          ,shared_trips_per_day_percent:=round(trips_per_day_shared/total_trips_per_day,2)]


#copy for compatibility with old code
industry_metrics = industry_metrics
industry_metrics$days = NULL

setorder(industry_metrics, license_class, -month_date)
industry_metrics = as.data.frame(industry_metrics)


#ui.R-----------------------------------------------------------------------------------------------------------------------------------------
ui = dashboardPage(skin = "yellow",
                   dashboardHeader(title = "TLC FastDash"),
                   dashboardSidebar(
                     sidebarMenu(
                       menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                       menuItem("Data Bank", tabName = "databank", icon =icon("fas fa-database")),
                       menuItem("Source code", icon = icon("file-code-o"), 
                                href = "https://gitlab.com/maverick_tlc/tlc_fast_dash.git")
                     )
                   ),
                   # Body content
                   dashboardBody(
                     tabItems(
                       tabItem(tabName = "dashboard",
                               fluidRow(
                                 #Dynamic infoBoxes
                                 valueBoxOutput("yellowtripbox", width = 3),
                                 valueBoxOutput("greentripbox", width = 3),
                                 valueBoxOutput("hvtripbox", width = 3),
                                 valueBoxOutput("bctripbox", width = 3),
                                 valueBoxOutput("lxtripbox", width = 3),
                                 valueBoxOutput("lvtripbox", width = 3),
                                 valueBoxOutput("hvsharing", width = 6),
                                 #box(textOutput("textbox2"), width = 6),
                                 #box(textOutput("textbox3")),
                                 box(textOutput("textbox4"), width = 6)
                               ),
                               fluidRow(
                                 box(background = "green", dateRangeInput("monthdate", label = h3("Choose a Date Range"),
                                                                          start = '2014-01-01',
                                                                          end = as.Date(Sys.time()))),
                                 box(background="green", selectInput(inputId = "dimension", label = strong("Choose Metric"),
                                                                     choices = c('Trips, Drivers & Vehicles'='1', 'Time & Money' = '2'), 
                                                                     multiple = FALSE, selectize = TRUE)),
                                 box(textOutput("textbox"))
                               ),
                               
                               fluidRow(
                                 box(plotlyOutput(outputId = 'trips_per_day'), width = 6),
                                 box(plotlyOutput(outputId = 'trips_year'), width = 6)),
                               fluidRow(
                                 box(plotlyOutput(outputId = 'trips_per_month')),
                                 box(plotlyOutput(outputId = 'medallions_per_month'))
                               ),
                               #variable switch box
                               fluidRow(
                                 sidebarLayout(
                                   sidebarPanel(
                                     dateRangeInput("monthlydate", label = h3("Date Range"),start = '2016-01-01',
                                                    end = as.Date(Sys.time())),
                                     selectInput("element_id1_m", "Select Your Variable for x-axis", c("month_date", 
                                                                                                       "week",
                                                                                                       "year"), selected = "month_date"),
                                     selectInput("element_id2_m", "Select Your Variable for y-axis", c("trips_per_day",
                                                                                                       "trips_per_day_shared",
                                                                                                       "farebox_per_day",
                                                                                                       "unique_drivers",
                                                                                                       "unique_vehicles",
                                                                                                       "avg_minutes_per_trip",
                                                                                                       "avg_days_vehicles_on_road", 
                                                                                                       "avg_hours_per_day_per_vehicle",
                                                                                                       "avg_days_drivers_on_road",
                                                                                                       "avg_hours_per_day_per_driver", 
                                                                                                       "percent_of_trips_paid_with_credit_card"), selected = "trips"),
                                     selectInput("element_id3_m", "Select Your Grouping Variable", c("license_class"), selected = 'license_class')),
                                   mainPanel(h3("Outputs"),
                                             textOutput("id1_m"),
                                             textOutput("id2_m"),
                                             textOutput("id3_m"),
                                             plotlyOutput("plt_m"),
                                             plotlyOutput("plt2_m"))
                                 ))
                       ),
                       tabItem(tabName = "databank", 
                               (fluidPage(
                                 title = 'Table Access',
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                     conditionalPanel(
                                       'input.dataset === "industry_metrics"',
                                       checkboxGroupInput('show_vars', 'Columns in Data Set to show:',
                                                          names(industry_metrics), selected = names(industry_metrics))
                                     ),
                                     downloadButton('downloadData1', 'Download Data Set')
                                   ),
                                   mainPanel(
                                     tabsetPanel(
                                       id = 'dataset',
                                       tabPanel('industry_metrics', DT::dataTableOutput('mytable1'))
                                     )
                                   )
                                 )
                               )
                               )))
                   )
)

