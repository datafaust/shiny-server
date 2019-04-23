#TLC FastDash---------------------------------------------------------------------------------------------------------------------
#author:fausto Lopez
#purpose: quick simple accessible dashboard for public metrics



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

