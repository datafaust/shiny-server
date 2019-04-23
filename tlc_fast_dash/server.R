#server.R  -------------------------------------------------------------------------------------------------

server = function(input, output) {
  
  #trips per day----
  output$trips_per_day = renderPlotly({
    start_date1 = input$monthdate[1]
    end_date1 = input$monthdate[2]
    td = subset(industry_metrics, 
                (month_date >= start_date1 & 
                   month_date <= end_date1), c('trips_per_day', 'month_date', 'license_class'))
    
    trips = plot_ly(td, x = ~month_date
                    , y = ~trips_per_day
                    ,type = 'scatter'
                    , split = ~license_class
                    , mode = 'lines'
                    ,color = ~license_class
                    ,colors = pal)   
    
    trips = layout(trips,             
                   title = "Average Trips per Day each Month", 
                   xaxis = list(           
                     title = "Month & Year",   
                     showgrid = F        
                   ),
                   yaxis = list(           
                     title = "Trips Per Day"      
                   ))
    
    #farebox per day----
    if (input$dimension == '2') {
      start_date1 = input$monthdate[1]
      end_date1 = input$monthdate[2]
      
      td = subset(industry_metrics,
                  (month_date >= start_date1 & month_date <= end_date1), 
                  c('farebox_per_day', 'month_date', 'license_class'))
      
      trips = plot_ly(td, x = ~month_date, y = ~farebox_per_day
                      , type = 'scatter'
                      , split = ~license_class
                      , mode = 'lines'
                      ,color = ~license_class
                      ,colors = pal)
      
      trips = layout(trips,              
                     title = "Average Farebox Per Day each Month", 
                     xaxis = list(          
                       title = "Month & Year",    
                       showgrid = F       
                     ),
                     yaxis = list(           
                       title = "Farebox Per Day"     
                     ))
    }
    trips
  })
  
  #trips per year----
  output$trips_year = renderPlotly({
    
    start_date1 = input$monthdate[1]
    end_date1 = input$monthdate[2]
    
    td = subset(industry_metrics, 
                (month_date >= start_date1 & month_date <= end_date1), 
                c('trips_per_month','trips_per_day','month_date','license_class', 'year','trips_per_week'))
    
    
    nd = aggregate(trips_per_day ~ year + license_class, data = td, FUN = sum)
    
    uniks = plot_ly(nd
                    , x = ~year
                    , y = ~trips_per_day
                    , split = ~license_class
                    , type = 'bar'
                    ,color = ~license_class
                    ,colors = pal)
    uniks = layout(uniks,             
                   title = "*Average Trips Per Year", 
                   xaxis = list(           
                     title = "Month & Year",    
                     showgrid = F        
                   ),
                   yaxis = list(          
                     title = "Trips"     
                   ))
    
    #farebox per year------------------------------------------
    if (input$dimension == '2') {
      start_date1 = input$monthdate[1]
      end_date1 = input$monthdate[2]
      td =  subset(industry_metrics, 
                   (month_date >= start_date1 & month_date <= end_date1), 
                   c('farebox_per_day','month_date','license_class', 'year','trips_per_week'))
      
      nd = aggregate(farebox_per_day ~ year + license_class, data = td, FUN = sum)
      
      uniks = plot_ly(nd
                      , x = ~year
                      , y = ~farebox_per_day
                      , split = ~license_class
                      , type = 'bar'
                      ,color = ~license_class
                      ,colors = pal)
      uniks = layout(uniks,              
                     title = "*Average Farebox Per Year", 
                     xaxis = list(           
                       title = "Month & Year",     
                       showgrid = F        
                     ),
                     yaxis = list(           
                       title = "Farebox"      
                     ))
    }
    uniks
  })
  
  #trips per month------------------------------------------
  output$trips_per_month = renderPlotly({
    
    start_date1 = input$monthdate[1]
    end_date1 = input$monthdate[2]
    
    td =  subset(industry_metrics,
                 (month_date >= start_date1 & month_date <= end_date1), 
                 c('trips_per_month','month_date','license_class', 'year'))
    uniks = plot_ly(td, 
                    x = ~month_date, y = ~trips_per_month
                    , type = 'scatter'
                    , split = ~license_class
                    , mode = 'lines'
                    ,color = ~license_class
                    ,colors = pal)
    
    uniks = layout(uniks,             
                   title = "*Trips Per Month Over Time", 
                   xaxis = list(          
                     title = "Month & Year",     
                     showgrid = F      
                   ),
                   yaxis = list(           
                     title = "Trips Per Month"      
                   ))
    uniks                
    
    
    #farebox per month----- 
    if (input$dimension == '2') {
      start_date1 = input$monthdate[1]
      end_date1 = input$monthdate[2]
      td =  subset(industry_metrics, 
                   (month_date >= start_date1 & month_date <= end_date1), 
                   c('farebox_per_month','month_date','license_class', 'year'))
      
      uniks = plot_ly(td, 
                      x = ~month_date, y = ~farebox_per_month
                      , type = 'scatter'
                      , split = ~license_class
                      , mode = 'lines'
                      ,color = ~license_class
                      ,colors = pal)
      
      uniks = layout(uniks,              
                     title = "*Farebox Per Month Over Time", 
                     xaxis = list(           
                       title = "Month & Year",     
                       showgrid = F        
                     ),
                     yaxis = list(           
                       title = "Farebox Per Month"      
                     ))
    }
    uniks     
  })
  
  #vehicles per month----
  output$medallions_per_month = renderPlotly({
    
    start_date1 = input$monthdate[1]
    end_date1 = input$monthdate[2]
    td =  subset(industry_metrics, 
                 (month_date >= start_date1 & month_date <= end_date1), 
                 c('unique_vehicles','month_date','license_class'))
    uniks = plot_ly(td, 
                    x = ~month_date, y = ~unique_vehicles
                    , type = 'scatter'
                    , split = ~license_class
                    , mode = 'lines'
                    ,color = ~license_class
                    ,colors = pal)
    uniks = layout(uniks,              
                   title = "Unique Vehicles Per Month Over Time", 
                   xaxis = list(          
                     title = "Month & Year",    
                     showgrid = F        
                   ),
                   yaxis = list(           
                     title = "Unique Vehicles"      
                   ))
    uniks                
    
    
    #vehicles_per_day ----
    if (input$dimension == '2') {
      start_date1 = input$monthdate[1]
      end_date1 = input$monthdate[2]
      td =  subset(industry_metrics, 
                   (month_date >= start_date1 & month_date <= end_date1), 
                   c('vehicles_per_day','month_date','license_class'))
      uniks = plot_ly(td, 
                      x = ~month_date, y = ~vehicles_per_day
                      , type = 'scatter'
                      , split = ~license_class
                      , mode = 'lines'
                      ,color = ~license_class
                      ,colors = pal)
      uniks = layout(uniks,              
                     title = "Vehicles Per Day Over Time", 
                     xaxis = list(           
                       title = "Month & Year",    
                       showgrid = F       
                     ),
                     yaxis = list(           
                       title = "Vehicless Per Day Per Month"      
                     ))
    }
    uniks     
  })
  
  #custom tool----
  output$id1_m = renderText({
    sprintf("You have selected %s on the x-axis", input$element_id1_m)
  })
  output$id2_m = renderText({
    sprintf("You have selected %s on the y-axis", input$element_id2_m)
  })
  output$id3_m = renderText({
    sprintf("You have selected %s as your spliting variable", input$element_id3_m)
  })
  
  
  output$plt_m = renderPlotly({
    start_date = input$monthlydate[1]
    end_date = input$monthlydate[2]
    
    td =  subset(industry_metrics, 
                 (month_date >= start_date & 
                    month_date <= end_date))
    
    print(td)
    boots = plot_ly(x = td[,input$element_id1_m], y = td[,input$element_id2_m],# type = "bar", 
                    data = td, split = td[,input$element_id3_m]
                    , type = 'scatter'
                    , mode = 'lines'
                    ,color = ~license_class
                    ,colors = pal) 
    boots = layout(boots,             
                   title = "Monthly Industry Trends Over Time", 
                   xaxis = list(           
                     title = input$element_id1_m,     
                     showgrid = F    
                   ),
                   yaxis = list(          
                     title = input$element_id2_m      
                   ))
    
  })
  
  #data banks----
  output$mytable1 = DT::renderDataTable({
    DT::datatable(industry_metrics[, input$show_vars, drop = FALSE])
    
  })
  
  output$downloadData1 = downloadHandler(
    filename = function() {
      paste('monthly_indicators', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(industry_metrics[, input$show_vars, drop = FALSE], con)
    }
  )
  
  output$mytable2 = DT::renderDataTable({
    DT::datatable(education, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  output$downloadData1 = downloadHandler(
    filename = function() {
      paste('industry_metrics', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(industry_metrics, con)
    }
  )
  
  
  #value boxes-------------------------------------------------------------------------------------------
  output$yellowtripbox = renderValueBox({
    recent_date = as.character(industry_metrics[1, "month_date"])
    my_query = "*Average Yellow trips per day as of 'SAMPLE'"
    med_trips = round(mean(industry_metrics[industry_metrics$license_class=="Yellow", "trips_per_day"][1:3]))
    valueBox(
      paste0(med_trips), sub("SAMPLE",recent_date,my_query), icon = icon("fas fa-taxi"),
      color = "yellow")
  }) 
  output$greentripbox = renderValueBox({
    recent_date = as.character(industry_metrics[1, "month_date"])
    my_query = "*Average Green trips per day as of 'SAMPLE'"
    shl_trips = round(mean(industry_metrics[industry_metrics$license_class=="Green", "trips_per_day"][1:3]))
    valueBox(
      paste0(shl_trips), sub("SAMPLE",recent_date,my_query), icon = icon("fas fa-taxi"),
      color = "green")
  }) 
  output$hvtripbox = renderValueBox({ 
    recent_date = as.character(industry_metrics[1, "month_date"])
    my_query = "Average FHV - High Volume trips per day as of 'SAMPLE'"
    ubers_etc = round(mean(industry_metrics[industry_metrics$license_class == 'FHV - High Volume', "trips_per_day"][1:3])) #simple[3,2]
    valueBox(
      paste0(ubers_etc), sub("SAMPLE",recent_date,my_query), icon = icon("fas fa-taxi"),
      color = "maroon")
  })
  output$bctripbox = renderValueBox({ 
    recent_date = as.character(industry_metrics[1, "month_date"])
    my_query = "Average FHV - Black Car trips per day as of 'SAMPLE'"
    ubers_etc = round(mean(industry_metrics[industry_metrics$license_class == 'FHV - Black Car', "trips_per_day"][1:3])) #simple[3,2]
    valueBox(
      paste0(ubers_etc), sub("SAMPLE",recent_date,my_query), icon = icon("fas fa-taxi"),
      color = "purple")
  })
  output$lxtripbox = renderValueBox({ 
    recent_date = as.character(industry_metrics[1, "month_date"])
    my_query = "Average FHV - Lux Limo trips per day as of 'SAMPLE'"
    ubers_etc = round(mean(industry_metrics[industry_metrics$license_class == 'FHV - Lux Limo', "trips_per_day"][1:3])) #simple[3,2]
    valueBox(
      paste0(ubers_etc), sub("SAMPLE",recent_date,my_query), icon = icon("fas fa-taxi"),
      color = "orange")
  })
  output$lvtripbox = renderValueBox({ 
    recent_date = as.character(industry_metrics[1, "month_date"])
    my_query = "Average FHV - Livery trips per day as of 'SAMPLE'"
    ubers_etc = round(mean(industry_metrics[industry_metrics$license_class == 'FHV - Livery', "trips_per_day"][1:3])) #simple[3,2]
    valueBox(
      paste0(ubers_etc), sub("SAMPLE",recent_date,my_query), icon = icon("fas fa-taxi"),
      color = "aqua")
  })
  output$hvsharing = renderValueBox({ 
    recent_date = as.character(industry_metrics[1, "month_date"])
    my_query = "% of FHV - High Volume trips per day are shared as of 'SAMPLE'"
    shared = paste(round(mean(industry_metrics[industry_metrics$license_class == 'FHV - High Volume', "shared_trips_per_day_percent"][1:3]),2) * 100, "%") #simple[3,2]
    valueBox(
      paste0(shared), sub("SAMPLE",recent_date,my_query), icon = icon("fas fa-taxi"),
      color = "red")
  })
  #choose columns to display----
  output$mytable = renderDataTable({
    industry_metrics
  })
  
  output$textbox = renderText({
    print("*Note that the * next to graphs designates these aggregations are based on daily averages going back and not on summations over selected periods")
  })
  
  #output$textbox2 = renderText({
  #  print("*Input a date range to see changes over time")
  #})
  
  output$textbox4 = renderText({
    print("*Averages above are 3 month rolling")
  })
  
  #output$textbox3 = renderText({
  # print("*Use the dropdown menu to select different metrics")
  #})
}
