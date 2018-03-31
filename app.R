
source( 'SourceOfSource.R' )    #... Loading necessary libraries and files

ui <-navbarPage(
  

  
  title = 'Dashboard',
# ....... ui tab C ......

tabPanel(

strong( 'Chicago Airport Analysis' ),

fluidRow(

column( 2, selectInput( "Input_Airport", label = "Airport", choices = list( "Midway" = "Midway", "OHare" = "OHare" ) ) ),

column( 2, selectInput( 'Project_Month', label = 'Month', choices = list( "January" = "jan", "February" = "feb",

" March" = "mar", "April" = "apr", "May" = "may", "June" = "jun", "July" = "jul",

"August" = "aug", "September" = "sep", "October" = "oct", "November" = "nov",

"December" = "dec" ), selected = "jan" ) ),

column( 2, selectInput( "Time_Format", label = "Time Format", choices = list( "12_Hour" = "12_Hour", "24_Hour" = "24_Hour" ) ) ),

br(),

column( 2, actionButton( 'project_submit_button', strong( 'Submit' ), width = '100%' ) )

),

br(),

fluidRow(

column(12,plotlyOutput( "Total_Dep_Arr_Table_Plot"  ))#12, plotlyOutput( "Total_Dep_Arr_Table_Plot" )
#  , column( 5, dataTableOutput( "Total_Dep_Arr_Table" ) )
),

br(),

fluidRow(

column( 12, dataTableOutput( "Total_Dep_Arr_Table" ) )

),

br(),

fluidRow(

column( 12, plotlyOutput( "Dep_Arr_Freq_Table_Plot" ) )

),

br(),

fluidRow(

column( 12, dataTableOutput( "Dep_Arr_Freq_Table" ) )

),

br(),

fluidRow(

column( 12, plotlyOutput( "Total_Dep_Arr_Each_Day_Table_Plot" ) )  # change

),

br(),

fluidRow(

column( 12, dataTableOutput( "Total_Dep_Arr_Each_Day_Table" ) )  # change

),

br(),

fluidRow(

column( 12, plotlyOutput( "Total_Delay_Table_Plot" ) )

),

br(),

fluidRow(

column( 12, dataTableOutput( "Total_Delay_Table" ) )

),

fluidRow(

column( 12, plotlyOutput( "Top_15_Dep_Airport_Table_Plot" ) )

),

br(),

fluidRow(

column( 12, plotlyOutput( "Top_15_Arr_Airport_Table_Plot" ) )

),

br(),

fluidRow(

column( 12, dataTableOutput( "Top_15_Dep_Arr_Airport_Table" ) )

)

),

# ....... ui tab A ......

tabPanel(

strong( 'Indivdual Airport Analysis' ),

fluidRow(

column( 4, selectInput( 'Input_Airline', label = 'City', choices = list( 'City' = 'City' ) ) ),

column( 2, selectInput( 'Input_Date', label = 'Date', choices = list( 'Date' = 'Date' ) ) ),

column( 2, selectInput( 'Input_Day', label = 'Day', choices = list( 'Day' = 'Day' ) ) ),

column( 2, selectInput( 'Input_Delay', label = 'Delay Type', choices = list( 'Delay' = 'Delay' ) ) ),

column( 2, selectInput( "A_Time_Format", label = "Time Format", choices = list( "12_Hour" = "12_Hour", "24_Hour" = "24_Hour" ) ) )

),

br(),

fluidRow(

column( 12, plotlyOutput( "Total_Dep_Arr_Change_Over_24_Hours_Table_Plot" ) )

),

br(),

fluidRow(

column( 12, dataTableOutput( "Total_Dep_Arr_Change_Over_24_Hours_Table" ) )

),

br(),

fluidRow(

column( 12, plotlyOutput( "Total_Dep_Arr_Change_Over_12_Months_Table_Plot" ) )

),

br(),

fluidRow(

column( 12, dataTableOutput( "Total_Dep_Arr_Change_Over_12_Months_Table" ) )

),

br(),

fluidRow(

column( 12, dataTableOutput( "Selected_Date_Dep_Arr_Delay_Table" ) )

),

br(),

fluidRow(

column( 12, dataTableOutput( "Selected_Day_Dep_Arr_Delay_Table" ) )

),

br(),

fluidRow(

column( 12, plotlyOutput( "Selected_Delay_Change_Over_24_Hours_Table_Plot" ) )

),

br(),

fluidRow(

column( 12, dataTableOutput( "Selected_Delay_Change_Over_24_Hours_Table" ) )

),

br(),

fluidRow(

column( 12, plotlyOutput( "Selected_Delay_Change_Over_12_Months_Table_Plot" ) )

),

br(),

fluidRow(

column( 12, dataTableOutput( "Selected_Delay_Change_Over_12_Months_Table" ) )

),

br(),

fluidRow(

column( 12, dataTableOutput( "Dep_Arr_Total_Percent_Table" ) )

),

br(),

fluidRow(

column( 12, plotlyOutput( "Interesting_Top_10_Day_Table_Plot" ) )

),

br(),

fluidRow(

column( 12, dataTableOutput( "Interesting_Top_10_Day_Table" ) )

)

)
,
tabPanel(

strong( 'About' ),
fluidRow(

column( 4, helpText( p( strong( 'Author : ' ), "Margi Katwala, Aditya Sinha and Vignan Thumu" ),
p( strong( 'Project Details : ' ), "Project 2 - Learning to Fly" ),
p( strong( 'Libraries Used : ' ), "shiny, data.table, ggplot2, tidyr, ggthemes, plotly" ) ) )


)
)

             
)

server <- function(input, output,session){
  
  dataInput_airport_tables_info = eventReactive( input$project_submit_button, {
      
      airport_name = tolower( input$Input_Airport )
      
      month_name = input$Project_Month
      
      month_raw_data= read.csv( paste0( month_name, ".csv" ), stringsAsFactors = F )
      
      if( airport_name == "midway" ){
          
          ORIGIN_AIRPORT_ID_13232_index = which( month_raw_data$ORIGIN_AIRPORT_ID == 13232 )
          
          DEST_AIRPORT_ID_13232_index = which( month_raw_data$DEST_AIRPORT_ID == 13232 )
          
          all_index = c( ORIGIN_AIRPORT_ID_13232_index, DEST_AIRPORT_ID_13232_index )
          
          month_data = month_raw_data[ all_index, ]
          
      } else{
          
          ORIGIN_AIRPORT_ID_13930_index = which( month_raw_data$ORIGIN_AIRPORT_ID == 13930 )
          
          DEST_AIRPORT_ID_13930_index = which( month_raw_data$DEST_AIRPORT_ID == 13930 )
          
          all_index = c( ORIGIN_AIRPORT_ID_13930_index, DEST_AIRPORT_ID_13930_index )
          
          month_data = month_raw_data[ all_index, ]
          
      }
      
      #table and chart showing the total number of departures and total number of arrivals for all of the domestic airlines
      
      df_1 = month_data[ -which( month_data$CANCELLED == 1 ), c( "ORIGIN_CITY_NAME", "DEST_CITY_NAME", "CANCELLED" ) ]
      
      origin_airport_name = unique( c( as.character( df_1$ORIGIN_CITY_NAME ), as.character( df_1$DEST_CITY_NAME ) ) )
      
      table_output = lapply( seq_along( origin_airport_name ), function( name_index ){
          
          tab_out = data.frame( airport = origin_airport_name[name_index], dep_cnt = length( which( as.character( df_1$ORIGIN_CITY_NAME ) == origin_airport_name[name_index] ) ),
          
          arrv_cnt = length( which( as.character(df_1$DEST_CITY_NAME) == origin_airport_name[name_index] ) ) )
          
      })
      
      total_dep_arr_table_0 = bind_rows(table_output)   #1st problem table
      
      total_dep_arr_table = total_dep_arr_table_0[ -which( total_dep_arr_table_0$airport == "Chicago, IL" ), ]
      
      names( total_dep_arr_table ) = c( "CITY_NAME", "DEPARTURE", "ARRIVAL" )
      
      # table and chart showing the total number of departures and total number of arrivals for each hour of the day across that month (i.e. how many from 9am to 10am summed over every day of that month)
      
      if( airport_name == "midway" ){
          
          ORIGIN_AIRPORT_ID_13232_index = which( month_raw_data$ORIGIN_AIRPORT_ID == 13232 )
          
          DEST_AIRPORT_ID_13232_index = which( month_raw_data$DEST_AIRPORT_ID == 13232 )
          
          df_time_00_dep = month_raw_data[ ORIGIN_AIRPORT_ID_13232_index, ]
          
          df_time_00_arr = month_raw_data[ DEST_AIRPORT_ID_13232_index, ]
          
      } else{
          
          ORIGIN_AIRPORT_ID_13930_index = which( month_raw_data$ORIGIN_AIRPORT_ID == 13930 )
          
          DEST_AIRPORT_ID_13930_index = which( month_raw_data$DEST_AIRPORT_ID == 13930 )
          
          df_time_00_dep = month_raw_data[ ORIGIN_AIRPORT_ID_13930_index, ]
          
          df_time_00_arr = month_raw_data[ DEST_AIRPORT_ID_13930_index, ]
          
      }
      
      df_time__dep_00 = df_time_00_dep[ -which( df_time_00_dep$CANCELLED == 1 ) , ]
      
      df_time__arr_00 = df_time_00_arr[ -which( df_time_00_arr$CANCELLED == 1 ) , ]
      
      df_time__dep_0 = df_time__dep_00$DEP_TIME ; df_time__arr_0 = df_time__arr_00$ARR_TIME
      
      # Departure and Arrival time calculation
      
      hr_vec = c( seq( 0 , 2300 , 100 ), 2359 )
      
      i = 1 ; total_dep_list = list() ; total_arr_list = list()
      
      for( i in 1:24 ){
          
          total_dep_list[[i]] = length( which( ( df_time__dep_0 >= hr_vec[i] )&( df_time__dep_0 < hr_vec[i+1] ) ) )
          
          total_arr_list[[i]] = length( which( ( df_time__arr_0 >= hr_vec[i] )&( df_time__arr_0 < hr_vec[i+1] ) ) )
          
      }
      
      dep_freq = unlist( total_dep_list )
      
      arr_freq = unlist( total_arr_list )
      
      dep_arr_freq_table = data.frame( dep_freq = dep_freq, arr_freq = arr_freq )
      
      dep_arr_freq_table$Format_24_Hour = c( "00:00-01:00", "01:00-02:00", "02:00-03:00", "03:00-04:00", "04:00-05:00",
      
      "05:00-06:00", "06:00-07:00", "07:00-08:00", "08:00-09:00", "09:00-10:00",
      
      "10:00-11:00", "11:00-12:00", "12:00-13:00", "13:00-14:00", "14:00-15:00",
      
      "15:00-16:00", "16:00-17:00", "17:00-18:00", "18:00-19:00", "19:00-20:00",
      
      "20:00-21:00", "21:00-22:00", "22:00-23:00", "23:00-23:59")
      
      dep_arr_freq_table$Format_12_Hour = c( "00:00-01:00 AM", "01:00-02:00 AM", "02:00-03:00 AM", "03:00-04:00 AM", "04:00-05:00 AM",
      
      "05:00-06:00 AM", "06:00-07:00 AM", "07:00-08:00 AM", "08:00-09:00 AM", "09:00-10:00 AM",
      
      "10:00-11:00 AM", "11:00-00:00 PM", "00:00-01:00 PM", "01:00-02:00 PM", "02:00-03:00 PM",
      
      "03:00-04:00 PM", "04:00-05:00 PM", "05:00-06:00 PM", "06:00-07:00 PM", "07:00-08:00 PM",
      
      "08:00-09:00 PM", "09:00-10:00 PM", "10:00-11:00 PM", "11:00-00:00 AM")
      
      dep_arr_freq_table = dep_arr_freq_table[ , c( "Format_24_Hour", "Format_12_Hour", "dep_freq", "arr_freq" ) ]
      
      # table and chart showing the total number of departures and total number of arrivals for each day of the week across that month (i.e. how many on all of the mondays of that month)
      
      origin_city_day_df_0 = df_time__dep_00; dest_city_day_df_0 = df_time__arr_00
      
      origin_city_day_df_0$FL_DAY = weekdays( as.Date( origin_city_day_df_0$FL_DATE ) ); dest_city_day_df_0$FL_DAY = weekdays( as.Date( dest_city_day_df_0$FL_DATE ) )
      
      origin_city_day_df = origin_city_day_df_0[ , c( "FL_DAY", "ORIGIN_CITY_NAME" ) ]
      
      origin_city_day_df = origin_city_day_df[ complete.cases( origin_city_day_df ), ]
      
      dest_city_day_df = dest_city_day_df_0[ , c( "FL_DAY", "DEST_CITY_NAME" ) ]
      
      dest_city_day_df = dest_city_day_df[ complete.cases( dest_city_day_df ), ]
      
      all_week_days = unique( dest_city_day_df$FL_DAY )
      
      l = 1; total_dep_arr_each_day = list()
      
      for( l in 1:length( all_week_days ) ){
          
          total_dep_arr_each_day[[l]] = data.frame( Days = all_week_days[l], Total_Departure = length( which( origin_city_day_df$FL_DAY == all_week_days[l] ) ),
          
          Total_Arrival = length( which( dest_city_day_df$FL_DAY == all_week_days[l] ) ) )
          
      }
      
      total_dep_arr_each_day_table = bind_rows( total_dep_arr_each_day )
      
      # table and chart showing the total number of delays for each hour of the day across that
      
      #month (i.e. how many from 9am to 10am summed over every day of that month), and what percentage that is of the total for that hour
      
      dep_delay_df = df_time__dep_00[ , c( "ORIGIN_CITY_NAME", "DEST_CITY_NAME", "CARRIER_DELAY", "DEP_TIME", "ARR_TIME"
      
      , "WEATHER_DELAY", "NAS_DELAY", "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY") ]
      
      arr_delay_df = df_time__arr_00[ , c( "ORIGIN_CITY_NAME", "DEST_CITY_NAME", "CARRIER_DELAY", "DEP_TIME", "ARR_TIME"
      
      , "WEATHER_DELAY", "NAS_DELAY", "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY") ]
      
      dep_delay_df = dep_delay_df[ complete.cases( dep_delay_df ), ]; arr_delay_df = arr_delay_df[ complete.cases( arr_delay_df ), ]
      
      #flight_name = unique( delay_df$ORIGIN_CITY_NAME )
      
      #flight_name_df = delay_df[ which( delay_df$ORIGIN_CITY_NAME == flight_name[1] ), ]
      
      dep_delay_df$TOTAL_DELAY = as.numeric( dep_delay_df$WEATHER_DELAY ) + as.numeric( dep_delay_df$NAS_DELAY ) + as.numeric( dep_delay_df$SECURITY_DELAY ) + as.numeric( dep_delay_df$CARRIER_DELAY ) + as.numeric( dep_delay_df$LATE_AIRCRAFT_DELAY )
      
      arr_delay_df$TOTAL_DELAY = as.numeric( arr_delay_df$WEATHER_DELAY ) + as.numeric( arr_delay_df$NAS_DELAY ) + as.numeric( arr_delay_df$SECURITY_DELAY ) + as.numeric( arr_delay_df$CARRIER_DELAY ) + as.numeric( arr_delay_df$LATE_AIRCRAFT_DELAY )
      
      selected_airport_dep_delay_df = dep_delay_df[ , c( "ORIGIN_CITY_NAME", "DEP_TIME", "TOTAL_DELAY" ) ]
      
      selected_airport_arr_delay_df = arr_delay_df[ , c( "DEST_CITY_NAME", "ARR_TIME", "TOTAL_DELAY" ) ]
      
      hr_vec = c( seq( 0 , 2300 , 100 ), 2359 )
      
      i = 1 ; total_departure_delay_list = list() ; total_arrival_delay_list = list()
      
      for( i in 1:24 ){
          
          total_departure_delay_list[[i]] = sum( selected_airport_dep_delay_df[ which( ( selected_airport_dep_delay_df$DEP_TIME >= hr_vec[i] )&( selected_airport_dep_delay_df$DEP_TIME < hr_vec[i+1] ) ), "TOTAL_DELAY" ] )
          
          total_arrival_delay_list[[i]] = sum( selected_airport_arr_delay_df[ which( ( selected_airport_arr_delay_df$ARR_TIME >= hr_vec[i] )&( selected_airport_arr_delay_df$ARR_TIME < hr_vec[i+1] ) ), "TOTAL_DELAY" ] )
          
      }
      
      total_delay_table = data.frame( Total_Dep_Delay = unlist( total_departure_delay_list ), Total_Arr_Delay = unlist( total_arrival_delay_list ) )
      
      total_delay_table$Format_24_Hour = c( "00:00-01:00", "01:00-02:00", "02:00-03:00", "03:00-04:00", "04:00-05:00", "05:00-06:00", "06:00-07:00", "07:00-08:00", "08:00-09:00", "09:00-10:00", "10:00-11:00", "11:00-12:00", "12:00-13:00", "13:00-14:00", "14:00-15:00", "15:00-16:00", "16:00-17:00", "17:00-18:00", "18:00-19:00", "19:00-20:00", "20:00-21:00", "21:00-22:00", "22:00-23:00", "23:00-23:59")
      
      total_delay_table$Format_12_Hour = c( "00:00-01:00 AM", "01:00-02:00 AM", "02:00-03:00 AM", "03:00-04:00 AM", "04:00-05:00 AM",
      
      "05:00-06:00 AM", "06:00-07:00 AM", "07:00-08:00 AM", "08:00-09:00 AM", "09:00-10:00 AM",
      
      "10:00-11:00 AM", "11:00-00:00 PM", "00:00-01:00 PM", "01:00-02:00 PM", "02:00-03:00 PM",
      
      "03:00-04:00 PM", "04:00-05:00 PM", "05:00-06:00 PM", "06:00-07:00 PM", "07:00-08:00 PM",
      
      "08:00-09:00 PM", "09:00-10:00 PM", "10:00-11:00 PM", "11:00-00:00 AM")
      
      total_delay_table = total_delay_table[ , c( "Format_24_Hour", "Format_12_Hour", "Total_Dep_Delay", "Total_Arr_Delay" ) ]
      
      # table and chart showing the number of flights for the most common 15 arrival and destination airports
      
      ORIGIN_DEST_CITY_df = month_data[ -which( month_data$CANCELLED == 1 ), c( "ORIGIN_CITY_NAME", "DEST_CITY_NAME" ) ]
      
      unique_ORIGIN_CITY_NAME = unique( ORIGIN_DEST_CITY_df$ORIGIN_CITY_NAME )
      
      freq_each_origin_city = unlist( lapply( unique_ORIGIN_CITY_NAME, function( ORIGIN_CITY ){
          
          freq = length( which( ORIGIN_DEST_CITY_df$ORIGIN_CITY_NAME == ORIGIN_CITY ) )
          
          return( freq )
          
      } ) )
      
      dep_df = data.frame( ORIGIN_CITY_NAME = unique_ORIGIN_CITY_NAME, TOTAL_DEP_FLIGHTS = freq_each_origin_city )
      
      dep_df = dep_df[ -which( dep_df$ORIGIN_CITY_NAME == "Chicago, IL" ), ]
      
      dep_df_index = order( dep_df$TOTAL_DEP_FLIGHTS, decreasing = T )[1:15]
      
      dep_df_0 = dep_df[dep_df_index, ]
      
      unique_DEST_CITY_NAME = unique( ORIGIN_DEST_CITY_df$DEST_CITY_NAME )
      
      freq_each_dest_city = unlist( lapply( unique_DEST_CITY_NAME, function( DEST_CITY ){
          
          freq_DEST_CITY = length( which( ORIGIN_DEST_CITY_df$DEST_CITY_NAME == DEST_CITY ) )
          
          return( freq_DEST_CITY )
          
      } ) )
      
      arr_df = data.frame( DEST_CITY_NAME = unique_DEST_CITY_NAME, TOTAL_ARR_FLIGHTS = freq_each_dest_city )
      
      arr_df = arr_df[ -which( arr_df$DEST_CITY_NAME == "Chicago, IL" ), ]
      
      arr_df_index = order( arr_df$TOTAL_ARR_FLIGHTS, decreasing = T )[1:15]
      
      arr_df_0 = arr_df[arr_df_index, ]
      
      top_15_dep_arr_airport_df = bind_cols( dep_df_0, arr_df_0 )
      
      output_tables = list( 'total_dep_arr_table' = total_dep_arr_table, 'dep_arr_freq_table' = dep_arr_freq_table, 'total_dep_arr_each_day_table' = total_dep_arr_each_day_table,
      
      #'total_dep_each_day_table' = total_dep_each_day_table, 'total_arr_each_day_table' = total_arr_each_day_table,
      
      'total_delay_table' = total_delay_table, 'top_15_dep_arr_airport_df' = top_15_dep_arr_airport_df )
      
      return( output_tables )
      
  } )
  
  #airport_tables_info = dataInput_airport_tables_info()
  
  output$Total_Dep_Arr_Table = renderDataTable( {
      
      dataInput_airport_tables_info()$total_dep_arr_table
      
  } )
  
  output$Dep_Arr_Freq_Table = renderDataTable( {
      
      Dep_Arr_Freq_Table_0 = dataInput_airport_tables_info()$dep_arr_freq_table
      
      Dep_Arr_Freq_Table_0[ , -2 ]
      
  } )
  
  output$Total_Dep_Arr_Each_Day_Table = renderDataTable( {
      
      dataInput_airport_tables_info()$total_dep_arr_each_day_table
      
  } )
  
  output$Total_Delay_Table = renderDataTable( {
      
      Total_Delay_Table_0 = dataInput_airport_tables_info()$total_delay_table
      
      Total_Delay_Table_0[ , -2 ]
      
  } )
  
  output$Top_15_Dep_Arr_Airport_Table = renderDataTable( {
      
      dataInput_airport_tables_info()$top_15_dep_arr_airport_df
      
  } )
  
  # Plots of the Tables
  #the first one
  output$Total_Dep_Arr_Table_Plot = renderPlotly({
      
      total_dep_arr_table_plot_data = dataInput_airport_tables_info()$total_dep_arr_table
      
      total_dep_arr_table_plot_data$CITY_NAME = factor( total_dep_arr_table_plot_data$CITY_NAME, levels = total_dep_arr_table_plot_data$CITY_NAME )
      
      plot_ly(total_dep_arr_table_plot_data, x = ~CITY_NAME, y = ~DEPARTURE, name = 'DEPARTURE', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4))%>%
      
      add_trace(y = ~ARRIVAL, name = 'ARRIVAL', line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
      
      layout(title = "Total number of departures and total number of arrivals for all of the domestic airlines",
      xaxis = list(title = ""),
      yaxis = list (title = "Total Departure & Arrival"))
      
  })
  
  output$Dep_Arr_Freq_Table_Plot = renderPlotly({
      
      Dep_Arr_Freq_time_format_type = input$Time_Format
      
      dep_arr_freq_table_plot_data = dataInput_airport_tables_info()$dep_arr_freq_table
      
      dep_arr_freq_table_plot_data$Format_24_Hour = factor( dep_arr_freq_table_plot_data$Format_24_Hour, levels = dep_arr_freq_table_plot_data$Format_24_Hour )
      
      dep_arr_freq_table_plot_data$Format_12_Hour = factor( dep_arr_freq_table_plot_data$Format_12_Hour, levels = dep_arr_freq_table_plot_data$Format_12_Hour )
      
      switch( Dep_Arr_Freq_time_format_type,
      
      "24_Hour" = plot_ly(dep_arr_freq_table_plot_data, x = ~Format_24_Hour, y = ~dep_freq, name = 'Departure Frequency', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
      
      add_trace(y = ~arr_freq, name = 'Arrival Frequency', line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
      
      layout(title = "Total number of departures and total number of arrivals for all of the domestic airlines",
      xaxis = list(title = ""),
      yaxis = list (title = "Departure & Arrival Frequency")),
      
      "12_Hour" = plot_ly(dep_arr_freq_table_plot_data, x = ~Format_12_Hour, y = ~dep_freq, name = 'Departure Frequency', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
      
      add_trace(y = ~arr_freq, name = 'Arrival Frequency', line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
      
      layout(title = "Total number of departures and total number of arrivals for all of the domestic airlines",
      xaxis = list(title = ""),
      yaxis = list (title = "Departure & Arrival Frequency"))
      
      )
      
  })
  
  output$Total_Dep_Arr_Each_Day_Table_Plot = renderPlotly({
      
      total_dep_each_day_table_plot_data =   dataInput_airport_tables_info()$total_dep_arr_each_day_table
      
      total_dep_each_day_table_plot_data$Days = factor( total_dep_each_day_table_plot_data$Days, levels = total_dep_each_day_table_plot_data$Days )
      
      plot_ly(total_dep_each_day_table_plot_data, x = ~Days, y = ~Total_Departure, name = 'Total Departure', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
      
      add_trace(y = ~Total_Arrival, name = 'Total Arrival', line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
      
      layout(title = "Total number of departures for each day of the week across that month",
      xaxis = list(title = ""),
      yaxis = list (title = "Total number of departures for each day of the week"))
      
  })
  
  output$Total_Delay_Table_Plot = renderPlotly({
      
      time_format_type = input$Time_Format
      
      total_delay_table_plot_data = dataInput_airport_tables_info()$total_delay_table
      
      total_delay_table_plot_data$Format_24_Hour = factor( total_delay_table_plot_data$Format_24_Hour, levels = total_delay_table_plot_data$Format_24_Hour )
      
      total_delay_table_plot_data$Format_12_Hour = factor( total_delay_table_plot_data$Format_12_Hour, levels = total_delay_table_plot_data$Format_12_Hour )
      
      switch( time_format_type,
      
      "24_Hour" = plot_ly(total_delay_table_plot_data, x = ~Format_24_Hour, y = ~Total_Dep_Delay, name = 'Total Departure Delay', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
      
      add_trace(y = ~Total_Arr_Delay, name = 'Total Arrival Delay', line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
      
      layout(title = "Total number of delays for each hour of the day",
      xaxis = list(title = ""),
      yaxis = list (title = "Total Delay")),
      
      "12_Hour" = plot_ly(total_delay_table_plot_data, x = ~Format_12_Hour, y = ~Total_Dep_Delay, name = 'Total Departure Delay', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
      
      add_trace(y = ~Total_Arr_Delay, name = 'Total Arrival Delay', line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
      
      layout(title = "Total number of delays for each hour of the day",
      xaxis = list(title = ""),
      yaxis = list (title = "Total Delay"))
      
      )
      
  })
  
  output$Top_15_Dep_Airport_Table_Plot = renderPlotly({
      
      Top_15_Dep_Airport_Table_Plot_data =   dataInput_airport_tables_info()$top_15_dep_arr_airport_df
      
      Top_15_Dep_Airport_Table_Plot_data$ORIGIN_CITY_NAME = factor( Top_15_Dep_Airport_Table_Plot_data$ORIGIN_CITY_NAME, levels = Top_15_Dep_Airport_Table_Plot_data$ORIGIN_CITY_NAME )
      
      plot_ly(Top_15_Dep_Airport_Table_Plot_data, x = ~ORIGIN_CITY_NAME, y = ~TOTAL_DEP_FLIGHTS, name = 'TOTAL_DEP_FLIGHTS', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
      
      layout(title = "Top 15 Departure Airport",
      xaxis = list(title = ""),
      yaxis = list (title = "Total number of departure flights"))
      
  })
  
  output$Top_15_Arr_Airport_Table_Plot = renderPlotly({
      
      Top_15_Dep_Airport_Table_Plot_data =   dataInput_airport_tables_info()$top_15_dep_arr_airport_df
      
      Top_15_Dep_Airport_Table_Plot_data$DEST_CITY_NAME = factor( Top_15_Dep_Airport_Table_Plot_data$DEST_CITY_NAME, levels = Top_15_Dep_Airport_Table_Plot_data$DEST_CITY_NAME )
      
      plot_ly(Top_15_Dep_Airport_Table_Plot_data, x = ~DEST_CITY_NAME, y = ~TOTAL_ARR_FLIGHTS, name = 'TOTAL_ARR_FLIGHTS', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
      
      layout(title = "Top 15 Arrival Airport",
      xaxis = list(title = ""),
      yaxis = list (title = "Total number of arrival flights"))
      
  })
  
  
  
  all_months_data = whole_dataset$jan_to_dec
  
  #pick a destination/arrival airport from the top 50 and see how the number of flights to and from that
  
  #location change over the 24 hours of the day and the 12 months of the year
  
  # Calculating change over 24 hours
  
  observe({
      
      all_city_names = unique( c( as.character( all_months_data$ORIGIN_CITY_NAME ), as.character( all_months_data$DEST_CITY_NAME ) ) )
      
      updateSelectInput( session, 'Input_Airline', choices = all_city_names, selected = all_city_names[1] )
      
  })
  
  selected_airport_0 = reactive({ input$Input_Airline })
  
  observe({
      
      all_dates_0 = seq(as.Date('2017-01-01'),as.Date('2017-12-31'),by = 1 )
      
      all_dates = factor( all_dates_0 )
      
      updateSelectInput( session, 'Input_Date', choices = all_dates, selected = all_dates[1] )
      
  })
  
  selected_date_0 = reactive({ input$Input_Date })
  
  observe({
      
      all_week_days = c( "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" )
      
      updateSelectInput( session, 'Input_Day', choices = all_week_days, selected = all_week_days[1] )
      
  })
  
  selected_week_day_0 = reactive({ input$Input_Day })
  
  observe({
      
      all_delay_type = c( "CARRIER_DELAY", "WEATHER_DELAY", "NAS_DELAY", "LATE_AIRCRAFT_DELAY", "SECURITY_DELAY" )
      
      updateSelectInput( session, 'Input_Delay', choices = all_delay_type, selected = all_delay_type[1] )
      
  })
  
  selected_delay_type_0 = reactive({ input$Input_Delay })
  
  dataInput_airport_tables_info_A = reactive( {
      
      selected_airport = selected_airport_0() ; selected_date = selected_date_0() ; selected_day = selected_week_day_0(); selected_delay_type = selected_delay_type_0()
      
      ORIGIN_CITY_DEP_TIME_df = all_months_data[ -which( all_months_data$CANCELLED == 1 ), c( "ORIGIN_CITY_NAME", "DEP_TIME" ) ]
      
      selected_ORIGIN_CITY_DEP_TIME_df = ORIGIN_CITY_DEP_TIME_df[ which( ORIGIN_CITY_DEP_TIME_df$ORIGIN_CITY_NAME == selected_airport ), ]
      
      DEST_CITY_ARR_TIME_df = all_months_data[ -which( all_months_data$CANCELLED == 1 ), c( "DEST_CITY_NAME", "ARR_TIME" ) ]
      
      selected_DEST_CITY_ARR_TIME_df = DEST_CITY_ARR_TIME_df[ which( DEST_CITY_ARR_TIME_df$DEST_CITY_NAME == selected_airport ), ]
      
      hr_vec = c( seq( 0 , 2300 , 100 ), 2359 )
      
      i = 1 ; dep_time_change = list() ; arr_time_change = list()
      
      for( i in 1:24 ){
          
          dep_time_change[[i]] = length( which( ( selected_ORIGIN_CITY_DEP_TIME_df$DEP_TIME >= hr_vec[i] )&( selected_ORIGIN_CITY_DEP_TIME_df$DEP_TIME < hr_vec[i+1] ) ) )
          
          arr_time_change[[i]] = length( which( ( selected_DEST_CITY_ARR_TIME_df$ARR_TIME >= hr_vec[i] )&( selected_DEST_CITY_ARR_TIME_df$ARR_TIME < hr_vec[i+1] ) ) )
          
      }
      
      Format_24_Hour = c( "00:00-01:00", "01:00-02:00", "02:00-03:00", "03:00-04:00", "04:00-05:00",
      
      "05:00-06:00", "06:00-07:00", "07:00-08:00", "08:00-09:00", "09:00-10:00",
      
      "10:00-11:00", "11:00-12:00", "12:00-13:00", "13:00-14:00", "14:00-15:00",
      
      "15:00-16:00", "16:00-17:00", "17:00-18:00", "18:00-19:00", "19:00-20:00",
      
      "20:00-21:00", "21:00-22:00", "22:00-23:00", "23:00-23:59")
      
      Format_12_Hour = c( "00:00AM-01:00AM", "01:00AM-02:00AM", "02:00AM-03:00AM", "03:00AM-04:00AM", "04:00AM-05:00AM",
      
      "05:00AM-06:00AM", "06:00AM-07:00AM", "07:00AM-08:00AM", "08:00AM-09:00AM", "09:00AM-10:00AM",
      
      "10:00AM-11:00AM", "11:00AM-00:00PM", "00:00PM-01:00PM", "01:00PM-02:00PM", "02:00PM-03:00PM",
      
      "03:00PM-04:00PM", "04:00PM-05:00PM", "05:00PM-06:00PM", "06:00PM-07:00PM", "07:00PM-08:00PM",
      
      "08:00PM-09:00PM", "09:00PM-10:00PM", "10:00PM-11:00PM", "11:00PM-00:00AM" )
      
      # df-1
      
      change_over_24_hours = data.frame( Format_24_Hour = Format_24_Hour, Format_12_Hour = Format_12_Hour, Total_Departure = unlist( dep_time_change ), Total_Arrival = unlist( arr_time_change ) )
      
      # Calculating change over 12 months
      
      month_name = c( "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec" )
      
      j = 1; selected_airport_total_dep = list(); selected_airport_total_arr = list()
      
      for( j in 1:length( month_name ) ){
          
          current_month_ORIGIN_CITY_DEP_TIME_df = whole_dataset[[month_name[j]]][ -which( whole_dataset[[month_name[j]]]$CANCELLED == 1 ), c( "ORIGIN_CITY_NAME", "DEP_TIME" ) ]
          
          selected_airport_total_dep[[j]] =  nrow( current_month_ORIGIN_CITY_DEP_TIME_df[ which( current_month_ORIGIN_CITY_DEP_TIME_df$ORIGIN_CITY_NAME == selected_airport ), ] )
          
          current_month_DEST_CITY_ARR_TIME_df = whole_dataset[[month_name[j]]][ -which( whole_dataset[[month_name[j]]]$CANCELLED == 1 ), c( "DEST_CITY_NAME", "ARR_TIME" ) ]
          
          selected_airport_total_arr[[j]] =  nrow( current_month_DEST_CITY_ARR_TIME_df[ which( current_month_DEST_CITY_ARR_TIME_df$DEST_CITY_NAME == selected_airport ), ] )
          
      }
      
      # df-2
      
      change_over_12_months = data.frame( Month = month.name, Total_Departure = unlist( selected_airport_total_dep ), Total_Arrival = unlist( selected_airport_total_arr ) )
      
      # pick a date in 2017 for more detail on the 24 hour breakdown of that day (how many departures and arrivals per hour, how many delays per hour)
      
      #selected_date = unique( all_months_data$FL_DATE )[1]
      
      selected_date_df = all_months_data[ which( all_months_data$FL_DATE == selected_date ), ]
      
      selected_date_df = selected_date_df[ -which( selected_date_df$CANCELLED == 1 ), c( "DEP_TIME", "ARR_TIME" ) ]
      
      selected_date_df = selected_date_df[ complete.cases( selected_date_df ), ]
      
      departure_per_hour = length( selected_date_df$DEP_TIME )/24
      
      arrival_per_hour = length( selected_date_df$ARR_TIME )/24
      
      selected_date_delay_df = all_months_data[ which( all_months_data$FL_DATE == selected_date ), ]
      
      selected_date_delay_df = selected_date_delay_df[ -which( selected_date_delay_df$CANCELLED == 1 ), c( "CARRIER_DELAY", "WEATHER_DELAY", "NAS_DELAY", "LATE_AIRCRAFT_DELAY", "SECURITY_DELAY" ) ]
      
      selected_date_delay_df = selected_date_delay_df[ complete.cases( selected_date_delay_df ), ]
      
      delay_per_hour = sum( colSums(selected_date_delay_df) )/24
      
      # df-3
      
      selected_date_dep_arr_delay_df = data.frame( "DATE" = selected_date, "DEPARTURE_RATE( per hour )" = departure_per_hour,
      
      "ARRIVAL_RATE( per hour )" = arrival_per_hour, "DELAY_RATE( per hour )" = delay_per_hour )
      
      # pick a day of the week for more detail on the 24 hour breakdown of that day of the week across the year
      
      day_df = all_months_data[ -which( all_months_data$CANCELLED == 1 ), c( "FL_DATE", "DEP_TIME", "ARR_TIME",
      
      "CARRIER_DELAY", "WEATHER_DELAY", "NAS_DELAY", "LATE_AIRCRAFT_DELAY", "SECURITY_DELAY" ) ]
      
      day_df$FL_DAY = weekdays( as.Date( day_df$FL_DATE ) )
      
      #selected_day = unique( day_df$FL_DAY )[1]
      
      selected_day_df = day_df[ which( day_df$FL_DAY == selected_day ), c( "DEP_TIME", "ARR_TIME" ) ]
      
      selected_day_df = selected_day_df[ complete.cases( selected_day_df ), ]
      
      departure_per_hour_day = length( selected_day_df$DEP_TIME )/24
      
      arrival_per_hour_day = length( selected_day_df$ARR_TIME )/24
      
      selected_day_delay_df = day_df[ which( day_df$FL_DAY == selected_day ), c( "CARRIER_DELAY", "WEATHER_DELAY", "NAS_DELAY", "LATE_AIRCRAFT_DELAY", "SECURITY_DELAY" ) ]
      
      selected_day_delay_df = selected_day_delay_df[ complete.cases( selected_day_delay_df ), ]
      
      delay_per_hour_day = sum( colSums(selected_day_delay_df) )/24
      
      # df-4
      
      selected_day_dep_arr_delay_df = data.frame( "DAY" = selected_day, "DEPARTURE_RATE( per hour )" = departure_per_hour_day,
      
      "ARRIVAL_RATE( per hour )" = arrival_per_hour_day, "DELAY_RATE( per hour )" = delay_per_hour_day )
      
      # pick a type of delay for more info on how it changes over the 24 hours of the day and the 12 months of the year
      
      selected_delay_df = all_months_data[ -which( all_months_data$CANCELLED == 1 ), c( "DEP_TIME", selected_delay_type ) ]
      
      selected_delay_df = selected_delay_df[ complete.cases( selected_delay_df ), ]
      
      selected_delay_df = selected_delay_df[ -which( selected_delay_df[ ,2] == 0 ), ]
      
      k = 1 ; total_delay = list()
      
      for( k in 1:24 ){
          
          total_delay[[k]] = sum( selected_delay_df[ ,2][which( ( selected_delay_df$DEP_TIME >= hr_vec[k] )&( selected_delay_df$DEP_TIME < hr_vec[k+1] ) )] )
          
      }
      
      # df-5
      
      selected_delay_table = data.frame( Format_24_Hour = Format_24_Hour, Format_12_Hour = Format_12_Hour, TOTAL_DELAY = unlist( total_delay ) )
      
      # Calculating change over 12 months
      
      m = 1; selected_month_total_delay = list()
      
      for( m in 1:length( month_name ) ){
          
          current_month_selected_delay_data = whole_dataset[[month_name[m]]][ -which( whole_dataset[[month_name[m]]]$CANCELLED == 1 ), c( "DEP_TIME", selected_delay_type ) ]
          
          current_month_selected_delay_data = current_month_selected_delay_data[ complete.cases( current_month_selected_delay_data ), ]
          
          current_month_selected_delay_data = current_month_selected_delay_data[ -which( current_month_selected_delay_data[ ,2] == 0 ), ]
          
          selected_month_total_delay[[m]] = sum( current_month_selected_delay_data[ ,2] )
          
      }
      
      # df-6
      
      delay_change_over_12_months = data.frame( Month = month.name, TOTAL_DELAY = unlist( selected_month_total_delay ) )
      
      # allow a user to show how many and what percentage of flights depart to and arrive from different states within the US
      
      # arrive from = origin city name ; departure to = dest city name
      
      dest_arr_city_names = unique( c( as.character( all_months_data$ORIGIN_CITY_NAME ), as.character( all_months_data$DEST_CITY_NAME ) ) )
      
      total_arr_dep_df = all_months_data[ -which( all_months_data$CANCELLED == 1 ), ]
      
      total_arr_dep = nrow( total_arr_dep_df )
      
      t = 1; total_percent_selected_city_name = list()
      
      for( t in 1:length( dest_arr_city_names ) ){
          
          total_dep_selected_city = length( which( total_arr_dep_df$ORIGIN_CITY_NAME == dest_arr_city_names[t] ) )
          
          total_arr_selected_city = length( which( total_arr_dep_df$DEST_CITY_NAME == dest_arr_city_names[t] ) )
          
          percent_dep_selected_city = ( total_dep_selected_city/total_arr_dep )*100
          
          percent_arr_selected_city = ( total_arr_selected_city/total_arr_dep )*100
          
          total_percent_selected_city_name[[t]] = data.frame( city_nd_state = dest_arr_city_names[t], Total_Departure = total_dep_selected_city,
          
          Total_Arrival = total_arr_selected_city, Departure_Percentage = percent_dep_selected_city,
          
          Arrival_Percentage = percent_arr_selected_city )
          
      }
      
      # df-7
      
      dep_arr_total_percent_df = bind_rows( total_percent_selected_city_name )
      
      #give the user quick access to a table of 10 'interesting' days for air travel in 2017
      
      all_unique_dates = unique( all_months_data$FL_DATE )
      
      df_00 = lapply( all_unique_dates, function( current_date ){
          
          selected_date_total_flight = length( which( total_arr_dep_df$FL_DATE == current_date ) )
          
          return( selected_date_total_flight )
          
      } )
      
      interesting_day_df_0 = data.frame( dates = all_unique_dates, Total_Departure_nd_Arrival = unlist( df_00 ) )
      
      top_five_index = order( interesting_day_df_0$Total_Departure_nd_Arrival, decreasing = T )[1:5]
      
      lowest_five_index = order( interesting_day_df_0$Total_Departure_nd_Arrival, decreasing = F )[1:5]
      
      interesting_day_df = interesting_day_df_0[ c( top_five_index, lowest_five_index ), ]
      
      
      A_output = list( 'change_over_24_hours' = change_over_24_hours, 'change_over_12_months' = change_over_12_months,
      
      'selected_date_dep_arr_delay_df' = selected_date_dep_arr_delay_df, 'selected_day_dep_arr_delay_df' = selected_day_dep_arr_delay_df,
      
      'selected_delay_table' = selected_delay_table, 'delay_change_over_12_months' = delay_change_over_12_months,
      
      'dep_arr_total_percent_df' = dep_arr_total_percent_df, 'interesting_day_df' = interesting_day_df )
      
      return( A_output )
      
  } )
  
  # Tables
  
  output$Total_Dep_Arr_Change_Over_24_Hours_Table = renderDataTable( {
      
      Total_Dep_Arr_Change_Over_24_Hours_Table_0 = dataInput_airport_tables_info_A()$change_over_24_hours
      
      Total_Dep_Arr_Change_Over_24_Hours_Table_0[ , -1 ]
      
  } )
  
  output$Total_Dep_Arr_Change_Over_12_Months_Table = renderDataTable( {
      
      dataInput_airport_tables_info_A()$change_over_12_months
      
  } )
  
  output$Selected_Date_Dep_Arr_Delay_Table = renderDataTable( {
      
      dataInput_airport_tables_info_A()$selected_date_dep_arr_delay_df
      
  } )
  
  output$Selected_Day_Dep_Arr_Delay_Table = renderDataTable( {
      
      dataInput_airport_tables_info_A()$selected_day_dep_arr_delay_df
      
  } )
  
  output$Selected_Delay_Change_Over_24_Hours_Table = renderDataTable( {
      
      Selected_Delay_Change_Over_24_Hours_Table_0 = dataInput_airport_tables_info_A()$selected_delay_table
      
      Selected_Delay_Change_Over_24_Hours_Table_0[ , -1 ]
      
  } )
  
  output$Selected_Delay_Change_Over_12_Months_Table = renderDataTable( {
      
      dataInput_airport_tables_info_A()$delay_change_over_12_months
      
  } )
  
  output$Dep_Arr_Total_Percent_Table = renderDataTable( {
      
      dataInput_airport_tables_info_A()$dep_arr_total_percent_df
      
  } )
  
  output$Interesting_Top_10_Day_Table = renderDataTable( {
      
      dataInput_airport_tables_info_A()$interesting_day_df
      
  } )
  
  # Plots
  
  output$Total_Dep_Arr_Change_Over_24_Hours_Table_Plot = renderPlotly({
      
      Total_Dep_Arr_Change_Over_24_Hours_time_format_type = input$A_Time_Format
      
      Total_Dep_Arr_Change_Over_24_Hours_Table_Plot_data =   dataInput_airport_tables_info_A()$change_over_24_hours
      
      Total_Dep_Arr_Change_Over_24_Hours_Table_Plot_data$Format_24_Hour = factor( Total_Dep_Arr_Change_Over_24_Hours_Table_Plot_data$Format_24_Hour, levels = Total_Dep_Arr_Change_Over_24_Hours_Table_Plot_data$Format_24_Hour )
      
      Total_Dep_Arr_Change_Over_24_Hours_Table_Plot_data$Format_12_Hour = factor( Total_Dep_Arr_Change_Over_24_Hours_Table_Plot_data$Format_12_Hour, levels = Total_Dep_Arr_Change_Over_24_Hours_Table_Plot_data$Format_12_Hour )
      
      switch( Total_Dep_Arr_Change_Over_24_Hours_time_format_type,
      
      "24_Hour" = plot_ly(Total_Dep_Arr_Change_Over_24_Hours_Table_Plot_data, x = ~Format_24_Hour, y = ~Total_Departure, name = 'Total Departure', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4))%>%
      
      add_trace(y = ~Total_Arrival, name = 'Total Arrival', line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
      
      layout(title = "Total number of departures and arrivals for that airline change over the 24 hours of the day",
      xaxis = list(title = ""),
      yaxis = list (title = "Total Departure & Arrival")),
      
      "12_Hour" = plot_ly(Total_Dep_Arr_Change_Over_24_Hours_Table_Plot_data, x = ~Format_12_Hour, y = ~Total_Departure, name = 'Total Departure', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4))%>%
      
      add_trace(y = ~Total_Arrival, name = 'Total Arrival', line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
      
      layout(title = "Total number of departures and arrivals for that airline change over the 24 hours of the day",
      xaxis = list(title = ""),
      yaxis = list (title = "Total Departure & Arrival"))
      
      )
      
  })
  
  output$Total_Dep_Arr_Change_Over_12_Months_Table_Plot = renderPlotly({
      
      Total_Dep_Arr_Change_Over_12_Months_Table_Plot_data =   dataInput_airport_tables_info_A()$change_over_12_months
      
      Total_Dep_Arr_Change_Over_12_Months_Table_Plot_data$Month = factor( Total_Dep_Arr_Change_Over_12_Months_Table_Plot_data$Month, levels = Total_Dep_Arr_Change_Over_12_Months_Table_Plot_data$Month )
      
      plot_ly(Total_Dep_Arr_Change_Over_12_Months_Table_Plot_data, x = ~Month, y = ~Total_Departure, name = 'Total Departure', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4))%>%
      
      add_trace(y = ~Total_Arrival, name = 'Total Arrival', line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
      
      layout(title = "Total number of departures and arrivals for that airline change over the 12 months",
      xaxis = list(title = ""),
      yaxis = list (title = "Total Departure & Arrival"))
      
  })
  
  output$Selected_Delay_Change_Over_24_Hours_Table_Plot = renderPlotly({
      
      Selected_Delay_Change_Over_24_Hours_time_format_type = input$A_Time_Format
      
      Selected_Delay_Change_Over_24_Hours_Table_Plot_data =   dataInput_airport_tables_info_A()$selected_delay_table
      
      Selected_Delay_Change_Over_24_Hours_Table_Plot_data$Format_24_Hour = factor( Selected_Delay_Change_Over_24_Hours_Table_Plot_data$Format_24_Hour, levels = Selected_Delay_Change_Over_24_Hours_Table_Plot_data$Format_24_Hour )
      
      Selected_Delay_Change_Over_24_Hours_Table_Plot_data$Format_12_Hour = factor( Selected_Delay_Change_Over_24_Hours_Table_Plot_data$Format_12_Hour, levels = Selected_Delay_Change_Over_24_Hours_Table_Plot_data$Format_12_Hour )
      
      switch( Selected_Delay_Change_Over_24_Hours_time_format_type,
      
      "24_Hour" = plot_ly(Selected_Delay_Change_Over_24_Hours_Table_Plot_data, x = ~Format_24_Hour, y = ~TOTAL_DELAY, name = 'TOTAL DELAY', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4))%>%
      
      layout(title = "Total delay changes over 24 hours",
      xaxis = list(title = ""),
      yaxis = list (title = "TOTAL DELAY")),
      
      "12_Hour" = plot_ly(Selected_Delay_Change_Over_24_Hours_Table_Plot_data, x = ~Format_12_Hour, y = ~TOTAL_DELAY, name = 'TOTAL DELAY', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4))%>%
      
      layout(title = "Total delay changes over 24 hours",
      xaxis = list(title = ""),
      yaxis = list (title = "TOTAL DELAY"))
      
      )
      
  })
  
  output$Selected_Delay_Change_Over_12_Months_Table_Plot = renderPlotly({
      
      Selected_Delay_Change_Over_12_Months_Table_data =   dataInput_airport_tables_info_A()$delay_change_over_12_months
      
      Selected_Delay_Change_Over_12_Months_Table_data$Month = factor( Selected_Delay_Change_Over_12_Months_Table_data$Month, levels = Selected_Delay_Change_Over_12_Months_Table_data$Month )
      
      plot_ly(Selected_Delay_Change_Over_12_Months_Table_data, x = ~Month, y = ~TOTAL_DELAY, name = 'TOTAL DELAY', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4))%>%
      
      layout(title = "Total delay changes over 12 Months",
      xaxis = list(title = ""),
      yaxis = list (title = "TOTAL DELAY"))
      
  })
  
  output$Interesting_Top_10_Day_Table_Plot = renderPlotly({
      
      Interesting_Top_10_Day_Table_data = dataInput_airport_tables_info_A()$interesting_day_df
      
      Interesting_Top_10_Day_Table_data$dates = factor( Interesting_Top_10_Day_Table_data$dates, levels = Interesting_Top_10_Day_Table_data$dates )
      
      plot_ly(Interesting_Top_10_Day_Table_data, x = ~dates, y = ~Total_Departure_nd_Arrival, name = 'Arrival & Departure', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4))%>%
      
      layout(title = "Graph 10 Interesting days for air travel in 2017",
      xaxis = list(title = ""),
      yaxis = list (title = ""))
      
  })
  
  
  

}

shinyApp( ui = ui, server = server )


