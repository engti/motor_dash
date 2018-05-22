## load lib
  library(RSiteCatalyst)
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(patchwork)
  library(stringr)
  library(lubridate)
  library(hrbrthemes)
  library(plotly)
  library(RSQLite)
## source auth token
  source("authenticate.R")
  rsid <- "kiamotors-uk-w"

## get segments
  segments_list <- GetSegments(rsid)
  segment_fetch <- c("","s200000506_5b03a21ab9b6c34010b7b64a","s200000506_5b03a2f498ca3a21b9ae39bd","s200000506_5b03a40841c44bb4bea740a3","s200000506_5a1d59db2778f82111db4097")
  report_events <- c("visits","pageviews","event2","event4","event8","event13","event14","event21","event25","event26","event27","event39","event50")
  
  report_date <- list(
    start = "2018-01-01",
    end = "2018-03-31"
  ) 
  
  
## fetch trended data
  ## get top level metrics
  df_overall <- data.frame()
  for (i in 1:length(segment_fetch)) {
    df_tmp <- QueueOvertime(
      reportsuite.id = rsid,
      date.from = report_date$start,
      date.to = report_date$end,
      metrics = report_events,
      segment.id = segment_fetch[i]
    )
    
    if(nrow(df_tmp) > 0){
      if("datetime" %in% names(df_tmp)){
        df_overall <- rbind(df_overall,df_tmp)
      }else{
        df_overall <- df_tmp
      }  
    }
  }
  
  write_csv(df_overall,"df_overall_uk.csv")
  write_rds(df_overall,"df_overall_uk.rds")
  
  ## accessories ####
  df_acc <- read_csv("data_accessories.csv") %>% 
    mutate(
      datetime = as.Date(datetime,format="%B %d, %Y")
    ) %>%
    select(-Row)
  write_rds(df_acc,"df_acc.rds")
  
  ## filter for uk and then get lat long for unique locatoins
  uk_cities <- df_acc %>%
    filter(geo_country == "united kingdom") %>%
    select(geo_city,vehicle_views) %>%
    group_by(geo_city) %>%
    summarise(volume = sum(vehicle_views)) %>%
    arrange(-volume) %>%
    top_n(2500) %>%
    arrange(-volume)
    
  
  ## geocode using ggmap
  library(ggmap)
  uk_cities_loc <- geocode(uk_cities$geo_city[1:2000],output = "latlon") 
  
  uk_city_joined <- cbind(uk_cities$geo_city[1:2000],uk_cities_loc)
  
  names(uk_city_joined)[1] <- "geo_city"
  
  write_rds(uk_cities_loc,"uk_cities_loc.rds")
  
  write_rds(uk_city_joined,"uk_city_joined.rds")
  write_csv(uk_city_joined,"uk_city_joined.csv")
  
  ## channel, 
  
  ## model summary ####
  model <- df_acc %>%
    select(vehicle_name,vehicle_views,kcc_complete,photo_view,vr_views,acc_event) %>%
    group_by(vehicle_name) %>%
    summarise(
      vehicle_views = sum(vehicle_views),
      kcc_complete = sum(kcc_complete),
      photo_view = sum(photo_view),
      vr_views = sum(vr_views),
      acc_event = sum(acc_event)
    )%>% 
    mutate(
      kcc_rate = kcc_complete/vehicle_views,
      photo_rate = photo_view/vehicle_views,
      vr_rate = vr_views/vehicle_views,
      accessory_rate = acc_event/vehicle_views
    ) %>%
    filter(vehicle_views > 500) %>%
    arrange(-vehicle_views)
  
  ## mapping ####
  library(readr)
  library(leaflet)
  
  
  tmp <- df_acc %>%
    select(geo_city,vehicle_views) %>%
    group_by(geo_city) %>%
    summarise(views= sum(vehicle_views)) %>%
    inner_join(uk_city_joined) %>%
    na.omit()

  ## create a pallette
  # Create a continuous palette function
  pal <- colorNumeric(
    palette = "Blues",
    domain = tmp$views)
  
  leaflet(data = tmp) %>% addTiles() %>% addCircleMarkers(radius = ~(views/10000),popup = ~geo_city,color = ~pal(views))
  
  
  
  
  df1 <- read_csv("D://Sync//Box Sync//Box Sync//rprog//motor_dash//kia_dash_V1//dealer_unique.csv")
  
  df1$type <- "dealer"
  
  icons <- awesomeIcons(
    icon = 'car',
    iconColor = 'black',
    library = 'ion',
    markerColor = "black"
  )
  
  oceanIcons <- iconList(
    dealer = makeIcon("D://Sync//Box Sync//Box Sync//rprog//motor_dash//kia_dash_V1//car.png", iconHeight =  18,iconWidth =  18
    ))
  
  leaflet(df1) %>% addTiles() %>%
    # Select from oceanIcons based on df$type
    addMarkers(icon = ~oceanIcons[type],color = "#03F")
  
  awesomeIconList()
  
  leaflet(data = df1) %>% addTiles() %>% addAwesomeMarkers(~longitude, ~latitude,icon=icons)
  