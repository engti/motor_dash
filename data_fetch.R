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
  
## report suite ids
  eu_rsids <- read_csv("eu_market_rsid.csv") %>%
    filter(eu == "Y") %>%
    select(-virtual,-eu) %>%
    mutate(
      site_title = str_remove(site_title,"Web - ")
    )
  
## set date range
  report_date <- list(
    start = "2017-10-01",
    end = "2018-05-18"
  ) 
  
  list_events <- GetSuccessEvents(eu_rsids$rsid[20])
  # list_elm <- GetElements(eu_rsids$rsid[20])
  report_events <- c("visits","pageviews","event2","event4","event8","event13","event14","event21","event25","event26","event27","event39")
  
## get top level metrics
  df_overall <- data.frame()
  for (i in 1:nrow(eu_rsids)) {
    print(eu_rsids$site_title[i])
    df_tmp <- QueueOvertime(
      reportsuite.id = eu_rsids$rsid[i],
      date.from = report_date$start,
      date.to = report_date$end,
      metrics = report_events
    )
    df_tmp$country <- eu_rsids$site_title[i]
    
    if(nrow(df_tmp) > 0){
      if("country" %in% names(df_tmp)){
        df_overall <- rbind(df_overall,df_tmp)
      }else{
        df_overall <- df_tmp
      }  
    }
  }
  
  ## rename the events
  rename_df <- list_events %>%
    select(id,name)
  
  ## write the file
  write_csv(df_overall,"df_overall.csv")
  write_rds(df_overall,"df_overall.rds")
  
  ## get car model metrics
  report_events_auto <- c("visits","pageviews","event2","event4","event8","event13","event14","event21","event25","event26","event27","event39")
  
  ## queue reports
  df_list <- list()
  for (i in 1:nrow(eu_rsids)) {
    print(eu_rsids$site_title[i])
    df_tmp <- QueueTrended(
      reportsuite.id = eu_rsids$rsid[i],
      date.from = report_date$start,
      date.to = report_date$end,
      metrics = report_events_auto,
      elements = c("geocity","referrertype","evar7"),
      enqueueOnly = T
    )
    # df_tmp$country <- eu_rsids$site_title[i]
    
    list_name <- paste0(eu_rsids$site_title[i])
    df_list[[list_name]] <-  df_tmp   
    # if(nrow(df_tmp) > 0){
    #   if("country" %in% names(df_tmp)){
    #     df_overall_auto <- rbind(df_overall_auto,df_tmp)
    #   }else{
    #     df_overall_auto <- df_tmp
    #   }  
    # }
  }
  
  ## fetch queud reports
  df_overall_auto <- data.frame()
  for (i in 1:nrow(eu_rsids)) {
    print(eu_rsids$site_title[i])
    df_tmp <- GetReport(df_list[[i]],max.attempts = 50)
    df_tmp$country <- eu_rsids$site_title[i]
    
    if(nrow(df_tmp) > 0){
      if("country" %in% names(df_tmp)){
        df_overall_auto <- rbind(df_overall_auto,df_tmp)
      }else{
        df_overall_auto <- df_tmp
      }
    }
  }
  
  
  
  
  
  
  ## trellis
  # with facet_trelliscope:
  
  tmp2 <- tmp %>% select(country = `GeoSegmentation Countries`,referrer_type =`Referrer Type`,visits = Visits, kcc_complete = `Build My Kia-Complete(e14) (event14)`)
  
  qplot(kcc_complete, visits, data = tmp2) +
    geom_abline(alpha = 0.5) +
    theme_bw() +
    facet_trelliscope(~country , nrow = 2, ncol = 4)
  
  
  ## how to make a segment
  mySegment<- list(container=list(type=unbox("visits"),
                                  rules = data.frame(
                                    name = "models",
                                    element = "evar7",
                                    operator = "contains",
                                    value = product_segment
                                  )))
  
  
  ## draw some graphs
  df_trend <- read_rds("df_overall.rds") %>%
    select(-name,-year,-month,-day,-segment.id,-segment.name) %>%
    select(datetime,country,visits,pageviews,vehicle_views = event2,brochure_download = event4,test_drive_request = event8,kcc_start = event13,kcc_complete = event14,contact_request = event21,vr_view = event25,video_view = event26,photo_view = event27,internal_search = event39) %>%
    filter(country == "Italy")
  
  library(hrbrthemes)
  library(ggplot2)  
  
  ggplot(df_trend,aes(x=datetime,y=visits,fill=country)) +
    geom_area() +
    scale_fill_ipsum() +
    scale_y_comma() +
    labs(title="Visit Trend to Italy Site",
         caption="") +
    theme_ipsum_rc(grid="XY") +
    theme(axis.text.x=element_text(hjust=c(0, 0.5, 0.5, 0.5, 1))) +
    theme(legend.position="none") 
  
  ## read the italy data and save it back as rds
  df_main <- read_csv(".//kia_dash_V1//italy_data_pull_4.csv") %>% 
    select(
      datetime=Date,
      geo_countries=`GeoSegmentation Countries`,
      geo_cities=`GeoSegmentation Cities`,
      ref_type=`Referrer Type`,
      ref_domain=`Referring Domains`,
      entry_pages=`Entry Pages`,
      sub_section=`Category 2 Depth (evar3)`,
      device_type=`Mobile Device Type`,
      visit_number=`Visit Number`,
      visitors=`Unique Visitors`,
      visits=`Visits`,
      pageviews=`Page Views`,
      brochure_start=`Download Brochure-Start (event3)`,
      brochure_complete=`Download Brochure-Complete (event4)`,
      kcc_start=`Car Configurator-Start (event13)`,
      kcc_complete=`Car Configurator-Complete (event14)`,
      vr_view=`360 VR Views (event25)`,
      photo_view=`Photo Views (event27)`,
      video_view=`Video Views (event26)`,
      scroll_view=`Scrolled Views (event28)`,
      internal_search=`Internal Searches (event39)`,
      compare_start=`Compare Car Start (event31)`,
      compare_complete=`Compare Car Complete (event32)`,
      contact_start=`Contact US Start (event20)`,
      contact_complete=`Contact US Complete (event21)`,
      finance_start=`Finance quote start (event88)`,
      lease_start=`Kia Lease quote start (event90)`,
      newsletter_start=`Newsletter-Start (event17)`,
      newsletter_complete=`Newsletter-Complete (event18)`,
      testdrive_start=`Request Test Drive-Start (event7)`,
      testdrive_complete=`Request Test Drive-Complete (event8)`,
      select_accessories=`Select Accessories-Start (event69)`) %>% 
    mutate(
      datetime = as.Date(datetime,format="%B %d, %Y")
    )
  
  df_tmp <- read_csv(".//kia_dash_V1//italy_data_pull_4.csv",n_max = 2)
  
  df_sample <- sample_frac(df_main,size = .1)
  write_rds(df_sample,"df_it_main_frac.rds")
  
  tmp <- as.POSIXct(df_main$datetime[1:10])
  
  ## do some plotting
  df_tmp <- df_main %>% 
    mutate(
      section = gsub("\\|.*","",sub_section)
    ) %>%
    filter(section == "new-cars") %>%
    select(sub_section,visits,pageviews) %>%
    group_by(sub_section) %>%
    summarise(visits=sum(visits),pageviews=sum(pageviews))
  
  ggplot(data = df_tmp,aes(x=visits,y=pageviews)) +
    geom_point(aes(color=factor(sub_section))) +
                 scale_color_ipsum() +
                 theme_ipsum_rc()
  
  
  ## sqllite db
  db <- dbConnect(SQLite(), dbname="italyDB2.sqlite")
  
  dbDisconnect(con)
  dbWriteTable(conn = db, name = "italyDB", df_main, overwrite=T,row.names=FALSE)  
  
  con <- DBI::dbConnect(RSQLite::SQLite(), path = "italyDB2.sqlite")
  mydb <- dbConnect(RSQLite::SQLite(), "italyDB2.sqlite")
  db_list_tables(mydb)

  mydb %>% head()  
  
  db_tmp <- tbl(mydb, "italyDB")
  db_tmp %>% head()  

  tmp <- db_tmp %>% select(datetime,visits) %>%
    group_by(datetime) %>%
    summarise(visits = sum(visits)) %>%
    collect()
  
  start_time <- Sys.time()
  tmp <- db_tmp %>% select(datetime,visits) %>%
    filter(datetime > "2018-01-01") %>%
    group_by(datetime) %>%
    summarise(visits = sum(visits)) %>%
    collect()
  end_time <- Sys.time()
  
  end_time - start_time
  
  ## small multiples ####
  tmp <- df_main %>%
    select(sub_section,pageviews,kcc_complete,vr_view,photo_view) %>%
    mutate(
      section = gsub("\\|.*","",sub_section),
      sub_section = gsub(".*\\|","",sub_section)
    ) %>%
    filter(section == "new-cars") %>%
    group_by(section,sub_section) %>%
    summarise(
      vehicle_views=sum(pageviews),
      kcc_rate=sum(kcc_complete)/sum(pageviews),
      vr_rate = sum(vr_view)/sum(pageviews),
      photo_view = sum(photo_view)/sum(pageviews)
      )