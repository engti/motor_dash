## load library
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyWidgets)
  library(readr)
  library(dplyr)
  library(RSQLite)
  library(DBI)
  library(scales)
  library(ggplot2)
  library(DT)  

## load data
df_trend <- read_rds("df_overall_uk.rds") %>%
  select(-name,-year,-month,-day,-segment.id) %>%
  select(segment.name,datetime,visits,pageviews,vehicle_views = event2,brochure_download = event4,test_drive_request = event8,kcc_start = event13,kcc_complete = event14,contact_request = event21,vr_view = event25,video_view = event26,photo_view = event27,internal_search = event39,accessory_click=event50) %>%
  mutate(
    datetime = as.Date(datetime),
    segment.name = case_when(
      segment.name == "" ~ "all",
      segment.name == "DemoTest_Browser" ~ "browser",
      segment.name == "DemoTest_Prospect" ~ "prospect",
      segment.name == "DemoTest_Service" ~ "service",
      segment.name == "Google Paid Traffic vSeg" ~ "paid"
    )
  ) 

# df_main <- read_csv("D://Sync//Box Sync//Box Sync//rprog//motor_dash//italy_data_pull_4.csv")
# df_main <- read_rds("df_it_main_frac.rds") %>%
#     mutate(
#       section = gsub("\\|.*","",sub_section),
#       sub_section = gsub(".*\\|","",sub_section),
#       sub_section = gsub("-.*","",sub_section)
#     )

## read in dealer data
df_acc <- read_rds("df_acc.rds")
uk_city_joined <- read_rds("uk_city_joined.rds") %>% select(geo_city,longitude=lon,latitude=lat)
dealer_location <- read_csv("dealer_unique.csv")
  
## sqllite db
# conn <- dbConnect(RSQLite::SQLite(), "italyDB2.sqlite")
# db_list_tables(conn)  

## body ##### 
  ### header 
  header <- dashboardHeaderPlus(
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "gears"
      )
  ### sidebar   
  sidebar <- dashboardSidebar(
      disable = T
      )
  
  rightsidebar <- rightSidebar(
    
    )  
  ### page   
  body <- dashboardBody(
        ## summary stats
      fluidRow(
        box(
          width = 2,
          descriptionBlock(
          # number = "17%",   
          number = textOutput("kpi_visits_change"),
          number_color = textOutput("kpi_visits_colour"),
          # number_color = "red", 
          # number_icon = "fa fa-caret-down",
          number_icon = textOutput("kpi_visits_icon"),
          header = textOutput("kpi_visits"), 
          text = "ALL VISITS", 
          right_border = TRUE,
          margin_bottom = FALSE
      )),
      box(
        width = 2,
        descriptionBlock(
          # number = "17%",   
          number = textOutput("kpi_browser_change"),
          number_color = textOutput("kpi_browser_colour"),
          # number_color = "red", 
          # number_icon = "fa fa-caret-down",
          number_icon = textOutput("kpi_browser_icon"),
          header = textOutput("kpi_browser"), 
          text = "BROWSER VISITS", 
          right_border = TRUE,
          margin_bottom = FALSE
        )),
      box(
        width = 2,
        descriptionBlock(
          # number = "17%",   
          number = textOutput("kpi_prospect_change"),
          number_color = textOutput("kpi_prospect_colour"),
          # number_color = "red", 
          # number_icon = "fa fa-caret-down",
          number_icon = textOutput("kpi_prospect_icon"),
          header = textOutput("kpi_prospect"), 
          text = "PROSPECT VISITS", 
          right_border = TRUE,
          margin_bottom = FALSE
        )),
      box(
        width = 2,
        descriptionBlock(
          # number = "17%",   
          number = textOutput("kpi_service_change"),
          number_color = textOutput("kpi_service_colour"),
          # number_color = "red", 
          # number_icon = "fa fa-caret-down",
          number_icon = textOutput("kpi_service_icon"),
          header = textOutput("kpi_service"), 
          text = "SERVICE VISITS", 
          right_border = TRUE,
          margin_bottom = FALSE
        )),
      box(
        width = 4,
        plotOutput("trendPlot",brush  = brushOpts(id="select_dates",direction = c("x")),width = "100%",height = "80px")
      )),
      fluidRow(
        box(
          width = 2,
          descriptionBlock(
            # number = "17%",   
            number = textOutput("kpi_paid_visits_change"),
            number_color = textOutput("kpi_paid_visits_colour"),
            # number_color = "red", 
            # number_icon = "fa fa-caret-down",
            number_icon = textOutput("kpi_paid_visits_icon"),
            header = textOutput("kpi_paid_visits"), 
            text = "PAID VISITS", 
            right_border = TRUE,
            margin_bottom = FALSE
          )),
        box(
          width = 2,
          descriptionBlock(
            # number = "17%",   
            number = textOutput("kpi_test_drive_request_change"),
            number_color = textOutput("kpi_test_drive_request_colour"),
            # number_color = "red", 
            # number_icon = "fa fa-caret-down",
            number_icon = textOutput("kpi_test_drive_request_icon"),
            header = textOutput("kpi_test_drive_request"), 
            text = "TEST DRIVES", 
            right_border = TRUE,
            margin_bottom = FALSE
          )),
        box(
          width = 2,
          descriptionBlock(
            # number = "17%",   
            number = textOutput("kpi_vehicle_views_change"),
            number_color = textOutput("kpi_vehicle_views_colour"),
            # number_color = "red", 
            # number_icon = "fa fa-caret-down",
            number_icon = textOutput("kpi_vehicle_views_icon"),
            header = textOutput("kpi_vehicle_views"), 
            text = "VEHICLE VIEWS", 
            right_border = TRUE,
            margin_bottom = FALSE
          )),
        box(
          width = 2,
          descriptionBlock(
            # number = "17%",   
            number = textOutput("kpi_brochure_download_change"),
            number_color = textOutput("kpi_brochure_download_colour"),
            # number_color = "red", 
            # number_icon = "fa fa-caret-down",
            number_icon = textOutput("kpi_brochure_download_icon"),
            header = textOutput("kpi_brochure_download"), 
            text = "BROCHURES", 
            right_border = TRUE,
            margin_bottom = FALSE
          )),
        box(
          width = 2,
          descriptionBlock(
            # number = "17%",   
            number = textOutput("kpi_kcc_complete_change"),
            number_color = textOutput("kpi_kcc_complete_colour"),
            # number_color = "red", 
            # number_icon = "fa fa-caret-down",
            number_icon = textOutput("kpi_kcc_complete_icon"),
            header = textOutput("kpi_kcc_complete"), 
            text = "CAR CONFIGURATOR", 
            right_border = TRUE,
            margin_bottom = FALSE
          )),
        box(
          width = 2,
          descriptionBlock(
            # number = "17%",   
            number = textOutput("kpi_internal_search_change"),
            number_color = textOutput("kpi_internal_search_colour"),
            # number_color = "red", 
            # number_icon = "fa fa-caret-down",
            number_icon = textOutput("kpi_internal_search_icon"),
            header = textOutput("kpi_internal_search"), 
            text = "INTERNAL SEARCH", 
            right_border = TRUE,
            margin_bottom = FALSE
          ))
      ),
      fluidRow(
        boxPlus(
          width = 4,
          title = "Closable Box",
          closable = FALSE,
          label_status = "danger",
          status = "warning",
          solidHeader = FALSE,
          collapsible = TRUE,
          p("Box Content")
        ),
        box(
          width = 4
        ),
        box(
          width = 4
        )
      ),
      fluidRow(
        boxPlus(
          width = 6,
          title = "Top Models",
          closable = FALSE,
          label_status = "danger",
          status = "warning",
          solidHeader = FALSE,
          collapsible = TRUE,
          DT::dataTableOutput("model_table")
        ),
        boxPlus(
          width = 6,
          title = "Geographic of Engagement",
          closable = FALSE,
          label_status = "danger",
          status = "warning",
          solidHeader = FALSE,
          collapsible = TRUE,
          leafletOutput("uk_map")
        )
      ),
      fluidRow(
        box(width = 4,height = "450px",
            verbatimTextOutput("validation")
        ),
        box(width = 4,height = "450px",
            verbatimTextOutput("validation2")
        )
      )
    ## body end
    )
      
    

    
## build the ui  
  ui <- dashboardPagePlus(title = "DashboardPage",header,sidebar,body,rightsidebar)

## server ####    
  server <- function(input, output) { 
    ## draw the brush plot
    output$trendPlot <- renderPlot({
      df1 <- df_trend %>%
        filter(segment.name == "all") %>%
        select(datetime,visits)
      
      ggplot(df1,aes(x=datetime,y=visits)) + geom_area() + 
        theme(
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank()
        )
      
    })
    
    ## data derive from dates
    derivedData <- reactive({
      if(is.null(input$select_dates)){
        df1 <- df_trend
      }else{
        df1 <- brushedPoints(df_trend,input$select_dates)
      }
      df1 %>% filter(segment.name == "all")
    })
    
    ## derived big data ####
    derivedData_big <- reactive({
      if(is.null(input$select_dates)){
        df1 <- df_main
      }else{
        df1 <- df_main %>% filter(datetime >= min(derivedData()$datetime),datetime <= max(derivedData()$datetime))
      }
      df1
    })
    

    ## compare data ####
    compare_data <- reactive({
      if(!is.null(input$select_dates)){
        df1 <- derivedData()$datetime
        diff <- max(df1) - min(df1)
        # diff <- (min(df1)-1) - diff

        compare_start <- (min(df1)) - (diff+1)
        compare_end <- min(df1) -1
        df_compare <- df_trend %>%
          filter(datetime >= compare_start,datetime <= compare_end) 
      }
    })
    
    compare_data_all <- reactive({
      if(!is.null(input$select_dates)){
        df1 <- derivedData()$datetime
        diff <- max(df1) - min(df1)
        # diff <- (min(df1)-1) - diff
        
        compare_start <- (min(df1)) - (diff+1)
        compare_end <- min(df1) -1
        df_compare <- df_trend %>%
          filter(datetime >= compare_start,datetime <= compare_end) %>%
          filter(segment.name == "all")
      }
    })
    
    ## visit kpi boxes
    output$kpi_visits <- renderText({ 
      df1 <- derivedData()
      ## "fa fa-caret-up"
      comma(sum(df1$visits))
    })
    
    output$kpi_visits_change <- renderText({ 
      df1 <- sum(compare_data_all()$visits)
      # if(!is.null(input$select_dates)){
      #   df1 <- compare_data() %>% filter(segment.name=="all") %>% select(visits) %>% sum()  
      # }
      
      df2 <- sum(derivedData()$visits)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else{
        percent(change)
      }
      ## "fa fa-caret-up"
      # str(df1$datetime[1])
    })
    
    output$kpi_visits_colour <- renderText({ 
      df1 <- sum(compare_data_all()$visits)
      df2 <- sum(derivedData()$visits)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        "red"
      }else if(change>0){
        "green"
      }else if(change<0){
        "red"
      }
      ## "fa fa-caret-up"
      # sum(df1$visits)
    })
    
    output$kpi_visits_icon <- renderText({
      df1 <- sum(compare_data_all()$visits)
      df2 <- sum(derivedData()$visits)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else if(change>0){
        "fa fa-caret-up"
      }else if(change<0){
        "fa fa-caret-down"
      }
    })
    
    ## vehicle views  kpi boxes
    output$kpi_vehicle_views <- renderText({ 
      df1 <- derivedData()
      ## "fa fa-caret-up"
      comma(sum(df1$vehicle_views))
    })
    
    output$kpi_vehicle_views_change <- renderText({ 
      df1 <- sum(compare_data_all()$vehicle_views)
      df2 <- sum(derivedData()$vehicle_views)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else{
        percent(change)
      }
      ## "fa fa-caret-up"
      # str(df1$datetime[1])
    })
    
    output$kpi_vehicle_views_colour <- renderText({ 
      df1 <- sum(compare_data_all()$vehicle_views)
      df2 <- sum(derivedData()$vehicle_views)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        "red"
      }else if(change>0){
        "green"
      }else if(change<0){
        "red"
      }
      ## "fa fa-caret-up"
      # sum(df1$visits)
    })
    
    output$kpi_vehicle_views_icon <- renderText({
      df1 <- sum(compare_data_all()$vehicle_views)
      df2 <- sum(derivedData()$vehicle_views)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else if(change>0){
        "fa fa-caret-up"
      }else if(change<0){
        "fa fa-caret-down"
      }
    })
    
    ## paid visit boxes ####
    paid_data <- reactive({
      if(is.null(input$select_dates)){
        df1 <- df_trend
      }else{
        df1 <- brushedPoints(df_trend,input$select_dates)
      }
      df1 %>% filter(segment.name == "paid")
    })
    
    paid_data_compare <- reactive({
      if(!is.null(input$select_dates)){
        df1 <- compare_data()
        df1 %>% filter(segment.name == "paid") 
      }
    })
    output$kpi_paid_visits <- renderText({ 
      df1 <- paid_data()
      ## "fa fa-caret-up"
      comma(sum(df1$visits))
    })
    
    output$kpi_paid_visits_change <- renderText({ 
      df1 <- sum(paid_data_compare()$visits)
      df2 <- sum(paid_data()$visits)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else{
        percent(change)
      }
      ## "fa fa-caret-up"
      # str(df1$datetime[1])
    })
    
    output$kpi_paid_visits_colour <- renderText({ 
      df1 <- sum(paid_data_compare()$visits)
      df2 <- sum(paid_data()$visits)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        "red"
      }else if(change>0){
        "green"
      }else if(change<0){
        "red"
      }
      ## "fa fa-caret-up"
      # sum(df1$visits)
    })
    
    output$kpi_paid_visits_icon <- renderText({
      df1 <- sum(paid_data_compare()$visits)
      df2 <- sum(paid_data()$visits)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else if(change>0){
        "fa fa-caret-up"
      }else if(change<0){
        "fa fa-caret-down"
      }
    })
    #### end #####
    
    ## test drive  kpi boxes ####
    output$kpi_test_drive_request <- renderText({ 
      df1 <- derivedData()
      ## "fa fa-caret-up"
      comma(sum(df1$test_drive_request))
    })
    
    output$kpi_test_drive_request_change <- renderText({ 
      df1 <- sum(compare_data_all()$test_drive_request)
      df2 <- sum(derivedData()$test_drive_request)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else{
        percent(change)
      }
      ## "fa fa-caret-up"
      # str(df1$datetime[1])
    })
    
    output$kpi_test_drive_request_colour <- renderText({ 
      df1 <- sum(compare_data_all()$test_drive_request)
      df2 <- sum(derivedData()$test_drive_request)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        "red"
      }else if(change>0){
        "green"
      }else if(change<0){
        "red"
      }
      ## "fa fa-caret-up"
      # sum(df1$visits)
    })
    
    output$kpi_test_drive_request_icon <- renderText({
      df1 <- sum(compare_data_all()$test_drive_request)
      df2 <- sum(derivedData()$test_drive_request)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else if(change>0){
        "fa fa-caret-up"
      }else if(change<0){
        "fa fa-caret-down"
      }
    })
    #### end #####
    
    ## kcc complete  kpi boxes
    output$kpi_kcc_complete <- renderText({ 
      df1 <- derivedData()
      ## "fa fa-caret-up"
      comma(sum(df1$kcc_complete))
    })
    
    output$kpi_kcc_complete_change <- renderText({ 
      df1 <- sum(compare_data_all()$kcc_complete)
      df2 <- sum(derivedData()$kcc_complete)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else{
        percent(change)
      }
      ## "fa fa-caret-up"
      # str(df1$datetime[1])
    })
    
    output$kpi_kcc_complete_colour <- renderText({ 
      df1 <- sum(compare_data_all()$kcc_complete)
      df2 <- sum(derivedData()$kcc_complete)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        "red"
      }else if(change>0){
        "green"
      }else if(change<0){
        "red"
      }
      ## "fa fa-caret-up"
      # sum(df1$visits)
    })
    
    output$kpi_kcc_complete_icon <- renderText({
      df1 <- sum(compare_data_all()$kcc_complete)
      df2 <- sum(derivedData()$kcc_complete)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else if(change>0){
        "fa fa-caret-up"
      }else if(change<0){
        "fa fa-caret-down"
      }
    })
    ## end ####
    
    ## internal search  kpi boxes ####
    output$kpi_internal_search <- renderText({ 
      df1 <- derivedData()
      ## "fa fa-caret-up"
      comma(sum(df1$internal_search))
    })
    
    output$kpi_internal_search_change <- renderText({ 
      df1 <- sum(compare_data_all()$internal_search)
      df2 <- sum(derivedData()$internal_search)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else{
        percent(change)
      }
      ## "fa fa-caret-up"
      # str(df1$datetime[1])
    })
    
    output$kpi_internal_search_colour <- renderText({ 
      df1 <- sum(compare_data_all()$internal_search)
      df2 <- sum(derivedData()$internal_search)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        "red"
      }else if(change>0){
        "green"
      }else if(change<0){
        "red"
      }
      ## "fa fa-caret-up"
      # sum(df1$visits)
    })
    
    output$kpi_internal_search_icon <- renderText({
      df1 <- sum(compare_data_all()$internal_search)
      df2 <- sum(derivedData()$internal_search)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else if(change>0){
        "fa fa-caret-up"
      }else if(change<0){
        "fa fa-caret-down"
      }
    })
    
    ## end ####
    
    
    ## brochure  kpi boxes
    output$kpi_brochure_download <- renderText({ 
      df1 <- derivedData()
      ## "fa fa-caret-up"
      comma(sum(df1$brochure_download))
    })
    
    output$kpi_brochure_download_change <- renderText({ 
      df1 <- sum(compare_data_all()$brochure_download)
      df2 <- sum(derivedData()$brochure_download)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else{
        percent(change)
      }
      ## "fa fa-caret-up"
      # str(df1$datetime[1])
    })
    
    output$kpi_brochure_download_colour <- renderText({ 
      df1 <- sum(compare_data_all()$brochure_download)
      df2 <- sum(derivedData()$brochure_download)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        "red"
      }else if(change>0){
        "green"
      }else if(change<0){
        "red"
      }
      ## "fa fa-caret-up"
      # sum(df1$visits)
    })
    
    output$kpi_brochure_download_icon <- renderText({
      df1 <- sum(compare_data_all()$brochure_download)
      df2 <- sum(derivedData()$brochure_download)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else if(change>0){
        "fa fa-caret-up"
      }else if(change<0){
        "fa fa-caret-down"
      }
    })
    
    ## all visits boxes ####
    browser_data <- reactive({
      if(is.null(input$select_dates)){
        df1 <- df_trend
      }else{
        df1 <- brushedPoints(df_trend,input$select_dates)
      }
      df1 %>% filter(segment.name == "browser")
    })
    
    browser_data_compare <- reactive({
      if(!is.null(input$select_dates)){
        df1 <- compare_data()
        df1 %>% filter(segment.name == "browser") 
      }
    })
    
    output$kpi_browser <- renderText({ 
      df1 <- browser_data()
      ## "fa fa-caret-up"
      comma(sum(df1$visits))
    })
    
    output$kpi_browser_change <- renderText({ 
      df1 <- sum(browser_data_compare()$visits)
      df2 <- sum(browser_data()$visits)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else{
        percent(change)
      }
      ## "fa fa-caret-up"
      # str(df1$datetime[1])
    })
    
    output$kpi_browser_colour <- renderText({ 
      df1 <- sum(browser_data_compare()$visits)
      df2 <- sum(derivedData()$visits)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        "red"
      }else if(change>0){
        "green"
      }else if(change<0){
        "red"
      }
      ## "fa fa-caret-up"
      # sum(df1$visits)
    })
    
    output$kpi_browser_icon <- renderText({
      df1 <- sum(browser_data_compare()$visits)
      df2 <- sum(derivedData()$visits)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else if(change>0){
        "fa fa-caret-up"
      }else if(change<0){
        "fa fa-caret-down"
      }
    })
    
    ## all visits box end ####
    
    ## browser boxes ####
    browser_data <- reactive({
      if(is.null(input$select_dates)){
        df1 <- df_trend
      }else{
        df1 <- brushedPoints(df_trend,input$select_dates)
      }
      df1 %>% filter(segment.name == "browser")
    })
    
    browser_data_compare <- reactive({
      if(!is.null(input$select_dates)){
        df1 <- compare_data()
        df1 %>% filter(segment.name == "browser") 
      }
    })
    
    output$kpi_browser <- renderText({ 
      df1 <- browser_data()
      ## "fa fa-caret-up"
      comma(sum(df1$visits))
    })
    
    output$kpi_browser_change <- renderText({ 
      df1 <- sum(browser_data_compare()$visits)
      df2 <- sum(browser_data()$visits)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else{
        percent(change)
      }
      ## "fa fa-caret-up"
      # str(df1$datetime[1])
    })
    
    output$kpi_browser_colour <- renderText({ 
      df1 <- sum(browser_data_compare()$visits)
      df2 <- sum(derivedData()$visits)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        "red"
      }else if(change>0){
        "green"
      }else if(change<0){
        "red"
      }
      ## "fa fa-caret-up"
      # sum(df1$visits)
    })
    
    output$kpi_browser_icon <- renderText({
      df1 <- sum(browser_data_compare()$visits)
      df2 <- sum(derivedData()$visits)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else if(change>0){
        "fa fa-caret-up"
      }else if(change<0){
        "fa fa-caret-down"
      }
    })
    
    ## browser box end ####
    
    ## prospect boxes ####
    prospect_data <- reactive({
      if(is.null(input$select_dates)){
        df1 <- df_trend
      }else{
        df1 <- brushedPoints(df_trend,input$select_dates)
      }
      df1 %>% filter(segment.name == "prospect")
    })
    
    prospect_data_compare <- reactive({
      if(!is.null(input$select_dates)){
        df1 <- compare_data()
        df1 %>% filter(segment.name == "prospect") 
      }
    })
    
    output$kpi_prospect <- renderText({ 
      df1 <- prospect_data()
      ## "fa fa-caret-up"
      comma(sum(df1$visits))
    })
    
    output$kpi_prospect_change <- renderText({ 
      df1 <- sum(prospect_data_compare()$visits)
      df2 <- sum(prospect_data()$visits)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else{
        percent(change)
      }
      ## "fa fa-caret-up"
      # str(df1$datetime[1])
    })
    
    output$kpi_prospect_colour <- renderText({ 
      df1 <- sum(prospect_data_compare()$visits)
      df2 <- sum(derivedData()$visits)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        "red"
      }else if(change>0){
        "green"
      }else if(change<0){
        "red"
      }
      ## "fa fa-caret-up"
      # sum(df1$visits)
    })
    
    output$kpi_prospect_icon <- renderText({
      df1 <- sum(prospect_data_compare()$visits)
      df2 <- sum(derivedData()$visits)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else if(change>0){
        "fa fa-caret-up"
      }else if(change<0){
        "fa fa-caret-down"
      }
    })
    
    ## prospect box end ####
    
    ## service boxes ####
    service_data <- reactive({
      if(is.null(input$select_dates)){
        df1 <- df_trend
      }else{
        df1 <- brushedPoints(df_trend,input$select_dates)
      }
      df1 %>% filter(segment.name == "service")
    })
    
    service_data_compare <- reactive({
      if(!is.null(input$select_dates)){
        df1 <- compare_data()
        df1 %>% filter(segment.name == "service") 
      }
    })
    
    output$kpi_service <- renderText({ 
      df1 <- service_data()
      ## "fa fa-caret-up"
      comma(sum(df1$visits))
    })
    
    output$kpi_service_change <- renderText({ 
      df1 <- sum(service_data_compare()$visits)
      df2 <- sum(service_data()$visits)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else{
        percent(change)
      }
      ## "fa fa-caret-up"
      # str(df1$datetime[1])
    })
    
    output$kpi_service_colour <- renderText({ 
      df1 <- sum(service_data_compare()$visits)
      df2 <- sum(derivedData()$visits)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        "red"
      }else if(change>0){
        "green"
      }else if(change<0){
        "red"
      }
      ## "fa fa-caret-up"
      # sum(df1$visits)
    })
    
    output$kpi_service_icon <- renderText({
      df1 <- sum(service_data_compare()$visits)
      df2 <- sum(derivedData()$visits)
      change <- (df2-df1)/df1
      if(is.infinite(change)){
        ""
      }else if(change>0){
        "fa fa-caret-up"
      }else if(change<0){
        "fa fa-caret-down"
      }
    })
    
    ## service box end ####
    
    ## render a table ####
    model_data <- reactive({
      derivedData_big() %>%
        filter(section == "new-cars") %>%
        select(sub_section,pageviews,kcc_complete,vr_view,photo_view) %>%
        group_by(sub_section) %>%
        summarise(
          vehicle_views=sum(pageviews),
          kcc_rate=sum(kcc_complete)/sum(pageviews),
          vr_rate = sum(vr_view)/sum(pageviews),
          photo_view = sum(photo_view)/sum(pageviews)
        )
    })
    
    output$model_table_old <- DT::renderDataTable({
      datatable(model_data(), options = list(dom = 't')) %>% formatPercentage(3:5,digits = 0)
    })
    
    ## model data ####
    model_data2 <- reactive({
      if(is.null(input$select_dates)){
        df1 <- df_acc
      }else{
        df1 <- df_acc %>% filter(datetime >= min(derivedData()$datetime),datetime <= max(derivedData()$datetime))
      }
    })
    
    model_table_data <- reactive({
      model_data2() %>%
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
    })
    
    model_table_data_selected <- reactive({
      selected <- model_table_data()$vehicle_name[input$model_table_rows_selected]
      df1 <- model_data2() %>% filter(vehicle_name %in% selected)
    })
    
    
    output$model_table <- DT::renderDataTable({
      df1 <- model_table_data() %>% 
        select(vehicle_name,views=vehicle_views,build=kcc_rate,photo=photo_rate,vr=vr_rate,acc=accessory_rate)
      datatable(df1,options = list(dom = 't')) %>% formatCurrency(2,"",digits = 0) %>% formatPercentage(3:6,digits = 0)
    })
    
    
    ## leaflet map ####
    dealer_data <- reactive({
      df1 <- dealer_location
      names(df1)[1] <- "geo_city"
      df1 %>% select(geo_city,longitude,latitude) %>%
        mutate(
          type = "dealer"
        )
      
    })
    
    map_data <- reactive({

      
      cities <- model_table_data_selected() %>% 
        select(geo_city,views=vehicle_views) %>%
        group_by(geo_city) %>%
        summarise(views = sum(views)) %>%
        inner_join(uk_city_joined) %>%
        na.omit() %>%
        filter(views > 1000) %>%
        select(-views,geo_city,latitude,longitude) %>%
        mutate(
          type = "visitors"
        ) %>%
        rbind(dealer_data()) %>%
        filter(longitude<10,longitude>-10)
      
      
      # df1 <- model_data2() %>% select(geo_city,views=vehicle_views)  %>%
      #   group_by(geo_city) %>%
      #   summarise(volume = sum(vehicle_views)) %>%
      #   arrange(-volume) %>%
      #   top_n(2500) %>%
      #   arrange(-volume) 
      # %>% inner_join(uk_city_joined) %>%
      #   na.omit()
    })
    
    output$uk_map <- renderLeaflet({
      
            
      oceanIcons <- iconList(
        dealer = makeIcon("car.png", iconHeight =  12,iconWidth =  12),
        visitors = makeIcon("user-circle.png", iconHeight =  12,iconWidth =  12)
        )

      leaflet(map_data()) %>% addTiles() %>%
        addMarkers(icon = ~oceanIcons[type])
    })
    
    
    
    
    ## validation ####
    output$validation <- renderPrint({
      # str(model_table_data()$vehicle_name[input$model_table_rows_selected])
      # str(input$model_table_rows_selected)
      # paste0("Compared End: ",max(compare_data()$datetime),".. diff: ",(max(compare_data()$datetime)-min(compare_data()$datetime))," ... sum: ",sum(compare_data()$visits))
      # 
      # prevData()
      # sum(compare_data()$visits)
      # paste0(min(compare_data()$datetime)," : ",max(compare_data()$datetime),".. sum:",sum(compare_data()$visits))
      str(map_data())
      
      
    })
    
    output$validation2 <- renderPrint({
      # paste0("Prev:",min(prevData()$datetime)," : ",max(prevData()$datetime),"   ","Selected:",min(derivedData()$datetime)," : ",max(derivedData()$datetime))
      # prevData()
      # paste0("Start: ",min(derivedData_big()$datetime)," ... End:",max(derivedData_big()$datetime),"... diff:",(max(derivedData_big()$datetime)-min(derivedData_big()$datetime)))
      # paste0(as.Date(input$select_dates$xmin,origin="1970-01-01"))
      # str(derivedData_big())
      # paste0("Selected Start: ",min(derivedData()$datetime),".. diff: ",(max(derivedData()$datetime)-min(derivedData()$datetime))," ... sum: ",sum(derivedData()$visits))
      str(dealer_data())
      
    })
    ## server end ####
    }

## run   
shinyApp(ui, server)