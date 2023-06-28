#----------------------------

#Goal: By uisng the app user will be able to extract the area profiles for selected 5 regions.
#Author: Dr. G. Nishani Musafer
#Completed: 2023/06/28

#-----------------------------------
library(shiny)
library(shinythemes)
library(shinyWidgets )
library(DT)
library(dplyr)
library(reshape2)
library(shinydashboard)
library(sparkline)
library(htmlwidgets)
library(shinyalert)
library(shinydashboardPlus)
library(data.table)
library(reactable)
library(tictoc)


percentage_column_style <- function(x){
  
  x <- gsub("% ", "", x)
  
  x <- paste0(x, " %")
}

ui = dashboardPage(
    
     # options = list(sidebarExpandOnHover = TRUE),
     dashboardHeader(controlbarIcon = shiny::icon("bars")),
     
     sidebar = dashboardSidebar(disable = TRUE),
     
      body = dashboardBody(
        
        tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #C8C882;
                                }
                                
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #C8C882;
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #C8C882;
                                }
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #C8C882;
                                }
                                
                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #ff0000;
                                }
                                
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #00ff00;
                                color: #000000;
                                }
                                
                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #ff69b4;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #ff69b4;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #DEDBD3;
                                }
                                
                                /* slider */
                                .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #C8C882;
                                                  border-top: 1px solid #C8C882 ;
                                                  border-bottom: 1px solid #C8C882 ;}
                                                  
                                .js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {
                                                  background: #C8C882;
                                                  border-top: 1px solid #C8C882 ;
                                                  border-bottom: 1px solid #C8C882 ;}
                                                  
                                 /* changes the colour of the number tags */
                                 .irs--shiny .irs-to, .irs--shiny .irs-from {background-color: #C8C882}
                                 
                                 /*drop down menue */
                                 .selected {background-color:#C8C882 !important;}
                                 
                                  /*progress bar */
                                 .progress-bar {background-color: #C8C882}

                                
                                '))),
     
        
        reactableOutput("summaryTable"),
        br(),br(), br(),
        
        radioButtons(
          "download_selection",
          label = "Download Options",
          choices = c("Full Table", "Filtered Table"),
          selected = "Full Table"
          
        ),
        
        br(),
        
        uiOutput("download_bttn_selection")

        
      ),
      controlbar = dashboardControlbar(width = 350, overlay = FALSE,  collapsed = FALSE, skin = "light",
        controlbarMenu(selected = NULL,
          
        controlbarItem(title = "ANCHADA - Area Profile Data",
          pickerInput(
            inputId  = "geo_resolution",
            label    = "Geographical Resolution" ,
            choices  = c("SA4", "SA3", "SA2", "LGA")
            # choicesOpt = list(
            #   style = rep(("color: black; background: lightgrey;"),4))
            
          ),
          
          pickerInput(
            inputId  = "state",
            label    = "State" ,
            choices  = c("New South Wales","Victoria" , "Queensland", "South Australia", 'Western Australia', "Tasmania", "Northern Territory", "Australian Capital Territory")
            
          ),
          
          pickerInput(
            inputId  = "region",
            label    = "Region" ,
            choices  = NULL,
            selected = NULL,
            multiple = TRUE,
            
            options  = pickerOptions(liveSearch = TRUE, virtual_scroll = TRUE, maxOptions  = 5)
           # options = list(`actions-box` = TRUE)
          ),
          
          # pickerInput(
          #   inputId  = "year_selection",
          #   label    = "Year" ,
          #   choices  = 2006:2022,
          #   selected = NULL,
          #   options  = pickerOptions(liveSearch = TRUE, virtual_scroll = TRUE, actionsBox = TRUE)
          # ),
          
          pickerInput(
            inputId  = "sex_selection",
            label    = "Sex" ,
            choices  = c("all", "male", "female"),
            selected = NULL,
            multiple = TRUE
            
          ),
          
          sliderInput("year_selection", "Year",
                      min = 2006, max = 2022,
                      value = c(2006, 2022),
                      sep =""
                      ),
          
          sliderInput("age_selection", "Age",
                      min = 0, max = 24,
                      value = c(0,24)),
          
          div(style="display: inline-block;vertical-align:top; width: 100px;",
              actionButton("view_table", "View Data"))
          
        ))),
     
    
      title = "ANCHADA - Area Profile Data"
    )

server = function(input, output, session) {

  
      #creating empty summary table
      summary_data <- reactiveValues(data = data.frame(resolution = character(),
                                                       region_name = character(),
                                                       year = character(),
                                                       sex = character(),
                                                       age_group = character(),
                                                       source = character(),
                                                       nest_domain = character(),
                                                       indicator_name = character(),
                                                       indicator_value = numeric(),
                                                       state_average = numeric(),
                                                       percentage_difference_from_state = numeric(),
                                                       national_average = numeric(),
                                                       percentage_difference_from_australia = character())
      )
      
      #
      
      area_code <- reactiveValues(value = NA)
      
      n_region <- reactiveValues(value = 0)
      

      
      
      
      #read 2016 ASGS area code and name dataset
      SA_data <- reactive(
        
        read.csv("./data/SA2_2016_AUST_no_geom.csv", header = TRUE, check.names = FALSE)
        
      )
      
      #read LGA geo data
      LGA_data <- reactive({
        site <- c("New South Wales","Victoria" , "Queensland", "South Australia", 'Western Australia', "Tasmania", "Northern Territory", "Australian Capital Territory")
        
        new_lga_data  <- read.csv("./data/LGA_geom.csv", header = TRUE, check.names = FALSE)
        
        new_lga_data$ste_code <- as.numeric(substr(new_lga_data$LGA_CODE_2016,1,1))
        
        new_lga_data$ste <- NA
        
        new_lga_data$ste <- unlist(lapply( new_lga_data$ste_code, function(x){
          
          site[x]
          
        }))
        new_lga_data
      })
      
      
      select_geo_para <- reactive({
        
        list(input$geo_resolution ,input$state)
        
      })
      
      
      #based on selected geo resolution and the state, region will be updated
      observeEvent(select_geo_para(),{
        
        if(input$geo_resolution != 'LGA'){
          
          
          new_sa_sata <- SA_data()
          
          new_sa_List <- unique(new_sa_sata[which(new_sa_sata$STATE_NAME_2016 == input$state ),paste0(input$geo_resolution, "_NAME_2016")])
          
          updatePickerInput(session, "region", choices = sort(unique(new_sa_List)))
          
        }else{
          
          
          new_lga_data <- LGA_data()
          
          new_lga_data <- new_lga_data[!duplicated(new_lga_data),]
          
          new_lga_List <-  unique(new_lga_data[which(new_lga_data$ste == input$state ),paste0(input$geo_resolution, "_NAME_2016")])
          
          updatePickerInput(session, "region", choices = sort(unique(new_lga_List)))
          
        }
        
        
      })
      
      
      #once user press the View Data button, summary table data will be updated based on the selected filters 
      observeEvent(input$view_table,{
        
       #tic("data loading time")
      #   #reading data file for selected geo resolution
        
        path <- "C:/Users/Nishani/Queensland University of Technology/ACWA_QUT - General/Data_Shiny_Area_Profile/new_shiny_area_profiles/"
        
        if(is.null(input$region) == TRUE){
          
          shinyalert("No data!", "Please select at least one region!", type = "error", size = "s")
          
          
        }else{
          
          n_region$value <- length(input$region)
          
          
          
          #-----------------------------------
          #extract the relevant area code for the selected region
          if(input$geo_resolution != 'LGA'){
            
            new_region_data <- SA_data()
            
            area_code$value  <- unique(new_region_data[which(new_region_data$STATE_NAME_2016 == input$state & new_region_data[, paste0(input$geo_resolution, "_NAME_2016")] %in% input$region  ),c(paste0(input$geo_resolution, "_CODE_2016"))])
            
            
          }else{
            
            new_region_data <- LGA_data()
            
            new_region_data <- new_region_data[!duplicated(new_region_data),]
            
            area_code$value  <- unique(new_region_data[which(new_region_data$ste == input$state & new_region_data[, paste0(input$geo_resolution, "_NAME_2016")] %in% input$region  ),c(paste0(input$geo_resolution, "_CODE_2016"))])
            
          }
          
          
          #---------------------------------------------
          
          file_names <- NULL
          for(i in 1:length(area_code$value)){
            
            file_names <- c(file_names, paste0(path,"area_profile_",area_code$value[i], "_" , input$geo_resolution, ".csv" ))
          }
          
          
          new_data <- lapply(file_names, function(x)read.csv(x, header = TRUE, check.names = FALSE))
          
          new_data <- do.call("rbind", new_data)
          
          #filter by year, sex
          
          sub_data_index <- which(new_data$Year %in% input$year_selection[1]:input$year_selection[2] & new_data$Sex %in% toupper(input$sex_selection ))
          
          
          if(length(sub_data_index) > 0){
            
            new_data <- new_data[sub_data_index,]
            
            #filter based on selected age
            select_age <- as.numeric(input$age_selection[1]:input$age_selection[2])
            
            #select relevant rows for selected age
            if(length(select_age ) == 1){
              
              age_index <- which( select_age >= new_data$ageRangeMin & select_age <= new_data$ageRangeMax)
              
            }else{
              
              age_index_list <- unlist(lapply(select_age, function(x){
                
                which(x >= new_data$ageRangeMin & x <= new_data$ageRangeMax)
                
              }))
              
              age_index <- unique(age_index_list)
            }
            
            if(length(age_index) > 0){
              
              sub_data <- new_data[age_index, ]
              sub_data$percentage_difference_from_state <- ifelse(sub_data$dataItemValue == 0 &  sub_data$state_avg == 0, 0, round(((sub_data$dataItemValue -  sub_data$state_avg)/ sub_data$state_avg) * 100,2))
              sub_data$percentage_difference_from_national <- ifelse(sub_data$dataItemValue == 0 &  sub_data$national_avg == 0, 0,round(((sub_data$dataItemValue -  sub_data$national_avg)/ sub_data$national_avg) * 100,2))
              
              inf_state_index <- which(is.infinite(sub_data$percentage_difference_from_state) == TRUE)
              inf_national_index <- which(is.infinite(sub_data$percentage_difference_from_national) == TRUE)
              
              if(length(inf_state_index) > 0){
                
                sub_data$percentage_difference_from_state[inf_state_index] <- NA
              }
              
              if(length(inf_national_index) > 0){
                
                sub_data$percentage_difference_from_national[inf_national_index] <- NA
              }
              
              
              sub_data$percentage_graphical_difference_from_state <- sub_data$percentage_difference_from_state
              sub_data$percentage_graphical_difference_from_national <- sub_data$percentage_difference_from_national 
              sub_data <- sub_data[, c("Code", "Name", "Year", "Sex", "age_group","Source","nestDomain", "dataItemName", "dataItemValue","state_avg", "percentage_difference_from_state", "percentage_graphical_difference_from_state", "national_avg", "percentage_difference_from_national", "percentage_graphical_difference_from_national" )]
              #sub_data$Code <- input$geo_geo_resolution
              names(sub_data)<- c("resolution", "region_name", "year", "sex", "age_group", "source","nest_domain","indicator_name", "indicator_value", "state_average", "percentage_difference_from_state","percentage_graphical_difference_from_state","national_average", "percentage_difference_from_national", "percentage_graphical_difference_from_national" )
              
              if(n_region$value > 1){
                
                sub_data <- sub_data %>% select(-percentage_difference_from_state, -percentage_graphical_difference_from_national)
              }
              
              sub_data$sex <- tolower(sub_data$sex)
              sub_data$sex <- paste(toupper(substr(sub_data$sex, 1, 1)), substr(sub_data$sex, 2, nchar(sub_data$sex)), sep="")
              
              sub_data$resolution <- input$geo_resolution
              
            }else{
              
              shinyalert("No data!", "Please use different values for filters!", type = "info", size = "s")
              
              sub_data <- data.frame(resolution = character(),
                                     region_name = character(),
                                     year = character(),
                                     sex = character(),
                                     age_group = character(),
                                     source = character(),
                                     nest_domain = character(),
                                     indicator_name = character(),
                                     indicator_value = numeric(),
                                     state_average = numeric(),
                                     percentage_difference_from_state = numeric(),
                                     national_average = numeric(),
                                     percentage_difference_from_australia = character())
              
              
            }
            
            
          }else{
            
            shinyalert("No data!", "Please use different values for filters!", type = "info", size = "s")
            
            sub_data <- data.frame(resolution = character(),
                                   region_name = character(),
                                   year = character(),
                                   sex = character(),
                                   age_group = character(),
                                   source = character(),
                                   nest_domain = character(),
                                   indicator_name = character(),
                                   indicator_value = numeric(),
                                   state_average = numeric(),
                                   percentage_difference_from_state = numeric(),
                                   national_average = numeric(),
                                   percentage_difference_from_australia = character())
            
          }
          
          
          summary_data$data <- sub_data
          
        }
      
        #toc()
        
      })
      
      # Plain with gray background:
      output$summaryTable <- renderReactable({ 
        
        #tic("table rendering time")
        
        # if(nrow(summary_data$data) > 0){
        #   
        #   withProgress({
        #     for (i in 1:30) {
        #       incProgress(1/30)
        #       Sys.sleep(0.1)
        #     }
        #   }, message = "Loading data!")
        # }
        
        if(n_region$value == 1){
          reactable(summary_data$data,  showSortIcon = TRUE,
                    columns = list(
                      percentage_graphical_difference_from_state = colDef(

                      cell = function(value) {


                        max_diff <- max(abs(summary_data$data[,"percentage_graphical_difference_from_state"]), na.rm = TRUE)
                        #value <- ifelse(is.na(value) == TRUE, 0, value)
                        bar_width <- abs(value) / (max_diff + 1e-6)
                        #bar_width <- ifelse(is.na(bar_width) == TRUE, 0, bar_width)
                        bar_color <- ifelse(value < 0, "red", "green")
                        bar_direction <- ifelse(value < 0, "-50%", "50%")
                        starting_point <- (max_diff - abs(value)) / (2 * max_diff) * 100

                        print(paste0("value -", value, "bar_width - ",  bar_width , "bar_color - ", bar_color, "bar_direction - ", bar_direction, "starting_point - ", starting_point))
                        div(
                           style = "display: flex; align-items: center; height: 100%;",
                             div(
                               style = sprintf("width: 100%%; height: 10px; display: flex; align-items: center; justify-content: center; position: relative;"),
                               div(
                                 style = sprintf("background-color: %s; width: %f%%; height: 100%%; position: absolute; left: %f%%; transform: translateX(%s); border-top-left-radius: %s; border-bottom-left-radius: %s; border-top-right-radius: %s; border-bottom-right-radius: %s; padding-left: 5px; padding-right: 5px;",
                                                 bar_color, bar_width * 100, starting_point, bar_direction,
                                             ifelse(value < 0, "5px", "0"), ifelse(value < 0, "5px", "0"),
                                                ifelse(value < 0, "0", "5px"), ifelse(value < 0, "0", "5px"))
                              ),
                               div(
                                 style = "position: absolute; width: 2px; height: 100%; background-color: black; left: calc(50% - 1);"
                               )
                             )
                           )

                         }

                       ),

                      percentage_graphical_difference_from_national = colDef(
                        cell = function(value) {

                          max_diff <- max(abs(summary_data$data[,"percentage_graphical_difference_from_national"]), na.rm = TRUE)
                          bar_width <- abs(value) / (max_diff + 1e-6)
                          #bar_width <- ifelse(is.na(bar_width) == TRUE, 0, bar_width)
                          bar_color <- ifelse(value < 0, "red", "green")
                          bar_direction <- ifelse(value < 0, "-50%", "50%")
                          starting_point <- (max_diff - abs(value)) / (2 * max_diff) * 100
                          div(
                            style = "display: flex; align-items: center; height: 100%;",
                            div(
                              style = sprintf("width: 100%%; height: 10px; display: flex; align-items: center; justify-content: center; position: relative;"),
                              div(
                                style = sprintf("background-color: %s; width: %f%%; height: 100%%; position: absolute; left: %f%%; transform: translateX(%s); border-top-left-radius: %s; border-bottom-left-radius: %s; border-top-right-radius: %s; border-bottom-right-radius: %s; padding-left: 5px; padding-right: 5px;",
                                                bar_color, bar_width * 100, starting_point, bar_direction,
                                                ifelse(value < 0, "5px", "0"), ifelse(value < 0, "5px", "0"),
                                                ifelse(value < 0, "0", "5px"), ifelse(value < 0, "0", "5px"))
                              ),
                              div(
                                style = "position: absolute; width: 2px; height: 100%; background-color: black; left: calc(50% - 1);"
                              )
                            )
                          )

                        }
                      ),

                      indicator_value = colDef( align = "right"),
                      
                      state_average = colDef( align = "right"),
                      
                      national_average = colDef( align = "right"),
                      
                      indicator_name = colDef( align = "left"),
                      
                      source = colDef( align = "left"),
                      
                      nest_domain = colDef( align = "left"),
                      
                      geo_name = colDef( align = "left"),
                      
                      percentage_difference_from_state = 
                        colDef( align = "right",
                                
                                style = function(value) {
                                  if (is.na(value) == TRUE) {
                                    color <- "black"
                                  }else{
                                    
                                    if(value >= 0){
                                      
                                      color <- "green"
                                    }else{
                                      
                                      color <- "red"
                                    }
                                    
                                  } 
                                  list(color = color)
                                }
                        ),
                      percentage_difference_from_national = 
                        colDef( align = "right",
                                
                                style = function(value) {
                                  if (is.na(value) == TRUE) {
                                    color <- "black"
                                  }else{
                                    
                                    if(value >= 0){
                                      
                                      color <- "green"
                                    }else{
                                      
                                      color <- "red"
                                    }
                                    
                                  } 
                                  list(color = color)
                                }
                        )),
                    
                    defaultColDef = colDef(
                      header = function(value){
                        value <- gsub("_", " ", value, fixed = TRUE)
                        value <- gsub("percentage", "%", value, fixed = TRUE)
                        value <- toupper(value)
                        value<- ifelse(grepl("%", value), percentage_column_style(value) , value)
                        
                        paste(toupper(substr(value, 1, 1)), substr(value, 2, nchar(value)), sep="")
                      } ,
                      
                      cell = function(value) format(value, nsmall = 1),
                      align = "center",
                      minWidth = 70,
                      headerStyle = list(background = "#DEDBD3")
                    ),
                    
                    
                    filterable = TRUE,
                    searchable = TRUE,
                    bordered = TRUE,
                    highlight = TRUE
                    
          )
          
        }else{
          
         
          reactable(summary_data$data,  showSortIcon = TRUE,
                    columns = list(
                      # percentage_graphical_difference_from_state = colDef(
                      # 
                      # cell = function(value) {
                      # 
                      #   
                      #   max_diff <- max(abs(summary_data$data[,"percentage_graphical_difference_from_state"]), na.rm = TRUE)
                      #   #value <- ifelse(is.na(value) == TRUE, 0, value)
                      #   bar_width <- abs(value) / (max_diff + 1e-6)
                      #   #bar_width <- ifelse(is.na(bar_width) == TRUE, 0, bar_width)
                      #   bar_color <- ifelse(value < 0, "red", "green")
                      #   bar_direction <- ifelse(value < 0, "-50%", "50%")
                      #   starting_point <- (max_diff - abs(value)) / (2 * max_diff) * 100
                      # 
                      #   print(paste0("value -", value, "bar_width - ",  bar_width , "bar_color - ", bar_color, "bar_direction - ", bar_direction, "starting_point - ", starting_point))
                      #   div(
                      #      style = "display: flex; align-items: center; height: 100%;",
                      #        div(
                      #          style = sprintf("width: 100%%; height: 10px; display: flex; align-items: center; justify-content: center; position: relative;"),
                      #          div(
                      #            style = sprintf("background-color: %s; width: %f%%; height: 100%%; position: absolute; left: %f%%; transform: translateX(%s); border-top-left-radius: %s; border-bottom-left-radius: %s; border-top-right-radius: %s; border-bottom-right-radius: %s; padding-left: 5px; padding-right: 5px;",
                      #                            bar_color, bar_width * 100, starting_point, bar_direction,
                      #                        ifelse(value < 0, "5px", "0"), ifelse(value < 0, "5px", "0"),
                      #                           ifelse(value < 0, "0", "5px"), ifelse(value < 0, "0", "5px"))
                      #         ),
                      #          div(
                      #            style = "position: absolute; width: 2px; height: 100%; background-color: black; left: calc(50% - 1);"
                      #          )
                      #        )
                      #      )
                      # 
                      #    }
                      # 
                      #  ),
                      
                      # percentage_graphical_difference_from_national = colDef(
                      #   cell = function(value) {
                      # 
                      #     max_diff <- max(abs(summary_data$data[,"percentage_graphical_difference_from_national"]), na.rm = TRUE)
                      #     bar_width <- abs(value) / (max_diff + 1e-6)
                      #     #bar_width <- ifelse(is.na(bar_width) == TRUE, 0, bar_width)
                      #     bar_color <- ifelse(value < 0, "red", "green")
                      #     bar_direction <- ifelse(value < 0, "-50%", "50%")
                      #     starting_point <- (max_diff - abs(value)) / (2 * max_diff) * 100
                      #     div(
                      #       style = "display: flex; align-items: center; height: 100%;",
                      #       div(
                      #         style = sprintf("width: 100%%; height: 10px; display: flex; align-items: center; justify-content: center; position: relative;"),
                      #         div(
                      #           style = sprintf("background-color: %s; width: %f%%; height: 100%%; position: absolute; left: %f%%; transform: translateX(%s); border-top-left-radius: %s; border-bottom-left-radius: %s; border-top-right-radius: %s; border-bottom-right-radius: %s; padding-left: 5px; padding-right: 5px;",
                      #                           bar_color, bar_width * 100, starting_point, bar_direction,
                      #                           ifelse(value < 0, "5px", "0"), ifelse(value < 0, "5px", "0"),
                      #                           ifelse(value < 0, "0", "5px"), ifelse(value < 0, "0", "5px"))
                      #         ),
                      #         div(
                      #           style = "position: absolute; width: 2px; height: 100%; background-color: black; left: calc(50% - 1);"
                      #         )
                      #       )
                      #     )
                      # 
                      #   }
                      # ),
                      # 
                      indicator_value = colDef( align = "right"),
                      
                      state_average = colDef( align = "right"),
                      
                      national_average = colDef( align = "right"),
                      
                      indicator_name = colDef( align = "left"),
                      
                      source = colDef( align = "left"),
                      
                      nest_domain = colDef( align = "left"),
                      
                      geo_name = colDef( align = "left"),
                      
                      percentage_difference_from_state = 
                        colDef( align = "right",
                                
                                style = function(value) {
                                  if (is.na(value) == TRUE) {
                                    color <- "black"
                                  }else{
                                    
                                    if(value >= 0){
                                      
                                      color <- "green"
                                    }else{
                                      
                                      color <- "red"
                                    }
                                    
                                  } 
                                  list(color = color)
                                }
                        ),
                      percentage_difference_from_national = 
                        colDef( align = "right",
                                
                                style = function(value) {
                                  if (is.na(value) == TRUE) {
                                    color <- "black"
                                  }else{
                                    
                                    if(value >= 0){
                                      
                                      color <- "green"
                                    }else{
                                      
                                      color <- "red"
                                    }
                                    
                                  } 
                                  list(color = color)
                                }
                        )),
                    
                    defaultColDef = colDef(
                      header = function(value){
                        value <- gsub("_", " ", value, fixed = TRUE)
                        value <- gsub("percentage", "%", value, fixed = TRUE)
                        value <- toupper(value)
                        value<- ifelse(grepl("%", value), percentage_column_style(value) , value)
                        
                        paste(toupper(substr(value, 1, 1)), substr(value, 2, nchar(value)), sep="")
                      } ,
                      
                      cell = function(value) format(value, nsmall = 1),
                      align = "center",
                      minWidth = 70,
                      headerStyle = list(background = "#DEDBD3")
                    ),
                    
                    
                    filterable = TRUE,
                    searchable = TRUE,
                    bordered = TRUE,
                    highlight = TRUE
                    
          )
        }
        
        
        
        #toc()
          
      })
      
    observeEvent(input$download_selection,{

      output$download_bttn_selection <- renderUI({
        
        if(input$download_selection == "Full Table"){
          
          downloadButton('download',"Download the data")
          
        }else{
          
          tags$button(icon("download"), "Download the data", onclick = "Reactable.downloadDataCSV('summaryTable', 'area_profile_filtered_table.csv')")
        }
      })
      
      })
        
      
      output$download <- downloadHandler(
        filename = function(){paste0("area_profile_", area_code$value, "_", input$geo_resolution, "_year_", input$year_selection[1], "_",input$year_selection[2] , "_age_group_",input$age_selection[1], "_" , input$age_selection[2], ".csv")}, 
        content = function(fname){
          write.csv(summary_data$data, fname, row.names = FALSE)
        }
      )
      
      
    }

# Run the application 
shinyApp(ui = ui, server = server)

