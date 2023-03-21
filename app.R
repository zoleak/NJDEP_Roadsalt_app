### Author: Kevin Zolea ###
### Shiny App for Roadsalt Project ###
if (!require(pacman)) {
  install.packages('pacman')
  
}
pacman::p_load("ggplot2","tidyr","plyr","dplyr","readxl","shinycssloaders",
               "readr","cowplot","lubridate","scales",
               "gridExtra","stringr","ggpmisc","data.table","rlang","purrr",
               "shiny","shinydashboard","shinydashboardPlus","DT","leaflet","rgdal","sf","rmapshaper",
               "rsconnect","shinyjs","shinyWidgets","plotly","ggpubr")

library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)
library(readxl)
library(shinycssloaders)
library(readr)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ggpmisc)
library(data.table)
library(rlang)
library(purrr)
library(DT)
library(leaflet)
library(sf)
library(rmapshaper)
library(rsconnect)
library(shinyjs)
library(rgeos)
library(leaflet.extras)
library(shinyWidgets)
library(plotly)
library(ggpubr)
###########################################################################################
### read in data  ###
roadsalt_data<-read_tsv("cleanest_qa_dataset.tsv")%>%
  mutate(year = lubridate::year(stdate))%>%
  mutate(month = lubridate::month(stdate))
###########################################################################################
### Data for correlation plots ###
roadsalt_corr<-read_tsv("cleanest_data_for_correlations.tsv")%>%
  dplyr::filter(!tds == "NA")%>%
  dplyr::filter(!Specific_conductance == "NA")%>%
  dplyr::filter(!Chloride == "NA")%>%
  dplyr::group_by(locid,stdate)%>%
  dplyr::mutate(ratio = format(tds/Specific_conductance,digits = 1))%>%
  dplyr::filter(!ratio >1 & !ratio < .4)
###########################################################################################
# Format WMA numbers with leading zeros using sprintf
road_salt_corr$WMA <- sprintf("%02d", as.numeric(road_salt_corr$WMA))
###########################################################################################
### Read in WMA land use data set that contains % impervious surface for each WMA ###
wma_imperv_calc<-read_xlsx("WMA_%impervious_calc.xlsx",col_names = T)%>%
  dplyr::mutate(WMA = as.character(WMA),PercentIS = round(PercentIS))
### Join WMA land use data set to roadsalt_corr ###
roadsalt_corr_wma_analysis<-left_join(roadsalt_corr,wma_imperv_calc,by = "WMA")
### Make dataframe calculating the median,mean,and maximum TDS for WMAs ###
wma_tds_stats<-roadsalt_corr_wma_analysis%>%
  dplyr::group_by(WMA)%>%
  dplyr::summarise(Median = median(tds),Mean = mean(tds), Max = max(tds))
###########################################################################################
### Join roadsalt_corr_wma_analysis with wma_tds_stats to get summary stats with % impervious surface ###
wma_final_df<-left_join(wma_tds_stats,wma_imperv_calc,by = "WMA")
###########################################################################################
### Read in WMA land use data that contains % impervious surface in 300' buffer for each WMA ###
wma_buffer_calc<-read_xlsx("WMA_%impervious_calc.xlsx",sheet = "Sheet1",col_names = T)%>%
  dplyr::mutate(WMA = as.character(WMA),PercentIS = round(PercentIS))
### Join buffered WMA land use data set to roadsalt_corr ###
buff_roadcorr_wma_anal<-left_join(roadsalt_corr,wma_buffer_calc,by="WMA")
### Make dataframe calculating the median,mean,and maximum TDS for buffered WMAs ###
buff_wma_tds_stats<-buff_roadcorr_wma_anal%>%
  dplyr::group_by(WMA)%>%
  dplyr::summarise(Median = median(tds),Mean = mean(tds), Max = max(tds))
###########################################################################################
### Join wma_buffer_calc with buff_wma_tds_stats to get summary stats with % impervious surface ###
buff_wma_final_df<-left_join(buff_wma_tds_stats,wma_buffer_calc,by="WMA")

### Filter correlation data to have north/south based on WMA ###
## Removes WMA's that are south ###
remove_south=c("12","13","14","15","16","17","18","19","20")
north_corr_road<-roadsalt_corr%>%
  filter(!WMA %in% remove_south)
#### Creates dataframe of chloride road salt data for south part of state
south_corr_road<-roadsalt_corr %>%
  filter(WMA %in% remove_south)
###########################################################################################
### Change column names ###
#names(roadsalt_corr)[names(roadsalt_corr) == "Specific_conductance"]<- "Specific conductance"
#names(roadsalt_corr)[names(roadsalt_corr) == "tds"]<- "Total dissolved solids"
###########################################################################################
### Read in physiographic province dataset with monitoring locations matched to province ###
physio_monitoring_stations<-read_xlsx("physio_monit_stations.xlsx",col_names = T)
### Need to do a join to get physio_monitoring_stations data matched with roadsalt_corr data ###
### In order to this, have to make cuts on roadsalt_corr dataset to locids column so they can match with physio_monitoring_stations dataset ###
# Define a named vector with the patterns to replace
patterns <- c("USGS-" = "", "21NJDEP1-" = "", "NJDEP_BB-" = "", "NJDEP_BFBM-" = "",
              "11NPSWRD-MORR_" = "", "BTMUA-" = "", "DRBC-" = "", "31DRBCSP-" = "",
              "31DELRBC-" = "", "31DELRBC_WQX-" = "", "KWMNDATA-" = "", "GSWA-" = "",
              "FOB-" = "", "SPC-" = "", "NJDEP_BEAR-" = "", "NJPC-" = "", "NJDEP_DSREH-" = "")

# Use str_replace_all to replace all patterns in the NewStation column
test_road_corr$NewStation <- str_replace_all(test_road_corr$locid, patterns)

physio_corr <- test_road_corr %>%
  inner_join(physio_monitoring_stations %>% 
                select(NewStation, PROVINCE), 
              by = "NewStation") %>%
  filter(PROVINCE %in% c("Coastal Plain", "Piedmont", "Highlands", "Valley and Ridge")) %>%
  select(-NewStation)
  
coastal_plain_df <- physio_corr %>%
  filter(PROVINCE == "Coastal Plain")

piedmont_df <- physio_corr %>%
  filter(PROVINCE == "Piedmont")

highlands_df <- physio_corr %>%
  filter(PROVINCE == "Highlands")

valley_ridge_df <- physio_corr %>%
  filter(PROVINCE == "Valley and Ridge")
###########################################################################################
### Read in tds vs. sc. model info to add to plot ###
wma_corr_tds<-read_xlsx("WMA_corr_table_tds_sc.xlsx",col_names = T)%>%
  dplyr::mutate(PercentIS = as.numeric(PercentIS))%>%
  dplyr::arrange(desc(PercentIS))%>%
  dplyr::mutate(PercentIS = paste0(PercentIS,"%"))%>%
  dplyr::mutate(r.squared = signif(r.squared,digits = 3))
###########################################################################################
### Read in cl vs. sc model info to add to plot ###
cl_wma_corr<-read_xlsx("WMA_corr_table_cl_sc.xlsx",col_names = T)
### Read in buff model info to add to plot ###
buff_wma_corr_tds<-read_xlsx("buff_WMA_corr_table_tds_sc.xlsx",col_names = T)%>%
  dplyr::mutate(PercentIS = as.numeric(PercentIS))%>%
  dplyr::arrange(desc(PercentIS))%>%
  dplyr::mutate(PercentIS = paste0(PercentIS,"%"))%>%
  dplyr::mutate(r.squared = signif(r.squared,digits = 3))
buff_wma_corr_cl<-read_xlsx("buff_WMA_corr_table_cl_sc.xlsx",col_names = T)
### Read in shapefiles and Impaired HUC table ###
NJ_Map_Road<-st_read("shapefiles/2014_NJ_Integrated_Report_AU.shp")
Impaired_HUCS<-st_read("shapefiles/Impaired_HUCS_Shapefile.shp")
imp_huc_table<-read_xls("imp_huc_table.xls",col_names = T)
###########################################################################################
### Change projection to work with leaflet ###
map_df <- st_transform(NJ_Map_Road, crs="+init=epsg:4326")
map_impaired_df<-st_transform(Impaired_HUCS,crs="+init=epsg:4326")
###########################################################################################
### simplify shapefiles ###
map_df<-ms_simplify(map_df)
map_impaired_df<-ms_simplify(map_impaired_df)
###########################################################################################
### This drop Z and M dimensions
good_map_df<- st_zm(map_df, drop = T, what = "ZM")
good_imp_map_df<- st_zm(map_impaired_df, drop = T, what = "ZM")
###########################################################################################
### Needed to get polygons on map because ms_simplify gives names to geometry; which gives error ###
names(st_geometry(good_map_df)) = NULL
names(st_geometry(good_imp_map_df)) = NULL
###########################################################################################
### Make dataframe to be displayed under leaflet map ###
leaflet_df<-imp_huc_table%>%
  dplyr::select('Assessment Unit Name',"HUC14",'Watershed Management Area',
                'Watershed Management Area Names','Year 1st Listed')
###########################################################################################
### theme for plots ###
shiny_plot_theme<- theme_linedraw()+
  theme(plot.title=element_text(size=15, face="bold",vjust=0.5,hjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        plot.caption = element_text(color = "navy",face = "bold",size = 12),
        legend.text=element_text(size=10, face="bold"),
        plot.subtitle = element_text(size=15, face="bold",vjust=0.5,hjust = 0.5))
###########################################################################################
### Create subset vectors for selectInput widget in app ###
parameters<-unique(roadsalt_data$charnam)
locids<- unique(roadsalt_corr$locid)
hucs_list<-unique(roadsalt_data$HUC14)
impaired_huc_list<-unique(imp_huc_table$HUC14)
#########################################################################################
### Formula for correlation plots to get pasted on plot ###
formula1 <- y ~ x
###########################################################################################
###########################################################################################
### Create ui ###
ui= dashboardPage(
                  header = dashboardHeader(title = "NJDEP Road Salt Project",titleWidth = 400,
                                           dropdownMenu(
                                             type = "notifications",
                                             notificationItem(text = "More road salt info here!",
                                                              href = "https://www.thoughtco.com/environmental-effects-of-road-salt-1204123"))),
                   sidebar = dashboardSidebar(sidebarMenu(id = "left_sidebar",
                                                          tags$li(class="dropdown",
                                                                  tags$a(href="https://www.nj.gov/dep/", target="_blank",
                                                                         img(width= 100,height = 100,src="https://www.nj.gov/dep/awards/images/deplogoB.jpg",class="road_pics"))
                                                          ),
                                                          menuItem("Home",
                                                                   tabName = "home",
                                                                   icon = icon("home")
                                                          ),
                                                          menuItem("Data",
                                                                   tabName = "data",
                                                                   icon = icon("calendar")),
                                                          menuItem("Map",tabName = "my_map",icon=icon("globe")),
                                                          menuItem("Statewide Data Plots",
                                                                   tabName = "boxplots",
                                                                   icon = icon("bar-chart-o")),
                                                          menuItem("Correlation Plots",
                                                                   tabName = "corr",
                                                                   icon = icon("bar-chart-o"))),
                                              br(),
                                              div(style="text-align:center",
                                                  "The data used for this analysis is from NJDEP's quality-assured,freshwater water quality assessment dataset"),
                                              br(),
                                              fluidPage(column(3,offset = 3.8,
                                                               a(actionButton(inputId = "email1", label = "Contact", 
                                                                              icon = icon("envelope", lib = "font-awesome")),
                                                                 href="mailto:kevin.zolea@gmail.com"))),
                                              #a(href="https://www.linkedin.com/in/kevinmichaelzolea", icon("linkedin-square","fa-2x")),
                                              HTML("<h4>&nbsp; &nbsp; &nbsp; Author: Kevin Zolea </h4>")),
                   body = dashboardBody(### Creates custom font ###
                     useShinyjs(),
                     tags$head(tags$style(HTML(
                       
                       '.main-header .logo {
                       font-family: "Georgia", Times, "Times New Roman", serif;
                       font-weight: bold;
                       font-size: 24px;
                       color: #ad1d28;
                       }
                       
                       .skin-blue .main-header .logo{
                       background-color: Navy;
                       }
                       
                       .skin-blue .main-header .navbar{
                       background-color: Navy;
                       }
                       
                       
                       \\h3 {
                       font-weight: bold;
                       color: Navy;
                       }
                       
                       \\h1 {
                       font-weight: bold;
                       color: Navy;
                       }
                       
                       .content-wrapper, .right-side {
                       background-color: #CCCCCC;
                       }
                       
                       .road_pics{
                       width: auto;
                       height: 100%;
                       max-height: 20vh;
                       }
                       
                       .bodytext{
                       color: #404040;
                       '
                       
                     ))),
                     ### This uses custom CSS to create a landing page for app ###
                     #tags$head(tags$style(HTML('
                     #.modal.in .modal-dialog{
                     #width:100%;
                     #height:100%;
                     #margin:0px;
                     #}
                     
                     #.modal-content{
                     #width:100%;
                     #height:100%;
                     # }
                     #))),
                     
                     ### Creates the different tabs on the left sidebar of app ###
                     tabItems(
                       tabItem(tabName = "home",
                               h1("Welcome to the NJDEP Road Salt Project App!"),
                               h3("Introduction:"),
                               div(class="bodytext",h4("This is a project of the Division of Water Monitoring and Standards",a("(DWMS)",href = "https://www.state.nj.us/dep/wms/",target = "_blank"),
                                                       "& the Bureau of Environmental Analysis, Restoration and Standards",a("(BEARS)",href = "https://www.state.nj.us/dep/wms/bears/index.html",target = "_blank"),
                                                       ",within the New Jersey Department of Environmental Protection",a("(NJDEP).",href = "https://www.nj.gov/dep/",target = "_blank"),
                                                       "For more information on road salt, click on the notification icon in the top header.")),
                               h3("How to use App:"),
                               div(class="bodytext",h4("Start by clicking through the side menu on the left and going through the different options available.
                                                       If you click on the Data tab, you can get a view of the data used for the analysis. There is also an option,
                                                       at the bottom of the interactive table, to download the data. By clicking on the Map tab, you can get a spatial view of all the HUCs in NJ, as well
                                                       as the impaired HUCs for TDS in the 2014 303(d) list. Click on the Statewide Data Plots tab and a right side bar will pop up 
                                                       giving you options to customize the different plots available. The Correlation Plots tab gives you options to see the different
                                                       correlations between the parameters.")),
                               br(),br(),br(),
                               img(width = 350, height = 200, src = "Picture2.png",class="road_pics"),
                               img(width = 350, height = 200,src = "Picture1.png",class="road_pics")),
                       tabItem(tabName = "data",
                               infoBox("Total # of HUCs:",810,color = "navy",icon = icon("map-marker")),
                               infoBox("Total # of Monitoring Stations:",3644,color = "navy",icon = icon("thermometer-empty")),
                               infoBox("Study Year Range:","1997-2018",color = "navy",icon = icon("calendar")),
                               DT::dataTableOutput("Table1")%>%
                                 withSpinner(type = 5,color = "blue"),
                               downloadButton('downloadData','Download Data')),
                       tabItem(tabName = "my_map",
                               fluidRow(box(width=12,closable = T,
                                            collapsible = T,
                                            tags$b("The following polygons in the map are the"),
                                            a("assessment units impaired ",
                                              href = "https://www.state.nj.us/dep/wms/bears/assessment.htm",target = "_blank"),
                                            tags$b("for TDS\nin the 2014 303(d) list")))%>%
                                 fluidRow(box(width=12,leafletOutput("leaf")%>%
                                                withSpinner(type=5,color = "blue"))),
                               fluidRow(box(width = 12,DT::dataTableOutput("Table2")))),
                       tabItem(tabName = "boxplots",
                               fluidRow(
                                 box(width = 6,plotOutput("plot1")%>%withSpinner(type = 5, color = "blue")),
                                 box(width = 6, plotOutput("plot2")%>%withSpinner(type = 5, color = "blue"))),
                               fluidRow(
                                 box(width = 6,plotOutput("plot3")%>%withSpinner(type = 5, color = "blue")),
                                 box(width = 6,plotOutput("plot4")%>%withSpinner(type = 5, color = "blue")))),
                       tabItem(tabName = "corr",
                               tabBox(width = 12,tabPanel("Site Specific",plotOutput("plot5")%>%withSpinner(type = 5, color = "blue"),
                                                          selectizeInput("huc1",label =em("Select HUC:",style="color:Navy;font-weight: bold;"),
                                                                         choices = sort(as.character(unique(roadsalt_corr$HUC14))),
                                                                         selected = "HUC02030103110020")),
                                      tabPanel("North/South Regions (WMAs)",fluidRow(column(6,plotOutput("plot7")%>%
                                                                                              withSpinner(type = 5, color = "blue")),
                                                                                     column(6,plotOutput("plot8")%>%withSpinner(type = 5, color = "blue")))),
                                      tabPanel("Physiographic Provinces",fluidRow(column(6,plotOutput("plot9")%>%withSpinner(type = 5, color = "blue")),
                                                                                  column(6,plotOutput("plot10")%>%withSpinner(type = 5, color = "blue"))),
                                               fluidRow(column(6,plotOutput("plot11")%>%withSpinner(type = 5, color = "blue")),
                                                        column(6,plotOutput("plot12")%>%withSpinner(type = 5, color = "blue")))),
                                      tabPanel("% Impervious Surface",plotOutput("plot6")%>%withSpinner(type = 5, color = "blue"),
                                               selectInput("stats",label =em("Select Y Variable:",style="color:Navy;font-weight: bold;"),choices = c("Mean","Median","Max"),
                                                           selected = "Mean"),
                                               plotOutput("lastplot")%>%withSpinner(type = 5, color = "blue")),
                                      tabPanel("% Impervious Surface in 300' Buffer",plotOutput("bufferplot")%>%withSpinner(type = 5, color = "blue"),
                                               selectInput("stats2",label =em("Select Y Variable:",style="color:Navy;font-weight: bold;"),choices = c("Mean","Median","Max"),
                                                           selected = "Mean"),
                                               plotOutput("bufferplot2")%>%withSpinner(type = 5, color = "blue")),
                                      awesomeCheckbox(inputId = "statewide",
                                                      label = em("Show Statewide Regression Line",style = "color:Navy;font-weight: bold;"),
                                                      value = FALSE),downloadButton("downloadplot","Download Plot")),
                               fluidRow(
                                 box(selectInput("x",label =em("Select X Variable:",style="color:Navy;font-weight: bold;"),
                                                 choices = c("tds","Chloride","Specific_conductance"),selected = "Specific_conductance")),
                                 box(selectInput("y",label = em("Select Y Variable:",style = "color:Navy;font-weight: bold;"),
                                                 choices =c("tds","Chloride","Specific_conductance") ,selected = "tds"))))),
                     #box(selectizeInput("huc1",label =em("Select HUC:",style="color:Navy;font-weight: bold;"),
                     #                                     choices = list("Impaired HUCs for TDS:"=as.character(unique(impaired_huc_list)),
                     #                                                    "All Other HUCs:"=as.character(unique(roadsalt_corr$HUC14))),
                     #                   selected = "HUC02030103110020"))))),
                     
                     #box(uiOutput("huc1"))))),
                     #box(uiOutput("locid1"))))),
                     tags$head(
                       tags$style(HTML("
                                       .shiny-output-error-validation {
                                       color: red;
                                       }
                                       ")))),
                   controlbar =  dashboardControlbar(skin = "dark",
                                                     id=1,
                                                           title = "Customize Plots",
                                                           selectInput("parameter_input","Select Parameter",
                                                                       parameters,selected = "Chloride"),
                                                           sliderInput("alpha","Select Shade of Point",min = 0,max = 0.8,value=0.5),
                                                           sliderInput("date","Select Year Range",
                                                                       min = 1997,
                                                                       max = 2018,
                                                                       value = c(1997,2018),
                                                                       sep = "",
                                                                       step = 1)))
###########################################################################################
### Create server of app ###
server<- function(input,output,session){
  
#### Creates option for rightsidebar to be opened once the plots tab in clicked on ###
  observe({
    if (input$left_sidebar == "boxplots") {
      shinyjs::addClass(selector = "aside.control-sidebar", class = "control-sidebar-open")
    } else {
      shinyjs::removeClass(selector = "aside.control-sidebar", class = "control-sidebar-open")
    }
  })
########################################################################################### 
### Create reactive dataframe ###
  parameter_selected<- reactive({
    roadsalt_data%>%
      filter(charnam == input$parameter_input)%>%
      filter(year >= input$date[1])%>%
      filter(year <= input$date[2])
  })
########################################################################################### 
### Creates reactive dataframe for median concentrations ###
  
  median_plot<- reactive({
    roadsalt_data%>%
      group_by(year,charnam)%>%
      summarise(median = median(val,na.rm = T))%>%
      filter(charnam == input$parameter_input)%>%
      filter(year >= input$date[1])%>%
      filter(year <= input$date[2])

  })
###########################################################################################
### This creates landing page to explain app ### 
  #observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = median_plot, { 
    # event will be called when median_plot changes, which only happens once, when it is initially calculated
    #showModal(modalDialog(
      #title = tags$b("Welcome to the NJDEP's Road Salt Project App!"), 
      #h1('About this App'),
      #HTML('<img src="http://www.google.nl/images/branding/googlelogo/2x/googlelogo_color_272x92dp.png">'),
      
      #p('Theoretically you can put whatever content you want in here')
    #))
  #})
###########################################################################################  
### Creates reactive dataframe for % over standard ###

  percent_over<- reactive({
    if(input$parameter_input == "Chloride"){
      roadsalt_data%>%
        filter(charnam == input$parameter_input)%>%
        mutate(val = as.numeric(val),greater = val >230) %>%
        mutate(year = lubridate::year(stdate)) %>%
        group_by(year) %>%
        summarize(n_greater = sum(greater),n_less=sum(!greater),percentage=mean(greater)*100)%>%
        filter(year >= input$date[1])%>%
        filter(year <= input$date[2])
        
    }
    else if(input$parameter_input == "Total dissolved solids"){
      roadsalt_data%>%
        filter(charnam == input$parameter_input)%>%
        mutate(val = as.numeric(val),greater = val >500) %>%
        mutate(year = lubridate::year(stdate)) %>%
        group_by(year) %>%
        summarize(n_greater = sum(greater),n_less=sum(!greater),percentage=mean(greater)*100)%>%
        filter(year >= input$date[1])%>%
        filter(year <= input$date[2])
    }
    
    else{
      validate(
        need(input$parameter_input != "Specific conductance", 
             "There is no surface water quality standard\nfor specific conductance"
             )
      )
    }
  })
###########################################################################################    
 month_percent_over<-reactive({
   if(input$parameter_input == "Chloride"){
     roadsalt_data%>%
       filter(charnam == input$parameter_input)%>%
       mutate(val = as.numeric(val),greater = val >230) %>%
       group_by(month,year) %>%
       summarize(n_greater = sum(greater),n_less=sum(!greater),percentage=mean(greater)*100)%>%
       filter(year >= input$date[1])%>%
       filter(year <= input$date[2])
     
   }
   else if(input$parameter_input == "Total dissolved solids"){
     roadsalt_data%>%
       filter(charnam == input$parameter_input)%>%
       mutate(val = as.numeric(val),greater = val >500) %>%
       group_by(month,year) %>%
       summarize(n_greater = sum(greater),n_less=sum(!greater),percentage=mean(greater)*100)%>%
       filter(year >= input$date[1])%>%
       filter(year <= input$date[2])
   }
   
   else{
     validate(
       need(input$parameter_input != "Specific conductance", 
            "There is no surface water quality standard\nfor specific conductance"
       )
     )
   }
 }) 

###########################################################################################  
### NOT USING THIS CODE ANYMORE BUT KEEP FOR LEARNING PURPOSES!!!!!! ###
 ####                                   ####### 
### Create reactive dataframe for huc & locid correlations ###
# output$huc1<- renderUI({
#   selectizeInput("huc",label =em("Select HUC:",style="color:Navy;font-weight: bold;"),
#                  choices = list("Impaired HUCs for TDS:"=impaired_huc_list,
#                                 "All Other HUCs:"=as.character(unique(roadsalt_corr$HUC14))),
#                  options = list(
#                    placeholder = 'Please select a HUC below',
#                    onInitialize = I('function() { this.setValue(""); }')))
# })
 
# datasub<-reactive({
#   foo <- subset(roadsalt_corr, HUC14 == input$huc)
#   return(foo)
# })
# 
# output$locid1<- renderUI({
#   selectizeInput("locid",label = em("Select Locid:",style="color:Navy;font-weight: bold;"),
#                  choices = unique(datasub()$locid),
#                  selected = unique(datasub()$locid)[1])#,
#                  #multiple = TRUE)
# })
# 
# datasub2<-reactive({
#   foo <- subset(datasub(), locid == input$locid)
#   return(foo)
# })
###########################################################################################  
### Create reactive dataframe for correlation plots based on user input for HUC14 ###
 huc_corr<-reactive({
   roadsalt_corr%>%
     dplyr::filter(HUC14 == input$huc1)
 })
###########################################################################################
###########################################################################################  
### Creates plots ### 
### Creates boxplots ###
  output$plot1<- renderPlot({
    if(input$parameter_input == "Chloride"){
    ggplot(data= parameter_selected(),aes(x=factor(year),y=val))+
      geom_boxplot(alpha = input$alpha)+
      xlab("Year")+ylab("Chloride (mg/L)")+
      ggtitle(input$parameter_input)+
      geom_hline(aes(yintercept = 230,color="red"),size=1.3)+
      geom_hline(aes(yintercept = 860,color="#FF3333"),size=1.3,linetype="dashed")+
      scale_color_manual("",
                           values = c("#FF3333","red"),
                           labels=c("Freshwater Aquatic\nLife Criteria (acute)\nfor chloride = 860 mg/L",
                                    "Freshwater Aquatic\nLife Criteria (chronic)\nfor chloride = 230 mg/L"),
                           guide=guide_legend(override.aes=list(linetype=c(2,1), lwd=c(0.5,1))))+
      shiny_plot_theme+theme(axis.text.x = element_text(angle =90))

    }
    else if(input$parameter_input == "Total dissolved solids"){
      
      ggplot(data= parameter_selected(),aes(x=factor(year),y=val))+
        geom_boxplot(alpha = input$alpha)+
        xlab("Year")+ylab("TDS (mg/L)")+
        ggtitle(input$parameter_input)+
        geom_hline(aes(yintercept=500,color = "red"),size = 1.3)+
        scale_color_manual("",
                           values = c("red"),
                           labels=c("Freshwater Aquatic Life Criteria for TDS = 500 mg/L"))+
                           
        shiny_plot_theme+theme(axis.text.x = element_text(angle =90))
    }
    else{
      ggplot(data= parameter_selected(),aes(x=factor(year),y=val))+
        geom_boxplot(alpha = input$alpha)+
        xlab("Year")+ylab("Specific Conductance (µS/cm)")+
        ggtitle(input$parameter_input)+
        shiny_plot_theme+theme(axis.text.x = element_text(angle =90))
      
    }

  })
###########################################################################################  
### Creates median time series plots ###
  output$plot2<- renderPlot({
    if(input$parameter_input == "Chloride"){
    ggplot(data = median_plot(),aes(x=year,y = median))+
      geom_line(aes(colour = "blue"),size=1.3,stat = "identity")+
      geom_smooth(method = "lm", se=FALSE,linetype = "dashed",aes(y=median,colour="red"))+
      scale_color_manual("",
                         values = c("blue","red"),
                         labels=c("Median Concentration","Trendline"),
                         guide=guide_legend(override.aes=list(linetype=c(1,2), lwd=c(1,0.5))))+
      ggtitle("Annual Median")+
      labs(subtitle= input$parameter_input,
           x="Year", y="Median Chloride (mg/L)" )+
      shiny_plot_theme
    }
    
    
    else if(input$parameter_input == "Total dissolved solids"){
      ggplot(data = median_plot(),aes(x=year,y = median))+
        geom_line(aes(colour = "blue"),size=1.3,stat = "identity")+
        geom_smooth(method = "lm", se=FALSE,linetype = "dashed",aes(y=median,colour="red"))+
        scale_color_manual("",
                           values = c("blue","red"),
                           labels=c("Median Concentration","Trendline"),
                           guide=guide_legend(override.aes=list(linetype=c(1,2), lwd=c(1,0.5))))+
        ggtitle("Annual Median")+
        labs(subtitle= input$parameter_input,
             x="Year", y="Median TDS (mg/L)" )+
        shiny_plot_theme
    }
    
    
    else {
      ggplot(data = median_plot(),aes(x=year,y = median))+
        geom_line(aes(colour = "blue"),size=1.3,stat = "identity")+
        geom_smooth(method = "lm", se=FALSE,linetype = "dashed",aes(y=median,colour="red"))+
        scale_color_manual("",
                           values = c("blue","red"),
                           labels=c("Median Concentration","Trendline"),
                           guide=guide_legend(override.aes=list(linetype=c(1,2), lwd=c(1,0.5))))+
        ggtitle("Annual Median")+
        labs(subtitle= input$parameter_input,
             x="Year", y="Median Specific Conductance (µS/cm)" )+
        shiny_plot_theme
      
      
    }
    
  })
###########################################################################################
  ### Creates % > Standard line scatter plots ###
  output$plot3<- renderPlot({
    if(input$parameter_input == "Chloride"){
    ggplot(data = percent_over(),aes(x=year,y = percentage))+
      geom_point(aes(colour="blue"),size=3,stat = "identity") +
      ggtitle("Percent Chloride Samples\nExceeding Chronic Standard") +
      geom_smooth(method = "lm", se=FALSE,linetype = "dashed",aes(y=percentage,colour="red"))+
      scale_color_manual("",
                         #breaks= c("per"),
                         values = c("blue","red"),
                         labels=c("% Samples > Standard","Trendline"),
                         guide=guide_legend(override.aes=list(linetype=c(0,2), lwd=c(3,0.5))))+
      labs(x="Year",y="Percent samples exceeding chronic standard(230 mg/L)")+
      shiny_plot_theme
    }
    else{
      ggplot(data = percent_over(),aes(x=year,y = percentage))+
        geom_point(aes(colour="blue"),size=3,stat = "identity") +
        ggtitle("Percent Total Dissolved Solid\nSamples Exceeding Standard") +
        geom_smooth(method = "lm", se=FALSE,linetype = "dashed",aes(y=percentage,colour="red"))+
        scale_color_manual("",
                           #breaks= c("per"),
                           values = c("blue","red"),
                           labels=c("% Samples > Standard","Trendline"),
                           guide=guide_legend(override.aes=list(linetype=c(0,2), lwd=c(3,0.5))))+
        labs(x="Year",y="Percent Samples Exceeding Standard(500 mg/L)")+
        shiny_plot_theme
    }
  })
###########################################################################################  
  ### Creates month % > Standard bar plots ###
  output$plot4<- renderPlot({
    if(input$parameter_input == "Chloride"){
    ggplot(data = month_percent_over(),aes(x=month,y = percentage,color=year))+
        geom_point(aes(colour="blue"),size=3,stat = "identity")+
        geom_smooth(method = "lm", se=FALSE,linetype = "dashed",aes(y=percentage,colour="red"))+
        ggtitle("Percent Chloride Samples\nExceeding Chronic Standard")+
        scale_color_manual("",
                           #breaks= c("per"),
                           values = c("blue","red"),
                           labels=c("% Samples > Standard","Trendline"),
                           guide=guide_legend(override.aes=list(linetype=c(0,2), lwd=c(3,0.5))))+
      labs(x="Month",y="Percent samples exceeding chronic standard(230 mg/L)")+
      shiny_plot_theme+
        scale_x_discrete(limits=c(12,11,10,9,8,7,6,5,4,3,2,1))
    }
    else{
      ggplot(data = month_percent_over(),aes(x=month,y = percentage,color = year))+
        geom_point(aes(colour="blue"),size=3,stat = "identity")+
        geom_smooth(method = "lm", se=FALSE,linetype = "dashed",aes(y=percentage,colour="red"))+
        ggtitle("Percent Total Dissolved Solid\nSamples Exceeding Standard")+
        scale_color_manual("",
                           #breaks= c("per"),
                           values = c("blue","red"),
                           labels=c("% Samples > Standard","Trendline"),
                           guide=guide_legend(override.aes=list(linetype=c(0,2), lwd=c(3,0.5))))+
        labs(x="Month",y="Percent Samples Exceeding Standard(500 mg/L)")+
        shiny_plot_theme+
        scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12))
      
    }
  })
###########################################################################################    
### Create correlation plots ###
### Make if/else statements to add statewide regression line to plots ###
  output$plot5 <- renderPlot({
    if(input$statewide == TRUE){
    ggplot(data= huc_corr(),aes_string(x=input$x,y=input$y))+
      geom_point(aes(color = locid))+
      geom_smooth(method = "lm", se = FALSE,formula=formula1,size = 5) +
      geom_smooth(data = roadsalt_corr,method = "lm",se =FALSE,formula = formula1,aes(color = "Statewide Correlation"))+
      stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
                   label.x.npc = 0.5, label.y.npc = 0.9,
                   eq.with.lhs = "italic(hat(y))~`=`~",
                   eq.x.rhs = "~italic(x)",
                   formula = formula1, parse = TRUE, size = 5) +
      stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
                   label.x.npc = 0.5, label.y.npc = 0.83,
                   formula = formula1, parse = TRUE, size = 5)+
      ggtitle(input$huc1)+
      shiny_plot_theme}
    else{
      ggplot(data= huc_corr(),aes_string(x=input$x,y=input$y))+
        geom_point(aes(color = locid))+
        geom_smooth(method = "lm", se = FALSE,formula=formula1) +
        stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
                     label.x.npc = 0.5, label.y.npc = 0.9,
                     eq.with.lhs = "italic(hat(y))~`=`~",
                     eq.x.rhs = "~italic(x)",
                     formula = formula1, parse = TRUE, size = 5) +
        stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
                     label.x.npc = 0.5, label.y.npc = 0.83,
                     formula = formula1, parse = TRUE, size = 5)+
        ggtitle(input$huc1)+
        shiny_plot_theme
    }
  })
###########################################################################################  
### Make plot for North Region correlations ###
  output$plot7<-renderPlot({
    if(input$statewide == TRUE){
    ggplot(data = north_corr_road,aes_string(input$x,y=input$y))+
      geom_point()+
      geom_smooth(method = "lm", se = FALSE,formula=formula1,size = 3) +
      geom_smooth(data = roadsalt_corr,method = "lm",se =FALSE,
                  formula = formula1,aes(color = "Statewide Correlation"))+
      stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
                   label.x.npc = 0.5, label.y.npc = 0.9,
                   eq.with.lhs = "italic(hat(y))~`=`~",
                   eq.x.rhs = "~italic(x)",
                   formula = formula1, parse = TRUE, size = 3) +
      stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
                   label.x.npc = 0.5, label.y.npc = 0.83,
                   formula = formula1, parse = TRUE, size = 5)+
      ggtitle("North Region (WMAs)")+
      labs(caption = "WMAs:1-11")+
      shiny_plot_theme}
    else{
      ggplot(data = north_corr_road,aes_string(input$x,y=input$y))+
        geom_point()+
        geom_smooth(method = "lm", se = FALSE,formula=formula1) +
        stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
                     label.x.npc = 0.5, label.y.npc = 0.9,
                     eq.with.lhs = "italic(hat(y))~`=`~",
                     eq.x.rhs = "~italic(x)",
                     formula = formula1, parse = TRUE, size = 5) +
        stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
                     label.x.npc = 0.5, label.y.npc = 0.83,
                     formula = formula1, parse = TRUE, size = 5)+
        ggtitle("North Region (WMAs)")+
        labs(caption = "WMAs:1-11")+
        shiny_plot_theme
    }
  })
###########################################################################################      
### Make plot for South Region Correlations ###
  output$plot8<-renderPlot({
    if(input$statewide == TRUE){
    ggplot(data = south_corr_road,aes_string(input$x,y=input$y))+
      geom_point()+
      geom_smooth(method = "lm", se = FALSE,formula=formula1,size = 3)+
      geom_smooth(data = roadsalt_corr,method = "lm",se =FALSE,
                    formula = formula1,aes(color = "Statewide Correlation"))+
      stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
                   label.x.npc = 0.5, label.y.npc = 0.9,
                   eq.with.lhs = "italic(hat(y))~`=`~",
                   eq.x.rhs = "~italic(x)",
                   formula = formula1, parse = TRUE, size = 5) +
      stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
                   label.x.npc = 0.5, label.y.npc = 0.83,
                   formula = formula1, parse = TRUE, size = 5)+
      ggtitle("South Region (WMAs)")+
      labs(caption = "WMAs:12-20")+
      shiny_plot_theme}
    else{
      ggplot(data = south_corr_road,aes_string(input$x,y=input$y))+
        geom_point()+
        geom_smooth(method = "lm", se = FALSE,formula=formula1)+
        stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
                     label.x.npc = 0.5, label.y.npc = 0.9,
                     eq.with.lhs = "italic(hat(y))~`=`~",
                     eq.x.rhs = "~italic(x)",
                     formula = formula1, parse = TRUE, size = 5) +
        stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
                     label.x.npc = 0.5, label.y.npc = 0.83,
                     formula = formula1, parse = TRUE, size = 5)+
        ggtitle("South Region (WMAs)")+
        labs(caption = "WMAs:12-20")+
        shiny_plot_theme
    }
  })
###########################################################################################    
### Make Physiographic Provinces Plots ###
  output$plot9<-renderPlot({
    if(input$statewide == TRUE){
    ggplot(data = coastal_plain_df,aes_string(input$x,y=input$y))+
      geom_point()+
      geom_smooth(method = "lm", se = FALSE,formula=formula1,size = 3)+
      geom_smooth(data = roadsalt_corr,method = "lm",se =FALSE,
                    formula = formula1,aes(color = "Statewide Correlation"))+
      stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
                   label.x.npc = 0.5, label.y.npc = 0.9,
                   eq.with.lhs = "italic(hat(y))~`=`~",
                   eq.x.rhs = "~italic(x)",
                   formula = formula1, parse = TRUE, size = 5) +
      stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
                   label.x.npc = 0.5, label.y.npc = 0.83,
                   formula = formula1, parse = TRUE, size = 5)+
      ggtitle("Coastal Plain")+
      shiny_plot_theme}
    else{
      ggplot(data = coastal_plain_df,aes_string(input$x,y=input$y))+
        geom_point()+
        geom_smooth(method = "lm", se = FALSE,formula=formula1)+
        stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
                     label.x.npc = 0.5, label.y.npc = 0.9,
                     eq.with.lhs = "italic(hat(y))~`=`~",
                     eq.x.rhs = "~italic(x)",
                     formula = formula1, parse = TRUE, size = 5) +
        stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
                     label.x.npc = 0.5, label.y.npc = 0.83,
                     formula = formula1, parse = TRUE, size = 5)+
        ggtitle("Coastal Plain")+
        shiny_plot_theme
    }
  })
  
  output$plot10<-renderPlot({
    if(input$statewide == TRUE){
    ggplot(data = piedmont_df,aes_string(input$x,y=input$y))+
      geom_point()+
      geom_smooth(method = "lm", se = FALSE,formula=formula1,size = 3)+
      geom_smooth(data = roadsalt_corr,method = "lm",se =FALSE,
                    formula = formula1,aes(color = "Statewide Correlation"))+
      stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
                   label.x.npc = 0.5, label.y.npc = 0.9,
                   eq.with.lhs = "italic(hat(y))~`=`~",
                   eq.x.rhs = "~italic(x)",
                   formula = formula1, parse = TRUE, size = 5) +
      stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
                   label.x.npc = 0.5, label.y.npc = 0.83,
                   formula = formula1, parse = TRUE, size = 5)+
      ggtitle("Piedmont")+
      shiny_plot_theme}
    else{
      ggplot(data = piedmont_df,aes_string(input$x,y=input$y))+
        geom_point()+
        geom_smooth(method = "lm", se = FALSE,formula=formula1)+
        stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
                     label.x.npc = 0.5, label.y.npc = 0.9,
                     eq.with.lhs = "italic(hat(y))~`=`~",
                     eq.x.rhs = "~italic(x)",
                     formula = formula1, parse = TRUE, size = 5) +
        stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
                     label.x.npc = 0.5, label.y.npc = 0.83,
                     formula = formula1, parse = TRUE, size = 5)+
        ggtitle("Piedmont")+
        shiny_plot_theme
    }
  })
  
  output$plot11<-renderPlot({
    if(input$statewide == TRUE){
    ggplot(data = highlands_df,aes_string(input$x,y=input$y))+
      geom_point()+
      geom_smooth(method = "lm", se = FALSE,formula=formula1,size = 3)+
      geom_smooth(data = roadsalt_corr,method = "lm",se =FALSE,
          formula = formula1,aes(color = "Statewide Correlation"))+
      stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
                   label.x.npc = 0.5, label.y.npc = 0.9,
                   eq.with.lhs = "italic(hat(y))~`=`~",
                   eq.x.rhs = "~italic(x)",
                   formula = formula1, parse = TRUE, size = 5) +
      stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
                   label.x.npc = 0.5, label.y.npc = 0.83,
                   formula = formula1, parse = TRUE, size = 5)+
      ggtitle("Highlands")+
      shiny_plot_theme}
    else{
      ggplot(data = highlands_df,aes_string(input$x,y=input$y))+
        geom_point()+
        geom_smooth(method = "lm", se = FALSE,formula=formula1)+
        stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
                     label.x.npc = 0.5, label.y.npc = 0.9,
                     eq.with.lhs = "italic(hat(y))~`=`~",
                     eq.x.rhs = "~italic(x)",
                     formula = formula1, parse = TRUE, size = 5) +
        stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
                     label.x.npc = 0.5, label.y.npc = 0.83,
                     formula = formula1, parse = TRUE, size = 5)+
        ggtitle("Highlands")+
        shiny_plot_theme
    }
  })
  
    output$plot12<-renderPlot({
      if(input$statewide == TRUE){
      ggplot(data = valley_ridge_df,aes_string(input$x,y=input$y))+
        geom_point()+
        geom_smooth(method = "lm", se = FALSE,formula=formula1,size = 3)+
        geom_smooth(data = roadsalt_corr,method = "lm",se =FALSE,
                      formula = formula1,aes(color = "Statewide Correlation"))+
        stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
                     label.x.npc = 0.5, label.y.npc = 0.9,
                     eq.with.lhs = "italic(hat(y))~`=`~",
                     eq.x.rhs = "~italic(x)",
                     formula = formula1, parse = TRUE, size = 5) +
        stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
                     label.x.npc = 0.5, label.y.npc = 0.83,
                     formula = formula1, parse = TRUE, size = 5)+
        ggtitle("Valley & Ridge")+
        shiny_plot_theme}
      else{
        ggplot(data = valley_ridge_df,aes_string(input$x,y=input$y))+
          geom_point()+
          geom_smooth(method = "lm", se = FALSE,formula=formula1)+
          stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
                       label.x.npc = 0.5, label.y.npc = 0.9,
                       eq.with.lhs = "italic(hat(y))~`=`~",
                       eq.x.rhs = "~italic(x)",
                       formula = formula1, parse = TRUE, size = 5) +
          stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
                       label.x.npc = 0.5, label.y.npc = 0.83,
                       formula = formula1, parse = TRUE, size = 5)+
          ggtitle("Valley & Ridge")+
          shiny_plot_theme
      }
    })
###########################################################################################      
### Create plot for % impervious surface vs. mean, median & max TDS ###
    output$plot6<-renderPlot({
      y_axis<-input$stats
      gg<-ggplot(data = wma_final_df,aes_string(x="PercentIS",y=y_axis))+
        geom_smooth(method = "lm", se = FALSE,formula=formula1)+
        stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
                     label.x.npc = 0.5, label.y.npc = 0.9,
                     eq.with.lhs = "italic(hat(y))~`=`~",
                     eq.x.rhs = "~italic(x)",
                     formula = formula1, parse = TRUE, size = 5) +
        stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
                     label.x.npc = 0.5, label.y.npc = 0.83,
                     formula = formula1, parse = TRUE, size = 5)+
        ggtitle(paste(y_axis,"TDS Vs. % Impervious Surface (2015) in each WMA"))+
        scale_x_continuous(labels=function(x) paste0(x,"%"))+
        labs(x= "% Impervious Surface")+
        shiny_plot_theme
      
      gg<-gg+geom_point()
      gg
    })
###########################################################################################      
    ### Create table of model stats ###
    mytable<-ggtexttable(wma_corr_tds, 
                                theme = ttheme("mBlue",padding = unit(c(10, 3), "mm")),rows=NULL)  
    
    mytable2<-ggtexttable(cl_wma_corr, 
                          theme = ttheme("mBlue",padding = unit(c(10, 3), "mm")),rows=NULL)
    ### Make plot #
    output$lastplot<-renderPlot({
      if(input$y == "tds"){
      pp<-ggplot(data = roadsalt_corr,aes_string(input$x,y=input$y))+
        geom_smooth(method = "lm", se = FALSE,formula=formula1,aes(color = WMA))+
        scale_color_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10",
                                      "11","12","13","14","15","16","17","18","19","20"))
        
          plot_grid(pp,mytable)}
      else{
        pp<-ggplot(data = roadsalt_corr,aes_string(input$x,y=input$y))+
          geom_smooth(method = "lm", se = FALSE,formula=formula1,aes(color = WMA))+
          scale_color_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10",
                                        "11","12","13","14","15","16","17","18","19","20"))
        
        plot_grid(pp,mytable2)}
      
    })
###########################################################################################          
### Make %impervious plot in 300' buffer plot ###
    output$bufferplot<-renderPlot({
      y_axis<-input$stats2
      bb<-ggplot(data = buff_wma_final_df,aes_string(x="PercentIS",y=y_axis))+
        geom_smooth(method = "lm", se = FALSE,formula=formula1)+
        stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
                     label.x.npc = 0.5, label.y.npc = 0.9,
                     eq.with.lhs = "italic(hat(y))~`=`~",
                     eq.x.rhs = "~italic(x)",
                     formula = formula1, parse = TRUE, size = 5) +
        stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
                     label.x.npc = 0.5, label.y.npc = 0.83,
                     formula = formula1, parse = TRUE, size = 5)+
        ggtitle(paste(y_axis,"TDS Vs. % Impervious Surface (2015) in each WMA\n(300'buffer)"))+
        scale_x_continuous(labels=function(x) paste0(x,"%"))+
        labs(x= "% Impervious Surface in 300'buffer")+
        shiny_plot_theme
      
      bb<-bb+geom_point()
      bb
    })
###########################################################################################          
    ### Create table of model stats ###
    mytable3<-ggtexttable(buff_wma_corr_tds, 
                         theme = ttheme("mBlue",padding = unit(c(10, 3), "mm")),rows=NULL)  
    
    mytable4<-ggtexttable(buff_wma_corr_cl, 
                          theme = ttheme("mBlue",padding = unit(c(10, 3), "mm")),rows=NULL)
    ### Make plot #
    output$bufferplot2<-renderPlot({
      if(input$y == "tds"){
        pp<-ggplot(data = roadsalt_corr,aes_string(input$x,y=input$y))+
          geom_smooth(method = "lm", se = FALSE,formula=formula1,aes(color = WMA))+
          scale_color_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10",
                                        "11","12","13","14","15","16","17","18","19","20"))
        
        plot_grid(pp,mytable3)}
      else{
        pp<-ggplot(data = roadsalt_corr,aes_string(input$x,y=input$y))+
          geom_smooth(method = "lm", se = FALSE,formula=formula1,aes(color = WMA))+
          scale_color_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10",
                                        "11","12","13","14","15","16","17","18","19","20"))
        
        plot_grid(pp,mytable4)}
      
    })
###########################################################################################          
    ### This creates interactive map ###
  output$leaf<- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 7))%>%
      addTiles()%>%
      addResetMapButton()%>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = "Grey") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      setView(lng = -74.4 ,lat =40, zoom = 7)%>%
      addPolygons(data = good_imp_map_df,color = "#F3161B",weight = 1,smoothFactor = 1,
                  opacity = 0.5, fillOpacity = 2,
                  group = "Impaired HUCs",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE),
                  label = ~ paste(HUC14TXT,AU_name),
                  layerId = ~ good_imp_map_df$AU_name)%>%
    addPolygons(data= good_map_df,color = "#636060",weight = 1,smoothFactor = 1,
                opacity = 0.5, fillOpacity = 0.1,group = "HUCs",fillColor = "white",
                highlightOptions = highlightOptions(color = "blue",
                                                    weight = 2,bringToFront = TRUE),
                popup = paste("<h6> HUC Name:</h6>",NJ_Map_Road$AU_NAME,"\n",
                              "<h6> HUC14#:<h6/>\n",NJ_Map_Road$HUC14TXT,sep = ""))%>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Grey", "Satellite"),
        overlayGroups = c("HUCs","Impaired HUCs"),
        options = layersControlOptions(collapsed = FALSE))%>%
    addLegend("bottomright",colors = c("#636060","#F3161B"),opacity = 2,
              labels = c("HUC14s","Impaired HUCs"))
  })
###########################################################################################  
  ### Allows user to have map zoomed in when impaired HUC is clicked ###
  observe({
    click <- input$leaf_shape_click
    if(is.null(click))
      return()
    else
      leafletProxy("leaf")%>%
        setView(lng = click$lng , lat = click$lat, zoom=10)
 })
###########################################################################################  
### Creates table of impaired HUCs under leaflet map ###
  
  output$Table2<- DT::renderDataTable({
    datatable(leaflet_df,filter = "top")
  })
########################################################################################### 
### Creates data table ###
  output$Table1<- DT::renderDataTable({
    DT::datatable(roadsalt_data,filter = 'top',options = list(scrollX = TRUE))
  }
  
  )
###########################################################################################  
  ### Allows user to download data from data table page ###  
  output$downloadData<-downloadHandler(
    filename = function(){
      paste("dataset-",Sys.Date(),".tsv",sep="")
    },
    content = function(file){
      write_tsv(roadsalt_data,file)
    })
  
  ### Allows user to download plot from correlation plots tab ###
  output$downloadplot<- downloadHandler(
    filename = function(){
      paste("Plot",".png",sep = "")
    },
    content = function(file){
      ggsave(file,width =11.5 , height = 8,device = "png")
    }
  )
  }
###########################################################################################
### Creates a shiny app object ###
shinyApp(ui,server)
###########################################################################################

#rsconnect::setAccountInfo(name='kzolea695',
                            #token='DC7A93A56CEAB9776CFADE6C8F5E1367',
                            #secret='9DSUwsPxNtDEDx2BrHXG8lZ6MPZ6OukS8LM/UQeX')

#rsconnect::deployApp()

