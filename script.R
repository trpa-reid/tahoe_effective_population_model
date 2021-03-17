library(shiny)
library(shinydashboard)
library(shinyBS)
library(sf)
library(data.table)
library(lubridate)
library(scales)
library(tidyverse)
library(leaflet)
library(shinycssloaders)
library(DT)
library(plotly)
library(shinyWidgets)
library(mapview)
library(tidycensus)
library(geojsonio)
options(scipen=999)
census_api_key("680398dff0a2f4c566f10c95888da7f25e55147b")
options(tigris_use_cache = TRUE)
# streetlight trips

entry<- read_csv("150140_2018_od_grid/150140_2018_od_grid_od_trip_all.csv",
                 col_types = list( "Origin Zone Name" = col_character())) %>%
  mutate(trip_type=case_when(`Origin Zone Is Pass-Through`=="no" & `Destination Zone Is Pass-Through` == "no"~ "II",
                             `Origin Zone Is Pass-Through`=="yes" & `Destination Zone Is Pass-Through` == "no"~ "EI",
                             `Origin Zone Is Pass-Through`=="no" & `Destination Zone Is Pass-Through` == "yes"~ "IE",
                             `Origin Zone Is Pass-Through`=="yes" & `Destination Zone Is Pass-Through` == "yes"~ "EE")) %>%
  filter(trip_type %in% c("EI","IE"))

#bind_rows(
#entry %>% filter(`Origin Zone Name` %in% c("Spooner Summit","Mount Rose","Luther Pass","Kingsbury Grade","Echo Summit","Brockway Summit","89 - Tahoe City")) %>% group_by(`Origin Zone Name`) %>%
 # summarise(count=sum(`Average Daily O-D Traffic (StL Volume)`)),
#entry %>% filter(`Destination Zone Name` %in% c("Spooner Summit","Mount Rose","Luther Pass","Kingsbury Grade","Echo Summit","Brockway Summit","89 - Tahoe City")) %>% group_by(`Destination Zone Name`) %>%
 # summarise(count=sum(`Average Daily O-D Traffic (StL Volume)`)) %>% rename(`Origin Zone Name`=`Destination Zone Name`)) %>%
  #group_by(`Origin Zone Name`) %>%
  #summarise(count=sum(count))

data18<- read_csv("150448_2018_od_grid_trav/150448_2018_od_grid_trav_od_traveler_all.csv")
HBW_entry <- data18 %>%
  filter(`Day Type`=="0: All Days (M-Su)" & `Day Part`=="0: All Day (12am-12am)") %>% 
  mutate(`Average Daily O-D Traffic (StL Volume)` = as.numeric(`Average Daily O-D Traffic (StL Volume)`), 
         `Purpose HBW (percent)`=as.numeric(na_if(`Purpose HBW (percent)`, "N/A" )),
         `Purpose HBO (percent)`=as.numeric(na_if(`Purpose HBO (percent)`, "N/A" )),
         `Purpose NHB (percent)`=as.numeric(na_if(`Purpose NHB (percent)`, "N/A" )),
    trip_type=case_when(`Origin Zone Is Pass-Through`=="no" & `Destination Zone Is Pass-Through` == "no"~ "II",
                        `Origin Zone Is Pass-Through`=="yes" & `Destination Zone Is Pass-Through` == "no"~ "EI",
                        `Origin Zone Is Pass-Through`=="no" & `Destination Zone Is Pass-Through` == "yes"~ "IE",
                        `Origin Zone Is Pass-Through`=="yes" & `Destination Zone Is Pass-Through` == "yes"~ "EE")) %>%
filter(trip_type %in% c("EI")) %>%
  mutate(HBW_Entry_Trips = `Average Daily O-D Traffic (StL Volume)` * `Purpose HBW (percent)`,
         HBO_Entry = `Average Daily O-D Traffic (StL Volume)` * `Purpose HBO (percent)`,
         NHB_Entry = `Average Daily O-D Traffic (StL Volume)` * `Purpose NHB (percent)`)

db18<-data18 %>%
  filter(`Day Type`=="0: All Days (M-Su)" & `Day Part`=="0: All Day (12am-12am)") %>% 
  mutate(#`Avg All Trip Length (mi)`=na_if(`Avg All Trip Length (mi)`, "N/A" ),
         `Average Daily O-D Traffic (StL Volume)` = as.numeric(`Average Daily O-D Traffic (StL Volume)`), 
         #`Avg All Trip Length (mi)`=as.numeric(`Avg All Trip Length (mi)`),
        # vmt=`Average Daily O-D Traffic (StL Volume)` * `Avg All Trip Length (mi)` ,
         trip_type=case_when(`Origin Zone Is Pass-Through`=="no" & `Destination Zone Is Pass-Through` == "no"~ "II",
                             `Origin Zone Is Pass-Through`=="yes" & `Destination Zone Is Pass-Through` == "no"~ "EI",
                             `Origin Zone Is Pass-Through`=="no" & `Destination Zone Is Pass-Through` == "yes"~ "IE",
                             `Origin Zone Is Pass-Through`=="yes" & `Destination Zone Is Pass-Through` == "yes"~ "EE")) %>%
  group_by(trip_type) %>%
  summarise(volume=sum(`Average Daily O-D Traffic (StL Volume)`))


## seasonal units
tract <- geojson_read("https://opendata.arcgis.com/datasets/85a2e8e4bf994742a5855c1339517681_3.geojson", what="sp") %>%
  st_as_sf() %>%
  st_transform(crs=4326)
nv_acs_18 <- get_acs(geography = "tract", year=2018, 
                     variables =  c("B25004_006"),
                     state = "NV", county=c("Washoe", "Douglas")) %>%
  mutate(data_source="2018 ACS 5-year Estimate")
ca_acs_18 <- get_acs(geography = "tract", year=2018, 
                     variables =  c("B25004_006"),
                     state = "CA",county=c("El Dorado", "Placer")) %>%
  mutate(data_source="2018 ACS 5-year Estimate")
seas_units<- bind_rows(ca_acs_18, nv_acs_18) %>%
  left_join(data.frame(tract), by="GEOID") %>%
  filter(!is.na(STATEFP)) %>%
  group_by(variable) %>% summarise(total=sum(estimate), moe=sum(moe)) %>%
  ungroup() %>%
  pull(total) - 6005 # subtract out the VHRs from the census seasonal/rec/occasional units
boundary<- geojson_read("https://opendata.arcgis.com/datasets/85a2e8e4bf994742a5855c1339517681_0.geojson", what="sp") %>% st_as_sf(crs=4326)


ui <- dashboardPage(skin="black",
  dashboardHeader(title="Tahoe Effective Population Model (TEPM)", titleWidth = 450),
  dashboardSidebar(width=300,
                   sidebarMenu(
                     menuItem("Model Tool",
                              tabName = "tool",icon = icon("chart-area")),
                     menuItem("Model Background Info",
                              tabName = "info",icon = icon("info"))),
                   box(width=12,background="black",
                       img(src='zoom_background_50th.jpg',  height = 150, width = 250),
                       p(tags$br(),"This tool demonstrates the parameters of the Tahoe Effective Population Model and allows the user to experiment with different quantitative assumptions to observe the impact on the estimated effective population of the Tahoe region. For more information on the model methodology, click on the background info tab above. This dashboard is under development is not currently an official source of TRPA data.")
                       )
                   ),
  dashboardBody(# tags$img(
  #  src = 'tahoe.jpg',
   ### style = 'position: absolute',
   # height=900,
   # width=1200
 #),
    tags$style(HTML("
      .box.box-solid.box-primary>.box-header { color:#fff;background:#057A73}
      .skin-black .main-sidebar { background-color: #000000;}
      .small-box.bg-yellow { background-color: #057A73 !important; color: #fff !important; }")),
    tabItems(
      tabItem(tabName = "tool",
    fluidRow(
      box(width=4, title="Select Model Assumptions",status = 'primary', solidHeader = TRUE,
      column(width=6,
                 numericInput("EI_vol_input","Annual Average Daily Entry Traffic Volumes", value=db18 %>% filter(trip_type=="EI") %>% pull(volume)),
    bsTooltip("EI_vol_input", "The number of vehicles that enter the Tahoe region on an annual average day","right", options = list(container = "body")),
    numericInput("res_input","Full Time Resident Population", value=51577),
    bsTooltip("res_input", "Data sourced from US Census American Community Survey (ACS)","right", options = list(container = "body")),
    numericInput("commuter_input","Number of Non-Resident Commuters", value=4949),
    bsTooltip("commuter_input", "The annual average number of total non-resident commuters","right", options = list(container = "body")),
    numericInput("resident_commute_input","Number of Resident Commuters", value=1800),
    bsTooltip("resident_commute_input", "The annual average number of total resident commuters","right", options = list(container = "body")),
    uiOutput("ui_freight"),
    bsTooltip("ui_freight", "The average number of freight entry trips","right", options = list(container = "body")),
    uiOutput("ui_discretionary"),
    bsTooltip("ui_discretionary", "These trips include residents and overnight visitors leaving the basin for shopping or recreation, such as trips to Costco or ski trips to Kirkwood.","right", options = list(container = "body")),
    numericInput("veh_occ_input_day","Day Visitor Vehicle Occupancy", value=2.62),
    bsTooltip("veh_occ_input_day", "The average number of people per day visitor vehicle","right", options = list(container = "body")),
    numericInput("seas_input","Number of Second Homes", value=seas_units),
    bsTooltip("seas_input", "Data sourced from US Census American Community Survey (ACS)","right", options = list(container = "body")),
    numericInput("second_home_occ_input","Second Home Occupancy", value=.19),
    bsTooltip("second_home_occ_input", "The annual average occupancy of second homes","right", options = list(container = "body")),
    numericInput("veh_occ_input_sec","Second Home Vehicle Occupancy", value=1.97),
    bsTooltip("veh_occ_input_sec", "The average number of people per second home visitor vehicle","right", options = list(container = "body")),
    numericInput("veh_per_room_input_sec","Number of Vehicles Per Second Home", value=1.27),
    bsTooltip("veh_per_room_input_sec", "The average number of vehicles per unit","right", options = list(container = "body")),
    numericInput("veh_occ_input_str","STR Visitor Vehicle Occupancy", value=3.92),
    bsTooltip("veh_occ_input_str", "The average number of people per overnight visitor vehicle","right", options = list(container = "body"))
    ),
    column(width=6,
               numericInput("vhr_rooms_rented_input","Number of Annual STR Units Rented", value=482940),
               bsTooltip("vhr_rooms_rented_input", "The total number annual rooms rented for STRs","right", options = list(container = "body")),
               numericInput("veh_per_room_input_str","Number of Vehicles Per STR Rented", value=2.07),
               bsTooltip("veh_per_room_input_str", "The average number of vehicles per STR rented","right", options = list(container = "body")),
           numericInput("leng_stay_input_str","Average Length of STR Overnight Visitor Stay", value=4.28),
           bsTooltip("leng_stay_input_str", "The average number of nights rented by STR overnight visitors","right", options = list(container = "body")),
               numericInput("hot_rooms_rented_input","Number of Annual Hotel/Resort/Casino Units Rented", value=1754130),
               bsTooltip("hot_rooms_rented_input", "The total number annual rooms rented for Hotel/Resort/Casino ","right", options = list(container = "body")),
               numericInput("veh_per_room_input_hot","Number of Vehicles Per Hotel/Motel/Resort/Casino Room Rented", value=1.08),
               bsTooltip("veh_per_room_input_hot", "The average number of vehicles per room rented", "right",options = list(container = "body")),
               numericInput("leng_stay_input_hot","Average Length of Hotel/Motel/Resort/Casino Stay", value=3.44),
               bsTooltip("leng_stay_input_hot", "The average number of nights rented by overnight visitor","right", options = list(container = "body")),
               numericInput("veh_occ_input_hot","Overnight Visitor Vehicle Occupancy", value=3.26),
               bsTooltip("veh_occ_input_ov_hot", "The average number of people per STR visitor vehicle","right", options = list(container = "body")),
                numericInput("camp_rented_input","Number of Camp Sites Rented", value=99284),
                bsTooltip("camp_rented_input", "The total number of annual camp sites rented","right", options = list(container = "body")),
           numericInput("veh_per_room_input_camp","Vehicles Per Camp Site", value=1.18),
           bsTooltip("veh_per_room_input_camp", "The total number of annual camp sites rented","right", options = list(container = "body")),
           numericInput("veh_occ_input_camp","Campground Vehicle Occupancy", value=3.26),
           bsTooltip("veh_occ_input_camp", "The total number of annual camp sites rented","right", options = list(container = "body")),
           numericInput("leng_stay_camp","Campsite length of stay", value=3.64),
           bsTooltip("leng_stay_camp", "The total number of annual camp sites rented","right", options = list(container = "body"))
    )
    ),column(width=7,
             box(title="Model Results",status = 'primary', solidHeader = TRUE, width=12,
         fluidRow(column( width=6,
         box(title="Effective Population", width=9, valueBoxOutput("eff_pop_output", width=12) %>% withSpinner(),
             bsTooltip("eff_pop_output", "Data is calculated by summing the sub-populations listed below.","right", options = list(container = "body")))),
             column(width=6,box(width = 12, tags$p("This tool was created with",tags$a(href="https://shiny.rstudio.com/", "R-Shiny")), 
                 tags$p("The full model methodology (code) can be found on",tags$a(href="https://github.com/trpa-reid/tahoe_effective_population_model/blob/main/script.R", "Github")),
                 img(src='50yrs_TRPA.jpg',  height = 70, width = 150)
             ))
        # box(title="Entry Volume Control", width=4, valueBoxOutput("range_output_no", width=12) %>% withSpinner(),
           #  bsTooltip("range_output_no", "Data is calculated...","right", options = list(container = "body")))
         ),
         fluidRow(
         box(title="Full Time Residents", width=4, valueBoxOutput("full_time_res_output", width=12) %>% withSpinner(),
             bsTooltip("full_time_res_output", "Data sourced directly from 2018 American Community Survey (ACS)","right", options = list(container = "body"))),
         box(title="Commuters", width=4, valueBoxOutput("commuter_output", width=12) %>% withSpinner(),
             bsTooltip("commuter_output", "Data sourced directly from 2018 LEHD/CTPP","right", options = list(container = "body"))),
         box(title="Day Visitors", width=4, valueBoxOutput("day_vis_output", width=12) %>% withSpinner(),
             bsTooltip("day_vis_output", "Data is calculated from entry station traffic volumes and vehicle occupancy.","left", options = list(container = "body"))),
         box(title="Overnight Visitors", width=4, valueBoxOutput("on_vis_output", width=12) %>% withSpinner(),
             bsTooltip("on_vis_output", "Data is calculated from TOT Occupancy Reporting, average length of stay, vehicle occupancy, and average number of vehicles per room","right", options = list(container = "body"))),
         box(title="Second Home Howners", width=4, valueBoxOutput("seas_vis_output", width=12) %>% withSpinner(),
             bsTooltip("seas_vis_output", "Data is calculated from ACS (Census) occupancy, available VHR data, vehicle occupancy, average length of stay, and average number of vehicles per home","right", options = list(container = "body")))
         ),
        fluidRow(
  ))))
         ),
  tabItem(tabName = "info",
          box(width = 7, status = 'primary', solidHeader = TRUE, title = "Background Information",
              tags$iframe(style="height:800px; width:100%", src="TEPM_Documentation.pdf")
          ),
          box(width = 5, status = 'primary', solidHeader = TRUE, title = "Effective Population Boundary",
              leafletOutput("boundary_map", height=800)))
    )
    )
)
server <- function(input, output) { 
output$ui_discretionary  <- renderUI({
    numericInput("discr_trips_input","Number of Discretionary Entry Trips", value=round(round(on_vis_pop(),0) * .07) + 5750 )
  })
output$ui_freight  <- renderUI({
  numericInput("freight_input","Number of Freight Entry Trips", value=round(db18 %>% filter(trip_type=="EI") %>% pull(volume) * 0.03,0))
})
  output$boundary_map<-renderLeaflet({
    boundary %>% leaflet() %>% addPolygons(color="#EC9D15") %>% addProviderTiles("Stamen.TonerLite")
  })
  output$full_time_res_output<-renderValueBox({
    valueBox(value=format(input$res_input, big.mark = ",", digits=0), subtitle="", color="navy")
  })
  output$commuter_output<-renderValueBox({
    valueBox(value=format(input$commuter_input, big.mark = ",", digits=0), subtitle="", color="navy")
  })
  output$eff_pop_output<-renderValueBox({
    valueBox(value=format(input$res_input + # resident pop
               input$commuter_input + #commuter pop
                 ((input$EI_vol_input - # total entry htrips
                     input$freight_input - # freight trips
                     input$discr_trips_input - # discretionary trips
                     input$commuter_input - # external worker commuter trips
                     input$resident_commute_input - # resident commute trips
                     ((((input$hot_rooms_rented_input)/365) * input$veh_per_room_input_hot) / input$leng_stay_input_hot) - # hotel motel visitors entering region
                     ((((input$vhr_rooms_rented_input)/365) * input$veh_per_room_input_str) / input$leng_stay_input_str) - # vhr visitors entering the region
                     (((input$seas_input * input$second_home_occ_input)* input$veh_per_room_input_sec) / input$leng_stay_input_str)- #second home owners entering region
                     ((((input$camp_rented_input)/365) * input$veh_per_room_input_camp) / input$leng_stay_camp) # campers entering the region
                 ) * input$veh_occ_input_day) + # day visitor pop
                 ((((input$hot_rooms_rented_input)/365) * input$veh_per_room_input_hot) * input$veh_occ_input_hot) + # hotel/motel visitors staying in region
                 (((((input$hot_rooms_rented_input)/365) * input$veh_per_room_input_hot) / input$leng_stay_input_hot) * input$veh_occ_input_hot)+ # hotelmotel visitors entering
                 ((((input$vhr_rooms_rented_input )/365) * input$veh_per_room_input_str) * input$veh_occ_input_str) + # vhr visitors staying in region
                 (((((input$vhr_rooms_rented_input)/365) * input$veh_per_room_input_str) / input$leng_stay_input_str) * input$veh_occ_input_str)+ # vhr visitors entering
                 ((((input$camp_rented_input )/365) * input$veh_per_room_input_camp) * input$veh_occ_input_camp) + # camp visitors staying in region
                 (((((input$camp_rented_input)/365) * input$veh_per_room_input_camp) / input$leng_stay_camp) * input$veh_occ_input_camp) + # camp entering
                 ((input$seas_input * input$second_home_occ_input) * input$veh_occ_input_sec) + # second home owners staying
                 ((((input$seas_input * input$second_home_occ_input)* input$veh_per_room_input_sec) / input$leng_stay_input_str) *input$veh_occ_input_sec), # second home owners entering
             big.mark = ",", digits=0),
             subtitle="", color="red")
  })
  output$day_vis_output<-renderValueBox({
    valueBox(value=format(
               (input$EI_vol_input - # total entry htrips
                  input$freight_input - # freight trips
                  input$discr_trips_input - # discretionary trips
                  input$commuter_input - # external worker commuter trips
                  input$resident_commute_input - # resident commute trips
                  ((((input$hot_rooms_rented_input)/365) * input$veh_per_room_input_hot) / input$leng_stay_input_hot) - # hotel motel visitors entering region
                  ((((input$vhr_rooms_rented_input)/365) * input$veh_per_room_input_str) / input$leng_stay_input_str) - # vhr visitors entering the region
                  (((input$seas_input * input$second_home_occ_input)* input$veh_per_room_input_sec) / input$leng_stay_input_str)- #second home owners entering region
                  ((((input$camp_rented_input)/365) * input$veh_per_room_input_camp) / input$leng_stay_camp) # campers entering the region
                ) * input$veh_occ_input_day, # multiple by vehicle occupacy
               big.mark = ",", digits=0), 
             subtitle="", color="navy")
  })
on_vis_pop<-  reactive({
  ((((input$hot_rooms_rented_input)/365) * input$veh_per_room_input_hot) * input$veh_occ_input_hot) + # hotel/motel visitors staying in region
    (((((input$hot_rooms_rented_input)/365) * input$veh_per_room_input_hot) / input$leng_stay_input_hot) * input$veh_occ_input_hot)+ # hotelmotel visitors entering
    ((((input$vhr_rooms_rented_input )/365) * input$veh_per_room_input_str) * input$veh_occ_input_str) + # vhr visitors staying in region
    (((((input$vhr_rooms_rented_input)/365) * input$veh_per_room_input_str) / input$leng_stay_input_str) * input$veh_occ_input_str)+ # vhr visitors entering
    ((((input$camp_rented_input )/365) * input$veh_per_room_input_camp) * input$veh_occ_input_camp) + # camp visitors staying in region
    (((((input$camp_rented_input)/365) * input$veh_per_room_input_camp) / input$leng_stay_camp) * input$veh_occ_input_camp)  # camp visitors entering
})
  output$on_vis_output<-renderValueBox({
    valueBox(value=format(on_vis_pop(),
      big.mark = ",", digits=0), 
      subtitle="", color="navy")
  })
  output$seas_vis_output<-renderValueBox({
    valueBox(value=format(
      ((input$seas_input * input$second_home_occ_input) * input$veh_occ_input_sec) + # second home owners staying
        ((((input$seas_input * input$second_home_occ_input)* input$veh_per_room_input_sec) / input$leng_stay_input_str) *input$veh_occ_input_sec), # second home owners entering
      big.mark = ",", digits=0), 
      subtitle="", color="navy")
  })
  }
shinyApp(ui, server)


