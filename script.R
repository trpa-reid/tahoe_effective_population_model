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

sum(HBW_entry$HBW_Entry_Trips,na.rm=T)

sum(HBW_entry$HBO_Entry,na.rm=T)

sum(HBW_entry$NHB_Entry,na.rm=T)

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

## entry station counts - 2018
data1818<- read_csv("150140_2018_od_grid/150140_2018_od_grid_od_trip_all.csv",col_types = cols(.default = "c"))
vol<-bind_rows(
data1818 %>%
  #filter(`Day Type`=="0: All Days (M-Su)" & `Day Part`=="0: All Day (12am-12am)") %>% 
  mutate(`Average Daily O-D Traffic (StL Volume)` = as.numeric(`Average Daily O-D Traffic (StL Volume)`)) %>%
  filter(`Origin Zone Is Pass-Through`=="yes") %>%
  group_by(`Origin Zone Name`) %>%
  summarise(vol=sum(`Average Daily O-D Traffic (StL Volume)`)) %>%
  rename(name=`Origin Zone Name`),
data1818 %>%
  #filter(`Day Type`=="0: All Days (M-Su)" & `Day Part`=="0: All Day (12am-12am)") %>% 
  mutate(`Average Daily O-D Traffic (StL Volume)` = as.numeric(`Average Daily O-D Traffic (StL Volume)`)) %>%
  filter(`Origin Zone Is Pass-Through`=="yes") %>%
  group_by(`Origin Zone Name`) %>%
  summarise(vol=sum(`Average Daily O-D Traffic (StL Volume)`))  %>%
  rename(name=`Origin Zone Name`)
) %>% group_by(name) %>%
  summarise(`Annual Average Daily Traffic Volumes`=comma(sum(vol))) %>%
  mutate(Source="StreetLight") %>%
  rename(`Entry/Exit Station`=name) %>%
  formattable::formattable()



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
  pull(total) - 6005 # subtract out the VHRs from the census seasonal/rec/occasional units


ui <- dashboardPage(skin="black",
  dashboardHeader(title="Tahoe Effective Population Model (TEPM)", titleWidth = 450),
  dashboardSidebar(width=300,
                   sidebarMenu(
                     menuItem("Model Tool",
                              tabName = "tool",icon = icon("bar-chart")),
                     menuItem("Model Background Info",
                              tabName = "info",icon = icon("map-marker"))),
                   box(width=12,background="black",p("This tool demonstrates the parameters of the Tahoe Effective Population Model and allows the user to experiment with different quantitative assumptions to observe the impact on the estimated effective population of the Tahoe region. For more information on the model methodology, click on the background info tab above. This dashboard is under development is not currently an official source of TRPA data. The full model methodology (code) can be found on github."))),
  dashboardBody(
    tags$style(HTML("
      .box.box-solid.box-primary>.box-header { color:#fff;background:#057A73}
      .skin-black .main-sidebar { background-color: #000000;}
      .small-box.bg-yellow { background-color: #057A73 !important; color: #fff !important; }")),
    tabItems(
      tabItem(tabName = "tool",
    fluidRow(
      box(width=4, title="Select Model Assumptions",
      column(width=6,
    numericInput("EI_vol_input","Annual Average Daily Entry Traffic Volumes", value=db18 %>% filter(trip_type=="EI") %>% pull(volume)),
    bsTooltip("EI_vol_input", "Data sourced from 2018 StreetLight Data O-D Analysis","right", options = list(container = "body")),
    numericInput("veh_occ_input_day","Day Visitor Vehicle Occupancy", value=2.62),
    bsTooltip("veh_occ_input_day", "Data sourced from TRPA Summer/Winter Travel Survey (2018 & 2020)","right", options = list(container = "body")),
    numericInput("veh_occ_input_ov","Overnight Visitor Vehicle Occupancy", value=3.24),
    bsTooltip("veh_occ_input_ov", "Data sourced from TRPA Summer/Winter Travel Survey (2018 & 2020)","right", options = list(container = "body")),
    numericInput("veh_occ_input_sec","Second Home Vehicle Occupancy", value=1.97),
    bsTooltip("veh_occ_input_sec", "Data sourced from TRPA Summer/Winter Travel Survey (2018 & 2020)","right", options = list(container = "body")),
    numericInput("rooms_rented_input","Number of Annual Hotel/Motel/STR Units Rented", value=2117833),
    bsTooltip("rooms_rented_input", "Data sourced from 2018 county and city TOT reporting","right", options = list(container = "body")),
    numericInput("veh_per_room_input_hot","Number of Vehicles Per Hotel/Motel/Resort/Casino Room Rented", value=1.8),
    bsTooltip("veh_per_room_input_hot", "Data sourced from...","right", options = list(container = "body")),
    numericInput("veh_per_room_input_sec","Number of Vehicles Per Second Home", value=1.8),
    bsTooltip("veh_per_room_input_sec", "Data sourced from...","right", options = list(container = "body"))),
    column(width=6,
    numericInput("veh_per_room_input_str","Number of Vehicles Per STR Room Rented", value=2.6),
    bsTooltip("veh_per_room_input_str", "Data sourced from...","right", options = list(container = "body")),
    numericInput("leng_stay_input","Average Length of Overnight Visitor Stay", value=3.9),
    bsTooltip("leng_stay_input", "Data sourced from...","right", options = list(container = "body")),
    numericInput("discr_trips_input","Number of Discretionary Entry Trips", value=6500),
    bsTooltip("discr_trips_input", "Data sourced from...","right", options = list(container = "body")),
    numericInput("seas_input","Number of Second Homes", value=seas_units),
    bsTooltip("seas_input", "Data sourced from US Census American Community Survey (ACS)","right", options = list(container = "body")),
    numericInput("second_home_occ_input","Second Home Occupancy", value=.25),
    bsTooltip("second_home_occ_input", "Data sourced from...","right", options = list(container = "body")),
    numericInput("res_input","Full Time Resident Population", value=51577),
    bsTooltip("res_input", "Data sourced from US Census American Community Survey (ACS)","right", options = list(container = "body")),
    numericInput("commuter_input","Number of Non-Resident Commuters", value=2400),
    bsTooltip("commuter_input", "Data sourced from CTPP","right", options = list(container = "body")),
    numericInput("resident_commute_input","Number of Resident Commuters", value=1300),
    bsTooltip("resident_commute_input", "Data sourced from...","right", options = list(container = "body")))
    ),column(width=8,
         fluidRow(
         box(title="Effective Population", width=4, valueBoxOutput("eff_pop_output", width=12) %>% withSpinner(),
             bsTooltip("eff_pop_output", "Data is calculated by summing the sub-populations listed below.","right", options = list(container = "body")))
        # box(title="Entry Volume Control", width=4, valueBoxOutput("range_output_no", width=12) %>% withSpinner(),
           #  bsTooltip("range_output_no", "Data is calculated...","right", options = list(container = "body")))
         ),
         fluidRow(
         box(title="Full Time Residents", width=4, valueBoxOutput("full_time_res_output", width=12) %>% withSpinner(),
             bsTooltip("full_time_res_output", "Data sourced directly from 2018 American Community Survey (ACS)","right", options = list(container = "body"))),
         box(title="Commuters", width=4, valueBoxOutput("commuter_output", width=12) %>% withSpinner(),
             bsTooltip("commuter_output", "Data sourced directly from 2018 LEHD/CTPP","right", options = list(container = "body"))),
         box(title="Day Visitors", width=4, valueBoxOutput("day_vis_output", width=12) %>% withSpinner(),
             bsTooltip("day_vis_output", "Data is calculated from entry station traffic volumes and vehicle occupancy.","right", options = list(container = "body"))),
         box(title="Overnight Visitors", width=4, valueBoxOutput("on_vis_output", width=12) %>% withSpinner(),
             bsTooltip("on_vis_output", "Data is calculated from TOT Occupancy Reporting, average length of stay, vehicle occupancy, and average number of vehicles per room","right", options = list(container = "body"))),
         box(title="Second Home Howners", width=4, valueBoxOutput("seas_vis_output", width=12) %>% withSpinner(),
             bsTooltip("seas_vis_output", "Data is calculated from ACS (Census) occupancy, available VHR data, vehicle occupancy, average length of stay, and average number of vehicles per home","right", options = list(container = "body")))
         )
    )
  )
         ),
  tabItem(tabName = "info",
          box(width = 6, status = 'primary', solidHeader = TRUE, title = "Background Information",
              h5(strong("Vehicle Occupancy"), style = "color:#057A73"),
              tags$ul(
                tags$li(p("Data was sourced from the last two TRPA travel surveys (summer 2018 & winter 2020).The winter 2020 survey showed a vehicle occupancy of 2.1 and the summer 2018 survey showed 2.7")),
                tags$li(
                  tags$p("For more information, visit ", 
                         tags$a(href="https://monitoring.laketahoeinfo.org/TravelBehavior", "LT-Info Monitoring")))
              ),
              h5(strong("Explore Model Outputs"), style = "color:#057A73"),
              tags$ul(
                tags$li(p("This tab allows the user to interact with and explore the model outputs in many different ways."))
              ),
              h5(strong("Model Inputs"), style = "color:#057A73"),
              tags$ul(
                tags$li(p("You can review the zonal input files that are used to generate model outputs. The data includes information such as the number of residential units, occupancy rates of hotels, and employment"))
              ),
              h5(strong("Provide Feedback"), style = "color:#057A73"),
              tags$ul(
                tags$li(p("Through this application you can submit feedback and comments to help TRPA improve our transportation modeling program."))
              )
          ))
    )
    )
)
server <- function(input, output) { 
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
                     input$discr_trips_input - # discretionary trips
                     input$commuter_input - # external worker commuter trips
                     input$resident_commute_input - # resident comute trips
                     ((((input$rooms_rented_input * .8)/365) * input$veh_per_room_input_hot) / input$leng_stay_input) - # hotel motel visitors entering region
                     ((((input$rooms_rented_input * .2)/365) * input$veh_per_room_input_str) / input$leng_stay_input) - 
                     (((input$seas_input * input$second_home_occ_input)* input$veh_per_room_input_sec) / input$leng_stay_input) #second home owners entering region
                 ) * input$veh_occ_input_day) + # day visitor pop
                 ((((input$rooms_rented_input* .8)/365) * input$veh_per_room_input_hot) * input$veh_occ_input_ov) + # hotel/motel visitors staying in region
                 (((((input$rooms_rented_input * .8)/365) * input$veh_per_room_input_hot) / input$leng_stay_input) * input$veh_occ_input_ov)+ # hotelmotel visitors entering
                 ((((input$rooms_rented_input * .2)/365) * input$veh_per_room_input_str) * input$veh_occ_input_ov) + # vhr visitors staying in region
                 (((((input$rooms_rented_input * .2)/365) * input$veh_per_room_input_str) / input$leng_stay_input) * input$veh_occ_input_ov) + # overnight vis entering pop
                 ((input$seas_input * input$second_home_occ_input) * input$veh_occ_input_sec) + # second home owners staying
                 ((((input$seas_input * input$second_home_occ_input)* input$veh_per_room_input_sec) / input$leng_stay_input) *input$veh_occ_input_sec), # second home owners entering
             big.mark = ",", digits=0),
             subtitle="", color="green")
  })
  output$day_vis_output<-renderValueBox({
    valueBox(value=format(
               (input$EI_vol_input - # total entry htrips
                  input$discr_trips_input - # discretionary trips
                  input$commuter_input - # external worker commuter trips
                  input$resident_commute_input - # resident comute trips
                  ((((input$rooms_rented_input * .8)/365) * input$veh_per_room_input_hot) / input$leng_stay_input) - # hotel motel visitors entering region
                  ((((input$rooms_rented_input * .2)/365) * input$veh_per_room_input_str) / input$leng_stay_input) - 
                  (((input$seas_input * input$second_home_occ_input)* input$veh_per_room_input_sec) / input$leng_stay_input) #second home owners entering region
                ) * input$veh_occ_input_day, # multiple by vehicle occupacy
               big.mark = ",", digits=0), 
             subtitle="", color="navy")
  })
  output$on_vis_output<-renderValueBox({
    valueBox(value=format(
      ((((input$rooms_rented_input* .8)/365) * input$veh_per_room_input_hot) * input$veh_occ_input_ov) + # hotel/motel visitors staying in region
        (((((input$rooms_rented_input * .8)/365) * input$veh_per_room_input_hot) / input$leng_stay_input) * input$veh_occ_input_ov)+ # hotelmotel visitors entering
        ((((input$rooms_rented_input * .2)/365) * input$veh_per_room_input_str) * input$veh_occ_input_ov) + # vhr visitors staying in region
        (((((input$rooms_rented_input * .2)/365) * input$veh_per_room_input_str) / input$leng_stay_input) * input$veh_occ_input_ov) , # vhr visitors entering
      big.mark = ",", digits=0), 
      subtitle="", color="navy")
  })
  output$seas_vis_output<-renderValueBox({
    valueBox(value=format(
      ((input$seas_input * input$second_home_occ_input) * input$veh_occ_input_sec) + # second home owners staying
        ((((input$seas_input * input$second_home_occ_input)* input$veh_per_room_input_sec) / input$leng_stay_input) *input$veh_occ_input_sec), # second home owners entering
      big.mark = ",", digits=0), 
      subtitle="", color="navy")
  })
  }
shinyApp(ui, server)

