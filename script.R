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

# freight


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
    numericInput("commuter_input","Number of Non-Resident Commuters", value=2400),
    bsTooltip("commuter_input", "The annual average number of total non-resident commuters","right", options = list(container = "body")),
    numericInput("resident_commute_input","Number of Resident Commuters", value=1300),
    bsTooltip("resident_commute_input", "The annual average number of total resident commuters","right", options = list(container = "body")),
    numericInput("freight_input","Number of Freight Entry Trips", value=1750),
    bsTooltip("freight_input", "The average number of freight entry trips","right", options = list(container = "body")),
    numericInput("discr_trips_input","Number of Discretionary Entry Trips", value=6500),
    bsTooltip("discr_trips_input", "These trips include residents and overnight visitors leaving the basin for shopping or recreation, such as trips to Costco or ski trips to Kirkwood.","right", options = list(container = "body")),
    numericInput("seas_input","Number of Second Homes", value=seas_units),
    bsTooltip("seas_input", "Data sourced from US Census American Community Survey (ACS)","right", options = list(container = "body")),
    numericInput("second_home_occ_input","Second Home Occupancy", value=.25),
    bsTooltip("second_home_occ_input", "The annual average occupancy of second homes","right", options = list(container = "body")),
    numericInput("veh_occ_input_day","Day Visitor Vehicle Occupancy", value=2.62),
    bsTooltip("veh_occ_input_day", "The average number of people per day visitor vehicle","right", options = list(container = "body")),
    numericInput("veh_occ_input_hot","Overnight Visitor Vehicle Occupancy", value=3.26),
    bsTooltip("veh_occ_input_ov_hot", "The average number of people per STR visitor vehicle","right", options = list(container = "body"))),
    column(width=6,
           numericInput("veh_occ_input_str","STR Visitor Vehicle Occupancy", value=3.92),
           bsTooltip("veh_occ_input_str", "The average number of people per overnight visitor vehicle","right", options = list(container = "body")),
           numericInput("veh_occ_input_sec","Second Home Vehicle Occupancy", value=1.97),
           bsTooltip("veh_occ_input_sec", "The average number of people per second home visitor vehicle","right", options = list(container = "body")),
           numericInput("hot_rooms_rented_input","Number of Annual Hotel/Resort/Casino Units Rented", value=1750392),
           bsTooltip("hot_rooms_rented_input", "The total number annual rooms rented for Hotel/Resort/Casino ","right", options = list(container = "body")),
           numericInput("vhr_rooms_rented_input","Number of Annual STR Units Rented", value=509564),
           bsTooltip("vhr_rooms_rented_input", "The total number annual rooms rented for STRs","right", options = list(container = "body")),
           numericInput("veh_per_room_input_hot","Number of Vehicles Per Hotel/Motel/Resort/Casino Room Rented", value=1.8),
           bsTooltip("veh_per_room_input_hot", "The average number of vehicles per room rented", "right",options = list(container = "body")),
           numericInput("veh_per_room_input_sec","Number of Vehicles Per Second Home", value=1.8),
           bsTooltip("veh_per_room_input_sec", "The average number of vehicles per unit","right", options = list(container = "body")),
    numericInput("veh_per_room_input_str","Number of Vehicles Per STR Rented", value=2),
    bsTooltip("veh_per_room_input_str", "The average number of vehicles per STR rented","right", options = list(container = "body")),
    numericInput("leng_stay_input","Average Length of Overnight Visitor Stay", value=3.9),
    bsTooltip("leng_stay_input", "The average number of nights rented by overnight visitor","right", options = list(container = "body")))
    ),column(width=8,
             box(title="Model Results",status = 'primary', solidHeader = TRUE, width=12,
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
             bsTooltip("day_vis_output", "Data is calculated from entry station traffic volumes and vehicle occupancy.","left", options = list(container = "body"))),
         box(title="Overnight Visitors", width=4, valueBoxOutput("on_vis_output", width=12) %>% withSpinner(),
             bsTooltip("on_vis_output", "Data is calculated from TOT Occupancy Reporting, average length of stay, vehicle occupancy, and average number of vehicles per room","right", options = list(container = "body"))),
         box(title="Second Home Howners", width=4, valueBoxOutput("seas_vis_output", width=12) %>% withSpinner(),
             bsTooltip("seas_vis_output", "Data is calculated from ACS (Census) occupancy, available VHR data, vehicle occupancy, average length of stay, and average number of vehicles per home","right", options = list(container = "body")))
         ),
        fluidRow(box(width = 5, tags$p("This tool was created with",tags$a(href="https://shiny.rstudio.com/", "R-Shiny")), 
                     tags$p("The full model methodology (code) can be found on",tags$a(href="https://github.com/trpa-reid/tahoe_effective_population_model/blob/main/script.R", "Github")),
                     img(src='50yrs_TRPA.jpg',  height = 70, width = 150)
                     )
  ))))
         ),
  tabItem(tabName = "info",
          box(width = 6, status = 'primary', solidHeader = TRUE, title = "Background Information",
              h5(strong("Vehicle Occupancy"), style = "color:#057A73"),
              tags$ul(
                tags$li(p("Data was sourced from the average of the last two TRPA travel surveys (summer 2018 & winter 2020), which demonstrated a vehicle occupancy of 2.62 for day visitors, 1.59 for residents, 3.24 for overnight visitors, and 1.97 for seasonal residents.")),
                tags$li(
                  tags$p("For more information, visit ", 
                         tags$a(href="https://monitoring.laketahoeinfo.org/TravelBehavior", "LT-Info Monitoring")))
              ),
              h5(strong("Entry Vehicle Volumes"), style = "color:#057A73"),
              tags$ul(
                tags$li(p("Data was sourced from the StreetLight Data Insights platform, using an O-D analysis for an average annual day in 2018."))
              ),
              h5(strong("Resident Population"), style = "color:#057A73"),
              tags$ul(
                tags$li(p("Data was sourced from the 2018 US Census American Community Survey (ACS) via the Census API using",tags$a(href="https://walker-data.com/tidycensus/" ,"tidycensus.")))
              ),
              h5(strong("Commuters"), style = "color:#057A73"),
              tags$ul(
                tags$li(p("Data was estimated from several different datasets. StreetLight Data was used to estimate the number of Home-Based-Work trips at ~ 3,000. CTPP data showed then number of external commuters at ~2,000. The National Household Travel Survey (NHTS) estimated that commute trips comprised 15% of total entry trips, which equates to ~3,700 commuters"))
              ),
              h5(strong("Freight Trips"), style = "color:#057A73"),
              tags$ul(
                tags$li(p("Explanation of freight trips"))
              ),
              h5(strong("Discretionary Trips"), style = "color:#057A73"),
              tags$ul(
                tags$li(p("National Household Travel Survey (NHTS) indicate that 23% of entry trips are resident discretionary, which equates to roughly 5,750 resident discretionary entry trips. The number of overnight visitor discretionary trips was estimated to be 750 on an average day (need source) to total 6,500 disretionary entry trips. For the 2018 model day, the number of overnight visitor day trips was 2,352"))
              ),
              h5(strong("Second Homes"), style = "color:#057A73"),
              tags$ul(
                tags$li(p("Data for the number of second home was sourced from the US Census American Community Survey (ACS) in conjunction with Short Term Rental (STR) information reported from local jurisdictions. Second home occupancy was estimated using observed occupancy of STRs in conjunction of survey data of second home occupancy. TRPA travel survey data from 2018 and 2020 reported that second homes were occupied for 29% of the year."))
              ),
              h5(strong("Hotel/Hotel/Casino Rooms Rented"), style = "color:#057A73"),
              tags$ul(
                tags$li(p("Data was sourced from TOT reporting statistics from local jurisdictions."))
              ),
              h5(strong("Vehicles Per Unit"), style = "color:#057A73"),
              tags$ul(
                tags$li(p("Data was sourced from..."))
              ),
              h5(strong("Length of Stay"), style = "color:#057A73"),
              tags$ul(
                tags$li(p("Data was sourced from the TRPA 2018 and 2020 travel surveys. The surveys showed an average length of stay of 4.28 days for STRs and 3.44 days for hotel/motel/resort."))
              )
          ),
          box(width = 6, status = 'primary', solidHeader = TRUE, title = "Effective Population Boundary",
              leafletOutput("boundary_map", height=800)))
    )
    )
)
server <- function(input, output) { 
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
                     input$discr_trips_input - # discretionary trips
                     input$freight_input - # freight trips
                     input$commuter_input - # external worker commuter trips
                     input$resident_commute_input - # resident comute trips
                     ((((input$hot_rooms_rented_input)/365) * input$veh_per_room_input_hot) / input$leng_stay_input) - # hotel motel visitors entering region
                     ((((input$vhr_rooms_rented_input)/365) * input$veh_per_room_input_str) / input$leng_stay_input) - 
                     (((input$seas_input * input$second_home_occ_input)* input$veh_per_room_input_sec) / input$leng_stay_input) #second home owners entering region
                 ) * input$veh_occ_input_day) + # day visitor pop
                 ((((input$hot_rooms_rented_input)/365) * input$veh_per_room_input_hot) * input$veh_occ_input_hot) + # hotel/motel visitors staying in region
                 (((((input$hot_rooms_rented_input)/365) * input$veh_per_room_input_hot) / input$leng_stay_input) * input$veh_occ_input_hot)+ # hotelmotel visitors entering
                 ((((input$vhr_rooms_rented_input)/365) * input$veh_per_room_input_str) * input$veh_occ_input_str) + # vhr visitors staying in region
                 (((((input$vhr_rooms_rented_input)/365) * input$veh_per_room_input_str) / input$leng_stay_input) * input$veh_occ_input_str) + # overnight vis entering pop
                 ((input$seas_input * input$second_home_occ_input) * input$veh_occ_input_sec) + # second home owners staying
                 ((((input$seas_input * input$second_home_occ_input)* input$veh_per_room_input_sec) / input$leng_stay_input) *input$veh_occ_input_sec), # second home owners entering
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
                  ((((input$hot_rooms_rented_input)/365) * input$veh_per_room_input_hot) / input$leng_stay_input) - # hotel motel visitors entering region
                  ((((input$vhr_rooms_rented_input)/365) * input$veh_per_room_input_str) / input$leng_stay_input) - 
                  (((input$seas_input * input$second_home_occ_input)* input$veh_per_room_input_sec) / input$leng_stay_input) #second home owners entering region
                ) * input$veh_occ_input_day, # multiple by vehicle occupacy
               big.mark = ",", digits=0), 
             subtitle="", color="navy")
  })
  output$on_vis_output<-renderValueBox({
    valueBox(value=format(
      ((((input$hot_rooms_rented_input)/365) * input$veh_per_room_input_hot) * input$veh_occ_input_hot) + # hotel/motel visitors staying in region
        (((((input$hot_rooms_rented_input)/365) * input$veh_per_room_input_hot) / input$leng_stay_input) * input$veh_occ_input_hot)+ # hotelmotel visitors entering
        ((((input$vhr_rooms_rented_input )/365) * input$veh_per_room_input_str) * input$veh_occ_input_str) + # vhr visitors staying in region
        (((((input$vhr_rooms_rented_input)/365) * input$veh_per_room_input_str) / input$leng_stay_input) * input$veh_occ_input_str) , # vhr visitors entering
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


