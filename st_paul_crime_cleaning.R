library(sf)
library(dplyr)
library(lubridate)
library(ggplot2)


crime <- st_read("/Users/matthewhockert/Desktop/Personal Info/st_paul_housing_crime/Crime_Incident_Report/Crime_Incident_Report.shp")

crime$DATE <- as.Date(crime$DATE, format = "%Y-%m-%d")

crime <- crime %>%
  mutate(Month = format(DATE, "%Y-%m"))

aggregated_data <- crime %>%
  group_by(INCIDENT, Month) %>%
  summarise(Count = n())

filtered_data <- aggregated_data %>%
  filter(as.Date(paste0(Month, "-01")) >= as.Date("2017-01-01"))

filtered_data <- filtered_data %>%
  filter(INCIDENT %in% c("Auto Theft", "Burglary", "Narcotics", "Robbery"))
filtered_data$Month <- as.Date(paste0(filtered_data$Month, "-01"))

ggplot(filtered_data, aes(x = Month, y = Count, color = INCIDENT, group = INCIDENT)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2023-01-01"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.Date("2021-11-01"), linetype = "dashed", color = "black") +
  labs(title = "Incident Counts Over Time",
       x = "Month",
       y = "Count of Incidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### neighborhood ####

aggregated_data_nhood <- crime %>%
  group_by(INCIDENT, Month, NEIGHBOR_1) %>%
  summarise(Count = n())

filtered_data_nhood <- aggregated_data_nhood %>%
  filter(!is.na(Month))

# Convert Month to Date format and filter for dates from 2017 onwards
filtered_data_nhood <- filtered_data_nhood %>%
  mutate(Month = as.Date(paste0(Month, "-01"))) %>%
  filter(Month >= as.Date("2017-01-01"))

filtered_data_nhood_1 <- filtered_data_nhood %>%
  filter(INCIDENT %in% c("Auto Theft", "Burglary", "Narcotics", "Robbery"))%>%
  filter(NEIGHBOR_1 %in% c("12 - St. Anthony", "10 - Como","11 - Hamline/Midway", "7 - Thomas/Dale(Frogtown)","15 - Highland"))
filtered_data_nhood_1$Month <- as.Date(paste0(filtered_data_nhood_1$Month, "-01"))

ggplot(filtered_data_nhood_1, aes(x = Month, y = Count, color = NEIGHBOR_1, group = NEIGHBOR_1)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2023-01-01"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.Date("2021-11-01"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2020-3-01"), linetype = "solid", color = "black") +
  labs(title = "Incident Counts Over Time",
       x = "Month",
       y = "Count of Incidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ INCIDENT, scales = "free_y")

#[1] "2 - Greater East Side"           "4 - Dayton's Bluff"              "13 - Union Park"                
#[4] "3 - West Side"                   "11 - Hamline/Midway"             "5 - Payne/Phalen"               
#[7] "8 - Summit/University"           "6 - North End"                   "14 - Macalester-Groveland"      
#[10] "15 - Highland"                   "16 - Summit Hill"                "1 - Conway/Battlecreek/Highwood"
#[13] "10 - Como"                       "9 - West Seventh"                "7 - Thomas/Dale(Frogtown)"      
#[16] "17 - Capitol River"              "12 - St. Anthony" 

#### geolocate ####

# DOUBLE CHECK FOR FAILED DEPOTS
remotes::install_github("chris-prener/censusxy")
library(censusxy)
crime$city <- "Saint Paul"
crime$state <- "MN"


samp <- crime[sample(1:nrow(crime), 1), ]
sampxy <- cxy_single(street = samp$`BLOCK`,
                    city = samp$`city`, 
                    state = samp$`state`)
library(leaflet)
leaflet() %>%
  addTiles() %>% 
  addCircleMarkers(
    data = sampxy, 
    lng = ~coordinates.x, 
    lat = ~coordinates.y, 
    col = 'red', 
    fillColor = 'red'
  )

crime_sf <- data.frame()
dates <- unique(crime$DATE)
for (date in dates){
  print(as.Date(date))
  sub = subset(crime, DATE == date)
  print(nrow(crime))
  print(nrow(sub))
  crime_sf_sub <- cxy_geocode(sub, street = "BLOCK",
                          city = "city", 
                          state = "state", 
                          class = "sf")
  print("binding")
  crime_sf_sub <- st_drop_geometry(crime_sf_sub)
  sub <- st_drop_geometry(sub)
  
  dropped_rows <- subset(sub, !(CASE_NUMBE %in% crime_sf_sub$CASE_NUMBE))
  if (nrow(dropped_rows) > 0) {
    cat("Number of dropped rows:", nrow(dropped_rows), "\n")
    
    # Assign NA geometry for dropped rows
    dropped_rows$geometry <- NA
  }
  
  # Combine geocoded and dropped rows
  all_rows <- bind_rows(crime_sf_sub, dropped_rows)
  
  # Bind to the final data frame
  crime_sf <- bind_rows(crime_sf, all_rows)
  print(nrow(crime_sf))
}

beep()


sub_x <- subset(crime, DATE == "2014-08-15")
crime_sf <- cxy_geocode(sub_x, street = "BLOCK",
                     city = "city",
                     state = "state",
                     class = "sf",
                     output = "full")
beep()

crime_sf_x <- subset(crime_sf,!(sub_x$CASE_NUMBE %in% crime_sf$CASE_NUMBE))


mapview::mapview(crime_sf, color = "blue", layer.name = "Geocoded Data") +
  mapview::mapview(crime_sf_x, color = "red", layer.name = "Unmatched Data")





#### Minneapolis permits ####

#minneapolis_permits <- st_read("/Users/matthewhockert/Downloads/shp_econ_residential_building_permts-2/ResidentialPermits.shp")









