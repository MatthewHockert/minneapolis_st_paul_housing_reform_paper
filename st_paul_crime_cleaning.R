library(sf)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(stringr)

crime <- st_read("/Users/matthewhockert/Desktop/Personal Info/st_paul_housing_crime/Crime_Incident_Report/Crime_Incident_Report.shp")
crime <- st_drop_geometry(crime)
crime$DATE <- as.Date(crime$DATE, format = "%Y-%m-%d")

crime <- crime %>%
  mutate(Month = format(DATE, "%Y-%m"),
         Year = format(DATE, "%Y"))

print(unique(crime$INCIDENT))


crime <- crime %>%
  mutate(
    Crime_Type = case_when(
      INCIDENT %in% c("Robbery", "Agg. Assault Dom.", "Agg. Assault", 
                      "Simple Assault Dom.", "Simple Asasult Dom.", 
                      "Discharge", "Rape", "Homicide") ~ "Violent",
      
      INCIDENT %in% c("Theft", "Auto Theft", "Vandalism", "Burglary", 
                      "Arson", "Graffiti", "Narcotics", "Criminal Damage", 
                      "Proactive Police Visit", "Community Engagement Event", 
                      "Proactive Foot Patrol", "Community Event", "THEFT", "Other") ~ "Non-Violent",
      
      INCIDENT == "0" ~ NA_character_,  # Mark bad data as missing
      
      TRUE ~ "Unclassified"  # Catch anything not explicitly listed
    )
  )

crime <- crime %>%
  filter(!is.na(Crime_Type) & Crime_Type != "Unclassified")


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

crime_summary <- crime %>%
  group_by(DATE,Crime_Type) %>%
  summarise(Total_Crimes = n(), .groups = "drop")

ggplot(crime_summary, aes(x = DATE, y = Total_Crimes, color = Crime_Type, group = Crime_Type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Violent vs. Non-Violent Crimes Over Time",
    x = "Year",
    y = "Total Crimes",
    color = "Crime Type"
  ) +
  geom_vline(xintercept = as.Date("2023-01-01"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.Date("2021-11-01"), linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

crime_nhood <- merge(crime,district_councils,by.x = "NEIGHBORHO",by.y="districtnu")


#### neighborhood ####
str(crime)
aggregated_crime_incident <- crime %>%
  group_by(NEIGHBORHO, Year, INCIDENT) %>%
  summarise(Count = n())

aggregated_crime_nhood <- crime %>%
  group_by(NEIGHBORHO, Year) %>%
  summarise(Count = n())%>%
  filter(!(is.na(Year)))%>%
  arrange(NEIGHBORHO, Year) %>%  # Ensure correct ordering
  mutate(
    Count_Lag = lag(Count),  # Previous year's count
    Percent_Change = (Count - Count_Lag) / Count_Lag * 100  # Percent change formula
  )

ggplot(aggregated_crime_nhood, aes(x = Year, y = Percent_Change, color = as.factor(NEIGHBORHO), group = NEIGHBORHO)) +
  geom_line() +
  geom_vline(xintercept = "2023", linetype = "dashed", color = "red") +   # Dashed line for 2023
  geom_vline(xintercept = "2022", linetype = "dashed", color = "black") + # Dashed line for 2022
  geom_vline(xintercept = "2020", linetype = "solid", color = "black") +  # Solid line for 2020
  labs(title = "Incident Counts Over Time",
       x = "Year",
       y = "Count of Incidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

aggregated_crime_nhood_poli <- crime %>%
  group_by(NEIGHBORHO,POLICE_GRI, Year) %>%
  summarise(Count = n())

filtered_data_nhood <- aggregated_data_nhood %>%
  filter(!is.na(Month))

# Convert Month to Date format and filter for dates from 2017 onwards
filtered_data_nhood <- filtered_data_nhood %>%
  mutate(Month = as.Date(paste0(Month, "-01"))) %>%
  filter(Month >= as.Date("2017-01-01"))

filtered_data_nhood_1 <- filtered_data_nhood %>%
  filter(INCIDENT %in% c("Auto Theft", "Burglary", "Narcotics", "Robbery"))%>%
  filter(NEIGHBOR_1 %in% c("12 - St. Anthony", "8 - Summit/University","15 - Highland"))
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

#### Police district ####

##### Year ----
aggregated_crime_incident <- crime %>%
  group_by(POLICE_GRI, Year, INCIDENT) %>%
  summarise(Count = n())

aggregated_crime_pd <- crime %>%
  group_by(POLICE_GRI, Year) %>%
  summarise(Count = n())%>%
  filter(!(is.na(Year)))%>%
  arrange(POLICE_GRI, Year) %>%  # Ensure correct ordering
  mutate(
    Count_Lag = lag(Count),  # Previous year's count
    Percent_Change = (Count - Count_Lag) / Count_Lag * 100  # Percent change formula
  )

aggregated_crime_pd_type <- crime %>%
  group_by(POLICE_GRI, Year, Crime_Type) %>%
  summarise(Count = n())%>%
  filter(!(is.na(Year)))%>%
  arrange(POLICE_GRI, Year) %>%  # Ensure correct ordering
  mutate(
    Count_Lag = lag(Count),  # Previous year's count
    Percent_Change = (Count - Count_Lag) / Count_Lag * 100  # Percent change formula
  )

ggplot(aggregated_crime_pd, aes(x = Year, y = Percent_Change, color = as.factor(POLICE_GRI), group = POLICE_GRI)) +
  geom_line() +
  geom_vline(xintercept = "2023", linetype = "dashed", color = "red") +   # Dashed line for 2023
  geom_vline(xintercept = "2022", linetype = "dashed", color = "black") + # Dashed line for 2022
  geom_vline(xintercept = "2020", linetype = "solid", color = "black") +  # Solid line for 2020
  labs(title = "Incident Counts Over Time",
       x = "Year",
       y = "Count of Incidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



aggregated_crime_nhood_poli <- crime %>%
  group_by(NEIGHBORHO,POLICE_GRI, Year) %>%
  summarise(Count = n())

##### Month ----
aggregated_crime_month_pd <- crime %>%
  group_by(POLICE_GRI, Month) %>%
  summarise(Count = n())%>%
  filter(!(is.na(Month)))%>%
  arrange(POLICE_GRI, Month) %>%  # Ensure correct ordering
  mutate(
    Count_Lag = lag(Count),  # Previous year's count
    Percent_Change = (Count - Count_Lag) / Count_Lag * 100  # Percent change formula
  )

aggregated_crime_month_pd <- aggregated_crime_month_pd %>%
  mutate(Month = ym(Month),  # Convert to Date format
         Year = year(Month)) 

ggplot(aggregated_crime_month_pd, aes(x = Month, y = Percent_Change, color = as.factor(POLICE_GRI), group = POLICE_GRI)) +
  geom_line() +
  geom_vline(xintercept = "2023", linetype = "dashed", color = "red") +   # Dashed line for 2023
  geom_vline(xintercept = "2022", linetype = "dashed", color = "black") + # Dashed line for 2022
  geom_vline(xintercept = "2020", linetype = "solid", color = "black") +  # Solid line for 2020
  labs(title = "Incident Counts Over Time",
       x = "Year",
       y = "Count of Incidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

aggregated_crime_month_pd_type <- crime %>%
  group_by(POLICE_GRI, Month,Crime_Type) %>%
  summarise(Count = n())%>%
  filter(!(is.na(Month)))%>%
  arrange(POLICE_GRI, Month) %>%  # Ensure correct ordering
  mutate(
    Count_Lag = lag(Count),  # Previous year's count
    Percent_Change = (Count - Count_Lag) / Count_Lag * 100  # Percent change formula
  )

aggregated_crime_month_pd_type <- aggregated_crime_month_pd_type %>%
  mutate(Month = ym(Month),  # Convert to Date format
         Year = year(Month)) 




##### Quarter ----
print(unique(crime$DATE))

aggregated_crime_quarter_pd <- crime %>%
  mutate(
    Quarter = floor_date(ymd(DATE), "quarter"),  # Extract quarter start date
    Year = year(Quarter)
  ) %>%
  group_by(POLICE_GRI, Quarter) %>%
  summarise(Count = n(), .groups = "drop") %>%
  filter(!is.na(Quarter)) %>%
  arrange(POLICE_GRI, Quarter) %>%  # Ensure correct ordering
  mutate(
    Count_Lag = lag(Count),  # Previous quarter's count
    Percent_Change = (Count - Count_Lag) / Count_Lag * 100  # Percent change formula
  )

aggregated_crime_quarter_pd <- aggregated_crime_quarter_pd %>%
  mutate(Year = year(Quarter)) 
names(aggregated_crime_quarter_pd)
# Step 2: Plot Percent Change Over Time by Quarter
ggplot(aggregated_crime_quarter_pd, 
       aes(x = Quarter, y = Percent_Change, color = as.factor(POLICE_GRI), group = POLICE_GRI)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2023-01-01"), linetype = "dashed", color = "red") +  # Dashed line for 2023
  geom_vline(xintercept = as.Date("2022-01-01"), linetype = "dashed", color = "black") + # Dashed line for 2022
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "solid", color = "black") +  # Solid line for 2020
  labs(title = "Incident Counts Over Time by Quarter",
       x = "Year",
       y = "Percent Change in Incidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


aggregated_crime_quarter_pd_type <- crime %>%
  mutate(
    Quarter = floor_date(ymd(DATE), "quarter"),  # Extract quarter start date
    Year = year(Quarter)
  ) %>%
  group_by(POLICE_GRI, Quarter,Crime_Type) %>%
  summarise(Count = n(), .groups = "drop") %>%
  filter(!is.na(Quarter)) %>%
  arrange(POLICE_GRI, Quarter) %>%  # Ensure correct ordering
  mutate(
    Count_Lag = lag(Count),  # Previous quarter's count
    Percent_Change = (Count - Count_Lag) / Count_Lag * 100  # Percent change formula
  )

aggregated_crime_quarter_pd_type <- aggregated_crime_quarter_pd_type %>%
  mutate(Year = year(Quarter))

#### Traffic stops ----

traffic_stops <- read.csv("/Users/matthewhockert/Desktop/Personal Info/minneapolis_st_paul_housing_reform_paper/Saint_Paul_Police_Department_Traffic_Stops.csv")
traffic_stops <- (subset(traffic_stops, !(is.na(POLICE_GRID_NUMBER))))
names(traffic_stops)
length(unique(traffic_stops$POLICE_GRID_NUMBER))

##### Year ----
traffic_stops_year <- traffic_stops %>%
  group_by(YEAR_OF_STOP, POLICE_GRID_NUMBER) %>%
  summarize(police_int = n_distinct(ObjectId), .groups = "drop")
traffic_stops_year

ggplot(traffic_stops_year, 
      aes(x = as.character(YEAR_OF_STOP), y = police_int, color = as.factor(POLICE_GRID_NUMBER), group = POLICE_GRID_NUMBER)) +
  geom_line() +
  geom_vline(xintercept = ("2023"), linetype = "dashed", color = "red") +  # Dashed line for 2023
  geom_vline(xintercept = ("2022"), linetype = "dashed", color = "black") + # Dashed line for 2022
  geom_vline(xintercept = ("2020"), linetype = "solid", color = "black") +  # Solid line for 2020
  labs(title = "Incident Counts Over Time by Quarter",
       x = "Year",
       y = "Percent Change in Incidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

names(rci_crime_year_pd)
test_merge <- merge(rci_crime_year_pd,traffic_stops_year,by.x = c("POLICE_GRI","Year"),by.y=c("POLICE_GRID_NUMBER","YEAR_OF_STOP"))
plot(test_merge$RCI,test_merge$counts)
hist(test_merge$counts)
plot(log(test_merge$counts),log(test_merge$Count))

length(unique(test_merge$POLICE_GRI))

summary(lm(log(Count)~log(counts)+as.factor(POLICE_GRI)+as.factor(Year),test_merge))

##### Month ----

traffic_stops_month <- traffic_stops %>%
  mutate(
    MONTH_OF_STOP = str_trim(MONTH_OF_STOP),  # Remove any leading/trailing spaces
    MONTH_OF_STOP = match(toupper(MONTH_OF_STOP), toupper(month.name)),  # Ensure proper matching
    YEAR_OF_STOP = as.numeric(YEAR_OF_STOP)  # Ensure year is numeric  # Ensure year is numeric
  ) %>%
  filter(!is.na(MONTH_OF_STOP) & !is.na(YEAR_OF_STOP)) %>%  # Remove rows with missing values
  group_by(YEAR_OF_STOP, MONTH_OF_STOP, POLICE_GRID_NUMBER) %>%
  summarize(police_int = n_distinct(ObjectId), .groups = "drop") %>%
  mutate(
    Month = as.Date(sprintf("%04d-%02d-01", YEAR_OF_STOP, MONTH_OF_STOP)),  # Proper date format
    POLICE_GRI = POLICE_GRID_NUMBER  # Rename to match the other dataset
  ) %>%
  select(Month, POLICE_GRI, police_int)  # Keep only necessary columns

sum(traffic_stops_month$police_int==0)
# Merge with rci_crime_month_pd
ggplot(traffic_stops_month, 
       aes(x = (Month), y = police_int, color = as.factor(POLICE_GRI), group = POLICE_GRI)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2023-01-01"), linetype = "dashed", color = "red") +  # Dashed line for 2023
  geom_vline(xintercept = as.Date("2022-01-01"), linetype = "dashed", color = "black") + # Dashed line for 2022
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "solid", color = "black") +  # Solid line for 2020
  labs(title = "Incident Counts Over Time by Quarter",
       x = "Year",
       y = "Percent Change in Incidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

names(rci_crime_month_pd)
merged_data <- merge(rci_crime_month_pd, traffic_stops_month, by = c("Month", "POLICE_GRI"))
plot(merged_data$RCI,merged_data$counts)
hist(merged_data$counts)
plot(log(merged_data$counts),log(merged_data$Count))

length(unique(merged_data$POLICE_GRI))

summary(lm(log(Count)~log(counts)+as.factor(POLICE_GRI)+as.factor(Year),merged_data))

str(rci_crime_month_pd)

##### Quarter----
# Aggregate traffic stops by quarter
traffic_stops_quarter <- traffic_stops %>%
  mutate(
    MONTH_OF_STOP = str_trim(MONTH_OF_STOP),
    MONTH_NUM = match(toupper(MONTH_OF_STOP), toupper(month.name)),
    Year = as.numeric(YEAR_OF_STOP),
    Quarter = quarter(make_date(YEAR_OF_STOP, MONTH_NUM, 1), with_year = TRUE)
  ) %>%
  filter(!is.na(MONTH_NUM), !is.na(Year)) %>%
  group_by(Quarter,Year, POLICE_GRID_NUMBER) %>%
  summarize(police_int = n_distinct(ObjectId), .groups = "drop") %>%
  mutate(
    Quarter = yq(Quarter),
    POLICE_GRI = POLICE_GRID_NUMBER
  ) %>%
  select(Quarter,Year, POLICE_GRI, police_int)

# Check for zeros
sum(traffic_stops_quarter$police_int == 0)

# Merge quarterly data
names(rci_crime_quarter_pd)

merged_data <- merge(rci_crime_quarter_pd, traffic_stops_quarter,
                     by.x = c("Quarter", "POLICE_GRI"),
                     by.y = c("Quarter", "POLICE_GRI"))

# Visualization by Quarter
ggplot(traffic_stops_quarter, 
       aes(x = Quarter, y = police_int, color = as.factor(POLICE_GRI), group = POLICE_GRI)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2023-01-01"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.Date("2022-01-01"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "solid", color = "black") +
  labs(title = "Incident Counts Over Time by Quarter",
       x = "Year",
       y = "Incident Counts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
names(merged_data)
# Exploratory plots
plot(merged_data$RCI, merged_data$counts)
hist(merged_data$counts)
plot(log(merged_data$police_int), log(merged_data$Count))

# Unique POLICE_GRI counts
length(unique(merged_data$POLICE_GRI))

# Regression summary
summary(lm(log(Count) ~ log(police_int) + as.factor(POLICE_GRI) + as.factor(Year), data = merged_data))



#
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





#### Batch geocode ####

library(parallel)
library(dplyr)
library(sf)
library(censusxy)

geocode_batch <- function(data) {
  tryCatch({
    geocoded <- cxy_geocode(data, street = "BLOCK", city = "city", state = "state", class = "sf")
    geocoded
  }, error = function(e) {
    warning("Geocoding failed for a batch.")
    data$geometry <- NA
    st_as_sf(data)
  })
}

dates <- unique(crime$DATE)
crime_split <- split(crime, crime$DATE)

cl <- makeCluster(detectCores() - 3) 
clusterExport(cl, c("geocode_batch", "crime_split", "cxy_geocode"))
clusterEvalQ(cl, library(censusxy))
clusterEvalQ(cl, library(sf))

crime_geocoded <- parLapply(cl, crime_split, geocode_batch)
stopCluster(cl)

crime_sf <- bind_rows(crime_geocoded)

crime_sf <- st_as_sf(crime_sf)

print(nrow(crime_sf))
beep()



