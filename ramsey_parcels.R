
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(beepr)
# Define the range of years
years <- 2014:2022

# Define the base file path and layer name pattern
base_file_path <- "/Users/matthewhockert/Desktop/HCRA/shp_plan_regonal_parcels_"
layer_name_pattern <- "Parcels%YEAR%Ramsey"

# Define column names to keep
column_names_to_keep <- c("COUNTY_ID", "PIN", "CITY","CTU_NAME", "ZIP", "USE1_DESC", "DWELL_TYPE", 
                          "ACRES_POLY", "NUM_UNITS", "EMV_LAND", "EMV_BLDG", "EMV_TOTAL", 
                          "TOTAL_TAX", "TAX_EXEMPT", "YEAR_BUILT", "OWNER_NAME", "HOMESTEAD", 
                          "geometry")

# Function to generate file path and layer name based on year
get_file_path <- function(year) {
  paste0(base_file_path, year)
}

get_layer_name <- function(year) {
  sub("%YEAR%", year, layer_name_pattern)
}

# Function to load and process each shapefile
process_shapefile <- function(file_path, layer, column_names,year) {
  df <- st_read(file_path, layer = layer)
  df$year <- year
  column_names <- c(column_names, "year")
  column_names <- column_names[column_names %in% names(df)]
  subset(df, select = column_names)
}

# Load, process, and combine all data frames
processed_data_frames <- lapply(years, function(year) {
  file_path <- get_file_path(year)
  layer_name <- get_layer_name(year)
  process_shapefile(file_path, layer_name, column_names_to_keep,year)
})

all_columns <- unique(unlist(lapply(processed_data_frames, names)))

# Function to ensure all data frames have the same columns
ensure_columns <- function(df, all_columns) {
  for (col in all_columns) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  df <- subset(df, select = all_columns)
  return(df)
}

# Process all data frames to ensure they have the same columns
processed_data_frames <- lapply(processed_data_frames, ensure_columns, all_columns = all_columns)

# Subset the processed data frames to keep only the desired columns
# subset_data_frames <- lapply(processed_data_frames, function(df) {
#   subset(df, select = column_names_to_keep)
# })

# Combine all processed data frames into one
ramsey_data <- do.call(rbind, processed_data_frames)
rm(processed_data_frames)
rm(subset_data_frames)
ramsey_data <- ramsey_data[grep("-\\d+$", ramsey_data$PIN),]
ramsey_data$PIN_2 <- sub("^[^-]*-", "", ramsey_data$PIN)
#combined_data_frame$PIN_2_num <- as.numeric(combined_data_frame$PIN_2)
ramsey_data <- ramsey_data %>%
  mutate(
    CITY_MERGED = ifelse(is.na(CITY) | CITY == "", CTU_NAME, CITY)
  )


beep()




# 
year <- 2022  # Replace with your desired year
base_file_path <- "/Users/matthewhockert/Desktop/HCRA/shp_plan_regonal_parcels_"
layer_name_pattern <- "Parcels2022Ramsey"

# Construct the file path and layer name
file_path <- paste0(base_file_path, year)
layer_name <- gsub("2022", year, layer_name_pattern)

ramsey_data_15 <- st_read(dsn = file_path, layer = layer_name)
names(ramsey_data_15)

# years <- 2015:2022
# base_file_path <- "/Users/matthewhockert/Desktop/HCRA/shp_plan_regonal_parcels_"
# 
# # Initialize an empty list to store data for each year
# ramsey_data_list <- list()
# 
# # Loop through each year
# for (year in years) {
#   # Construct the file path and layer name
#   file_path <- paste0(base_file_path, year)
#   layer_name <- gsub("2015", year, "Parcels2015Ramsey")
#   
#   # Read the shapefile for the current year
#   ramsey_data <- st_read(dsn = file_path, layer = layer_name)
#   
#   # Add a Year column to the data
#   ramsey_data$Year <- year
#   
#   # Store the data in the list
#   ramsey_data_list[[as.character(year)]] <- ramsey_data
# }
# 
# # Combine all years into a single data frame
# combined_ramsey_data <- do.call(rbind, ramsey_data_list)

st_paul <- subset(ramsey_data, CITY_MERGED %in% c("Saint Paul"))
#plot(st_paul$geometry)
validity_check <- st_is_valid(st_paul)
st_paul <- st_paul[validity_check, ]

st_paul <- st_paul %>%
  mutate(geometry = st_centroid(geometry))

print(unique(ramsey_data$year))

crime_nhood <- st_as_sf(crime_nhood)
st_paul <- st_transform(st_paul,4326)

st_paul_nhood<- st_intersection(st_paul,district_councils)

parcel_counts_by_year <- list()
# Unique years in the dataset
years <- unique(st_paul$year)
# Loop over years
for (yr in years) {
  print(yr)
  print(nrow(st_paul))
  
  # Filter data for the current year
  yearly_data <- st_paul %>%
    filter(year == yr)
  print(nrow(yearly_data))
  district_parcel_counts<- st_intersection(district_councils)
  # Store the result in the list
  parcel_counts_by_year[[as.character(yr)]] <- district_parcel_counts
}

# Combine all years into a single data frame
st_paul_nhood2 <- bind_rows(parcel_counts_by_year)
beep()
#012823210345

# rental_parcels <- st_paul_nhood %>%
#   mutate(
#     is_rental = ifelse(
#       NUM_UNITS > 1 & USE1_DESC %in% c("Res 2-3 units", "Apt 4+ units"), 1, 0
#     )
#   )

# st_paul_nhood <- st_paul_nhood %>%
#   arrange(PIN_2, year) %>% 
#   group_by(PIN_2) %>%
#   fill(DWELL_TYPE, .direction = "up") %>% 
#   fill(USE1_DESC, .direction = "down") %>% 
#   ungroup()
# beep()

current_year <- 2023
exemption_year_threshold <- current_year - 20  # Properties built after this year are exempt

unique_geometries <- st_paul_nhood %>%
  group_by(year) %>%
  distinct(geometry, .keep_all = TRUE) %>%  # Keep only unique geometries within each year
  ungroup()

rental_parcels <- unique_geometries %>%
  mutate(
    is_rental = ifelse(
      # Pre-2017 dwelling types
      DWELL_TYPE %in% c(
        "DOUBLE DWELLING", "TWO FAMILY DWELLING", "THREE FAMILY DWELLING",
        "APARTMENTS 1-9 UNITS", "APARTMENTS 10-19 UNITS", 
        "APARTMENTS 20-49 UNITS", "APARTMENTS 50 OR MORE UNITS",
        # Post-2017 dwelling types
        "APARTMENTS 20-49 RENTAL UNITS", "APT OR COMPLEX 100+ UNITS",
        "APARTMENTS 50-99 RENTAL UNITS", "APARTMENTS 7-19 RENTAL UNITS",
        "APARTMENTS 4-6 RENTAL UNITS", "ASSISTED LIVING APT COMPLEX",
        "TWO FAMILY DWELLING - SIDE/SI", "TWO FAMILY DWELLING - UP/DWN"
      ) & 
        YEAR_BUILT <= exemption_year_threshold,  # Exclude exempt properties
      1,
      0
    )
  )

#
#### Overlapping Parcels ####
data_2017 <- st_paul_nhood %>% filter(year == 2017)
data_2018 <- st_paul_nhood %>% filter(year == 2018)

# Identify new PIN_2 values in 2018
new_pins_in_2018 <- setdiff(data_2018$PIN_2, data_2017$PIN_2)

# Count the new PIN_2 values
new_pins_count <- length(new_pins_in_2018)

# View the results
cat("Number of new PIN_2 values in 2018:", new_pins_count, "\n")

# Optionally, list the new PIN_2 values
print(new_pins_in_2018)
data_2018_sub <- subset(data_2018,PIN_2 %in% new_pins_in_2018)
print(unique(data_2018_sub$DWELL_TYPE))
print(unique(data_2018_sub$districtnu))

plot(st_geometry(data_2018_sub), main = "New Parcels in 2018")

overlapping_parcels <- st_join(
  data_2017 %>% select(PIN_2, geometry),
  data_2018 %>% select(PIN_2, geometry),
  join = st_equals
)

# Count overlapping parcels
overlapping_count <- overlapping_parcels %>%
  filter(!is.na(PIN_2.y)) %>% # Ensure a match exists in 2018
  nrow()

cat("Number of overlapping parcels between 2017 and 2018:", overlapping_count, "\n")



#
#### Create parent parcels ####

mismatched_pins <- overlapping_parcels %>%
  filter(!is.na(PIN_2.y) & PIN_2.x != PIN_2.y) %>% # Filter mismatched PINs
  select(PIN_2.x, PIN_2.y)

# View mismatched PINs
print(mismatched_pins)

pin_year_counts <- st_paul_nhood %>%
  group_by(PIN_2) %>%
  summarise(year_count = n_distinct(year)) %>%
  ungroup()

pin_mapping <- st_drop_geometry(pin_mapping)

mismatched_pins_with_counts <- mismatched_pins %>%
  left_join(pin_year_counts, by = c("PIN_2.y" = "PIN_2")) %>%
  rename(year_count_y = year_count) %>%
  left_join(pin_year_counts, by = c("PIN_2.x" = "PIN_2")) %>%
  rename(year_count_x = year_count)
beep()

pin_mapping <- mismatched_pins_with_counts %>%
  mutate(
    parent_pin = ifelse(year_count_x >= year_count_y, PIN_2.x, PIN_2.y),
    child_pin = ifelse(year_count_x >= year_count_y, PIN_2.y, PIN_2.x)
  ) %>%
  select(parent_pin, child_pin) %>%
  distinct()

st_paul_nhood <- st_paul_nhood %>%
  left_join(pin_mapping, by = c("PIN_2" = "child_pin")) %>%
  mutate(
    parent_pin = ifelse(!is.na(parent_pin), parent_pin, PIN_2)  # Assign parent_pin or keep original PIN_2
  )



table(st_paul_nhood$parent_pin, st_paul_nhood$year)
pin_mapping_duplicates <- pin_mapping %>%
  group_by(child_pin) %>%
  summarise(count = n()) %>%
  filter(count > 1)


#####

# Check the number of rental vs non-rental parcels
rental_by_year <- rental_parcels %>%
  group_by(year) %>%
  summarise(rentals = sum(is_rental,na.rm = T),
            total_parcels = n(),
            rci =(rentals/total_parcels)*100, .groups = 'drop')

# Print the results
print(rental_by_year)

# st_paul
st_paul_by_year <- st_paul %>%
  group_by(year) %>%
  summarise(total_parcels = n(),.groups = 'drop')
print(st_paul_by_year)

rci_data <- rental_parcels %>%
  group_by(districtnu,year) %>% 
  summarise(
    total_parcels = n(),
    rental_parcels = sum(is_rental, na.rm = TRUE),
    RCI = (rental_parcels / total_parcels)*100
  )
rci_data <- st_drop_geometry(rci_data)

ggplot(rci_data, aes(x = year, y = RCI, color = as.factor(districtnu), group = districtnu)) +
  geom_line(size = 1) +
  labs(
    title = "Rent Control Intensity (RCI) by Neighborhood",
    x = "Year",
    y = "RCI (%)",
    color = "Neighborhood"
  ) +
  theme_minimal()

#
##### 2 ######

current_year <- 2023
exemption_year_threshold <- current_year - 20  # Properties built after this year are exempt

rental_parcels2 <- st_paul_nhood2 %>%
  mutate(
    is_rental = ifelse(
      # Pre-2017 dwelling types
      DWELL_TYPE %in% c(
        "DOUBLE DWELLING", "TWO FAMILY DWELLING", "THREE FAMILY DWELLING",
        "APARTMENTS 1-9 UNITS", "APARTMENTS 10-19 UNITS", 
        "APARTMENTS 20-49 UNITS", "APARTMENTS 50 OR MORE UNITS",
        # Post-2017 dwelling types
        "APARTMENTS 20-49 RENTAL UNITS", "APT OR COMPLEX 100+ UNITS",
        "APARTMENTS 50-99 RENTAL UNITS", "APARTMENTS 7-19 RENTAL UNITS",
        "APARTMENTS 4-6 RENTAL UNITS", "ASSISTED LIVING APT COMPLEX",
        "TWO FAMILY DWELLING - SIDE/SI", "TWO FAMILY DWELLING - UP/DWN"
      ) & 
        YEAR_BUILT <= exemption_year_threshold,  # Exclude exempt properties
      1,
      0
    )
  )
# Check the number of rental vs non-rental parcels
rental_by_year2 <- rental_parcels2 %>%
  group_by(year) %>%
  summarise(rentals = sum(is_rental,na.rm = T),
            total_parcels = n(),.groups = 'drop')

# Print the results
print(rental_by_year2)

rci_data2 <- rental_parcels2 %>%
  group_by(districtnu,year) %>% 
  summarise(
    total_parcels = n(),
    rental_parcels = sum(is_rental, na.rm = TRUE),
    RCI = (rental_parcels / total_parcels)*100
  )
rci_data2 <- st_drop_geometry(rci_data2)

ggplot(rci_data2, aes(x = year, y = RCI, color = as.factor(districtnu), group = districtnu)) +
  geom_line(size = 1) +
  labs(
    title = "Rent Control Intensity (RCI) by Neighborhood",
    x = "Year",
    y = "RCI (%)",
    color = "Neighborhood"
  ) +
  theme_minimal()


#####
##### dwelling changes by district#####
dwelling_distribution <- st_paul_nhood %>%
  filter(districtnu == 8 & year %in% c(2017, 2018)) %>%
  group_by(year, DWELL_TYPE) %>%
  summarise(
    count = n(),
    .groups = 'drop'  # To avoid grouped output
  )

ggplot(dwelling_distribution, aes(x = DWELL_TYPE, y = count, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal()

dwelling_mapping <- c(
  # Pre-2017 types
  "DOUBLE DWELLING" = "TWO FAMILY DWELLING",
  "TWO FAMILY DWELLING" = "TWO FAMILY DWELLING",
  "THREE FAMILY DWELLING" = "THREE FAMILY DWELLING",
  "APARTMENTS 1-9 UNITS" = "APARTMENTS <10 UNITS",
  "APARTMENTS 10-19 UNITS" = "APARTMENTS 10-19 UNITS",
  "APARTMENTS 20-49 UNITS" = "APARTMENTS 20-49 UNITS",
  "APARTMENTS 50 OR MORE UNITS" = "APARTMENTS 50+ UNITS",
  
  # Post-2017 types
  "APARTMENTS 20-49 RENTAL UNITS" = "APARTMENTS 20-49 UNITS",
  "APT OR COMPLEX 100+ UNITS" = "APARTMENTS 50+ UNITS",
  "APARTMENTS 50-99 RENTAL UNITS" = "APARTMENTS 50+ UNITS",
  "APARTMENTS 7-19 RENTAL UNITS" = "APARTMENTS 10-19 UNITS",
  "APARTMENTS 4-6 RENTAL UNITS" = "APARTMENTS <10 UNITS",
  "ASSISTED LIVING APT COMPLEX" = "ASSISTED LIVING",
  "TWO FAMILY DWELLING - SIDE/SI" = "TWO FAMILY DWELLING",
  "TWO FAMILY DWELLING - UP/DWN" = "TWO FAMILY DWELLING"
)

# Apply mapping to DWELL_TYPE
standardized_dwelling <- st_paul_nhood %>%
  mutate(
    DWELL_TYPE_STANDARDIZED = recode(DWELL_TYPE, !!!dwelling_mapping)
  )

# Filter and summarize for District 8, 2017 & 2018
dwelling_distribution <- standardized_dwelling %>%
  filter(districtnu == 8 & year %in% c(2017, 2018)) %>%
  group_by(year, DWELL_TYPE_STANDARDIZED) %>%
  summarise(
    count = n(),
    .groups = 'drop'
  )

# Visualize the standardized distribution
ggplot(dwelling_distribution, aes(x = DWELL_TYPE_STANDARDIZED, y = count, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribution of Dwelling Types in District 8 (2017 vs 2018)",
    x = "Standardized Dwelling Type",
    y = "Count",
    fill = "Year"
  ) +
  theme_minimal()

dwelling_table <- standardized_dwelling %>%
  filter(districtnu == 8 & year %in% c(2017, 2018)) %>%
  group_by(year, DWELL_TYPE_STANDARDIZED) %>%
  summarise(
    count = n(),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = count,
    values_fill = list(count = 0)  # Fill missing values with 0
  ) 

#### Methods ####
rci_crime_nhood <- merge(aggregated_data_nhood,rci_data,by.x = c("NEIGHBORHO","Year"),by.y=c("districtnu","year"))
rci_crime_nhood$post <- ifelse(rci_crime_nhood$Year >2020, 1,0)
rci_crime_nhood <- subset(rci_crime_nhood, !is.na(Year))

# "Agg. Assault"               "Agg. Assault Dom."          "Arson"                     
# [4] "Auto Theft"                 "Burglary"                   "Discharge"                 
# [7] "Graffiti"                   "Narcotics"                  "Rape"                      
# [10] "Robbery"                    "Simple Asasult Dom."        "Theft"                     
# [13] "Vandalism"                  "Community Engagement Event" "Proactive Police Visit"    
# [16] "Community Event"            "Criminal Damage"            "Homicide"                  
# [19] "Simple Assault Dom."        "Agg. Assault Dom"           "Simple Assault Dom"        
# [22] "Other"                      "Proactive Foot Patrol"      "0"                         
# [25] "THEFT"                     

ggplot(subset(rci_crime_nhood,INCIDENT=="Auto Theft"), aes(x = RCI, y = Count, color = as.factor(NEIGHBORHO))) +
  geom_point(size = 1) +
  geom_text(aes(label = NEIGHBORHO), hjust = -0.2, vjust = -0.5, size = 2.5) +
  facet_wrap(~Year, scales = "fixed")+
  geom_smooth(aes(color = NEIGHBORHO), method = "lm", se = T, linetype = "solid")+
  labs(
    title = "Relationship Between RCI and Theft",
    x = "Rent Control Intensity (RCI)",
    y = "Count"
  ) +
  theme_minimal()

ggplot(rci_crime_nhood, aes(x = Year, y = Count,group = as.factor(NEIGHBORHO),color = as.factor(NEIGHBORHO))) +
  geom_line() +
  geom_vline(xintercept = c(2021, 2022), linetype = "dashed") +
  labs(title = "Outcome Trends by Neighborhood", x = "Year", y = "Outcome") +
  theme_minimal()

# ggplot(rci_crime_nhood, aes(x = RCI, y = Count, color = as.factor(NEIGHBORHO))) +
#   geom_line(size = 1) +
#   facet_wrap(~Year)+
#   geom_smooth(aes(color = NEIGHBORHO), method = "lm", se = FALSE, linetype = "solid")+
#   labs(
#     title = "Relationship Between RCI and Count",
#     x = "Rent Control Intensity (RCI)",
#     y = "Count"
#   ) +
#   theme_minimal()

ggplot(rci_crime_nhood, aes(x = RCI)) +
  geom_histogram() +
  labs(
    title = "Relationship Between RCI",
    x = "Rent Control Intensity (RCI)",
    y = "Rent Control Intensity (RCI)"
  )

ggplot(rci_crime_nhood, aes(x = Count, color = NEIGHBORHO)) +
  geom_histogram() +
  facet_wrap(~Year)+
  labs(
    title = "Count",
    x = "distribution",
    y = "Count"
  ) +
  theme_minimal()


summary(lm(Count ~ RCI*post, subset(rci_crime_nhood,INCIDENT=="Auto Theft")))



### districts ####

district_councils <- st_read("District_Councils_3386664414246726701")
district_councils <- st_transform(district_councils,4326)
print(unique(district_councils$districtnu))
names(district_councils)

district_councils <- subset(district_councils, select = c("districtnu", "planning_1"))
district_councils <- st_as_sf(district_councils)
