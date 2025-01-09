
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
year <- 2022 
base_file_path <- "/Users/matthewhockert/Desktop/HCRA/shp_plan_regonal_parcels_"
layer_name_pattern <- "Parcels2022Ramsey"

# Construct the file path and layer name
file_path <- paste0(base_file_path, year)
layer_name <- gsub("2022", year, layer_name_pattern)

ramsey_data_15 <- st_read(dsn = file_path, layer = layer_name)
names(ramsey_data_15)


#### 2023 and 2024 ####

year <- 2023  
base_file_path <- "/Users/matthewhockert/Desktop/Personal Info/minneapolis_st_paul_housing_reform_paper/fgdb_plan_regonal_parcels_"
layer_name_pattern <- "Parcels2023Ramsey.lyr"

file_path <- paste0(base_file_path, year)
layer_name <- gsub("2023", year, layer_name_pattern)

ramsey_data_23 <- st_read(dsn = file_path, layer = layer_name)
names(ramsey_data_23)

ramsey_data_23 <- st_read("/Users/matthewhockert/Downloads/fgdb_plan_regonal_parcels_2023/plan_regonal_parcels_2023.gdb",layer="Parcels2023Ramsey")
st_paul_23 <- subset(ramsey_data_23, CTU_NAME %in% c("Saint Paul") & !(COUNTY_PIN %in% c("Water", "Undetermined RoW", "State RoW", "Pedestrian - Municipal",
                                                                                         "Pedestrian", "Park", "Municipal RoW", "Metes & Bounds",
                                                                                         "Lot", "Federal RoW", "Drainage", "Ditch - Judicial",
                                                                                         "Ditch - County", "County RoW", "C94", "C92",
                                                                                         "C91", "C90", "C89", "C88",
                                                                                         "C87", "C86", "C84", "C83",
                                                                                         "C78", "C76", "C75", "C74",
                                                                                         "C70", "C69", "C60", "C59",
                                                                                         "C585", "C58", "C57", "C562",
                                                                                         "C56", "C55", "C54", "C45",
                                                                                         "C44", "C43", "C42", "C41",
                                                                                         "C40", "C39", "C107", "C100",
                                                                                         "C")))
print(unique(ramsey_data_23$PIN_2))

st_paul_23 <- st_paul_23[grep("-\\d+$", st_paul_23$PIN),]
st_paul_23$PIN_2 <- sub("^[^-]*-", "", st_paul_23$PIN)

st_paul_23 <- st_paul_23 %>%
  rename(geometry = Shape)

st_paul_23 <- st_as_sf(st_paul_23)
st_paul_23 <- st_transform(st_paul_23,4326)

st_paul_23 <- st_cast(st_paul_23, "MULTIPOLYGON", warn = FALSE)

st_paul_23 <- st_make_valid(st_paul_23)

# Identify invalid geometries
invalid <- which(!st_is_valid(st_paul_23))
if (length(invalid) > 0) {
  print(invalid) # View problematic geometries
  st_paul_23 <- st_cast(st_paul_23, "POLYGON", warn = FALSE) # Fix with casting
}
st_paul_23 <- st_paul_23[-invalid, ]
# Transform to a projected CRS (if needed)
st_paul_23 <- st_transform(st_paul_23, 4326)

# Recalculate centroids
st_paul_23 <- st_paul_23 %>%
  mutate(geometry = st_centroid(geometry))

names(st_paul_23)

st_paul_23$USE1_DESC <-st_paul_23$USECLASS1
st_paul_23$CITY <- st_paul_23$CTU_NAME
st_paul_23$CITY_MERGED <- st_paul_23$CTU_NAME
st_paul_23$year <- 2023
st_paul_23$COUNTY_ID <- st_paul_23$COUNTY_PIN

# correct names
st_paul_23_nhood<- st_intersection(st_paul_23,district_councils)
common_cols <- intersect(colnames(st_paul_23_nhood), colnames(st_paul_nhood))
st_paul_23_subset <- st_paul_23_nhood[, common_cols]
names(st_paul_23_subset)
names(st_paul_nhood)
missing_cols <- setdiff(names(st_paul_nhood), names(st_paul_23_subset))
missing_cols


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

st_paul_combined <- rbind(st_paul_nhood,st_paul_23_subset)

current_year <- 2023
exemption_year_threshold <- current_year - 20  # Properties built after this year are exempt

unique_geometries <- st_paul_combined %>%
  group_by(year) %>%
  distinct(geometry, .keep_all = TRUE) %>%  # Keep only unique geometries within each year
  ungroup()

rental_parcels <- unique_geometries %>%
  mutate(
    is_rental = ifelse(
      # Check for relevant dwelling types
      DWELL_TYPE %in% c(
        "DOUBLE DWELLING", "TWO FAMILY DWELLING", "THREE FAMILY DWELLING",
        "APARTMENTS 1-9 UNITS", "APARTMENTS 10-19 UNITS", 
        "APARTMENTS 20-49 UNITS", "APARTMENTS 50 OR MORE UNITS",
        # Post-2017 dwelling types
        "APARTMENTS 20-49 RENTAL UNITS", "APT OR COMPLEX 100+ UNITS",
        "APARTMENTS 50-99 RENTAL UNITS", "APARTMENTS 7-19 RENTAL UNITS",
        "APARTMENTS 4-6 RENTAL UNITS", "ASSISTED LIVING APT COMPLEX",
        "TWO FAMILY DWELLING - SIDE/SI", "TWO FAMILY DWELLING - UP/DWN"
      ),
      # Apply year-specific logic
      ifelse(
        year == 2023,
        ifelse(YEAR_BUILT <= exemption_year_threshold, 1, 0),  # Exclude exempt properties
        1  # For other years, set as rental
      ),
      0  # Not a relevant dwelling type
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

rental_parcels$ZIP <- ifelse(rental_parcels$ZIP =="2344","55105",rental_parcels$ZIP)
rental_parcels$ZIP <- ifelse(rental_parcels$ZIP =="05117","55117",rental_parcels$ZIP)

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
  group_by(ZIP,districtnu,year) %>% 
  summarise(
    total_parcels = n(),
    rental_parcels = sum(is_rental, na.rm = TRUE),
    RCI = (rental_parcels / total_parcels)*100
  )
rci_data <- st_drop_geometry(rci_data)

ggplot(rci_data, aes(x = year, y = RCI, color = as.factor(ZIP), group = ZIP)) +
  geom_line(size = 1) +
  labs(
    title = "Rent Control Intensity (RCI) by Neighborhood",
    x = "Year",
    y = "RCI (%)",
    color = "Neighborhood"
  ) +
  theme_minimal()


#### rent data ####
rent_long <- st_paul_zip_code_rents %>%
  pivot_longer(cols = starts_with("20"), 
               names_to = "Date", 
               values_to = "Rent") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  mutate(Year = as.numeric(format(Date, "%Y")))

rent_long_avg <- rent_long %>%
  # Extract Year from Date if not already done
  mutate(Year = as.numeric(format(Date, "%Y"))) %>%
  # Group by RegionName and Year to calculate the yearly average Rent
  group_by(RegionName, Year) %>%
  summarize(AvgRent = mean(Rent, na.rm = TRUE), .groups = "drop") %>%
  # Calculate the percent change in yearly average rent
  arrange(RegionName, Year) %>%
  group_by(RegionName) %>%
  mutate(PercentChangeAvgRent = (AvgRent - lag(AvgRent)) / lag(AvgRent) * 100) %>%
  ungroup()


ggplot(rent_long_avg %>% filter(!is.na(PercentChangeAvgRent)), 
       aes(x = Year, y = PercentChangeAvgRent, color = RegionName, group = RegionName)) +
  geom_line() +
  geom_vline(xintercept = c(2021, 2022), linetype = "dashed") +  # Use numeric year for xintercept
  labs(title = "Percent Change in Yearly Average Rent by Zip Code",
       x = "Year",
       y = "Percent Change in Rent (%)",
       color = "Zip Code") +
  theme_minimal()

names(rent_long)
names(rci_data)
rent_long_rci <- merge(rent_long_avg, rci_data, 
                       by.x = c("RegionName", "Year"), 
                       by.y = c("ZIP", "year"))

ggplot(rent_long_rci, aes(x = RCI, y = PercentChangeAvgRent)) +
  geom_point(alpha = 0.6, color = "blue") +  # Add points with transparency for better visibility
  geom_smooth(method = "lm", color = "red")+
  labs(title = "Scatter Plot of Rent vs RCI",
       x = "Rent Control Intensity (RCI)",
       y = "Rent") +
  # facet_wrap(~Year)
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
rci_crime_nhood <- merge(aggregated_crime_nhood,rent_long_rci,by.x = c("NEIGHBORHO","Year"),by.y=c("districtnu","year"))
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

ggplot(subset(rci_crime_nhood,INCIDENT=="Agg. Assault"), aes(x = RCI, y = Count, color = as.factor(NEIGHBORHO))) +
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

slopes <- rci_crime_nhood %>%
  # filter(INCIDENT == "Homicide") %>%
  group_by(Year) %>%
  summarise(
    slope = round(coef(lm((Count) ~ RCI))[2], 2),  # Extract the slope from the linear model
    .groups = 'drop'
  )

# Merge slopes
rci_crime_nhood_with_slopes <- rci_crime_nhood %>%
  # filter(INCIDENT == "Homicide") %>%
  left_join(slopes, by = c("Year"))

ggplot(rci_crime_nhood_with_slopes, aes(x = RCI, y = (Count), color = as.factor(NEIGHBORHO))) +
  geom_point(size = 1) +
  geom_text(aes(label = NEIGHBORHO), hjust = -0.2, vjust = -0.5, size = 2.5) +  # Add neighborhood labels
  geom_text(data = slopes, aes(x = 15, y = 0, label = paste0("Slope: ", slope)), inherit.aes = FALSE, size = 3, hjust = 1) +  # Add slope labels
  facet_wrap(~Year, scales = "fixed") +
  geom_smooth(aes(color = NEIGHBORHO), method = "lm", se = TRUE, linetype = "solid") +
  labs(
    title = "Relationship Between RCI and Homicide",
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
  geom_histogram(bins = 40) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)) +
  labs(
    title = "Distribution of Rent Control Intensity (RCI)",
    x = "Rent Control Intensity (RCI)",
    y = "Frequency"
  )
# 3. Top 20% vs. Rest:
#   •	Define High RCI as the top 20% of neighborhoods with the highest RCI (e.g., RCI ≥ 15).
# •	Define Low RCI as the remaining 80%.
ggplot(rci_crime_nhood, aes(x = Count, color = NEIGHBORHO)) +
  geom_histogram() +
  facet_wrap(~Year)+
  labs(
    title = "Count",
    x = "distribution",
    y = "Count"
  ) +
  theme_minimal()

# MEDIAN
pre_policy_years <- 2014:2023
# Create high vs. low RCI groups
rci_crime_nhood <- rci_crime_nhood %>%
  mutate(
    RCI_group = ifelse(RCI > median(RCI, na.rm = TRUE), "High RCI", "Low RCI")
  )

# Aggregate data by RCI group and year
parallel_trends_data <- rci_crime_nhood %>%
  filter(Year %in% pre_policy_years) %>%
  group_by(RCI_group, Year) %>%
  summarise(
    mean_crime = mean(Count, na.rm = TRUE),
    .groups = "drop"
  )
ggplot(parallel_trends_data, aes(x = Year, y = mean_crime, group = RCI_group,color = RCI_group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Parallel Trends in Crime count by RCI Group",
    x = "Year",
    y = "Mean Crime Count",
    color = "RCI Group"
  ) +
  theme_minimal()


## Quartiles

rci_crime_nhood <- rci_crime_nhood %>%
  group_by(Year) %>%
  mutate(
    RCI_Quartile = ntile(RCI, 4)  # Divide RCI into 4 quartiles
  ) %>%
  ungroup()

# Prepare data for parallel trends graph
parallel_trends_quartiles <- rci_crime_nhood %>%
  group_by(Year, RCI_Quartile) %>%
  summarise(
    mean_crime_count = mean(Count, na.rm = TRUE),
    .groups = 'drop'
  )

# Plot parallel trends for quartiles
ggplot(parallel_trends_quartiles, aes(x = Year, y = mean_crime_count,group = as.factor(RCI_Quartile), color = as.factor(RCI_Quartile))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = c("2021", "2022"), linetype = "dashed") +  
  scale_color_brewer(palette = "Set1", name = "RCI Quartile") +
  labs(
    title = "Parallel Trends in Crime Rates by RCI Quartile",
    x = "Year",
    y = "Mean Crime Count"
  ) +
  theme_minimal()

quartile_changes <- rci_crime_nhood %>%
  group_by(NEIGHBORHO) %>%
  summarise(
    quartile_changes = n_distinct(RCI_Quartile),  
    first_quartile = first(RCI_Quartile),        
    last_quartile = last(RCI_Quartile)          
  ) %>%
  filter(quartile_changes > 1)  

print(quartile_changes)

changed_neighborhoods <- rci_crime_nhood %>%
  filter(NEIGHBORHO %in% quartile_changes$NEIGHBORHO)

ggplot(changed_neighborhoods, aes(x = Year, y = RCI_Quartile, group = NEIGHBORHO, color = as.factor(NEIGHBORHO))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1", name = "Neighborhood") +
  labs(
    title = "Changes in RCI Quartiles Over Time",
    x = "Year",
    y = "RCI Quartile"
  ) +
  theme_minimal()


## 20-80

rci_percentiles <- quantile(rci_crime_nhood$RCI, probs = c(0.2, 0.8), na.rm = TRUE)

rci_crime_nhood <- rci_crime_nhood %>%
  mutate(
    RCI_Group = case_when(
      RCI >= rci_percentiles[2] ~ "Top 20%",
      RCI <= rci_percentiles[1] ~ "Bottom 20%",
      TRUE ~ "Middle 60%"
    )
  )

rci_crime_nhood_filtered <- rci_crime_nhood %>%
  filter(RCI_Group %in% c("Top 20%", "Bottom 20%"))

parallel_trends_top_bottom <- rci_crime_nhood_filtered %>%
  group_by(Year, RCI_Group) %>%
  summarise(
    mean_crime_count = mean(Count, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(parallel_trends_top_bottom, aes(x = Year, y = mean_crime_count, group = RCI_Group, color = RCI_Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Top 20%" = "red", "Bottom 20%" = "blue")) +
  labs(
    title = "Parallel Trends in Crime Rates by RCI Group (Top 20% vs. Bottom 20%)",
    x = "Year",
    y = "Mean Crime Count",
    color = "RCI Group"
  ) +
  theme_minimal()




parallel_trends_quartiles$post <- ifelse(parallel_trends_quartiles$Year >2020,1,0)
summary(lm(mean_crime_count ~ as.factor(RCI_Quartile)*post + as.factor(Year), parallel_trends_quartiles))

#### Aggregating rental prices, RCI, and crime data ####


#	1.	Step 1: Merge the Police Grid Data with the Parcels Data
#	Use the Neighborhood field as the common key.
rci_crime_step1 <- merge(aggregated_crime_nhood,rci_data,by.x = c("NEIGHBORHO","Year"),by.y=c("districtnu","year"))

# Check neighborhoods
missing_nhoods_step1 <- setdiff(unique(aggregated_crime_nhood$NEIGHBORHO), unique(rci_crime_step1$NEIGHBORHO))
print(missing_nhoods_step1)

# Check years
missing_years_step1 <- setdiff(unique(aggregated_crime_nhood$Year), unique(rci_crime_step1$Year))
print(missing_years_step1)

# 2.	Step 2: Merge the Combined Dataset with the Rental Prices Data
# Use the Zip Code field as the common key.

rci_crime_rents_step2 <- merge(rci_crime_step1,rent_long_avg,by.x = c("ZIP","Year"),by.y=c("RegionName","Year"),all.x = T)

# Check ZIPs
missing_zips_step2 <- setdiff(unique(rci_crime_step1$ZIP), unique(rci_crime_rents_step2$ZIP))
print(missing_zips_step2)

# Check years
missing_years_step2 <- setdiff(unique(rci_crime_step1$Year), unique(rci_crime_rents_step2$Year))
print(missing_years_step2)


table(rci_crime_rents_step2$NEIGHBORHO, rci_crime_rents_step2$ZIP)


ggplot(rci_crime_rents_step2, aes(x = PercentChangeAvgRent, y = RCI)) +
  geom_point(alpha = 0.6, color = "blue") +  # Add points with transparency for better visibility
  geom_smooth(method = "lm", color = "red") +
  #facet_wrap(~Year)
  theme_minimal()

ggplot(rci_crime_rents_step2, aes(x = AvgRent, y = Count)) +
  geom_point(alpha = 0.6, color = "blue") +  # Add points with transparency for better visibility
  geom_smooth(method = "lm", color = "red") +
  # facet_wrap(~Year)
  theme_minimal()

ggplot(rci_crime_rents_step2, aes(x = log(RCI+1), y = log(AvgRent+1))) +
  geom_point(alpha = 0.6, color = "blue") +  # Add points with transparency for better visibility
  geom_smooth(method = "lm", color = "red") +
  # facet_wrap(~Year)
  theme_minimal()

summary(lm(log(AvgRent+1)~log(RCI+1)+log(Count+1)+ as.factor(NEIGHBORHO)+as.factor(Year),rci_crime_rents_step2))
summary(lm(log(AvgRent+1)~log(Count+1)+ as.factor(NEIGHBORHO)+as.factor(Year),rci_crime_rents_step2))
summary(lm(log(Count+1)~log(RCI+1)+ as.factor(NEIGHBORHO)+as.factor(Year),rci_crime_rents_step2))



#
### districts ####

district_councils <- st_read("District_Councils_3386664414246726701")
district_councils <- st_transform(district_councils,4326)
print(unique(district_councils$districtnu))
names(district_councils)

district_councils <- subset(district_councils, select = c("districtnu", "planning_1"))
district_councils <- st_as_sf(district_councils)
