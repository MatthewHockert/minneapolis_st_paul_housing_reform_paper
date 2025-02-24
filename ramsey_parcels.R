
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


#### 2023 and 2024 ####

# year <- 2023  
# base_file_path <- "/Users/matthewhockert/Desktop/Personal Info/minneapolis_st_paul_housing_reform_paper/fgdb_plan_regonal_parcels_"
# layer_name_pattern <- "Parcels2023Ramsey.lyr"
# 
# file_path <- paste0(base_file_path, year)
# layer_name <- gsub("2023", year, layer_name_pattern)
# print(layer_name)
# ramsey_data_23 <- st_read(dsn = file_path, layer = layer_name)
# names(ramsey_data_23)

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


invalid_geometries <- st_paul_23 %>% filter(!st_is_valid(geometry))
print(nrow(invalid_geometries))
st_paul_23 <- st_paul_23 %>%
  filter(st_is_valid(geometry))



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

# 2024
ramsey_data_24 <- st_read("shp_plan_regonal_parcels_2024",layer="Parcels2024Ramsey")
names(ramsey_data_24)
st_paul_24 <- subset(ramsey_data_24, CTU_NAME %in% c("Saint Paul") & !(COUNTY_PIN %in% c("Water", "Undetermined RoW", "State RoW", "Pedestrian - Municipal",
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
# print(unique(ramsey_data_23$PIN_2))

st_paul_24 <- st_paul_24[grep("-\\d+$", st_paul_24$PIN),]
st_paul_24$PIN_2 <- sub("^[^-]*-", "", st_paul_24$PIN)

st_paul_24 <- st_as_sf(st_paul_24)
st_paul_24 <- st_transform(st_paul_24,4326)

st_paul_24 <- st_cast(st_paul_24, "MULTIPOLYGON", warn = FALSE)

st_paul_24 <- st_make_valid(st_paul_24)

# Identify invalid geometries
invalid <- which(!st_is_valid(st_paul_24))
if (length(invalid) > 0) {
  print(invalid) # View problematic geometries
  st_paul_24 <- st_cast(st_paul_24, "POLYGON", warn = FALSE) # Fix with casting
}
st_paul_24 <- st_paul_24[-invalid, ]
# Transform to a projected CRS (if needed)
st_paul_24 <- st_transform(st_paul_24, 4326)

invalid_geometries <- st_paul_24 %>% filter(!st_is_valid(geometry))
print(nrow(invalid_geometries))
st_paul_24 <- st_paul_24 %>%
  filter(st_is_valid(geometry))
st_paul_24 <- st_paul_24 %>%
  mutate(geometry = st_centroid(geometry))

names(st_paul_24)

st_paul_24$USE1_DESC <-st_paul_24$USECLASS1
st_paul_24$CITY <- st_paul_24$CTU_NAME
st_paul_24$CITY_MERGED <- st_paul_24$CTU_NAME
st_paul_24$year <- 2023
st_paul_24$COUNTY_ID <- st_paul_24$COUNTY_PIN

# correct names
st_paul_24_nhood<- st_intersection(st_paul_24,district_councils)
common_cols <- intersect(colnames(st_paul_24_nhood), colnames(st_paul_nhood))
st_paul_24_subset <- st_paul_24_nhood[, common_cols]
names(st_paul_24_subset)
names(st_paul_nhood)
missing_cols <- setdiff(names(st_paul_nhood), names(st_paul_24_subset))
missing_cols


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

st_paul_combined <- rbind(st_paul_nhood,st_paul_23_subset,st_paul_24_subset)
st_paul_combined <- subset(st_paul_combined,ZIP %in% st_paul_zips$GEOID20)
st_paul_combined <- st_paul_combined %>%
  mutate(
    PID = if_else(
      nchar(PIN_2) == 12,            
      paste0("0", PIN_2),            
      PIN_2                         
    )
  )
beep()

years <- unique(st_paul_combined$year)
st_paul_combined_pdist <- data.frame()
for(yearx in years){
  print(yearx)
  st_paul_combined_sub <- subset(st_paul_combined,year == yearx)
  print(nrow(st_paul_combined_sub))
  
  intersection_result <- st_intersection(st_paul_combined_sub, st_paul_police_districts)
  
  st_paul_combined_pdist <- rbind(st_paul_combined_pdist, intersection_result)
  
}
beep()

#st_paul_combined_pdist <- st_intersection(st_paul_combined,st_paul_police_districts)
# MOVE TO CALCULATE_RCI.R





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
  geom_point(alpha = 0.6, color = "blue") +  
  geom_smooth(method = "lm", color = "red") +
  #facet_wrap(~Year)
  theme_minimal()

ggplot(rci_crime_rents_step2, aes(x = AvgRent, y = Count)) +
  geom_point(alpha = 0.6, color = "blue") +  
  geom_smooth(method = "lm", color = "red") +
  # facet_wrap(~Year)
  theme_minimal()

ggplot(rci_crime_rents_step2, aes(x = RCI, y = log(AvgRent+1))) +
  geom_point(alpha = 0.6, color = "blue") +  
  geom_smooth(method = "lm", color = "red") +
  # facet_wrap(~Year)
  theme_minimal()

summary(lm(log(AvgRent+1)~log(RCI+1)+log(Count+1)+ as.factor(NEIGHBORHO)+as.factor(Year),rci_crime_rents_step2))
summary(lm(log(AvgRent+1)~log(Count+1)+ as.factor(NEIGHBORHO)+as.factor(Year),rci_crime_rents_step2))
summary(lm(log(Count+1)~log(RCI+1)+ as.factor(NEIGHBORHO)+as.factor(Year),rci_crime_rents_step2))


#rci_crime_rents_step2
# how does crime change for the high changing rent in zip codes 
# 55101, 55102, 55104, 55106

crime_zip <- rci_crime_rents_step2 %>%
  group_by(Year, ZIP) %>%
  summarize(
    mean_count = mean(Count, na.rm = TRUE),  # Calculate the mean of Count
    sd_count = sd(Count, na.rm = TRUE),     # Calculate the standard deviation of Count
    RCI = mean(RCI, na.rm = TRUE)           # Calculate the mean of RCI
  ) %>%
  mutate(
    z_score = (mean_count - mean(mean_count, na.rm = TRUE)) / sd(mean_count, na.rm = TRUE) # Calculate z-score
  )


crime_zip <- merge(crime_zip, rent_long_avg, by.x = c("Year","ZIP"),by.y = c("Year","RegionName"))

ggplot(crime_zip, aes(x = RCI, y = z_score)) +
  geom_point(alpha = 0.6, color = "blue") +  
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~Year)
  theme_minimal()

ggplot(crime_zip, aes(x = Year, y = RCI, group=ZIP,color=ZIP)) +
  geom_line()+
  # facet_wrap(~Year)
  theme_minimal()

# Higher changes in rent correlate to a decrease in crime counts.
# The 


#
