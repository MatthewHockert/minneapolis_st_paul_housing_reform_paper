
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
# Define the range of years
years <- 2005:2022

# Define the base file path and layer name pattern
base_file_path <- "/Users/matthewhockert/Desktop/HCRA/shp_plan_regonal_parcels_"
layer_name_pattern <- "Parcels%YEAR%Ramsey"

# Define column names to keep
column_names_to_keep <- c("COUNTY_PIN","STATE_PIN","CTU_NAME","CO_NAME","HOMESTEAD","EMV_LAND","EMV_BLDG","EMV_TOTAL",
                          "TAX_CAPAC","TOTAL_TAX","SALE_DATE","SALE_VALUE","GREEN_ACRE","OPEN_SPACE","AG_PRESERV",
                          "SCHOOL_DST","PIN","year","geometry")

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
parcels_ag_hmst_acres_met_data <- do.call(rbind, processed_data_frames)
rm(processed_data_frames)
rm(subset_data_frames)
parcels_ag_hmst_acres_met_data <- parcels_ag_hmst_acres_met_data[grep("-\\d+$", parcels_ag_hmst_acres_met_data$PIN),]
parcels_ag_hmst_acres_met_data$PIN_2 <- sub("^[^-]*-", "", parcels_ag_hmst_acres_met_data$PIN)
#combined_data_frame$PIN_2_num <- as.numeric(combined_data_frame$PIN_2)

year <- 2015  # Replace with your desired year
base_file_path <- "/Users/matthewhockert/Desktop/HCRA/shp_plan_regonal_parcels_"
layer_name_pattern <- "Parcels2015Ramsey"
# Construct the file path and layer name
file_path <- paste0(base_file_path, year)
layer_name <- gsub("2015", year, layer_name_pattern)

ramsey_data <- st_read(dsn = file_path, layer = layer_name)
st_paul <- subset(ramsey_data, CITY %in% c("Saint Paul"))
plot(st_paul$geometry)
validity_check <- st_is_valid(st_paul)
st_paul <- st_paul[validity_check, ]

st_paul <- st_paul %>%
  mutate(geometry = st_centroid(geometry))


crime_nhood <- st_as_sf(crime_nhood)
st_paul <- st_transform(st_paul,4326)
st_paul_nhood <- st_intersection(st_paul,district_councils)

rental_parcels <- st_paul_nhood %>%
  mutate(
    is_rental = ifelse(
      NUM_UNITS > 1 & USE1_DESC %in% c("Res 2-3 units", "Apt 4+ units"), 1, 0
    )
  )

rci_data <- rental_parcels %>%
  group_by(districtnu) %>% 
  summarise(
    total_parcels = n(),
    rental_parcels = sum(is_rental, na.rm = TRUE),
    RCI = (rental_parcels / total_parcels)*100
  )
rci_data <- st_drop_geometry(rci_data)

rci_crime_nhood <- merge(aggregated_data_nhood,rci_data,by.x = "NEIGHBORHO",by.y="districtnu")
rci_crime_nhood$post <- ifelse(rci_crime_nhood$Year >2020, 1,0)

ggplot(rci_crime_nhood, aes(x = RCI, y = Count, color = as.factor(NEIGHBORHO))) +
  geom_point(size = 1) +
  facet_wrap(~Year)+
  geom_smooth(aes(color = NEIGHBORHO), method = "lm", se = FALSE, linetype = "solid")+
  labs(
    title = "Relationship Between RCI and Count",
    x = "Rent Control Intensity (RCI)",
    y = "Count"
  ) +
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
  facet_wrap(~Year)+
  labs(
    title = "Relationship Between RCI and Count",
    x = "Rent Control Intensity (RCI)",
    y = "Count"
  ) +
  theme_minimal()

ggplot(rci_crime_nhood, aes(x = Count)) +
  geom_histogram() +
  facet_wrap(~Year)+
  labs(
    title = "Relationship Between RCI and Count",
    x = "Rent Control Intensity (RCI)",
    y = "Count"
  ) +
  theme_minimal()

summary(lm(Count ~ RCI*post + as.factor(NEIGHBORHO)+ as.factor(Year), rci_crime_nhood))

### districts ####

district_councils <- st_read("District_Councils_3386664414246726701")
district_councils <- st_transform(district_councils,4326)
print(unique(district_councils$districtnu))
names(district_councils)

district_councils <- subset(district_councils, select = c("districtnu", "planning_1"))
district_councils <- st_as_sf(district_councils)
