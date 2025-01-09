# file to start the Minneapolis analysis on Rent control, land use, crime, etc.
library(dplyr)
library(sf)

parcel_geometries <- st_read("/Users/matthewhockert/Desktop/Personal Info/minneapolis_st_paul_housing_reform_paper/parcel_geometries.shp")

csv_data <- read.csv("/Users/matthewhockert/Desktop/Personal Info/minneapolis_st_paul_housing_reform_paper/parcels_ag_hmst_acres_df_x.csv")
minneapolis <- subset(csv_data,CTU_NAME == "Minneapolis")

minneapolis_geom <- merge(minneapolis,parcel_geometries,by.x = c("PID","TAX_YEAR"),by.y = c("PIN_2","year"))
minneapolis_geom <- st_as_sf(minneapolis_geom)
minneapolis_geom <- st_transform(minneapolis_geom,26915)

# Transit lines
transit_lines <- st_read("/Users/matthewhockert/Downloads/shp_trans_transitways_generalized/TransitwayAlignmentsGeneralized.shp")
transit_lines <- subset(transit_lines, subset = NameTransi %in% c("Green Line","Blue Line"))
transit_lines <- st_transform(transit_lines,26915)
plot(transit_lines$geometry)

transit_lines_buff <- st_buffer(transit_lines, dist = 1609.34*2)

years <- unique(minneapolis_geom$year)
minneapolis_geom_transit <- data.frame()
for (year in years){
  print(year)
  minneapolis_geom_sub <- minneapolis_geom[minneapolis_geom$TAX_YEAR == year, ]
  print(nrow(minneapolis_geom))
  print(nrow(minneapolis_geom_sub))
  
  joined_data <- st_join(minneapolis_geom_sub, transit_lines_buff, st_intersects) 
  print("joined")
  joined_data <- st_drop_geometry(joined_data)
  print("data dropped")
  joined_data_dedup <- joined_data %>%
    distinct(PID, .keep_all = TRUE)
  print("completed distinction")
  minneapolis_geom_transit <- rbind(minneapolis_geom_transit, joined_data_dedup)
  
}
# minneapolis_geom_transit <- st_join(minneapolis_geom,transit_lines_buff,st_intersects)

minneapolis_transit <- st_drop_geometry(minneapolis_geom_transit)



#### St. Paul Housing ####

st_paul_housing <- st_read("Housing_Production")
Saint_Paul_City_Boundary <-st_read("Saint_Paul_City_Boundary")

st_paul_zip_code_rents <- read_csv("Zip_zori_uc_sfrcondomfr_sm_month.csv")
st_paul_zip_code_rents <- subset(st_paul_zip_code_rents,City == "Saint Paul")

st_paul_zip_dist <- subset(rental_parcels,select = c("ZIP","districtnu",'planning_1'))
st_paul_zip_dist <- st_drop_geometry(st_paul_zip_dist)
st_paul_zip_dist <- unique(st_paul_zip_dist)
st_paul_zip_dist <- subset(st_paul_zip_dist,!is.na(ZIP))

st_paul_zips <- st_read("shp_bdry_zip_code_tabulation_areas")
# plot(st_paul_zips$geometry)
st_paul_zips <- st_transform(st_paul_zips,4326)
st_paul_zips <- st_intersection(st_paul_zips,Saint_Paul_City_Boundary)
# plot(st_paul_zips$geometry)
st_paul_zips <- subset(st_paul_zips,select = c("GEOID20","geometry"))
st_paul_zips <- st_transform(st_paul_zips,4326)
# st_paul_zips <- st_simplify(st_paul_zips, dTolerance = 10)

ggplot() +
  # Plot the ZIP codes
  geom_sf(data = st_paul_zips, fill = "blue", alpha = 0.3, color = "black", size = 0.5) +
  # Plot the district councils on top
  geom_sf(data = district_councils, fill = NA, color = "red", size = 0.8)+
  # geom_sf(data = subset(unique_geometries,year = 2022), color = "red")+
  theme_minimal()
ggplot() +
  # Plot the ZIP codes
  geom_sf(data = st_paul_zips, fill = "blue", alpha = 0.3, color = "black", size = 2) +
  # Plot the district councils on top
  geom_sf(data = district_councils, fill = NA, color = "red", size = 0.8) +
  # geom_sf(data = st_paul_zips_dist, fill = NA, color = "green", size = 0.8) +
  # Add ZIP code labels
  geom_sf_text(data = st_paul_zips, aes(label = GEOID20), size = 3, color = "black") +
  # Add district council names
  geom_sf_text(data = district_councils, aes(label = planning_1), size = 2, color = "red") +
  theme_minimal() +
  labs(
    title = "St. Paul ZIP Codes and District Councils",
    subtitle = "Blue: ZIP Codes, Red: District Councils",
    caption = "Data Source: St. Paul"
  )

st_paul_zips_dist <- st_intersection(st_paul_zips,district_councils)
st_paul_zips_dist <- st_as_sf(st_paul_zips_dist)



