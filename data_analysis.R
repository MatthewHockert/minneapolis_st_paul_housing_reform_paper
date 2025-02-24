library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)

#### renfixest#### rent data ####
#Pulled from Main.R

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

names(rent_long_avg)
names(rci_data_zip)
rent_long_rci_zip <- merge(rent_long_avg, rci_data_zip, 
                       by.x = c("RegionName", "Year"), 
                       by.y = c("ZIP", "year"))

names(rent_long_rci_zip)
hist(rent_long_rci_zip$RCI)
hist(rent_long_rci_zip$AvgRent)
hist(log(rent_long_rci_zip$AvgRent))
hist(rent_long_rci_zip$PercentChangeAvgRent)

ggplot(rent_long_rci_zip, aes(x = RCI, y = log(AvgRent))) +
  geom_point(alpha = 0.6, color = "blue") + 
  geom_smooth(method = "lm", color = "red")+
  labs(title = "Scatter Plot of Rent vs RCI",
       x = "Rent Control Intensity (RCI)",
       y = "Avg. Rent") +
  # facet_wrap(~Year)+
  theme_minimal()

ggplot(rent_long_rci_zip, aes(x = RCI, y = PercentChangeAvgRent)) +
  geom_point(alpha = 0.6, color = "blue") + 
  geom_smooth(method = "lm", color = "red")+
  labs(title = "Scatter Plot of Rent vs RCI",
       x = "Rent Control Intensity (RCI)",
       y = "PercentChangeAvgRent") +
  # facet_wrap(~Year)
  theme_minimal()

rent_long_rci_zip_sub <- rent_long_rci_zip %>%
  filter(Year %in% c(2021, 2022)) %>%
  select(RegionName, Year, AvgRent, RCI) %>%
  pivot_wider(names_from = Year, values_from = c(AvgRent, RCI)) %>%
  mutate(
    PercentChangeAvgRent = (AvgRent_2022 - AvgRent_2021) / AvgRent_2021 * 100,
    PercentChangeRCI = (RCI_2022 - RCI_2021) / RCI_2021 * 100
  )

ggplot(rent_long_rci_zip_sub, aes(x = PercentChangeRCI, y = PercentChangeAvgRent)) +
  geom_point(alpha = 0.6, color = "blue") + 
  geom_smooth(method = "lm", color = "red")+
  labs(title = "Scatter Plot of Rent vs RCI",
       x = "Rent Control Intensity (RCI)",
       y = "PercentChangeAvgRent") +
  # facet_wrap(~Year)
  theme_minimal()

#### Neighborhood Methods ####
rci_data_zip_nhood <- rental_parcels %>%
  group_by(ZIP,districtnu,year) %>% 
  summarise(
    total_parcels = n(),
    rental_parcels = sum(is_rental, na.rm = TRUE),
    RCI = (rental_parcels / total_parcels)*100
  )

rent_long_rci_nhoods <- merge(rent_long_avg, rci_data_zip_nhood, 
                       by.x = c("RegionName", "Year"), 
                       by.y = c("ZIP", "year"))

names(rent_long_rci_nhoods)
names(aggregated_crime_nhood)

rci_crime_nhood <- merge(aggregated_crime_nhood,rci_data_zip_nhood,by.x = c("NEIGHBORHO","Year"),by.y=c("districtnu","Year"))
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

ggplot(rci_crime_nhood, aes(x = RCI, y = Count, color = as.factor(NEIGHBORHO))) +
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

names(rci_data_nhood)
rci_crime_nhood <- merge(aggregated_crime_nhood,rci_data_nhood,by.x = c("NEIGHBORHO","Year"),by.y=c("districtnu","year"))

rci_crime_nhood_filtered$post <- ifelse(rci_crime_nhood_filtered$Year >2020,1,0)
rci_crime_nhood_filtered$treated <- ifelse(rci_crime_nhood_filtered$RCI_Group =="Top 20%",1,0)


names(rci_crime_nhood_filtered)
summary(lm(Count ~ treated*post + as.factor(Year) + as.factor(NEIGHBORHO), rci_crime_nhood_filtered))


#### RCI by year ####
rci_crime_nhood <- merge(aggregated_crime_nhood,rci_data_nhood,by.x = c("NEIGHBORHO","Year"),by.y=c("districtnu","year"))

rci_crime_nhood <- rci_crime_nhood %>%
  group_by(Year) %>%
  mutate(
    rci_percentiles = list(quantile(RCI, probs = c(0.2, 0.8), na.rm = TRUE)), # Compute percentiles per year
    RCI_Group = case_when(
      RCI >= rci_percentiles[[1]][2] ~ "Top 20%",
      RCI <= rci_percentiles[[1]][1] ~ "Bottom 20%",
      TRUE ~ "Middle 60%"
    )
  ) %>%
  ungroup()

rci_crime_nhood_filtered <- rci_crime_nhood %>%
  filter(RCI_Group %in% c("Top 20%", "Bottom 20%"))

rci_crime_nhood_filtered$post <- ifelse(rci_crime_nhood_filtered$Year >=2021,1,0)
rci_crime_nhood_filtered$rent_vote <- ifelse(rci_crime_nhood_filtered$Year >=2021 & rci_crime_nhood_filtered$Year <=2022,1,0)
rci_crime_nhood_filtered$post_rent_vote <- ifelse(rci_crime_nhood_filtered$Year >2022,1,0)
rci_crime_nhood_filtered$treated <- ifelse(rci_crime_nhood_filtered$RCI_Group =="Top 20%",1,0)

rci_crime_nhood_filtered <- rci_crime_nhood_filtered %>%
  mutate(
    phase = case_when(
      Year < 2021 ~ "Pre-Vote",           # Before rent control was voted on
      Year == 2021 ~ "Anticipation",      # Year of the vote, before implementation
      Year >= 2022 ~ "Post-Implementation" # After rent control is enforced
    ),
    phase = factor(phase, levels = c("Pre-Vote", "Anticipation", "Post-Implementation")) # Set reference level
  )
# rci_crime_nhood_filtered$covid <-ifelse(rci_crime_nhood_filtered$Year >=2020,1,0)


treated_nhoods_2020 <- rci_crime_nhood_filtered %>%
  filter(Year == 2021, RCI_Group == "Top 20%") %>%
  pull(NEIGHBORHO)  
treated_nhoods_2020

rci_crime_nhood_filtered <- rci_crime_nhood_filtered %>%
  mutate(treated = ifelse(NEIGHBORHO %in% treated_nhoods_2020, 1, 0))

hist(rci_crime_nhood_filtered$Count)
hist(log(rci_crime_nhood_filtered$Count+1))
hist(rci_crime_nhood_filtered$RCI)
table(rci_crime_nhood_filtered$RCI)


summary(lm(log(Count+1) ~ treated*phase+as.factor(Year) + as.factor(NEIGHBORHO), rci_crime_nhood_filtered))


parallel_trends_top_bottom <- rci_crime_nhood_filtered %>%
  group_by(Year, RCI_Group) %>%
  summarise(
    mean_crime_count = mean(Count, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(parallel_trends_top_bottom, aes(x = Year, y = mean_crime_count, group = RCI_Group, color = RCI_Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Parallel Trends in Crime Rates by RCI Group (2021) (Top 20% vs. Bottom 20%)",
    x = "Year",
    y = "Mean Crime Count",
    color = "RCI Group"
  ) +
  theme_minimal()


#### Police District Methods ####



names(aggregated_crime_pd)
names(rci_data_pd)

rci_crime_pd <- merge(aggregated_crime_pd,rci_data_pd,by.x = c("POLICE_GRI","Year"),by.y=c("id","year"))
rci_crime_pd$post <- ifelse(rci_crime_pd$Year >2020, 1,0)
rci_crime_pd <- subset(rci_crime_pd, !is.na(Year))

# "Agg. Assault"               "Agg. Assault Dom."          "Arson"                     
# [4] "Auto Theft"                 "Burglary"                   "Discharge"                 
# [7] "Graffiti"                   "Narcotics"                  "Rape"                      
# [10] "Robbery"                    "Simple Asasult Dom."        "Theft"                     
# [13] "Vandalism"                  "Community Engagement Event" "Proactive Police Visit"    
# [16] "Community Event"            "Criminal Damage"            "Homicide"                  
# [19] "Simple Assault Dom."        "Agg. Assault Dom"           "Simple Assault Dom"        
# [22] "Other"                      "Proactive Foot Patrol"      "0"                         
# [25] "THEFT"                     

ggplot(rci_crime_pd, aes(x = RCI, y = Count, color = as.factor(POLICE_GRI))) +
  geom_point(size = 1) +
  geom_text(aes(label = POLICE_GRI), hjust = -0.2, vjust = -0.5, size = 2.5) +
  facet_wrap(~Year, scales = "fixed")+
  geom_smooth(aes(color = POLICE_GRI), method = "lm", se = T, linetype = "solid")+
  labs(
    title = "Relationship Between RCI and Theft",
    x = "Rent Control Intensity (RCI)",
    y = "Count"
  ) +
  theme_minimal()

slopes <- rci_crime_pd %>%
  # filter(INCIDENT == "Homicide") %>%
  group_by(Year) %>%
  summarise(
    slope = round(coef(lm((Count) ~ RCI))[2], 2),  # Extract the slope from the linear model
    .groups = 'drop'
  )

# Merge slopes
rci_crime_pd_with_slopes <- rci_crime_pd %>%
  # filter(INCIDENT == "Homicide") %>%
  left_join(slopes, by = c("Year"))

ggplot(rci_crime_pd_with_slopes, aes(x = RCI, y = (Count), color = as.factor(POLICE_GRI))) +
  geom_point(size = 1) +
  geom_text(aes(label = POLICE_GRI), hjust = -0.2, vjust = -0.5, size = 2.5) +  # Add neighborhood labels
  geom_text(data = slopes, aes(x = 15, y = 0, label = paste0("Slope: ", slope)), inherit.aes = FALSE, size = 3, hjust = 1) +  # Add slope labels
  facet_wrap(~Year, scales = "fixed") +
  geom_smooth(aes(color = POLICE_GRI), method = "lm", se = TRUE, linetype = "solid") +
  labs(
    title = "Relationship Between RCI and Homicide",
    x = "Rent Control Intensity (RCI)",
    y = "Count"
  ) +
  theme_minimal()

ggplot(rci_crime_pd, aes(x = Year, y = Count,group = as.factor(POLICE_GRI),color = as.factor(POLICE_GRI))) +
  geom_line() +
  geom_vline(xintercept = c(2021, 2022), linetype = "dashed") +
  labs(title = "Outcome Trends by Neighborhood", x = "Year", y = "Outcome") +
  theme_minimal()

# ggplot(rci_crime_pd, aes(x = RCI, y = Count, color = as.factor(NEIGHBORHO))) +
#   geom_line(size = 1) +
#   facet_wrap(~Year)+
#   geom_smooth(aes(color = NEIGHBORHO), method = "lm", se = FALSE, linetype = "solid")+
#   labs(
#     title = "Relationship Between RCI and Count",
#     x = "Rent Control Intensity (RCI)",
#     y = "Count"
#   ) +
#   theme_minimal()

ggplot(rci_crime_pd, aes(x = RCI)) +
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
ggplot(rci_crime_pd, aes(x = Count, color = POLICE_GRI)) +
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
rci_crime_pd <- rci_crime_pd %>%
  mutate(
    RCI_group = ifelse(RCI > median(RCI, na.rm = TRUE), "High RCI", "Low RCI")
  )

# Aggregate data by RCI group and year
parallel_trends_data <- rci_crime_pd %>%
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

rci_crime_pd <- rci_crime_pd %>%
  group_by(Year) %>%
  mutate(
    RCI_Quartile = ntile(RCI, 4)  # Divide RCI into 4 quartiles
  ) %>%
  ungroup()

# Prepare data for parallel trends graph
parallel_trends_quartiles <- rci_crime_pd %>%
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

quartile_changes <- rci_crime_pd %>%
  group_by(POLICE_GRI) %>%
  summarise(
    quartile_changes = n_distinct(RCI_Quartile),  
    first_quartile = first(RCI_Quartile),        
    last_quartile = last(RCI_Quartile)          
  ) %>%
  filter(quartile_changes > 1)  

print(quartile_changes)

changed_neighborhoods <- rci_crime_pd %>%
  filter(POLICE_GRI %in% quartile_changes$POLICE_GRI)

ggplot(changed_neighborhoods, aes(x = Year, y = RCI_Quartile, group = POLICE_GRI, color = as.factor(POLICE_GRI))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1", name = "POLICE_GRI") +
  labs(
    title = "Changes in RCI Quartiles Over Time",
    x = "Year",
    y = "RCI Quartile"
  ) +
  theme_minimal()


## 20-80

rci_percentiles <- quantile(rci_crime_pd$RCI, probs = c(0.3, 0.7), na.rm = TRUE)

rci_crime_pd <- rci_crime_pd %>%
  group_by(Year)%>%
  mutate(
    RCI_Group = case_when(
      RCI >= rci_percentiles[2] ~ "Top 20%",
      RCI <= rci_percentiles[1] ~ "Bottom 20%",
      TRUE ~ "Middle 60%"
    )
  )
table(rci_crime_pd$RCI_Group)

rci_crime_pd_filtered <- rci_crime_pd %>%
  filter(RCI_Group %in% c("Top 20%", "Bottom 20%"))

parallel_trends_top_bottom <- rci_crime_pd_filtered %>%
  group_by(Year, RCI_Group) %>%
  summarise(
    mean_crime_count = mean(Count, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(parallel_trends_top_bottom, aes(x = Year, y = log(mean_crime_count), group = RCI_Group, color = RCI_Group)) +
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


top_20_2020 <- unique(rci_crime_pd_filtered$POLICE_GRI[rci_crime_pd_filtered$RCI_Group == "Top 20%"])
bottom_20_2020 <- unique(rci_crime_pd_filtered$POLICE_GRI[rci_crime_pd_filtered$RCI_Group == "Bottom 20%"])

# Add crime classification to police districts
st_paul_police_districts <- st_paul_police_districts %>%
  mutate(
    crime_group = case_when(
      id %in% top_20_2020 ~ "Top 20%",
      id %in% bottom_20_2020 ~ "Bottom 20%",
      TRUE ~ "Other"
    )
  )

ggplot() +
  geom_sf(data = st_paul_police_districts, aes(fill = crime_group), color = "black") +
  scale_fill_manual(values = c("Top 20%" = "red", "Bottom 20%" = "blue", "Other" = "white")) +  
  theme_minimal() +
  labs(title = "Top and Bottom 20% Crime Districts in St. Paul (2020)", fill = "Crime Level")



parallel_trends_quartiles$post <- ifelse(parallel_trends_quartiles$Year >2020,1,0)

names(rci_data_nhood)
# rci_crime_pd <- merge(aggregated_crime_pd,rci_data_pd,by.x = c("POLICE_GRI","Year"),by.y=c("id","year"))

rci_crime_pd_filtered$post <- ifelse(rci_crime_pd_filtered$Year >2020,1,0)
top_20_2020 <- unique(rci_crime_pd_filtered$POLICE_GRI[rci_crime_pd_filtered$Year == 2020 & 
                                                         rci_crime_pd_filtered$RCI_Group == "Top 20%"])
rci_crime_pd_filtered <- rci_crime_pd_filtered %>%
  mutate(treated = ifelse(POLICE_GRI %in% top_20_2020, 1, 0))

names(rci_crime_pd_filtered)
summary(lm(Count ~ treated*post + as.factor(Year) + as.factor(POLICE_GRI), rci_crime_pd_filtered))

ggplot(rci_crime_pd_filtered %>%
         group_by(Year, treated) %>%
         summarise(mean_crime_count = mean(Count, na.rm = TRUE), .groups = "drop"),
       aes(x = Year, y = mean_crime_count, group = treated, color = factor(treated))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Parallel Trends Check: 2020 Top 20% as Treated",
       x = "Year", y = "Mean Crime Count", color = "Treated") +
  theme_minimal()

## Event study ###

rci_crime_pd_filtered <- rci_crime_pd_filtered %>%
  mutate(first_treatment_year = ifelse(treated == 1, 2021, NA))

# Replace Inf values (if a unit is never treated, avoid issues)
rci_crime_pd_filtered <- rci_crime_pd_filtered %>%
  mutate(first_treatment_year = ifelse(is.infinite(first_treatment_year), NA, first_treatment_year))

rci_crime_pd_filtered$Year_num <- as.numeric(rci_crime_pd_filtered$Year)

rci_crime_pd_filtered <- rci_crime_pd_filtered %>%
  mutate(event_time = Year_num - first_treatment_year)

rci_crime_pd_filtered <- rci_crime_pd_filtered %>%
  mutate(event_time = as.numeric(Year_num - first_treatment_year)) %>%
  mutate(event_time = factor(event_time, levels = sort(unique(event_time))))

rci_crime_pd_filtered <- rci_crime_pd_filtered %>%
  mutate(event_time = factor(event_time, levels = sort(unique(event_time))))

baseline_crime <- rci_crime_pd_filtered %>%
  filter(Year < 2020) %>%
  group_by(POLICE_GRI) %>%
  summarise(baseline_crime = mean(Count, na.rm = TRUE))

rci_crime_pd_filtered <- left_join(rci_crime_pd_filtered, baseline_crime, by = "POLICE_GRI")
rci_crime_pd_filtered <- rci_crime_pd_filtered[!is.na(rci_crime_pd_filtered$baseline_crime),]

levels(rci_crime_pd_filtered$event_time)
event_study_model <- feols(log(Count+1) ~ i(Year, treated, ref = "2019")| 
                             as.factor(Year)+
                            as.factor(POLICE_GRI), 
                           data = rci_crime_pd_filtered)
event_study_model$collin.var
summary(event_study_model)
iplot(event_study_model, order = "Year")

event_study_model <- feols(Count ~ i(Year, treated, ref = 2019) + i(Year, treated, ref = 2019):baseline_crime | 
                             as.factor(POLICE_GRI), 
                           data = rci_crime_pd_filtered)

summary(event_study_model)
event_study_model$collin.var
summary(event_study_model)
iplot(event_study_model, order = "Year")



colSums(is.na(rci_crime_pd_filtered))
table(rci_crime_pd_filtered$treated)

rci_crime_pd_filtered <- rci_crime_pd %>%
  filter(RCI_Group %in% c("Top 20%", "Bottom 20%"))
rci_crime_pd_filtered$post <- ifelse(rci_crime_pd_filtered$Year >2020,1,0)
top_20_2020 <- unique(rci_crime_pd_filtered$POLICE_GRI[rci_crime_pd_filtered$Year == 2020 & 
                                                         rci_crime_pd_filtered$RCI_Group == "Top 20%"])
rci_crime_pd_filtered <- rci_crime_pd_filtered %>%
  mutate(treated = ifelse(POLICE_GRI %in% top_20_2020, 1, 0))

baseline_crime_2019 <- rci_crime_pd_filtered %>%
  filter(Year == 2020) %>%
  select(POLICE_GRI, Count) %>%
  rename(Count_2019 = Count)

rci_crime_pd_filtered <- left_join(rci_crime_pd_filtered, baseline_crime_2019, by = "POLICE_GRI") %>%
  mutate(crime_index = Count / Count_2019)

event_study_model <- feols(Count ~ i(Year, treated, ref = 2020) |
                             as.factor(POLICE_GRI) + as.factor(year), 
                           data = rci_crime_pd_filtered)

summary(event_study_model)
event_study_model$collin.var
iplot(event_study_model, order = "Year")

parallel_trends_index <- rci_crime_pd_filtered %>%
  group_by(Year, treated) %>%
  summarise(mean_crime_index = mean(crime_index, na.rm = TRUE),
            mean_crime_count = mean(Count, na.rm = TRUE),
            .groups = "drop")

library(ggplot2)

ggplot(parallel_trends_index, aes(x = Year, y = mean_crime_count, group = treated, color = factor(treated))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Parallel Trends of Crime Index (Relative to 2019)",
    x = "Year",
    y = "Mean Crime Index",
    color = "Treated"
  ) +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal()

event_study_model <- feols(crime_index ~ i(Year, treated, ref = 2020) |
                             as.factor(POLICE_GRI) + as.factor(year), 
                           data = rci_crime_pd_filtered)

summary(event_study_model)
event_study_model$collin.var
summary(event_study_model)
iplot(event_study_model, order = "Year")




# Crime by month or quarter
rci_data_pd
hist(rci_data_pd$RCI)

ggplot(rci_data_pd, aes(x = RCI)) +
  geom_histogram(bins = 40) +
  scale_x_continuous() +
  labs(
    title = "Distribution of Rent Control Intensity (RCI)",
    x = "Rent Control Intensity (RCI)",
    y = "Frequency"
  )

names(rci_data_pd)
rci_data_pd <- rci_data_pd %>%
  group_by(year) %>%
  mutate(
    rci_percentiles_30 = quantile(RCI, probs = 0.3, na.rm = TRUE),
    rci_percentiles_70 = quantile(RCI, probs = 0.7, na.rm = TRUE),
    RCI_Group = case_when(
      RCI >= rci_percentiles_70 ~ "Top 30%",
      RCI <= rci_percentiles_30 ~ "Bottom 30%",
      TRUE ~ "Middle 60%"
    )
  ) %>%
  ungroup()  # Remove grouping after assigning categories

# Check distribution
table(rci_data_pd$RCI_Group)

top_20_2020 <- unique(rci_data_pd$id[rci_data_pd$RCI_Group == "Top 30%" & rci_data_pd$year == 2020])
bottom_20_2020 <- unique(rci_data_pd$id[rci_data_pd$RCI_Group == "Bottom 30%" & rci_data_pd$year == 2020])
bottom_20_2020

st_paul_police_districts <- st_paul_police_districts %>%
  mutate(
    rci_group = case_when(
      id %in% top_20_2020 ~ "Top 30%",
      id %in% bottom_20_2020 ~ "Bottom 30%",
      TRUE ~ "Other"
    )
  )

ggplot() +
  geom_sf(data = st_paul_police_districts, aes(fill = rci_group), color = "black") +
  scale_fill_manual(values = c("Top 30%" = "red", "Bottom 30%" = "blue", "Other" = "white")) +  
  theme_minimal() +
  labs(title = "Top and Bottom 20% Crime Districts in St. Paul (2020)", fill = "Crime Level")



rci_crime_month_pd <- merge(aggregated_crime_month_pd,subset(rci_data_pd,neighborhood != 17 & year >= 2017),by.x = c("POLICE_GRI","Year"),by.y=c("id","year"))
# rci_crime_month_pd <- merge(aggregated_crime_month_pd,rci_data_pd,by.x = c("POLICE_GRI","Year"),by.y=c("id","year"))

# rci_crime_pd_filtered <- rci_crime_pd %>%
#   filter(RCI_Group %in% c("Top 20%", "Bottom 20%"))
rci_crime_month_pd$post <- ifelse(rci_crime_month_pd$Year >2020,1,0)
top_20_2020 <- unique(rci_crime_month_pd$POLICE_GRI[rci_crime_month_pd$Year == 2020 & rci_crime_month_pd$RCI_Group == "Top 30%"])
top_20_2020

rci_crime_month_pd_filtered <- rci_crime_month_pd %>%
  mutate(treated = ifelse(POLICE_GRI %in% top_20_2020, 1, 0))

rci_crime_month_pd_filtered <- rci_crime_month_pd_filtered %>%
  mutate(gf = ifelse(Year >= 2020 & Year <=2022, 1, 0),
         reporting_diff = ifelse(Year < 2018, 1, 0))

parallel_trends_month <- rci_crime_month_pd_filtered %>%
  group_by(Month, treated) %>%
  summarise(
    year = first(Year),
    mean_crime = mean(Count, na.rm = TRUE), .groups = "drop")

ggplot(parallel_trends_month, aes(x = (Month), y = (mean_crime), group = treated, color = factor(treated))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Parallel Trends of Crime",
    x = "Year",
    y = "Mean Crime ",
    color = "Treated"
  ) +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal()

event_study_model_month <- feols((Count) ~ i(Month, treated, ref = as.Date("2021-11-01")) |
                                   as.factor(POLICE_GRI) + as.factor(Month), 
                           data = rci_crime_month_pd_filtered)

summary(event_study_model_month)
event_study_model_month$collin.var
iplot(event_study_model_month)
coefplot(event_study_model_month)

#12.584546



## QUARTER 

names(aggregated_crime_quarter_pd)
names(rci_data_pd)
rci_crime_quarter_pd <- merge(aggregated_crime_quarter_pd,subset(rci_data_pd,neighborhood != 17 & year >= 2017),by.x = c("POLICE_GRI","Year"),by.y=c("id","year"))
# rci_crime_quarter_pd <- merge(aggregated_crime_quarter_pd,rci_data_pd,by.x = c("POLICE_GRI","Year"),by.y=c("id","year"))

# rci_crime_pd_filtered <- rci_crime_pd %>%
#   filter(RCI_Group %in% c("Top 20%", "Bottom 20%"))
rci_crime_quarter_pd$post <- ifelse(rci_crime_quarter_pd$Year >2020,1,0)
top_20_2020 <- unique(rci_crime_quarter_pd$POLICE_GRI[rci_crime_quarter_pd$Year == 2020 & rci_crime_quarter_pd$RCI_Group == "Top 30%"])
top_20_2020

rci_crime_quarter_pd_filtered <- rci_crime_quarter_pd %>%
  mutate(treated = ifelse(POLICE_GRI %in% top_20_2020, 1, 0))

rci_crime_quarter_pd_filtered <- rci_crime_quarter_pd_filtered %>%
  mutate(gf = ifelse(Year >= 2020 & Year <=2022, 1, 0),
         reporting_diff = ifelse(Year < 2018, 1, 0))

parallel_trends_quarter <- rci_crime_quarter_pd_filtered %>%
  group_by(Quarter, treated) %>%
  summarise(
    year = first(Year),
    mean_crime = sum(Count, na.rm = TRUE), .groups = "drop")

ggplot(parallel_trends_quarter, aes(x = (Quarter), y = (mean_crime), group = treated, color = factor(treated))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Parallel Trends of Crime",
    x = "Year",
    y = "Mean Crime ",
    color = "Treated"
  ) +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal()

rci_crime_quarter_pd_filtered <- rci_crime_quarter_pd_filtered%>%
  mutate(event_time = as.numeric(Quarter - as.Date("2021-10-01")) / 90)


event_study_model_quarter <- feols((Count) ~ i(event_time, treated, ref = 0) |
                                   as.factor(POLICE_GRI) + as.factor(Quarter), 
                                 data = rci_crime_quarter_pd_filtered)

summary(event_study_model_quarter)
event_study_model_quarter$collin.var
iplot(event_study_model_quarter)

  # Is 2020 consistently spiked or 1 month for example?
# Also break RCI down by quantiles or not percentiles


# Balanced test between groups 
# standardize crime variable
# controls
  # school quality/quantity
  # 

#
#### RCI by year ####
rci_crime_pd <- merge(aggregated_crime_pd,rci_data_pd,by.x = c("POLICE_GRI","Year"),by.y=c("id","year"))

rci_crime_pd_year <- rci_crime_pd %>%
  group_by(Year) %>%
  mutate(
    rci_percentiles = list(quantile(RCI, probs = c(0.2, 0.8), na.rm = TRUE)), # Compute percentiles per year
    RCI_Group = case_when(
      RCI >= rci_percentiles[[1]][2] ~ "Top 20%",
      RCI <= rci_percentiles[[1]][1] ~ "Bottom 20%",
      TRUE ~ "Middle 60%"
    )
  ) %>%
  ungroup()

rci_crime_pd_filtered <- rci_crime_pd_year %>%
  filter(RCI_Group %in% c("Top 20%", "Bottom 20%"))

rci_crime_pd_filtered$post <- ifelse(rci_crime_pd_filtered$Year >=2021,1,0)
rci_crime_pd_filtered$rent_vote <- ifelse(rci_crime_pd_filtered$Year >=2021 & rci_crime_pd_filtered$Year <=2022,1,0)
rci_crime_pd_filtered$post_rent_vote <- ifelse(rci_crime_pd_filtered$Year >2022,1,0)
rci_crime_pd_filtered$treated <- ifelse(rci_crime_pd_filtered$RCI_Group =="Top 20%",1,0)

rci_crime_pd_filtered <- rci_crime_pd_filtered %>%
  mutate(
    phase = case_when(
      Year < 2021 ~ "Pre-Vote",           # Before rent control was voted on
      Year == 2021 ~ "Anticipation",      # Year of the vote, before implementation
      Year >= 2022 ~ "Post-Implementation" # After rent control is enforced
    ),
    phase = factor(phase, levels = c("Pre-Vote", "Anticipation", "Post-Implementation")) # Set reference level
  )
# rci_crime_pd_filtered$covid <-ifelse(rci_crime_pd_filtered$Year >=2020,1,0)


treated_nhoods_2020 <- rci_crime_pd_filtered %>%
  filter(Year == 2021, RCI_Group == "Top 20%") %>%
  pull(POLICE_GRI)  
treated_nhoods_2020

rci_crime_pd_filtered <- rci_crime_pd_filtered %>%
  mutate(treated = ifelse(POLICE_GRI %in% treated_nhoods_2020, 1, 0))

hist(rci_crime_pd_filtered$Count)
hist(log(rci_crime_pd_filtered$Count+1))
hist(rci_crime_pd_filtered$RCI)
table(rci_crime_pd_filtered$RCI)


summary(lm(log(Count+1) ~ treated*post_rent_vote+as.factor(Year) + as.factor(POLICE_GRI), rci_crime_pd_filtered))


parallel_trends_top_bottom <- rci_crime_pd_filtered %>%
  group_by(Year, RCI_Group) %>%
  summarise(
    mean_crime_count = mean(Count, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(parallel_trends_top_bottom, aes(x = Year, y = mean_crime_count, group = RCI_Group, color = RCI_Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Parallel Trends in Crime Rates by RCI Group (2021) (Top 20% vs. Bottom 20%)",
    x = "Year",
    y = "Mean Crime Count",
    color = "RCI Group"
  ) +
  theme_minimal()


#### Event study practice #####

library(tidyverse); library(modelsummary); library(fixest)
od <- causaldata::organ_donations

od <- od %>% mutate(California = State == 'California')

# Interact quarter with being in the treated group using
# the fixest i() function, which also lets us specify
# a reference period (using the numeric version of Quarter)
clfe <- feols(Rate ~ i(Quarter_Num, California, ref = 3) | 
                State + Quarter_Num, data = od)
coefplot(clfe)

