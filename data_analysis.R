library(dplyr)
library(tidyr)
library(ggplot2)

#### rent data ####
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

#### Methods ####
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


### RCI by year ###
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



