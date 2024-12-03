library(dplyr)
library(tidyr)
library(ggplot2)

taxes_transit <- minneapolis_transit %>%
  group_by(NameTransi,year)%>%
  summarise(EMV_LAND = mean(EMV_LAND,na.rm=T),
            EMV_BLDG = mean(EMV_BLDG,na.rm=T),
            ESTIMATED_MARKET_VALUE = mean(ESTIMATED_MARKET_VALUE,na.rm=T),
            LOCAL_NET_TAX_CAPACITY_RATE = mean(LOCAL_NET_TAX_CAPACITY_RATE, na.rm = T),
            city_tax_share = mean(city_tax_share, na.rm = T))

# Create a line plot
ggplot(taxes_transit, aes(x = as.character(year), group = NameTransi, color = NameTransi)) +
  geom_line(aes(y = city_tax_share), size = 1) +
  labs(
    title = "Summary of Property and Tax Metrics Over Time",
    x = "Year",
    y = "Value",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(taxes_transit, aes(x = as.character(year), group = NameTransi, color = NameTransi)) +
  # geom_line(aes(y = EMV_LAND), size = 1) +
  # geom_line(aes(y = EMV_BLDG), size = 1) +
  geom_line(aes(y = ESTIMATED_MARKET_VALUE,), size = 1) +
  #geom_line(aes(y = LOCAL_NET_TAX_CAPACITY_RATE), size = 1) +
  labs(
    title = "Summary of Property and Tax Metrics Over Time",
    x = "Year",
    y = "Value",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
