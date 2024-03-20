# Load required libraries
library(tidyverse)
library(lubridate)
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)

#load 
e13_temperatures <- read_excel("data/climate/sites/e13_temperatures.xlsx")

# Convert date_time to proper date format
e13_temperatures$date_time <- dmy_hm(e13_temperatures$date_time)

# Extract day of the year
e13_temperatures$day_of_year <- yday(e13_temperatures$date_time)

# Aggregate data by day of the year for each site
e13_temperatures_air <- e13_temperatures %>%
  group_by(day_of_year) %>%
  summarise(
    min_E13A_AT = min(E13A_AT), max_E13A_AT = max(E13A_AT), mean_E13A_AT = mean(E13A_AT),
    min_E13B_AT = min(E13B_AT), max_E13B_AT = max(E13B_AT), mean_E13B_AT = mean(E13B_AT),
    min_E13C_AT = min(E13C_AT), max_E13C_AT = max(E13C_AT), mean_E13C_AT = mean(E13C_AT),
    min_E13D_AT = min(E13D_AT), max_E13D_AT = max(E13D_AT), mean_E13D_AT = mean(E13D_AT)
  )

# Plotting
#Data by site
#E13A
ggplot(e13_temperatures_air, aes(x = day_of_year)) +
  geom_line(aes(y = min_E13A_AT, color = "Min Temp"), size = 0.5) +
  geom_line(aes(y = max_E13A_AT, color = "Max Temp"), size = 0.5) +
  geom_line(aes(y = mean_E13A_AT, color = "Mean Temp"), size = 0.5) +
  scale_color_manual(values = c("#D55E00", "#009E73", "#56B4E9")) +
  labs(x = "Day of the Year", y = "Temperature (°C)",
       #title = "Temperature Data by Day of the Year for E13A",
       color = "E13A") +
  theme_minimal()

ggsave("figures/climate/e13a.jpg", width = 7, height = 4)

#E13B
ggplot(e13_temperatures_air, aes(x = day_of_year)) +
  geom_line(aes(y = min_E13B_AT, color = "Min Temp"), size = 0.5) +
  geom_line(aes(y = max_E13B_AT, color = "Max Temp"), size = 0.5) +
  geom_line(aes(y = mean_E13B_AT, color = "Mean Temp"), size = 0.5) +
  scale_color_manual(values = c("#D55E00", "#009E73", "#56B4E9")) +
  labs(x = "Day of the Year", y = "Temperature (°C)",
       #title = "Temperature Data by Day of the Year for E13B",
       color = "E13B") +
  theme_minimal()
ggsave("figures/climate/e13b.jpg", width = 7, height = 4)

#E13C
ggplot(e13_temperatures_air, aes(x = day_of_year)) +
  geom_line(aes(y = min_E13C_AT, color = "Min Temp"), size = 0.5) +
  geom_line(aes(y = max_E13C_AT, color = "Max Temp"), size = 0.5) +
  geom_line(aes(y = mean_E13C_AT, color = "Mean Temp"), size = 0.5) +
  scale_color_manual(values = c("#D55E00", "#009E73", "#56B4E9")) +
  labs(x = "Day of the Year", y = "Temperature (°C)",
       #title = "Temperature Data by Day of the Year for E13C",
       color = "E13C") +
  theme_minimal()
ggsave("figures/climate/e13c.jpg", width = 7, height = 4)

#E13D
ggplot(e13_temperatures_air, aes(x = day_of_year)) +
  geom_line(aes(y = min_E13D_AT, color = "Min Temp"), size = 0.5) +
  geom_line(aes(y = max_E13D_AT, color = "Max Temp"), size = 0.5) +
  geom_line(aes(y = mean_E13D_AT, color = "Mean Temp"), size = 0.5) +
  scale_color_manual(values = c("#D55E00", "#009E73", "#56B4E9")) +
  labs(x = "Day of the Year", y = "Temperature (°C)",
       #title = "Temperature Data by Day of the Year for E13D",
       color = "E13D") +
  theme_minimal()
ggsave("figures/climate/e13d.jpg", width = 7, height = 4)

####Site comparison###

#Min temperature
ggplot(e13_temperatures_air, aes(x = day_of_year)) +
  geom_line(aes(y = min_E13A_AT, color = "E13A"), size = 0.5) +
  geom_line(aes(y = min_E13B_AT, color = "E13B"), size = 0.5) +
  geom_line(aes(y = min_E13C_AT, color = "E13C"), size = 0.5) +
  geom_line(aes(y = min_E13D_AT, color = "E13D"), size = 0.5) +
  scale_color_manual(values = c("#D55E00", "#009E73", "#56B4E9","#CC79A7")) +
  labs(x = "Day of the Year", y = "Air Minumum Temperature (°C)",
       #title = "Temperature Data by Day of the Year for E13A",
       color = "Sites") +
  theme_minimal()

ggsave("figures/climate/min_temp.jpg", width = 7, height = 4)

#Max temperature
ggplot(e13_temperatures_air, aes(x = day_of_year)) +
  geom_line(aes(y = max_E13A_AT, color = "E13A"), size = 0.5) +
  geom_line(aes(y = max_E13B_AT, color = "E13B"), size = 0.5) +
  geom_line(aes(y = max_E13C_AT, color = "E13C"), size = 0.5) +
  geom_line(aes(y = max_E13D_AT, color = "E13D"), size = 0.5) +
  scale_color_manual(values = c("#D55E00", "#009E73", "#56B4E9","#CC79A7")) +
  labs(x = "Day of the Year", y = "Air Maximum Temperature (°C)",
       #title = "Temperature Data by Day of the Year for E13A",
       color = "Sites") +
  theme_minimal()

ggsave("figures/climate/max_temp.jpg", width = 7, height = 4)

#Mean Temperature
ggplot(e13_temperatures_air, aes(x = day_of_year)) +
  geom_line(aes(y = mean_E13A_AT, color = "E13A"), size = 0.5) +
  geom_line(aes(y = mean_E13B_AT, color = "E13B"), size = 0.5) +
  geom_line(aes(y = mean_E13C_AT, color = "E13C"), size = 0.5) +
  geom_line(aes(y = mean_E13D_AT, color = "E13D"), size = 0.5) +
  scale_color_manual(values = c("#D55E00", "#009E73", "#56B4E9","#CC79A7")) +
  labs(x = "Day of the Year", y = "Aire Mean Temperature (°C)",
       #title = "Temperature Data by Day of the Year for E13A",
       color = "Sites") +
  theme_minimal()

ggsave("figures/climate/mean_temp.jpg", width = 7, height = 4)

#comparison between sites################

# Convert to long format
e13_temperatures_air_long <- rbind(
  data.frame(day_of_year=e13_temperatures_air$day_of_year, 
             site="E13A", 
             min = e13_temperatures_air$min_E13A_AT, 
             max = e13_temperatures_air$max_E13A_AT, 
             mean = e13_temperatures_air$mean_E13A_AT),
  data.frame(day_of_year=e13_temperatures_air$day_of_year, 
             site="E13B", 
             min = e13_temperatures_air$min_E13B_AT, 
             max = e13_temperatures_air$max_E13B_AT, 
             mean = e13_temperatures_air$mean_E13B_AT),
  data.frame(day_of_year=e13_temperatures_air$day_of_year, 
             site="E13C", 
             min = e13_temperatures_air$min_E13C_AT, 
             max = e13_temperatures_air$max_E13C_AT, 
             mean = e13_temperatures_air$mean_E13C_AT),
  data.frame(day_of_year=e13_temperatures_air$day_of_year, 
             site="E13D", 
             min = e13_temperatures_air$min_E13D_AT, 
             max = e13_temperatures_air$max_E13D_AT, 
             mean = e13_temperatures_air$mean_E13D_AT)
)

# Plotting Mean temperature
ggplot(e13_temperatures_air_long, aes(x = mean, y = site)) +
  geom_density_ridges_gradient(scale = 2.5, rel_min_height = 0.001, fill = viridis(2048)) +
  scale_fill_viridis_c(name = "Temperature [°C]", option = "C") +
  labs(y = 'Sites', x = "Mean Temperature (°C)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 8)
  )

ggsave("figures/climate/mean_comparison.jpg", width = 7, height = 5)

#plotting minimum temperature
ggplot(e13_temperatures_air_long, aes(x = min, y = site)) +
  geom_density_ridges_gradient(scale = 2.5, rel_min_height = 0.01, fill = viridis(2048)) +
  scale_fill_viridis_c(name = "Temperature [°C]", option = "C") +
  labs(y = 'Sites', x = "Minimum Temperature (°C)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    axis.text.y = element_text(size = 8)
  )

ggsave("figures/climate/min_comparison.jpg", width = 7, height = 4)

#Plotting Maximum temperature
ggplot(e13_temperatures_air_long, aes(x = max, y = site)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, fill = viridis(2048)) +
  scale_fill_viridis_c(name = "Temperature [°C]", option = "C") +
  labs(y = 'Sites', x = "Maximum Temperature (°C)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    axis.text.y = element_text(size = 8)
  )

ggsave("figures/climate/max_comparison.jpg", width = 7, height = 4)

