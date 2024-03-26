#Load libraries#
library(tidyverse)
library(lubridate)
library(dplyr)

#Prepare data####
#load data
e13_temperatures_base <- read_excel("data/climate/sites/e13_temperatures.xlsx")

#convert to format YYYYMMDD
e13_temperatures_base$date_time <- dmy_hm(e13_temperatures_base$date_time)

#extract day of the year
e13_temperatures_base$doy <- yday(e13_temperatures_base$date_time)
e13_temperatures_base$year <- year(e13_temperatures_base$date_time)

#calculate the extremes (max and min) for each day of the year
e13_temperatures_extremes <- e13_temperatures_base %>%
  group_by(year, doy) %>%
  summarise(
    across(starts_with("E13"), list(min = min, max = max)),
    .groups = "drop"
    )%>%
  group_by(doy) %>%
  summarise(
    across(starts_with("E13"), list(mean = mean)),
    .groups = "drop"
  )%>%
  rename_with(~ paste0("min_", .x), ends_with("min_mean")) %>%
  rename_with(~ paste0("max_", .x), ends_with("max_mean")) %>%
  rename_with(~ gsub("_min_mean$|_max_mean$", "", .), -doy)

  colnames(e13_temperatures_extremes)

#calculate the mean
e13_temperatures_mean <- e13_temperatures_base %>%
  group_by(doy) %>%
  summarise(
    mean_E13A_AT = mean(E13A_AT),
    mean_E13B_AT = mean(E13B_AT),
    mean_E13C_AT = mean(E13C_AT),
    mean_E13D_AT = mean(E13D_AT),
    mean_E13A_ST = mean(E13A_ST),
    mean_E13B_ST = mean(E13B_ST),
    mean_E13C_ST = mean(E13C_ST),
    mean_E13D_ST = mean(E13D_ST)
  )

e13_temperatures <- left_join(e13_temperatures_extremes, e13_temperatures_mean, by = "doy")

#AIR TEMPERATURE############################################################
#extract only temperatures from air)
e13_temperatures_air <- e13_temperatures %>%
  select(doy, 
         ends_with("_AT"), 
         )

# Reshape the data into a longer format
e13_temperatures_air_long <- e13_temperatures_air %>%
  pivot_longer(
    cols = starts_with(c("min_", "max_", "mean_")),
    names_to = c(".value", "site"),
    names_sep = "_"
  ) %>%
  mutate(site = substr(site, 1, 4)) %>%
  rename(day_of_year = doy) %>%
  pivot_longer(
    cols = c(min, max, mean),
    names_to = "temperature_stat",
    values_to = "temperature"
  )

#Summary for all sites and stats####
# Create separate plots for min, max, and mean temperatures
ggplot(e13_temperatures_air_long, aes(x = day_of_year, y = temperature, color = site)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.2, alpha = 0.6)+
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2, aes(group = site)) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#CC79A7", "#56B4E9", "#009E73"))+
  labs(color = "Site", x = "Day of the Year", y = "Maximun Air Temperature (ºC)") +
  facet_wrap(~ temperature_stat, scales = "free_y", ncol = 1,
             labeller = labeller(temperature_stat = c(min = "Minimum Air Temperature", 
                                                      max = "Maximum Air Temperature", 
                                                      mean = "Temperature Air Temperature")))+
  coord_cartesian(ylim = range(e13_temperatures_air_long$temperature))

ggsave(filename = "temperature_sites.pdf", path = "figures/climate/air_temperature/", device = "pdf", 
       width = 7, height = 10, units = "in", dpi = 320)  

#Mean Temperature####
# Filter the data to include only mean temperatures
mean_e13_temperatures_air_long <- e13_temperatures_air_long %>%
  filter(temperature_stat == "mean")

# Create separate plots for mean temperatures of each site separated
ggplot(mean_e13_temperatures_air_long, aes(x = day_of_year, y = temperature, color = site)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#CC79A7", "#56B4E9", "#009E73")) +
  labs(color = "Site", x = "Day of the Year", y = "Mean Air Temperature (ºC)") +
  facet_wrap(~ site, scales = "free_y", ncol = 2) +
  scale_x_continuous(limits = range(mean_e13_temperatures_air_long$day_of_year), expand = c(0.01,0))+
  scale_y_continuous(limits = range(mean_e13_temperatures_air_long$temperature), expand = c(0.01,0))+
  guides(color = FALSE)
  
ggsave(filename = "mean_temperature_sites.jpg", path = "figures/climate/air_temperature/", device = "jpg", 
       width = 15, height = 7, units = "in", dpi = 320)

# Create separate plots for mean temperatures of each site together
ggplot(mean_e13_temperatures_air_long, aes(x = day_of_year, y = temperature, color = site)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#CC79A7", "#56B4E9", "#009E73")) +
  labs(color = "Site", x = "Day of the Year", y = "Mean Air Temperature (ºC)") +
  scale_x_continuous(limits = range(mean_e13_temperatures_air_long$day_of_year), expand = c(0.01,0))+
  scale_y_continuous(limits = range(mean_e13_temperatures_air_long$temperature), expand = c(0,0))

ggsave(filename = "mean_temperature_sites_together.jpg", path = "figures/climate/air_temperature/", 
       device = "jpg", 
       width = 15, height = 7, units = "in", 
       dpi = 320)  

#Max Temp ####
# Filter the data to include only mean temperatures
max_e13_temperatures_air_long <- e13_temperatures_air_long %>%
  filter(temperature_stat == "max")

# Create separate plots for mean temperatures of each site separated
ggplot(max_e13_temperatures_air_long, aes(x = day_of_year, y = temperature, color = site)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#CC79A7", "#56B4E9", "#009E73")) +
  labs(color = "Site", x = "Day of the Year", y = "Maximum Air Temperature (ºC)") +
  facet_wrap(~ site, scales = "free_y", ncol = 2) +
  scale_x_continuous(limits = range(max_e13_temperatures_air_long$day_of_year), expand = c(0.01,0))+
  scale_y_continuous(limits = range(max_e13_temperatures_air_long$temperature), expand = c(0.02,0))+
  guides(color = FALSE)

ggsave(filename = "max_temperature_sites.jpg", path = "figures/climate/air_temperature/", device = "jpg", 
       width = 15, height = 7, units = "in", dpi = 320)

# Create separate plots for mean temperatures of each site together
ggplot(max_e13_temperatures_air_long, aes(x = day_of_year, y = temperature, color = site)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#CC79A7", "#56B4E9", "#009E73")) +
  labs(color = "Sites", x = "Day of the Year", y = "Maximum Air Temperature (ºC)") +
  scale_x_continuous(limits = range(max_e13_temperatures_air_long$day_of_year), expand = c(0.01,0))+
  scale_y_continuous(limits = range(max_e13_temperatures_air_long$temperature), expand = c(0.02,0))

ggsave(filename = "max_temperature_sites_together.jpg", path = "figures/climate/air_temperature/", 
       device = "jpg", 
       width = 15, height = 7, units = "in", 
       dpi = 320)

#Min Temp####
# Filter the data to include only mean temperatures
min_e13_temperatures_air_long <- e13_temperatures_air_long %>%
  filter(temperature_stat == "min")

# Create separate plots for mean temperatures of each site separated
ggplot(min_e13_temperatures_air_long, aes(x = day_of_year, y = temperature, color = site)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#CC79A7", "#56B4E9", "#009E73")) +
  labs(color = "Site", x = "Day of the Year", y = "Maximum Air Temperature (ºC)") +
  facet_wrap(~ site, scales = "free_y", ncol = 2) +
  scale_x_continuous(limits = range(min_e13_temperatures_air_long$day_of_year), expand = c(0.01,0))+
  scale_y_continuous(limits = range(min_e13_temperatures_air_long$temperature), expand = c(0.02,0))+
  guides(color = FALSE)

ggsave(filename = "min_temperature_sites.jpg", path = "figures/climate/air_temperature/", 
       device = "jpg", 
       width = 15, height = 7, units = "in", 
       dpi = 320)

# Create separate plots for mean temperatures of each site together
ggplot(min_e13_temperatures_air_long, aes(x = day_of_year, y = temperature, color = site)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#CC79A7", "#56B4E9", "#009E73")) +
  labs(color = "Site", x = "Day of the Year", y = "Minimum Air Temperature (ºC)") +
  scale_x_continuous(limits = range(min_e13_temperatures_air_long$day_of_year), expand = c(0.01,0))+
  scale_y_continuous(limits = range(min_e13_temperatures_air_long$temperature), expand = c(0.02,0))
  
ggsave(filename = "min_temperature_sites_together.jpg", path = "figures/climate/air_temperature/", 
       device = "jpg", 
       width = 15, height = 7, units = "in", 
       dpi = 320)

#By site####
#E13A
# Filter the data to include only mean temperatures
e13a_temperatures_air_long <- e13_temperatures_air_long %>%
  filter(site == "E13A")

# Create separate plots for mean temperatures of each site separated
ggplot(e13a_temperatures_air_long, aes(x = day_of_year, y = temperature, color = temperature_stat)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#009E73", "#56B4E9"),
                      labels = c("Maximum", "Mean", "Minimum")) +
  labs(color = "Temperature", x = "Day of the Year", y = "E13A Air Temperatures (ºC)") +
  scale_x_continuous(limits = range(e13a_temperatures_air_long$day_of_year), expand = c(0.01,0))+
  scale_y_continuous(limits = c(-25,30), expand = c(0,0))

ggsave(filename = "e13a_temperatures.jpg", path = "figures/climate/air_temperature/", device = "jpg", 
       width = 15, height = 7, units = "in", dpi = 320)

#E13B
# Filter the data to include only mean temperatures
e13b_temperatures_air_long <- e13_temperatures_air_long %>%
  filter(site == "E13B")

# Create separate plots for mean temperatures of each site separated
ggplot(e13b_temperatures_air_long, aes(x = day_of_year, y = temperature, color = temperature_stat)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#009E73", "#56B4E9"),
                      labels = c("Maximum", "Mean", "Minimum")) +
  labs(color = "Temperature", x = "Day of the Year", y = "E13B Air Temperatures (ºC)") +
  scale_x_continuous(limits = range(e13b_temperatures_air_long$day_of_year), expand = c(0.01,0))+
  scale_y_continuous(limits = c(-25,30), expand = c(0,0))
#guides(color = FALSE)

ggsave(filename = "e13b_temperatures.jpg", path = "figures/climate/air_temperature/", device = "jpg", 
       width = 15, height = 7, units = "in", dpi = 320)

#E13C
# Filter the data to include only mean temperatures
e13c_temperatures_air_long <- e13_temperatures_air_long %>%
  filter(site == "E13C")

# Create separate plots for mean temperatures of each site separated
ggplot(e13c_temperatures_air_long, aes(x = day_of_year, y = temperature, color = temperature_stat)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#009E73", "#56B4E9"),
                      labels = c("Maximum", "Mean", "Minimum")) +
  labs(color = "Temperature", x = "Day of the Year", y = "E13C Air Temperatures (ºC)") +
  scale_x_continuous(limits = range(e13c_temperatures_air_long$day_of_year), expand = c(0.01,0))+
  scale_y_continuous(limits = c(-25,30), expand = c(0,0))

ggsave(filename = "e13c_temperatures.jpg", path = "figures/climate/air_temperature/", device = "jpg", 
       width = 15, height = 7, units = "in", dpi = 320)

#E13D
# Filter the data to include only mean temperatures
e13d_temperatures_air_long <- e13_temperatures_air_long %>%
  filter(site == "E13D")

# Create separate plots for mean temperatures of each site separated
ggplot(e13d_temperatures_air_long, aes(x = day_of_year, y = temperature, color = temperature_stat)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#009E73", "#56B4E9"),
                      labels = c("Maximum", "Mean", "Minimum")) +
  labs(color = "Temperature", x = "Day of the Year", y = "E13D Air Temperatures (ºC)") +
  scale_x_continuous(limits = range(e13d_temperatures_air_long$day_of_year), expand = c(0.01,0))+
  scale_y_continuous(limits = c(-25,30), expand = c(0,0))

ggsave(filename = "e13d_temperatures.jpg", path = "figures/climate/air_temperature/", device = "jpg", 
       width = 15, height = 7, units = "in", dpi = 320)
#Soil TEMPERATURE###########################
#extract only temperatures from soil
e13_temperatures_soil <- e13_temperatures %>%
  select(doy, 
         ends_with("_ST"), 
  )
# Reshape the data into a longer format
e13_temperatures_soil_long <- e13_temperatures_soil %>%
  pivot_longer(
    cols = starts_with(c("min_", "max_", "mean_")),
    names_to = c(".value", "site"),
    names_sep = "_"
    ) %>%
  mutate(site = substr(site, 1, 4)) %>%
  rename(day_of_year = doy) %>%
  pivot_longer(
    cols = c(min, max, mean),
    names_to = "temperature_stat",
    values_to = "temperature"
    )

#Summary for all sites and stats####
# Create separate plots for min, max, and mean temperatures
ggplot(e13_temperatures_soil_long, aes(x = day_of_year, y = temperature, color = site)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.2, alpha = 0.6)+
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2, aes(group = site)) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#CC79A7", "#56B4E9", "#009E73"))+
  labs(color = "Site", x = "Day of the Year", y = "Temperature (ºC)") +
  facet_wrap(~ temperature_stat, scales = "free_y", ncol = 1,
             labeller = labeller(temperature_stat = c(min = "Minimum Soil Temperature", 
                                                      max = "Maximum Soil Temperature", 
                                                      mean = "Mean Soil Temperature")))+
  coord_cartesian(ylim = range(e13_temperatures_soil_long$temperature))

ggsave(filename = "temperature_sites.pdf", path = "figures/climate/soil_temperature/", device = "pdf", 
       width = 7, height = 10, units = "in", dpi = 320)  

#Mean Temperature####
# Filter the data to include only mean temperatures
mean_e13_temperatures_soil_long <- e13_temperatures_soil_long %>%
  filter(temperature_stat == "mean")

# Create separate plots for mean temperatures of each site separated
ggplot(mean_e13_temperatures_soil_long, aes(x = day_of_year, y = temperature, color = site)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#CC79A7", "#56B4E9", "#009E73")) +
  labs(color = "Site", x = "Day of the Year", y = "Mean Soil Temperature (ºC)") +
  facet_wrap(~ site, scales = "free_y", ncol = 2) +
  scale_x_continuous(limits = range(mean_e13_temperatures_soil_long$day_of_year), expand = c(0.01,0))+
  scale_y_continuous(limits = range(mean_e13_temperatures_soil_long$temperature), expand = c(0.01,0))+
  guides(color = FALSE)

ggsave(filename = "mean_temperature_sites.jpg", path = "figures/climate/soil_temperature/", device = "jpg", 
       width = 15, height = 7, units = "in", dpi = 320)

# Create separate plots for mean temperatures of each site together
ggplot(mean_e13_temperatures_soil_long, aes(x = day_of_year, y = temperature, color = site)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#CC79A7", "#56B4E9", "#009E73")) +
  labs(color = "Site", x = "Day of the Year", y = "Mean Soil Temperature (ºC)") +
  scale_x_continuous(limits = range(mean_e13_temperatures_soil_long$day_of_year), expand = c(0.01,0))+
  scale_y_continuous(limits = range(mean_e13_temperatures_soil_long$temperature), expand = c(0,0))

ggsave(filename = "mean_temperature_sites_together.jpg", path = "figures/climate/soil_temperature/", 
       device = "jpg", 
       width = 15, height = 7, units = "in", 
       dpi = 320)  

#Max Temp ####
# Filter the data to include only mean temperatures
max_e13_temperatures_soil_long <- e13_temperatures_soil_long %>%
  filter(temperature_stat == "max")

# Create separate plots for mean temperatures of each site separated
ggplot(max_e13_temperatures_soil_long, aes(x = day_of_year, y = temperature, color = site)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#CC79A7", "#56B4E9", "#009E73")) +
  labs(color = "Site", x = "Day of the Year", y = "Maximum Soil Temperature (ºC)") +
  facet_wrap(~ site, scales = "free_y", ncol = 2) +
  scale_x_continuous(limits = range(max_e13_temperatures_soil_long$day_of_year), expand = c(0.01,0))+
  scale_y_continuous(limits = range(max_e13_temperatures_soil_long$temperature), expand = c(0.02,0))+
  guides(color = FALSE)

ggsave(filename = "max_temperature_sites.jpg", path = "figures/climate/soil_temperature/", device = "jpg", 
       width = 15, height = 7, units = "in", dpi = 320)

# Create separate plots for mean temperatures of each site together
ggplot(max_e13_temperatures_soil_long, aes(x = day_of_year, y = temperature, color = site)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#CC79A7", "#56B4E9", "#009E73")) +
  labs(color = "Sites", x = "Day of the Year", y = "Maximum Soil Temperature (ºC)") +
  scale_x_continuous(limits = range(max_e13_temperatures_soil_long$day_of_year), expand = c(0.01,0))+
  scale_y_continuous(limits = range(max_e13_temperatures_soil_long$temperature), expand = c(0.02,0))

ggsave(filename = "max_temperature_sites_together.jpg", path = "figures/climate/soil_temperature/", 
       device = "jpg", 
       width = 15, height = 7, units = "in", 
       dpi = 320)

#Min Temp####
# Filter the data to include only mean temperatures
min_e13_temperatures_soil_long <- e13_temperatures_soil_long %>%
  filter(temperature_stat == "min")

# Create separate plots for mean temperatures of each site separated
ggplot(min_e13_temperatures_soil_long, aes(x = day_of_year, y = temperature, color = site)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#CC79A7", "#56B4E9", "#009E73")) +
  labs(color = "Site", x = "Day of the Year", y = "Maximum Soil Temperature (ºC)") +
  facet_wrap(~ site, scales = "free_y", ncol = 2) +
  scale_x_continuous(limits = range(min_e13_temperatures_soil_long$day_of_year), expand = c(0.01,0))+
  scale_y_continuous(limits = range(min_e13_temperatures_soil_long$temperature), expand = c(0.02,0))+
  guides(color = FALSE)

ggsave(filename = "min_temperature_sites.jpg", path = "figures/climate/soil_temperature/", 
       device = "jpg", 
       width = 15, height = 7, units = "in", 
       dpi = 320)

# Create separate plots for mean temperatures of each site together
ggplot(min_e13_temperatures_soil_long, aes(x = day_of_year, y = temperature, color = site)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#CC79A7", "#56B4E9", "#009E73")) +
  labs(color = "Site", x = "Day of the Year", y = "Minimum Soil Temperature (ºC)") +
  scale_x_continuous(limits = range(min_e13_temperatures_soil_long$day_of_year), expand = c(0.01,0))+
  scale_y_continuous(limits = range(min_e13_temperatures_soil_long$temperature), expand = c(0.02,0))

ggsave(filename = "min_temperature_sites_together.jpg", path = "figures/climate/soil_temperature/", 
       device = "jpg", 
       width = 15, height = 7, units = "in", 
       dpi = 320)

#By site####
#E13A
# Filter the data to include only mean temperatures
e13a_temperatures_soil_long <- e13_temperatures_soil_long %>%
  filter(site == "E13A")

# Create separate plots for mean temperatures of each site separated
ggplot(e13a_temperatures_soil_long, aes(x = day_of_year, y = temperature, color = temperature_stat)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#009E73", "#56B4E9"),
                      labels = c("Maximum", "Mean", "Minimum")) +
  labs(color = "Temperature", x = "Day of the Year", y = "E13A Soil Temperatures (ºC)") +
  scale_x_continuous(limits = range(e13a_temperatures_soil_long$day_of_year), expand = c(0.01,0))+
  scale_y_continuous(limits = c(-25,30), expand = c(0,0))

ggsave(filename = "e13a_temperatures.jpg", path = "figures/climate/soil_temperature/", device = "jpg", 
       width = 15, height = 7, units = "in", dpi = 320)

#E13B
# Filter the data to include only mean temperatures
e13b_temperatures_soil_long <- e13_temperatures_soil_long %>%
  filter(site == "E13B")

# Create separate plots for mean temperatures of each site separated
ggplot(e13b_temperatures_soil_long, aes(x = day_of_year, y = temperature, color = temperature_stat)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#009E73", "#56B4E9"),
                      labels = c("Maximum", "Mean", "Minimum")) +
  labs(color = "Temperature", x = "Day of the Year", y = "E13B Soil Temperatures (ºC)") +
  scale_x_continuous(limits = range(e13b_temperatures_soil_long$day_of_year), expand = c(0.01,0))+
  scale_y_continuous(limits = c(-25,30), expand = c(0,0))
#guides(color = FALSE)

ggsave(filename = "e13b_temperatures.jpg", path = "figures/climate/soil_temperature/", device = "jpg", 
       width = 15, height = 7, units = "in", dpi = 320)

#E13C
# Filter the data to include only mean temperatures
e13c_temperatures_soil_long <- e13_temperatures_soil_long %>%
  filter(site == "E13C")

# Create separate plots for mean temperatures of each site separated
ggplot(e13c_temperatures_soil_long, aes(x = day_of_year, y = temperature, color = temperature_stat)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#009E73", "#56B4E9"),
                      labels = c("Maximum", "Mean", "Minimum")) +
  labs(color = "Temperature", x = "Day of the Year", y = "E13C Soil Temperatures (ºC)") +
  scale_x_continuous(limits = range(e13c_temperatures_soil_long$day_of_year), expand = c(0.01,0))+
  scale_y_continuous(limits = c(-25,30), expand = c(0,0))

ggsave(filename = "e13c_temperatures.jpg", path = "figures/climate/soil_temperature/", device = "jpg", 
       width = 15, height = 7, units = "in", dpi = 320)

#E13D
# Filter the data to include only mean temperatures
e13d_temperatures_soil_long <- e13_temperatures_soil_long %>%
  filter(site == "E13D")

# Create separate plots for mean temperatures of each site separated
ggplot(e13d_temperatures_soil_long, aes(x = day_of_year, y = temperature, color = temperature_stat)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_line(size = 0.2) +
  geom_point(shape = 21, fill = "white", size = 1) +
  geom_smooth(se = FALSE, size = 0.7, span = 0.2) +
  theme_minimal() +
  scale_colour_manual(values =  c("#E69F00", "#009E73", "#56B4E9"),
                      labels = c("Maximum", "Mean", "Minimum")) +
  labs(color = "Temperature", x = "Day of the Year", y = "E13D Soil Temperatures (ºC)") +
  scale_x_continuous(limits = range(e13d_temperatures_soil_long$day_of_year), expand = c(0.01,0))+
  scale_y_continuous(limits = c(-25,30), expand = c(0,0))

ggsave(filename = "e13d_temperatures.jpg", path = "figures/climate/soil_temperature/", device = "jpg", 
       width = 15, height = 7, units = "in", dpi = 320)
