# ============================================
# 01 - Data Loading Script
# ============================================
# In Collaboration with Earth Fire Alliance and WFF CONVEI
# Rosemary Juarez and Renato Molina

#-------------------------------
# Order of script documentation
# ------------------------------

#01a - reading CSV Files
#01b - reading 

# -----------------------
# Install packages (run once)
# -----------------------
# install.packages("tidyverse")
# install.packages("sf")
# install.packages("tidync")

# -----------------------
# Load libraries
# -----------------------
library(tidyverse)
library(sf)
library(tidync)
library(here)
library(janitor)
library(plotly)
library(tidylog)


here()

################################
#     01A: Reading in Data
################################

# -----------------------
# CSV files (incident data, costs, etc.)
# -----------------------
# CAL FIRE incident data, NIFC SIT-209 exports
# this data originates from the National intraagency Fire Council. Link to the data:

wfigs <- read_csv("data/raw/WFIGS_Interagency_Perimeters.csv")

# Quick exploration

glimpse(wfigs)
summary(wfigs)

str(wfigs)

# checking number of fires by year:
# 
wfigs %>% 
  min(attr_FireDiscoveryDateTime)


# summarizing data:


summary_stats <- wfigs %>%
  summarise(
    total_fires = n(),
    total_acres = sum(attr_FinalAcres, na.rm = TRUE),
    avg_acres = mean(attr_FinalAcres, na.rm = TRUE),
    median_acres = median(attr_FinalAcres, na.rm = TRUE),
    total_cost = sum(attr_EstimatedCostToDate, na.rm = TRUE),
    avg_cost = mean(attr_EstimatedCostToDate, na.rm = TRUE)
  )



# -----------------------
# 2. Aggregate by state
# -----------------------
state_summary <- wfigs %>%
  group_by(attr_POOState) %>%
  summarise(
    fire_count = n(),
    total_acres = sum(attr_FinalAcres, na.rm = TRUE),
    avg_acres = mean(attr_FinalAcres, na.rm = TRUE),
    total_cost = sum(attr_EstimatedCostToDate, na.rm = TRUE),
    avg_cost = mean(attr_EstimatedCostToDate, na.rm = TRUE)
  ) %>%
  arrange(desc(total_acres))


#remove US from attr_POOState
state_summary <- state_summary %>% 
  mutate(attr_POOState = str_remove(attr_POOState, "US-")) %>% 
  filter(attr_POOState != "MX-CA" )

# View state summary
state_summary

# -----------------------
# 3. Map by state
# -----------------------
# Get US state boundaries
library(tigris)
options(tigris_use_cache = TRUE)

state_t <- states(cb = TRUE)

states_sf <- states(cb = TRUE) %>%
  filter(!STUSPS %in% c( "VI", "AS", "MP", "GU", "PR"))  # continental US

# Join your data to state shapes
state_map_data <- states_sf %>%
  left_join(state_summary, by = c("STUSPS" = "attr_POOState")) %>% 
  st_crop(xmin = -175, xmax = -60, ymin = 15, ymax = 75) %>% 
  st_transform(4269)


# Create map
ggplot(state_map_data) +
  geom_sf(aes(fill = avg_acres), color = "white", size = 0.2) +
  scale_fill_gradient(low = "#fee8c8", high = "#b30000", 
                      name = "Avg Acres\nBurned",
                      na.value = "grey90") +
  labs(
    title = "Average Wildfire Size by State",
    subtitle = "WFIGS Data",
    caption = "Source: NIFC WFIGS"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "white", color = "white")
  )


ggsave("outputs/figures/average_wildfire_size_by_state.png", width = 8, height = 5, dpi = 300 )


# -----------------------
# 3. fire discovery
# -----------------------


#changing the date of the discover of the fire date. including two new columns that are date time and year
wfigs <- wfigs %>%
  mutate(
    discovery_date = mdy_hms(attr_FireDiscoveryDateTime),
    discovery_year = year(discovery_date)
  )

# Annual trend
annual_trend <- wfigs %>%
  group_by(discovery_year) %>%
  summarise(
    fire_count = n(),
    total_acres = sum(attr_FinalAcres, na.rm = TRUE),
    total_cost = sum(attr_EstimatedCostToDate, na.rm = TRUE)
  ) %>%
  filter(!is.na(discovery_year))

# Plot: acres burned over time
ggplot(annual_trend, aes(x = discovery_year, y = total_acres)) +
  geom_col(fill = "#b30000", alpha = .5) +
  labs(
    title = "Total Acres Burned by Year",
    x = "Year",
    y = "Acres Burned",
    caption = "Source: NIFC WFIGS"
  ) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

ggsave("outputs/figures/total_acres_burned_by_year_histogram.png",  width = 8, height = 5, dpi = 300)
#---------------------------------
#       fire detection time and response
# --------------------------------
wfigs_clean <- wfigs %>%
mutate(
  discovery_time = mdy_hms(attr_FireDiscoveryDateTime),
  containment_time = mdy_hms(attr_ContainmentDateTime),
  containment_delay_hrs = as.numeric(difftime(containment_time, discovery_time, units = "hours"))
) %>%
  filter(
    containment_delay_hrs >= 0,  # under 30 days
    !is.na(poly_GISAcres)
  )

#PLOT
ggplot(wfigs_clean, aes(x = containment_delay_hrs, y = log(poly_GISAcres))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Time to Containment vs Fire Size",
    x = "Hours from Discovery to Containment",
    y = "Acres Burned (Log)",
    caption = "Source: NIFC WFIGS"
  ) +
  theme_bw()

ggsave("outputs/figures/time_to_containment_vs_fire_size.png", width = 8, height = 5, dpi = 300)
# 

# -----------------------
# GDB files (fire perimeters, spatial data)
# -----------------------

# List layers in the geodatabase first
# st_layers("path/to/your/fire_perimeters.gdb")

# Read a specific layer
# fire_perimeters <- st_read("path/to/your/fire_perimeters.gdb", layer = "layer_name")

# If i just need the attribute table (no geometry)
# fire_perimeters_df <- st_drop_geometry(fire_perimeters)

# Quick exploration
# glimpse(fire_perimeters)

# -----------------------
# NetCDF files (emissions, air quality grids?)
# -----------------------

#     -------------------------------------------------------------------------
#     
#     WARNING: RUNNING EMISSIONS DOWN BELOW WILL TAKE APPROX 2-5 MINUTES TO LOAD 
# 
#     -------------------------------------------------------------------------


# us bounding box
ca_lon_min <- -124
ca_lon_max <- -114
ca_lat_min <- 32
ca_lat_max <- 42

# Load 2023 and 2024 monthly data
monthly_2023 <- tidync("data/raw/GFED5.1ext_Beta/Monthly/GFED5.1ext_monthly_2023.nc", force = TRUE) %>%
  hyper_tibble() %>%
  mutate(
    lon = as.numeric(lon),
    lat = as.numeric(lat),
    time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%S"),
    year = year(time),
    month = month(time)
  ) %>%
  filter(
    lon >= us_lon_min & lon <= us_lon_max,
    lat >= us_lat_min & lat <= us_lat_max
  )

monthly_2024 <- tidync("data/raw/GFED5.1ext_Beta/Monthly/GFED5.1ext_monthly_2024.nc", force = TRUE) %>%
  hyper_tibble() %>%
  mutate(
    lon = as.numeric(lon),
    lat = as.numeric(lat),
    time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%S"),
    year = year(time),
    month = month(time)
  ) %>%
  filter(
    lon >= us_lon_min & lon <= us_lon_max,
    lat >= us_lat_min & lat <= us_lat_max
  )

# Combine both years
all_monthly_us <- bind_rows(monthly_2023, monthly_2024)

# Summary by year and month
monthly_summary <- all_monthly_us %>%
  group_by(year, month) %>%
  summarise(
    total_PM25 = sum(PM2.5, na.rm = TRUE),
    total_CO2 = sum(CO2, na.rm = TRUE),
    total_DM = sum(DM, na.rm = TRUE),
    grid_cells_with_fire = sum(DM > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, month)

monthly_summary
