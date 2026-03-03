# ============================================
# FireSat VOI Analysis - Data Loading Script
# ============================================

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
ggplot(wfigs_clean, aes(x = containment_delay_hrs, y = poly_GISAcres)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Time to Containment vs Fire Size",
    x = "Hours from Discovery to Containment",
    y = "Acres Burned (GIS)",
    caption = "Source: NIFC WFIGS"
  ) +
  theme_bw()

ggsave("outputs/figures/time_to_containment_vs_fire_size.png", width = 8, height = 5, dpi = 300)
# 
# RESTARTING BECAUSE FINAL ACRES BURNED ONLY HAS 2K ROWS OUT OF THE POSSIBLE 34K.
# #HAS CONTAINMENT HAS ABOUT 27K
# 
# wfigs <- wfigs %>% 
#   mutate(
#     discovery_time = mdy_hms(attr_FireDiscoveryDateTime),
#     response_time = mdy_hms(attr_InitialResponseDateTime),
#     response_delay_hrs = as.numeric(difftime(response_time, discovery_time, units = "hours"))
#   )
# 
# # Filter out weird values (negative delays, extreme outliers)
# 
# wfigs_clean <- wfigs %>%
#   filter(response_delay_hrs >= 0, response_delay_hrs < 48) #%>% 
#   filter(OBJECTID != 14630)
# 
# class(wfigs_clean)
# 
# #check 
# 
# wfigs_clean %>% 
#   select(response_delay_hrs, response_time, attr_FinalAcres, OBJECTID) %>% 
#   view()
# 
# wfigs %>%
#   summarise(
#     total = n(),
#     has_response_time = sum(!is.na(attr_InitialResponseDateTime)),
#     has_discovery_time = sum(!is.na(attr_FireDiscoveryDateTime)),
#     has_final_acres = sum(!is.na(attr_FinalAcres))
#   )
# 
# wfigs %>%
#   summarise(
#     has_containment = sum(!is.na(attr_ContainmentDateTime))
#   )
# # Scatterplot: does longer delay = bigger fire?
# 
# #cant really use this anymore because final acres has approx only 2k data entries, while containment is nearly complete
# ggplot(wfigs_clean, aes(x = response_delay_hrs, y = attr_FinalAcres)) +
#   geom_point(alpha = 0.99) +
#   geom_smooth(method = "lm", color = "red", alpha = .5) +
#   labs(
#     title = "Response Delay vs Final Fire Size",
#     x = "Hours from Discovery to Initial Response",
#     y = "Final Acres Burned",
#     caption = "Source: NIFC WFIGS"
#   ) +
#   theme_minimal()
# 
# ggsave("delay_vs_fire_size.png", width = 8, height = 5, dpi = 300)
# 
# 
# ggplotly(p)

## -------------------------
##  map version
##  -------------------------

# library(tigris)
# states_sf <- states(cb = TRUE) %>%
#   filter(!STUSPS %in% c("PR", "VI", "GU", "AS", "MP")) 
#   
# # If not, convert it back
# wfigs_clean <- st_as_sf(wfigs_clean)
# 
# wfigs_clean <- wfigs_clean %>%
#   filter(!is.na(attr_InitialLongitude), !is.na(attr_InitialLatitude)) %>%
#   st_as_sf(coords = c("attr_InitialLongitude", "attr_InitialLatitude"), crs = 4326)
# 
# # Map colored by response delay
# ggplot() +
#   geom_sf(data = states_sf, fill = "grey95", color = "grey50", size = 0.2) +
#   geom_sf(data = wfigs_clean, aes(color = response_delay_hrs), size = 0.5, alpha = 0.5) +
#   scale_color_gradient(low = "yellow", high = "red", name = "Response\nDelay (hrs)") +
#   labs(
#     title = "Wildfire Response Delay by Location",
#     caption = "Source: NIFC WFIGS"
#   ) +
#   theme_void()
# 
# ggsave("response_delay_map.png", width = 10, height = 6, dpi = 300)
# 
# # Or map colored by final fire size
# ggplot() +
#   #geom_sf(data = states_sf, fill = "grey95", color = "grey50", size = 0.2) +
#   geom_sf(data = wfigs_clean, aes(color = attr_FinalAcres), size = 0.5, alpha = 0.5) +
#   scale_color_gradient(low = "yellow", high = "darkred", name = "Final\nAcres") +
#   labs(
#     title = "Wildfire Size by Location",
#     caption = "Source: NIFC WFIGS"
#   ) +
#   theme_void() 
# 
# 
# 
# ggsave("fire_size_map.png", width = 10, height = 6, dpi = 300)

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
# NetCDF files (emissions, air quality grids)
# -----------------------
# Example: GFED emissions, EPA air quality

# See what's in the file
# tidync("emissions.nc")

# Pull out a specific variable
# emissions <- tidync("missions.nc") %>%
#   hyper_tibble()

# Quick exploration
# glimpse(emissions)

# -----------------------
# Data cleaning template
# -----------------------
# Once loaded, i want to standardize:

# Dates - make sure they're in date format
# incident_data <- incident_data %>%
#   mutate(
#     detection_date = as.POSIXct(detection_date, format = "%Y-%m-%d %H:%M:%S"),
#     containment_date = as.POSIXct(containment_date, format = "%Y-%m-%d %H:%M:%S")
#   )

# Column names - lowercase and snake_case
# incident_data <- incident_data %>%
#   rename_with(~ tolower(gsub(" ", "_", .x)))

# Calculate detection delay
# incident_data <- incident_data %>%
#   mutate(
#     detection_delay_hrs = as.numeric(difftime(dispatch_time, detection_time, units = "hours"))
#   )

# -----------------------
# Joining datasets
# -----------------------
# Once cleaned, join by common key (incident ID, date, location)

# combined_data <- incident_data %>%
#   left_join(cost_data, by = "incident_id") %>%
#   left_join(damage_data, by = "incident_id")