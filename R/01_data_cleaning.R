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
library(tigris)
library(patchwork)
library(broom)
library(sandwich)
library(lmtest)
options(tigris_use_cache = TRUE)


here()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#          READING DATA

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# -----------------------
# CSV files 
# -----------------------
# SHIELDUS DATA
# sourced from the University of Miami

sheldus <- read_csv(here("data", "raw", "direct_loss_aggregated_output_29866.csv"))
str(sheldus)
glimpse(sheldus)

insured_sheldus <- read_csv(here("data", "raw", "insured_crop_loss_aggregated_output_29866.csv"))
str(insured_sheldus)

# NIFC wfigs data
# this data originates from the National intraagency Fire Council.
# Fire perimeter polygons and incident attributes.
#  Link to the data:

wfigs <- read_csv("data/raw/WFIGS_Interagency_Perimeters.csv")

# Quick exploration
glimpse(wfigs)
summary(wfigs)
str(wfigs)

#data cleanup process:

#changing the date of the discover of the fire date.
#including new columns that are discovery date, time,and year.
#including new columns that are containment time and deay by hrs.
clean_wfigs <- wfigs %>%
  mutate(
    discovery_time = mdy_hms(attr_FireDiscoveryDateTime),
    discovery_date = mdy_hms(attr_FireDiscoveryDateTime),
    discovery_year = year(discovery_date),
    containment_time = mdy_hms(attr_ContainmentDateTime),
    containment_delay_hrs = as.numeric(difftime(containment_time, discovery_time, units = "hours"))
  ) %>% 
  mutate(
    discovery_month = month(discovery_date))


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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 #     PRELIMINARY ANALYSIS

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# =============================================
#     mapping wildfire size in acres by state
# =============================================

#------------------------
# 1. Aggregate by state 
#------------------------
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

#----------------------------
# 2. Map by state  
#-------------------------
# Get US state boundaries from TIGRIS
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


#=================================
# 3. Total acres burned by year                 
#=================================

# Annual trend
annual_trend <- clean_wfigs %>%
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



#=============================================
#     Fire containment time vs Fire Size
#=============================================
wfigs_filter <- clean_wfigs %>%      
  filter(
    containment_delay_hrs >= 0,  # under 30 days
    !is.na(poly_GISAcres)
  )

#PLOT
ggplot(wfigs_filter, aes(x = containment_delay_hrs, y = log(poly_GISAcres))) +
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





#######################################
#          SHELDUS
#######################################

#sheldus preliminary analysis


# -----------------------
# Merge WFIGS + SHELDUS
# -----------------------


clean_wfigs %>% 
  select(attr_POOFips) %>% 
  slice_sample(n = 5)

sheldus %>% 
  select(County_FIPS) %>% 
  slice_sample(n = 5)

reg_data <- clean_wfigs %>%
  left_join(sheldus,
            by = c("attr_POOFips" = "County_FIPS",
                   "discovery_year" = "Year",
                   "discovery_month" = "Month")) %>%
  filter(
    containment_delay_hrs > 0,
    attr_FinalAcres > 0
  )  %>%
  mutate(
    log_acres       = log(attr_FinalAcres),
    log_cost        = log(attr_EstimatedCostToDate + 1),
    log_delay       = log(containment_delay_hrs),
    log_prop_dmg    = log(`PropertyDmg(ADJ 2024)` + 1),
    log_crop_dmg    = log(`CropDmg(ADJ 2024)` + 1),
    has_sheldus     = !is.na(PropertyDmg),
    fire_season     = ifelse(discovery_month %in% 6:10, 1, 0)
  )

reg_data %>%
  summarise(
    na_delay = sum(is.na(log_delay)),
    na_acres = sum(is.na(log_acres)),
    na_cost  = sum(is.na(log_cost)),
    na_state = sum(is.na(attr_POOState)),
    total    = n()
  )


# left_join: added 19 columns (StateName, CountyName, Hazard, Fatalities, FatalitiesPerCapita, …)
# > rows only in x   33,615
# > rows only in y  ( 3,571)
# > matched rows      1,322
# >                 ========
#   > rows total       34,937
#   
#   

reg_data <- clean_wfigs %>%
  left_join(sheldus,
            by = c("attr_POOFips" = "County_FIPS",
                   "discovery_year" = "Year",
                   "discovery_month" = "Month")) %>%
  mutate(
    acres = coalesce(poly_GISAcres, attr_FinalAcres),
    cost  = coalesce(attr_EstimatedCostToDate, attr_EstimatedFinalCost)
  ) %>%
  filter(
    containment_delay_hrs > 0,
    acres > 0
  ) %>%
  mutate(                    #using log transformation as histogram shows heavy skewing
    log_acres    = log(acres),
    log_cost     = log(cost + 1),
    log_delay    = log(containment_delay_hrs),
    log_prop_dmg = log(`PropertyDmg(ADJ 2024)` + 1),
    log_crop_dmg = log(`CropDmg(ADJ 2024)` + 1),
    has_sheldus  = !is.na(PropertyDmg),
    has_cost     = !is.na(cost),
    fire_season  = ifelse(discovery_month %in% 6:10, 1, 0)
  )




# --- Skewness check: raw vs log-transformed distributions ---

p1 <- ggplot(reg_data, aes(x = acres)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "Raw Acres", x = "Acres", y = "Count") +
  theme_minimal()

p2 <- ggplot(reg_data, aes(x = log_acres)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "Log Acres", x = "Log(Acres)", y = "Count") +
  theme_minimal()

p3 <- ggplot(reg_data, aes(x = containment_delay_hrs)) +
  geom_histogram(bins = 50, fill = "coral", color = "white") +
  labs(title = "Raw Delay (hrs)", x = "Hours", y = "Count") +
  theme_minimal()

p4 <- ggplot(reg_data, aes(x = log_delay)) +
  geom_histogram(bins = 50, fill = "coral", color = "white") +
  labs(title = "Log Delay", x = "Log(Hours)", y = "Count") +
  theme_minimal()

skew_plot <- (p1 + p2) / (p3 + p4) +
  plot_annotation(title = "Distribution Check for Log Transformation")

plot(skew_plot)

ggsave("skewness_check.png", skew_plot, width = 10, height = 7, dpi = 300)
