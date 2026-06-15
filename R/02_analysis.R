# 02 - analysis
# =============================================
# Empirical Analysis
# =============================================


# Using the merged wfigs-sheldus dataset from the join in previous data cleaning R script.
# The goal here is to estimate how response delays affect fire outcomes,
# then use those estimates to project what would change under FireSat detection.

# --- Variable construction ---
# 
# based on the model:
# # Estimating: Y_i = α + β·log(delay) + X·γ + ε

# 
# Combining available acreage fields — poly_GISAcres is the perimeter
# measure with the best coverage, falling back to attr_FinalAcres where needed this
# will provide the most data.as for cost, process will be the same.
# Log-transforming the skewed variables.


#the following analysis was inspired by the following article:
# Machine learning estimates on the impacts of detection times on wildfire suppression costs
# https://pmc.ncbi.nlm.nih.gov/articles/PMC11578465/#notes1


#the following repository for the research above:
# https://data.mendeley.com/datasets/3smv4bv5wt/1



# ***specification testing***
# geographic, temporal, and environmental factors are considered, so choosing year

# (shown in the previous r script, not needed to run)
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
    fire_season  = ifelse(discovery_month %in% 6:10, 1, 0) ##been playing around with fire season
  )



# =============================================
#      Reduced-Form Regressions
# =============================================

# Estimating: Y_i = α + β·log(delay) + X·γ + ε

# Y = log_acres, outcome
# β·Delay = log_delay , independent variable
# X·γ = attr_POOState, discovery_year, fire_season  these are the control variables
# α = the intercept 
# ε = the error term (also automatic)


# variables are log-transformed bc fire data is heavily right-skewed. 
# a few massive fires dominate the raw distributions.
# using log on both sides means the coefficients read as elasticities:
# example: a 1% increase in delay -> X% increase in outcome

# --- Model 1: Fire size (full sample) ---
# does taking longer to respond lead to bigger fires?
# controlling for state (some states just have bigger fires due to climate),
# year (fire activity varies year to year), and season 
# (summer fires behave differently).
# "factor(attr_POOState)" just means each state gets its own baseline.
# that way we're comparing fires within similar geographies


model_acres <- lm(log_acres ~    # outcome (Y): fire size
                    log_delay +     # key independent variable: response delay (gives us β)
                    factor(attr_POOState) +   # control: state fixed effects
                    factor(discovery_year) +  # control: year fixed effects
                    fire_season,             # control: season indicator
                  data = reg_data)


# --- Model 2: Suppression cost (subsample w/ cost data) ---
# does delay make fires more expensive to fight?
# 
# only have ~1,100 fires with cost data.

model_cost <- lm(log_cost ~ 
                   log_delay +
                   fire_season +  #season indicator
                   factor(discovery_year),
                 data = reg_data %>% 
                   filter(has_cost == TRUE))

# --- Model 3: Property damage from SHELDUS (matched subset) ---
# does delay lead to more property damage?
# 
# only ~1,000 matched fires.
model_prop <- lm(log_prop_dmg ~ #property damage
                   log_delay +
                   fire_season + 
                   factor(discovery_year),
                 data = reg_data %>% 
                   filter(has_sheldus == TRUE))

# =============================================
# Checks
# =============================================

# --- Check 1: Key coefficient from each model ---
# pulling out just the delay coefficient from all three models
# to see direction, size, and significance side by side
bind_rows(
  tidy(model_acres, conf.int = TRUE) %>% 
    filter(term == "log_delay") %>% mutate(model = "Fire Size"),
  tidy(model_cost, conf.int = TRUE) %>% 
    filter(term == "log_delay") %>% mutate(model = "Suppression Cost"),
  tidy(model_prop, conf.int = TRUE) %>% 
    filter(term == "log_delay") %>% mutate(model = "Property Damage")
) %>%
  select(model, estimate, std.error, p.value, conf.low, conf.high)

#   model            estimate std.error  p.value conf.low conf.high
#   <chr>               <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
# 1 Fire Size          0.892    0.00814 0           0.876    0.908 
# 2 Suppression Cost   1.74     0.0950  1.40e-65    1.55     1.92  
# 3 Property Damage   -0.0827   0.0677  2.22e- 1   -0.216    0.0502
 

# interpretation: fire size and suppression cost both show strong positive
# relationships with delay. longer response = bigger, more expensive fires.
# property damage is not significant, monthly ?





# --- Check 2: Degrees of freedom ---
# making sure we're not using up too many parameters relative to observations.

data.frame(
  Model = c("Fire Size", "Suppression Cost", "Property Damage"),
  N = c(nobs(model_acres), nobs(model_cost), nobs(model_prop)),
  Coefficients = c(length(coef(model_acres)), length(coef(model_cost)), length(coef(model_prop))),
  DF_Residual = c(model_acres$df.residual, model_cost$df.residual, model_prop$df.residual),
  R_Squared = c(summary(model_acres)$r.squared, summary(model_cost)$r.squared, summary(model_prop)$r.squared)
)
# Model     N Coefficients DF_Residual  R_Squared
# 1        Fire Size 24400           59       24341 0.38854253
# 2 Suppression Cost  1121            9        1112 0.36250610
# 3  Property Damage  1017            7        1010 0.06468846

# interpretation: all three models have plenty of room. the smallest
# model (property damage) has 1,010 residual df for 7 coefficients.





# --- Check 3: Residual plot ---
# looking at whether the model is systematically wrong in certain ranges.
# want to see a random cloud centered on zero, not a funnel or curve.
p_resid <- ggplot(data.frame(fitted = fitted(model_acres), 
                             resid = resid(model_acres)),
                  aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.1, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted: Fire Size Model", 
       x = "Fitted Values", y = "Residuals") +
  theme_minimal()

ggsave("diagnostics_residuals.png", p_resid, width = 8, height = 5, dpi = 300)

# interpretation: residuals look evenly spread around zero, slight spread to the right.
#  no fanning
#  not sure how to interpret darker lines




# --- Check 4: standard errors ---
# default standard errors assume variance is constant across all fires.

coeftest(model_acres, vcov = vcovHC(model_acres, type = "HC1"))
coeftest(model_cost, vcov = vcovHC(model_cost, type = "HC1"))
coeftest(model_prop, vcov = vcovHC(model_prop, type = "HC1"))

# interpretation: robust SEs are nearly identical to the defaults for all 
# three models. fire size and cost stay highly significant.

