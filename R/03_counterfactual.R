# =============================================
# Counterfactual: What would change with FireSat?
# =============================================

## =============================================
# Counterfactual: Value of FireSat
# =============================================

# pulling out the β coefficients. these are the estimated marginal effects
# of delay on each outcome. in Katz & Murphy, this is what connects
# the "quality" of information to the "value" it produces.
beta_acres <- coef(model_acres)["log_delay"]
beta_cost  <- coef(model_cost)["log_delay"]

# FireSat improves detection latency by ~2.75 hrs over VIIRS/MODIS
# (from ~3 hrs down to ~15 min). using 2.5 as the conservative end.
# this is our change in information quality.
# q from one level to a higher one 
delta_delay <- 2.5

# --- Counterfactual fire size ---
# the logic here follows V(q) = E(0) - E(q) from Chapter 6:
# E(0) is the expected outcome under current detection (what actually happened),
# E(q) is the expected outcome under improved detection (FireSat scenario).
# the difference is the value of the better information.
reg_data <- reg_data %>%
  mutate(
    # simulate what each fire's delay would have been with faster detection.
    # pmax sets a floor at 0.5 hrs 
    cf_delay           = pmax(containment_delay_hrs - delta_delay, 0.5),
    log_delay_cf       = log(cf_delay),
    
    # E(0): predicted acres under current satellite detection
    pred_acres_current = exp(fitted(model_acres)),
    
    # E(q): predicted acres if delay had been reduced by delta_delay.
    # using the β to shift the prediction
    # the change in predicted log(acres) is just β * (new log delay - old log delay)
    pred_acres_firesat = exp(fitted(model_acres) + 
                               beta_acres * (log_delay_cf - log_delay)),
    
    # V(q) = E(0) - E(q): the avoided acres from better detection
    avoided_acres      = pred_acres_current - pred_acres_firesat
  )

# --- Counterfactual suppression cost ---
# same logic, just applied to cost instead of acres.
# only using the ~1,100 fires that have cost data.
reg_cost <- reg_data %>%
  filter(has_cost == TRUE) %>%
  mutate(
    pred_cost_current = exp(fitted(model_cost)),
    pred_cost_firesat = exp(fitted(model_cost) + 
                              beta_cost * (log_delay_cf - log_delay)),
    avoided_cost      = pred_cost_current - pred_cost_firesat
  )

# --- Value summary ---
# aggregating across all fires to get total estimated benefits.
# total avoided acres and costs
# attributable to FireSat's faster detection.
value_acres <- reg_data %>%
  summarise(
    total_avoided_acres = sum(avoided_acres, na.rm = TRUE),
    avg_avoided_acres   = mean(avoided_acres, na.rm = TRUE),
    median_avoided      = median(avoided_acres, na.rm = TRUE),
    n_fires             = n()
  )

value_cost <- reg_cost %>%
  summarise(
    total_avoided_cost = sum(avoided_cost, na.rm = TRUE),
    avg_avoided_cost   = mean(avoided_cost, na.rm = TRUE),
    n_fires            = n()
  )

value_acres
# total_avoided_acres avg_avoided_acres median_avoided n_fires
# <dbl>             <dbl>          <dbl>   <int>
#   1              31606.              1.30          0.771   24400

value_cost

# total_avoided_cost avg_avoided_cost n_fires
# <dbl>            <dbl>   <int>
#   1           1522069.            1358.    1121
