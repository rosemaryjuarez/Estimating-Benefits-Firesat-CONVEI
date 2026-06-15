
# =============================================
# Sensitivity Analysis
# =============================================

# the 2.5 hr improvement is our best guess, but FireSat hasn't launched yet
# so we don't know the exact improvement for sure. the methodology includes
# testing a range of assumptions to see how sensitive the results are.
# 

# we're tracing out V(q) at different levels of q:
# what happens to the value as the information improvement gets
# smaller or larger than our baseline assumption?

# three scenarios for sensitivity:
# conservative (1.5 hrs): maybe integration with existing systems is slow
# baseline (2.5 hrs) : our best estimate from the satellite specs
# optimistic (3.5 hrs) : FireSat performs at full spec and agencies
#   respond quickly to the improved data


#table for sensitivity scenarios
scenarios <- data.frame(
  scenario    = c("Conservative", "Baseline", "Optimistic"),
  delta_delay = c(1.5, 2.5, 3.5)
)

# running the same counterfactual logic from above, just swapping in
# each scenario's delay reduction. for each one we get a different
# V(q), a different total avoided-acres estimate.
sensitivity <- scenarios %>%
  rowwise() %>%
  mutate(
    result = list({
      reg_data %>%
        mutate(
          cf_delay_s     = pmax(containment_delay_hrs - delta_delay, 0.5),
          log_delay_cf_s = log(cf_delay_s),
          avoided_acres  = exp(fitted(model_acres)) - 
            exp(fitted(model_acres) + 
                  beta_acres * (log_delay_cf_s - log_delay))
        ) %>%
        summarise(
          total_avoided_acres = sum(avoided_acres, na.rm = TRUE),
          avg_avoided_acres   = mean(avoided_acres, na.rm = TRUE)
        )
    })
  ) %>%
  unnest(result)

# this table is the key output. If even the conservative scenario
# shows meaningful avoided acres, then the case for FireSat holds
# regardless of the exact detection improvement. thats the whole
# point of sensitivity analysis: showing that the conclusion doesn't
# come from one specific assumption.
sensitivity

# --- same thing but for suppression cost ---
sensitivity_cost <- scenarios %>%
  rowwise() %>%
  mutate(
    result = list({
      reg_data %>%
        filter(has_cost == TRUE) %>%
        mutate(
          cf_delay_s     = pmax(containment_delay_hrs - delta_delay, 0.5),
          log_delay_cf_s = log(cf_delay_s),
          avoided_cost   = exp(fitted(model_cost)) - 
            exp(fitted(model_cost) + 
                  beta_cost * (log_delay_cf_s - log_delay))
        ) %>%
        summarise(
          total_avoided_cost = sum(avoided_cost, na.rm = TRUE),
          avg_avoided_cost   = mean(avoided_cost, na.rm = TRUE)
        )
    })
  ) %>%
  unnest(result)

sensitivity_cost

# --- visualizing the range of estimates ---
# bar chart showing how benefits scale with the assumed improvement.
# if the bars grow roughly proportionally, the relationship is stable.
# if there's a huge jump between scenarios, that tells us the results
# are sensitive to the assumption and we should be cautious.
p_sens <- ggplot(sensitivity, aes(x = scenario, y = total_avoided_acres)) +
  geom_col(fill = "steelblue", width = 0.5) +
  labs(
    title = "Estimated Avoided Acres Burned Under FireSat",
    subtitle = "How results change under different detection improvement assumptions",
    x = NULL,
    y = "Total Avoided Acres"
  ) +
  scale_x_discrete(limits = c("Conservative", "Baseline", "Optimistic")) +
  theme_minimal()

ggsave("sensitivity_acres.png", p_sens, width = 8, height = 5, dpi = 300)

p_sens_cost <- ggplot(sensitivity_cost, aes(x = scenario, y = total_avoided_cost)) +
  geom_col(fill = "coral", width = 0.5) +
  labs(
    title = "Estimated Avoided Suppression Costs Under FireSat",
    subtitle = "How results change under different detection improvement assumptions",
    x = NULL,
    y = "Total Avoided Cost ($)"
  ) +
  scale_x_discrete(limits = c("Conservative", "Baseline", "Optimistic")) +
  theme_minimal()

ggsave("sensitivity_cost.png", p_sens_cost, width = 8, height = 5, dpi = 300)



value_acres
value_cost
sensitivity
sensitivity_cost
