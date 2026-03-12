##Inconsistency in streak logic ----
# Only participate events matter for streak updates
streak_audit_correct <- wannagive %>%
  filter(
    date(timestamp) >= as.Date("2024-12-03"),
    date(timestamp) <= as.Date("2024-12-24"),
    event == "participate"                     # ← critical change
  ) %>%
  mutate(activity_date = as.Date(timestamp)) %>%
  group_by(unique_customer_id, activity_date) %>%
  slice_tail(n = 1) %>%                        # if multiple participates same day, take last
  ungroup() %>%
  select(unique_customer_id, activity_date, current_streak_value) %>%
  arrange(unique_customer_id, activity_date)

# Now detect gaps & inconsistencies
streak_gap_analysis_correct <- streak_audit_correct %>%
  group_by(unique_customer_id) %>%
  arrange(activity_date) %>%
  mutate(
    prev_date         = lag(activity_date),
    prev_streak       = lag(current_streak_value),
    days_gap          = as.numeric(activity_date - prev_date),
    missed_day        = days_gap > 1 | is.na(days_gap),  # first row has NA gap
    streak_after_gap  = ifelse(missed_day, current_streak_value, NA_real_),
    
    # More complete inconsistency definition
    inconsistent_streak = missed_day & 
      !is.na(current_streak_value) & 
      current_streak_value > 1          # still >1 after skip
    # You can also add: | current_streak_value == prev_streak (stayed same)
  ) %>%
  ungroup()

# Summaries 
streak_inconsistency_summary_correct <- streak_gap_analysis_correct %>%
  filter(missed_day) %>%
  summarise(
    total_gap_events       = n(),
    inconsistent_events    = sum(inconsistent_streak, na.rm = TRUE),
    pct_inconsistent       = round(inconsistent_events / total_gap_events * 100, 1),
    affected_customers     = n_distinct(unique_customer_id[inconsistent_streak])
  )

customer_inconsistency_correct <- streak_gap_analysis_correct %>%
  group_by(unique_customer_id) %>%
  summarise(
    had_gap                = any(missed_day, na.rm = TRUE),
    had_inconsistent_streak = any(inconsistent_streak, na.rm = TRUE),
    .groups = "drop"
  )

# Results
print(streak_inconsistency_summary_correct)
table(customer_inconsistency_correct$had_inconsistent_streak)
mean(customer_inconsistency_correct$had_inconsistent_streak, na.rm = TRUE) * 100
mean(master$max_streak)
median(master$max_streak)

streak_gap_analysis_correct %>%
  filter(inconsistent_streak) %>%
  summarise(
    avg_extra_levels = mean(current_streak_value - 1, na.rm = TRUE),
    median_extra     = median(current_streak_value - 1, na.rm = TRUE)
  )

inconsistent_users <- customer_inconsistency_correct %>%
  filter(had_inconsistent_streak) %>%
  pull(unique_customer_id)

# Selections (claimed streak cards)
selections <- wannagive %>%
  filter(unique_customer_id %in% inconsistent_users,
         event == "collectable_interaction",
         action == "selected",
         str_detect(collectable_value, "NIEUWJAARSKORTING"))

num_selected <- n_distinct(selections$unique_customer_id)

# Usage in transactions (actual claims)
usage <- transaction %>%
  inner_join(selections %>% select(unique_customer_id, collectable_value),
             by = c("unique_customer_id" = "unique_customer_id", "coupon_code" = "collectable_value")) %>%
  filter(coupon_discount > 0)

num_used <- n_distinct(usage$unique_customer_id)
actual_leakage_sum <- sum(usage$coupon_discount, na.rm = TRUE)

cat("Affected users who selected a streak card:", num_selected, "\n")
cat("Affected users who used a streak card in transactions:", num_used, "\n")
cat("Actual revenue leakage (€):", actual_leakage_sum, "\n")

# Define the streak map (code to value)
streak_map <- tibble(
  code = c("NIEUWJAARSKORTING1", "NIEUWJAARSKORTING2", "NIEUWJAARSKORTING3", "NIEUWJAARSKORTING4", "NIEUWJAARSKORTING5"),
  value = c(5, 10, 15, 20, 25)
)

# Join selections to map and sum values
potential <- selections %>%
  left_join(streak_map, by = c("collectable_value" = "code")) %>%
  summarise(potential_loss = sum(value, na.rm = TRUE))

# Print the result
print(potential)