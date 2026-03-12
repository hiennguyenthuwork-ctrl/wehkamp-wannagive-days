rm(list=ls())

#Loading Libraries ----
library(dplyr)
library(tidyr)
library(lubridate)
#install.packages("janitor")  
library(janitor)
library(readr)
library(ggplot2)
library(stringr)
library(scales)

#Question 1 ----
#In total, how many unique participants were there for the Wannagive days?
unique(wannagive_campaign$event)

#This counts unique users for each event independently, without considering whether they completed prior steps
funnel_table <- wannagive_campaign %>%
  summarise(
    total_active_users = n_distinct(unique_customer_id),
    entered = n_distinct(unique_customer_id[event == "enter"]),
    onboarded = n_distinct(unique_customer_id[event == "onboarding_completed"]),
    participated = n_distinct(unique_customer_id[event == "participate"])
  )

funnel_table

#True Sequential funnel
#This counts unique users for each event but conditionally, requiring presence in the prior stage's user set.
# Step 1: Users who entered
entered_users <- wannagive_campaign %>%
  filter(event == "enter") %>%
  distinct(unique_customer_id)

# Step 2: Users who onboarded (from those who entered)
onboarded_users <- wannagive_campaign %>%
  filter(event == "onboarding_completed",
         unique_customer_id %in% entered_users$unique_customer_id) %>%
  distinct(unique_customer_id)

# Step 3: Users who participated (from those onboarded)
participated_users <- wannagive_campaign %>%
  filter(event == "participate",
         unique_customer_id %in% onboarded_users$unique_customer_id) %>%
  distinct(unique_customer_id)

true_funnel <- tibble::tibble(
  stage = c("Entered", "Onboarded", "Participated"),
  unique_customers = c(
    nrow(entered_users),
    nrow(onboarded_users),
    nrow(participated_users)
  )
)

true_funnel

#conversion metric
conversion_metrics <- true_funnel %>%
  mutate(
    conversion_rate = unique_customers / lag(unique_customers)
  )

conversion_metrics

##Understanding Streak logic ----
wannagive_campaign %>%
  filter(event == "participate") %>%
  summarise(
    pct_with_streak = mean(!is.na(current_streak_value))
  )

#current_streak_value is automatically calculated by the third party service 
#logic is the event == "participate"

unique(wannagive_campaign$action)
unique(wannagive_campaign$event)

summary(wannagive_campaign$current_streak_value)
table(wannagive_campaign$current_streak_value, useNA = "ifany")


#Create User level summary table ----
##Step 1: Base Customer Table ----
#Create base table with all unique customers for comparison between participants and nonparticipants
# Collect unique IDs from each table
all_ids <- bind_rows(
  wannagive %>% select(unique_customer_id),
  coupons %>% select(unique_customer_id),
  transaction %>% select(unique_customer_id),
  customer_info_clean %>% select(unique_customer_id)
) %>% 
  distinct()

# Check the total
nrow(all_ids)  #20668
head(all_ids)

##Step 2: Merge with Wannagive ----
# Step A: Make sure current_streak_value is numeric in the MAIN wannagive table
wannagive <- wannagive %>%
  mutate(
    current_streak_value = as.numeric(as.character(current_streak_value)),
    current_streak_value = replace(current_streak_value, 
                                   is.na(current_streak_value) | current_streak_value < 0, 
                                   0)
  )

# Step B: Confirm numeric
str(wannagive$current_streak_value)          
summary(wannagive$current_streak_value)      

# Step C: Now re-create wannagive_campaign from the updated wannagive
wannagive_campaign <- wannagive %>%
  filter(date(timestamp) >= "2024-12-03",
         date(timestamp) <= "2024-12-24")

# Step D: Re-run the summary with the fixed data
wannagive_summary <- wannagive_campaign %>%
  group_by(unique_customer_id) %>%
  summarise(
    n_enter = sum(event == "enter"),
    n_onboarded = sum(event == "onboarding_completed"),
    n_participate = sum(event == "participate"),
    n_opens = sum(event == "participate") + 
      sum(event == "collectable_interaction" & action == "opened"),
    n_claims = sum(event == "collectable_interaction" & action == "claimed"),
    n_interactions = n(),
    
    max_streak = max(current_streak_value, na.rm = TRUE),
    last_event_idx = which.max(timestamp[timestamp <= as.POSIXct("2024-12-24 23:59:59")]),
    final_streak_dec24 = if (length(last_event_idx) > 0) 
      current_streak_value[last_event_idx] 
    else NA_real_,
    
    first_engagement = min(timestamp),
    last_engagement = max(timestamp),
    first_participate = min(timestamp[event == "participate"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    is_participant = n_participate > 0,
    reached_ultimate_streak = max_streak == 22,
    max_streak = replace(max_streak, is.na(max_streak), 0),
    final_streak_dec24 = replace(final_streak_dec24, is.na(final_streak_dec24), 0)
  )

# Quick checks
nrow(wannagive_summary)          # 18922
summary(wannagive_summary$max_streak)
summary(wannagive_summary$final_streak_dec24)
table(wannagive_summary$reached_ultimate_streak)

##Step 3: Join Customer Info ----
# Join customer_info_clean to the base all_ids
master <- all_ids %>%
  left_join(customer_info_clean, by = "unique_customer_id") %>%
  # Add a flag to see join success
  mutate(
    has_customer_info = !is.na(gender)  # or any non-NA column from customer_info
  )

# Quick checks
nrow(master)                           #  still 20668
table(master$has_customer_info, useNA = "always")  # Matched - 20489
summary(as.numeric(master$orders))     # Pre-campaign order count (from customer_info)
head(master %>% select(unique_customer_id, gender, age, province, has_customer_info))

##Step 4: Join wannagive_summary to master ----
# Join wannagive_summary to master
master <- master %>%
  left_join(wannagive_summary, by = "unique_customer_id") %>%
  # Fill NA engagement metrics with 0 for non-participants
  mutate(
    across(
      c(n_enter, n_onboarded, n_participate, n_opens, n_claims, n_interactions,
        max_streak, final_streak_dec24),
      ~ replace(., is.na(.), 0)
    ),
    # Flags: if NA after join, they didn't participate
    is_participant = coalesce(is_participant, FALSE),
    reached_ultimate_streak = coalesce(reached_ultimate_streak, FALSE)
  )

# Quick checks after join
nrow(master)  # Should still be 20668

# How many are now marked as participants?
table(master$is_participant, useNA = "always")

# Check streak distribution (should match earlier summary but now includes zeros)
summary(master$max_streak)
summary(master$final_streak_dec24)

# Number who reached ultimate streak
table(master$reached_ultimate_streak)

# Look at a few rows (pick some participants and non-participants)
master %>%
  select(unique_customer_id, is_participant, max_streak, final_streak_dec24, 
         n_opens, n_claims, gender, age, orders) %>%
  slice_sample(n = 10)  # random 10 rows

##Step 5: Join the coupon summary ----
coupons_summary <- coupons %>%
  mutate(date = date(timestamp)) %>%
  # Optional: restrict to campaign window if you want strict consistency
  # filter(date >= "2024-12-02", date <= "2024-12-24")  # uncomment if desired
  group_by(unique_customer_id) %>%
  summarise(
    n_coupons_issued = n(),
    total_coupon_value = sum(value, na.rm = TRUE),
    avg_coupon_value = mean(value, na.rm = TRUE),
    # Count by value_type (e.g., Percentage vs Fixed)
    n_percentage = sum(value_type == "Percentage", na.rm = TRUE),
    n_fixed = sum(value_type == "Fixed", na.rm = TRUE),  # adjust if other types exist
    first_coupon_date = min(timestamp, na.rm = TRUE),
    last_coupon_date = max(timestamp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    has_coupon = n_coupons_issued > 0
  )

# Quick checks on coupons_summary
nrow(coupons_summary)          # 13,244 
summary(coupons_summary$n_coupons_issued)
table(coupons_summary$has_coupon)
head(coupons_summary)

master <- master %>%
  left_join(coupons_summary, by = "unique_customer_id") %>%
  mutate(
    across(
      c(n_coupons_issued, total_coupon_value, n_percentage, n_fixed),
      ~ replace(., is.na(.), 0)
    ),
    has_coupon = coalesce(has_coupon, FALSE),
    avg_coupon_value = replace(avg_coupon_value, is.na(avg_coupon_value), 0)
  )

# Quick post-join checks
nrow(master)  # still 20668
table(master$has_coupon, useNA = "always")
summary(master$n_coupons_issued)

##Step 6: Transaction summary
transaction_summary_wide <- transaction %>%
  mutate(
    date = date(order_local_timestamp),
    period = case_when(
      date < "2024-12-03"              ~ "pre",
      date >= "2024-12-03" & date <= "2024-12-24" ~ "during",
      TRUE                             ~ "post"
    )
  ) %>%
  group_by(unique_customer_id, period) %>%
  summarise(
    n_orders = n(),
    total_demand = sum(demand, na.rm = TRUE),
    avg_order_value = mean(demand, na.rm = TRUE),
    n_coupon_orders = sum(coupon_discount > 0 | !is.na(coupon_code), na.rm = TRUE),
    total_coupon_discount = sum(coupon_discount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Pivot to wide format: one column per period
  pivot_wider(
    names_from = period,
    values_from = c(n_orders, total_demand, avg_order_value, n_coupon_orders, total_coupon_discount),
    values_fill = 0,
    names_sep = "_"
  ) %>%
  # Add overall totals and simple uplift
  mutate(
    total_orders_all_time = n_orders_pre + n_orders_during + n_orders_post,
    total_demand_all_time = total_demand_pre + total_demand_during + total_demand_post,
    order_uplift_pct = if_else(n_orders_pre > 0,
                               (n_orders_post - n_orders_pre) / n_orders_pre * 100,
                               NA_real_),
    demand_uplift_pct = if_else(total_demand_pre > 0,
                                (total_demand_post - total_demand_pre) / total_demand_pre * 100,
                                NA_real_)
  )

# Quick checks
nrow(transaction_summary_wide)          # Should be ~20,014
summary(transaction_summary_wide$n_orders_pre)
summary(transaction_summary_wide$n_orders_post)
summary(transaction_summary_wide$order_uplift_pct, na.rm = TRUE)
head(transaction_summary_wide)

# Join transaction_summary_wide to master
master <- master %>%
  left_join(transaction_summary_wide, by = "unique_customer_id") %>%
  # Fill NA order/demand metrics with 0 for non-buyers
  mutate(
    across(
      starts_with(c("n_orders_", "total_demand_", "avg_order_value_", 
                    "n_coupon_orders_", "total_coupon_discount_")),
      ~ replace(., is.na(.), 0)
    ),
    # Uplift percentages: keep NA if pre=0 (no baseline)
    # But fill any unexpected NAs in totals
    total_orders_all_time = replace(total_orders_all_time, is.na(total_orders_all_time), 0),
    total_demand_all_time = replace(total_demand_all_time, is.na(total_demand_all_time), 0)
  )

# Final quick checks
nrow(master)  # still 20668

# How many customers have any orders at all?
sum(master$total_orders_all_time > 0)  #20,014

# Participant vs non-participant order behavior
master %>%
  group_by(is_participant) %>%
  summarise(
    n_customers = n(),
    avg_orders_pre = mean(n_orders_pre),
    avg_orders_during = mean(n_orders_during),
    avg_orders_post = mean(n_orders_post),
    avg_uplift_pct = mean(order_uplift_pct, na.rm = TRUE)
  )

# Quick glimpse of final columns (first 10–15)
glimpse(master)

# Or random sample of 10 rows with key columns
master %>%
  select(unique_customer_id, is_participant, reached_ultimate_streak,
         n_opens, max_streak, n_coupons_issued, has_coupon,
         n_orders_pre, n_orders_during, n_orders_post,
         order_uplift_pct, gender, age) %>%
  slice_sample(n = 10)



# QUESTION 2: Open rate & opens per day ----
# Independent counts 
funnel <- wannagive_campaign %>%
  summarise(
    Entered     = n_distinct(unique_customer_id[event == "enter"]),
    Onboarded   = n_distinct(unique_customer_id[event == "onboarding_completed"]),
    Participated = n_distinct(unique_customer_id[event == "participate"])
  )

print(funnel)

# Sequential funnel (use participated count for Q2 base)
participated_users <- wannagive_campaign %>%
  filter(event == "participate") %>%
  distinct(unique_customer_id) %>%
  pull(unique_customer_id)

total_participants <- length(participated_users)

cat("\nCUSTOMER JOURNEY (%):\n")
cat("Campaign Opens (Enter)    ", funnel$Entered, "   100%\n")
cat("Completed Onboard         ", funnel$Onboarded, "   ", round(funnel$Onboarded / funnel$Entered * 100, 1), "%\n")
cat("Participated              ", total_participants, "   ", round(total_participants / funnel$Onboarded * 100, 1), "%\n\n")

cat("→ Final Q1: ", total_participants, " unique participants (used for Q2 base)\n\n")

opens_daily <- wannagive_campaign %>%
  filter(event == "participate") %>%
  mutate(open_date = as.Date(timestamp)) %>%
  group_by(open_date) %>%
  summarise(
    total_opens   = n(),                              # Total participate events
    unique_opens  = n_distinct(unique_customer_id),   # Unique users opening
    .groups       = "drop"
  ) %>%
  # Ensure all 22 days are present (fill 0 for missing)
  right_join(
    tibble(open_date = seq(as.Date("2024-12-03"), as.Date("2024-12-24"), by = "day")),
    by = "open_date"
  ) %>%
  mutate(
    total_opens   = replace_na(total_opens, 0),
    unique_opens  = replace_na(unique_opens, 0),
    open_rate_pct = round(unique_opens / total_participants * 100, 2),
    campaign_day  = as.integer(open_date - as.Date("2024-12-03")) + 1,
    day_of_week   = wday(open_date, label = TRUE, abbr = TRUE),
    is_weekend    = wday(open_date) %in% c(1, 7),  # Sunday=1, Saturday=7
    pct_change    = if_else(lag(unique_opens) > 0,
                            round((unique_opens - lag(unique_opens)) / lag(unique_opens) * 100, 1),
                            NA_real_)
  )

# Display table
cat("Daily Opens & Open Rate Table:\n")
print(opens_daily %>%
        select(campaign_day, open_date, day_of_week, unique_opens, open_rate_pct, pct_change) %>%
        mutate(pct_change = sprintf("%.1f%%", pct_change)))

# Summary statistics for slide
cat("\nSUMMARY STATISTICS:\n")
cat("• Total participants base:      ", total_participants, "\n")
cat("• Average daily unique opens:   ", round(mean(opens_daily$unique_opens), 1), "\n")
cat("• Average open rate:            ", round(mean(opens_daily$open_rate_pct), 1), "%\n")
cat("• Weekday average open rate:    ", round(mean(opens_daily$open_rate_pct[!opens_daily$is_weekend]), 1), "%\n")
cat("• Weekend average open rate:    ", round(mean(opens_daily$open_rate_pct[opens_daily$is_weekend]), 1), "%\n")
cat("• Largest day-to-day drop:      ", min(opens_daily$pct_change, na.rm = TRUE), "%\n")


# Q3: Largest drop in opens ----
cat("Q3: Largest drop in opens\n")
opens_daily <- opens_daily %>%
  mutate(change = unique_opens - lag(unique_opens)) %>%
  filter(!is.na(change))

largest_drop <- opens_daily %>% slice_min(change, with_ties = FALSE)

print(largest_drop)
cat("\nLargest drop on", as.character(largest_drop$date), ":", abs(largest_drop$change), "opens\n\n")


# Q4: Unique claims, claim rate, best cards ----
cat("Q4: Unique claims, claim rate, best cards\n")
claims <- wannagive_campaign %>%
  filter(event == "collectable_interaction", action == "claimed") %>%
  group_by(collectable_type) %>%
  summarise(claims = n_distinct(unique_customer_id), .groups = "drop")

opens_card <- wannagive_campaign %>%
  filter(event == "participate", !is.na(collectable_type)) %>%
  group_by(collectable_type) %>%
  summarise(opens = n_distinct(unique_customer_id), .groups = "drop")

card_perf <- opens_card %>%
  left_join(claims, by = "collectable_type") %>%
  mutate(
    claims = replace_na(claims, 0),
    claim_rate = round(claims / opens * 100, 2)
  ) %>%
  arrange(desc(claim_rate))

print(card_perf)
cat("\nBest performing card:", card_perf$collectable_type[1], "with", card_perf$claim_rate[1], "%\n\n")



# Q5: Days with most effect on ordering customers ----
#Daily unique ordering customer
# 1. Daily unique ordering customers + demand/orders (transaction data)
daily_orders <- transaction %>%
  mutate(order_date = as.Date(order_local_timestamp)) %>%
  filter(order_date >= as.Date("2024-11-20"), order_date <= as.Date("2025-01-06")) %>%
  group_by(order_date) %>%
  summarise(
    unique_customers = n_distinct(unique_customer_id),
    total_orders = n(),
    total_demand = sum(demand, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    day_of_week = wday(order_date, label = TRUE),
    is_weekend = day_of_week %in% c("Sat", "Sun"),
    campaign_period = case_when(
      order_date < "2024-12-03" ~ "Pre",
      order_date <= "2024-12-24" ~ "During",
      TRUE ~ "Post"
    )
  )

# 2. Daily participate activity (card opens / engagement)
daily_activity <- wannagive %>%
  filter(event == "participate",
         date(timestamp) >= "2024-11-20",
         date(timestamp) <= "2025-01-06") %>%
  mutate(day = as.Date(timestamp)) %>%
  group_by(day) %>%
  summarise(
    participate_count = n(),
    unique_active_users = n_distinct(unique_customer_id),
    .groups = "drop"
  )

# 3. Full daily campaign events / rewards schedule (with discount flags)
campaign_daily_events <- tribble(
  ~date,         ~type,     ~description,                                      ~code,           ~flat_discount, ~flat_discount_amount, ~percentage_discount, ~percentage_rate,
  "2024-12-03",  "coupon",  "10% (extra) discount on Sinterklaas gifts",        "WANNA3",        0,              0,                     1,                   10,
  "2024-12-04",  "coupon",  "10% (extra) discount on Rituals",                  "WANNA4",        0,              0,                     1,                   10,
  "2024-12-05",  "win",     "WIN! Beauty Adventskalender",                      "WIN5",          0,              0,                     0,                   0,
  "2024-12-06",  "coupon",  "15% (extra) discount on christmas trees & decor",  "WANNA6",        0,              0,                     1,                   15,
  "2024-12-07",  "tip",     "Decorate your house for Christmas",               "TIP7",          0,              0,                     0,                   0,
  "2024-12-08",  "coupon",  "€5.- korting op alles",                           "WANNA8",        1,              5,                     0,                   0,
  "2024-12-09",  "coupon",  "10% (extra) discount on NOUS Living",             "WANNA9",        0,              0,                     1,                   10,
  "2024-12-10",  "win",     "WIN! HK Living servies-pakket",                   "WIN10",         0,              0,                     0,                   0,
  "2024-12-11",  "coupon",  "10% (extra) discount on kersttruien",             "WANNA11",       0,              0,                     1,                   10,
  "2024-12-12",  "tip",     "Christmas presents for the whole family",         "TIP12",         0,              0,                     0,                   0,
  "2024-12-13",  "win",     "WIN! 50.- shoptegoed",                            "WIN13",         0,              0,                     0,                   0,
  "2024-12-14",  "coupon",  "10% (extra) discount on Cook & Dine",             "WANNA14",       0,              0,                     1,                   10,
  "2024-12-15",  "win",     "WIN! Rituals pakket",                             "WIN15",         0,              0,                     0,                   0,
  "2024-12-16",  "coupon",  "15% (extra) discount on sieraden",                "WANNA16",       0,              0,                     1,                   15,
  "2024-12-17",  "tip",     "Christmas outfits for the whole family",          "TIP17",         0,              0,                     0,                   0,
  "2024-12-18",  "coupon",  "€5.- korting op alles",                           "WANNA18",       1,              5,                     0,                   0,
  "2024-12-19",  "coupon",  "20% (extra) discount on Miljuschka by Wehkamp",   "WANNA19",       0,              0,                     1,                   20,
  "2024-12-20",  "win",     "WIN! 400.- shoptegoed",                           "WIN20",         0,              0,                     0,                   0,
  "2024-12-21",  "coupon",  "10% (extra) discount on geuren/fragrances",      "WANNA21",       0,              0,                     1,                   10,
  "2024-12-22",  "tip",     "Christmas table inspiration",                    "TIP22",         0,              0,                     0,                   0,
  "2024-12-23",  "coupon",  "10% (extra) discount on party outfits & gifts",  "WANNA23",       0,              0,                     1,                   10,
  "2024-12-24",  "coupon",  "15% discount on family games",                   "WANNA24",       0,              0,                     1,                   15
) %>%
  mutate(
    date = as.Date(date),
    has_discount = if_else(flat_discount == 1 | percentage_discount == 1, 1, 0),
    reward_label = case_when(
      !is.na(code) & flat_discount == 1 ~ paste("€", flat_discount_amount, "off"),
      !is.na(code) & percentage_discount == 1 ~ paste(percentage_rate, "% extra"),
      type == "win" ~ "Big Win!",
      type == "tip" ~ "Tip day",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(date)

# 4. Final combined daily dataset
daily_full <- daily_orders %>%
  left_join(daily_activity, by = c("order_date" = "day")) %>%
  left_join(campaign_daily_events, by = c("order_date" = "date")) %>%
  mutate(
    participate_count = replace_na(participate_count, 0),
    unique_active_users = replace_na(unique_active_users, 0),
    reward_label = replace_na(reward_label, "No reward"),
    # Optional: flag high-value days for annotation
    is_high_value = flat_discount_amount >= 5 | percentage_rate >= 15 | type == "win"
  )

# 5. View top 10 days by unique ordering customers (with all activity)
top_days <- daily_full %>%
  arrange(desc(unique_customers)) %>%
  select(
    order_date, 
    unique_customers, 
    total_orders, 
    total_demand, 
    participate_count, 
    unique_active_users, 
    reward_label, 
    is_high_value, 
    day_of_week, 
    is_weekend, 
    campaign_period
  ) %>%
  head(15)  # show more for context

print(top_days, n = 15)


# Scaling factor for secondary axis
scale_factor <- max(daily_full$unique_customers, na.rm = TRUE) / 
  max(daily_full$unique_active_users, na.rm = TRUE)

ggplot(daily_full, aes(x = order_date)) +
  
  # BARS: Unique Ordering Customers (Primary Y-axis)
  geom_col(aes(y = unique_customers),
           fill = "#C8102E", alpha = 0.85, width = 0.8) +
  
  # LINE: Daily Participation (Secondary Y-axis, SOLID)
  geom_line(aes(y = unique_active_users * scale_factor,
                color = "Daily Participation"),
            size = 1.3, lineend = "round") +
  
  # Primary + Secondary Axis
  scale_y_continuous(
    name = "Number of Unique Ordering Customers",
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Daily Participation (Active Users)")
  ) +
  
  scale_color_manual(values = c("Daily Participation" = "#1F77B4")) +
  
  labs(
    title = "Daily Participation vs Unique Ordering Customers",
    subtitle = "Nov 20, 2024 – Jan 6, 2025",
    x = "Date",
    color = ""
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    axis.title.y = element_text(color = "#C8102E", face = "bold"),
    axis.title.y.right = element_text(color = "#1F77B4", face = "bold"),
    plot.title = element_text(face = "bold")
  )

ggplot(daily_full, aes(x = order_date)) +
  
  # Campaign period highlight (Dec 3–24)
  annotate("rect",
           xmin = as.Date("2024-12-03"),
           xmax = as.Date("2024-12-24"),
           ymin = -Inf, ymax = Inf,
           fill = "grey70", alpha = 0.15) +
  
  # Bars: Unique Ordering Customers
  geom_col(aes(y = unique_customers),
           fill = "#C8102E", alpha = 0.85, width = 0.8) +
  
  # Solid Line: Daily Participation
  geom_line(aes(y = unique_active_users * scale_factor,
                color = "Daily Participation"),
            size = 1.3) +
  
  scale_y_continuous(
    name = "Unique Ordering Customers",
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Daily Participation (Active Users)")
  ) +
  
  scale_color_manual(values = c("Daily Participation" = "#1F77B4")) +
  
  labs(
    title = "Do Participation Spikes Translate into More Ordering Customers?",
    subtitle = "Grey area = Campaign Period (Dec 3–24)",
    x = "Date",
    color = ""
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    axis.title.y = element_text(color = "#C8102E", face = "bold"),
    axis.title.y.right = element_text(color = "#1F77B4", face = "bold"),
    plot.title = element_text(face = "bold")
  )


# Q6: ULTIMATE STREAK (22) & STREAK DISTRIBUTION ----
cat("\n" %+% strrep("=", 80) %+% "\n")
cat("QUESTION 6: Customers with Ultimate Streak (22) & Streak Distribution\n")
cat(strrep("=", 80) %+% "\n\n")

# ── 1. Filter participants (only those who participated at least once) ───────
participants <- wannagive_campaign %>%
  filter(event == "participate") %>%
  distinct(unique_customer_id) %>%
  pull(unique_customer_id)

cat("Total participants (with at least one participate event):", length(participants), "\n\n")

# ── 2. Calculate max streak per user ─────────────────────────────────────────
user_max_streak <- wannagive_campaign %>%
  filter(unique_customer_id %in% participants) %>%
  group_by(unique_customer_id) %>%
  summarise(
    max_streak = max(current_streak_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(max_streak = replace_na(max_streak, 0))

# ── 3. Ultimate streak (22 days) count ───────────────────────────────────────
ultimate_22_count <- sum(user_max_streak$max_streak == 22)
pct_ultimate <- round(ultimate_22_count / nrow(user_max_streak) * 100, 2)

cat("Customers who reached ultimate streak of 22:", ultimate_22_count, "\n")
cat("Percentage:", pct_ultimate, "%\n\n")

# ── 4. Streak Distribution (count + percentage) ──────────────────────────────
streak_dist <- user_max_streak %>%
  count(max_streak) %>%
  arrange(max_streak) %>%
  mutate(percentage = round(n / sum(n) * 100, 2))

cat("Streak Distribution (0–22):\n")
print(streak_dist, n = 23)

# ── 5. Histogram of Max Streak ───────────────────────────────────────────────
ggplot(user_max_streak, aes(x = max_streak)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Maximum Streak Length",
    x = "Max Streak (days)",
    y = "Number of Users"
  ) +
  scale_x_continuous(breaks = 0:22) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Save for slide
ggsave("q6_max_streak_histogram.png", width = 8, height = 5, dpi = 300)

# ── 6. Summary Stats for Slide ───────────────────────────────────────────────
cat("\nSUMMARY FOR SLIDE:\n")
cat("• Total participants:", nrow(user_max_streak), "\n")
cat("• Users reached 22-day streak:", ultimate_22_count, "(", pct_ultimate, "%)\n")
cat("• Most common streak: ", streak_dist$max_streak[which.max(streak_dist$n)], 
    "days (", max(streak_dist$n), " users)\n")
cat("• Average max streak: ", round(mean(user_max_streak$max_streak), 1), "days\n")
cat("• Median max streak: ", median(user_max_streak$max_streak), "days\n\n")

#Q6: Additional insights ----
table(master$reached_ultimate_streak) 
sum(master$max_streak == 22, na.rm = TRUE)

#Distribution of streak
streak_dis <- master %>%
  filter(is_participant == TRUE) %>%
  count(max_streak) %>%
  arrange(max_streak) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  print(n = 23)  # show all 0–22

# Histogram of max streak
ggplot(master %>% filter(is_participant == TRUE), aes(x = max_streak)) +
  geom_histogram(binwidth = 1, fill = "steelblue") +
  labs(title = "Distribution of Maximum Streak Length", x = "Max Streak", y = "Number of Users")


#Distribution of cards opened
card_open <- master %>%
  filter(is_participant == TRUE) %>%
  count(n_opens) %>%
  arrange(n_opens) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  print(n = 30)  # adjust if more than 30 values

master %>%
  filter(is_participant == TRUE) %>%
  summarise(
    min_opens = min(n_opens),
    median_opens = median(n_opens),
    mean_opens = mean(n_opens),
    max_opens = max(n_opens)
  )

# Histogram of total opens
ggplot(master %>% filter(is_participant == TRUE), aes(x = n_opens)) +
  geom_histogram(binwidth = 5, fill = "darkgreen") +
  labs(title = "Distribution of Total Cards Opened", x = "Total Opens (n_opens)", y = "Number of Users")

#Q7: Segmentation Analysis ----
# === GENDER SEGMENTATION ANALYSIS ===
gender_analysis_enhanced <- master %>%
  filter(is_participant == TRUE) %>%
  mutate(gender_clean = case_when(
    gender %in% c("V", "v", "Vrouw") ~ "Female",
    gender %in% c("M", "m", "Man")   ~ "Male",
    TRUE                             ~ "Unknown"
  )) %>%
  group_by(gender_clean) %>%
  summarise(
    n_participants = n(),
    avg_opens = mean(n_opens, na.rm = TRUE),
    repeat_open_rate = sum(n_opens > 1) / n() * 100,
    claim_rate = sum(n_claims > 0) / n() * 100,
    avg_max_streak = mean(max_streak, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  arrange(desc(n_participants))

print(gender_analysis_enhanced)

# === AGE SEGMENTATION ANALYSIS ===

age_cleaned_analysis <- master %>%
  filter(is_participant == TRUE) %>%
  mutate(
    # Extract lower bound as numeric (safer than midpoint for outliers)
    age_lower = as.numeric(str_extract(age, "^\\d+")),
    
    # Flag invalid ages
    age_valid = case_when(
      is.na(age_lower) | age_lower < 15 | age_lower > 99 ~ "Unknown",
      age_lower >= 100 ~ "Error (100+)",
      TRUE ~ "Valid"
    ),
    
    # Create clean age groups (only for valid)
    age_group = case_when(
      age_valid != "Valid" ~ "Unknown / Error",
      age_lower <= 24 ~ "15-24",
      age_lower <= 34 ~ "25-34",
      age_lower <= 44 ~ "35-44",
      age_lower <= 54 ~ "45-54",
      age_lower <= 64 ~ "55-64",
      TRUE ~ "65+"
    )
  ) %>%
  group_by(age_group) %>%
  summarise(
    n_participants     = n(),
    avg_opens = mean(n_opens, na.rm = TRUE),
    repeat_open_rate = sum(n_opens > 1) / n() * 100,
    claim_rate         = sum(n_claims > 0) / n() * 100,
    avg_max_streak     = mean(max_streak, na.rm = TRUE),
    median_max_streak  = median(max_streak, na.rm = TRUE),
    avg_opens          = mean(n_opens, na.rm = TRUE),
    repeat_open_rate   = sum(n_opens > 1) / n() * 100,
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric), ~round(., 1))
  ) %>%
  arrange(factor(age_group, levels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+", "Unknown / Error")))

# Show the cleaned table
print(age_cleaned_analysis)

# Optional: Check how many were recoded as Unknown/Error
master %>%
  filter(is_participant == TRUE) %>%
  count(age_valid = case_when(
    is.na(age) | age %in% c("null", "NA") ~ "Missing/null",
    as.numeric(str_extract(age, "^\\d+")) >= 100 ~ "Error (100+)",
    TRUE ~ "Valid"
  ))

# === AGE × GENDER CROSS-TABULATION ===
age_gender_analysis <- master %>%
  filter(is_participant == TRUE) %>%
  mutate(
    gender_clean = case_when(
      gender %in% c("V", "v", "Vrouw") ~ "Female",
      gender %in% c("M", "m", "Man")   ~ "Male",
      TRUE                             ~ "Unknown"
    ),
    # Same age cleaning as before
    age_lower = as.numeric(str_extract(age, "^\\d+")),
    age_group = case_when(
      is.na(age_lower) | age_lower < 15 | age_lower > 99 ~ "Unknown",
      age_lower <= 24 ~ "15-24",
      age_lower <= 34 ~ "25-34",
      age_lower <= 44 ~ "35-44",
      age_lower <= 54 ~ "45-54",
      age_lower <= 64 ~ "55-64",
      TRUE ~ "65+"
    )
  ) %>%
  group_by(age_group, gender_clean) %>%
  summarise(
    n_participants     = n(),
    avg_opens          = mean(n_opens, na.rm = TRUE),
    repeat_open_rate   = sum(n_opens > 1) / n() * 100,
    claim_rate         = sum(n_claims > 0) / n() * 100,
    avg_max_streak     = mean(max_streak, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric), ~round(., 1))
  ) %>%
  # Sort age groups logically
  mutate(age_group = factor(age_group, 
                            levels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+", "Unknown"))) %>%
  arrange(age_group, desc(gender_clean))

# Print wide table
print(age_gender_analysis, n = 30)

# === MOST USED DEVICE SEGMENTATION ANALYSIS ===
device_analysis <- master %>%
  filter(is_participant == TRUE) %>%
  group_by(most_used_device) %>%
  summarise(
    n_participants     = n(),
    avg_opens          = mean(n_opens, na.rm = TRUE),
    repeat_open_rate   = sum(n_opens > 1) / n() * 100,
    claim_rate         = sum(n_claims > 0) / n() * 100,
    avg_max_streak     = mean(max_streak, na.rm = TRUE),
    median_max_streak  = median(max_streak, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric), ~round(., 1))
  ) %>%
  arrange(desc(n_participants))   # Largest group first

# Display the result
print(device_analysis)

# === URBANISATION SEGMENTATION ANALYSIS ===
urban_analysis <- master %>%
  filter(is_participant == TRUE) %>%
  mutate(
    # Optional: group into 3 levels if you prefer fewer categories
    urban_level = case_when(
      urbanisation <= 2 ~ "Low (rural/suburban)",
      urbanisation == 3 ~ "Medium",
      urbanisation >= 4 ~ "High (urban)",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(urban_level) %>%
  summarise(
    n_participants     = n(),
    avg_opens          = mean(n_opens, na.rm = TRUE),
    repeat_open_rate   = sum(n_opens > 1) / n() * 100,
    claim_rate         = sum(n_claims > 0) / n() * 100,
    avg_max_streak     = mean(max_streak, na.rm = TRUE),
    median_max_streak  = median(max_streak, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric), ~round(., 1))
  ) %>%
  arrange(desc(n_participants))

# Display
print(urban_analysis)

# === MOST USED PLATFORM SEGMENTATION ANALYSIS ===
platform_analysis <- master %>%
  filter(is_participant == TRUE) %>%
  group_by(most_used_platform) %>%
  summarise(
    n_participants     = n(),
    avg_opens          = mean(n_opens, na.rm = TRUE),
    repeat_open_rate   = sum(n_opens > 1) / n() * 100,
    claim_rate         = sum(n_claims > 0) / n() * 100,
    avg_max_streak     = mean(max_streak, na.rm = TRUE),
    median_max_streak  = median(max_streak, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric), ~round(., 1))
  ) %>%
  arrange(desc(n_participants))

# Display
print(platform_analysis)

# === MOST POPULAR Category ANALYSIS ===
popular_category <- master %>%
  filter(is_participant == TRUE) %>%
  group_by(most_popular_category) %>%
  summarise(
    n_participants     = n(),
    avg_opens          = mean(n_opens, na.rm = TRUE),
    repeat_open_rate   = sum(n_opens > 1) / n() * 100,
    claim_rate         = sum(n_claims > 0) / n() * 100,
    avg_max_streak     = mean(max_streak, na.rm = TRUE),
    median_max_streak  = median(max_streak, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric), ~round(., 1))
  ) %>%
  arrange(desc(n_participants))

# Display
print(popular_category)

# === Province ANALYSIS ===
province <- master %>%
  filter(is_participant == TRUE) %>%
  group_by(province) %>%
  summarise(
    n_participants     = n(),
    avg_opens          = mean(n_opens, na.rm = TRUE),
    repeat_open_rate   = sum(n_opens > 1) / n() * 100,
    claim_rate         = sum(n_claims > 0) / n() * 100,
    avg_max_streak     = mean(max_streak, na.rm = TRUE),
    median_max_streak  = median(max_streak, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric), ~round(., 1))
  ) %>%
  arrange(desc(n_participants))

# Display
print(province)

#Tenure segmentation
table(master$first_order_year)
tenure_3cat_analysis <- master %>%
  filter(is_participant == TRUE) %>%
  mutate(
    tenure_3cat = case_when(
      is.na(first_order_year) ~ "Unknown",
      first_order_year >= 2023 ~ "New (2023-2024)",
      first_order_year >= 2016 ~ "Established (2016-2022)",
      first_order_year <= 2015 ~ "Legacy Loyalists (≤2015)"
    )
  ) %>%
  group_by(tenure_3cat) %>%
  summarise(
    n_participants     = n(),
    avg_opens          = mean(n_opens, na.rm = TRUE),
    repeat_open_rate   = sum(n_opens > 1) / n() * 100,
    claim_rate         = sum(n_claims > 0) / n() * 100,
    avg_max_streak     = mean(max_streak, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric), ~round(., 1))
  ) %>%
  arrange(desc(n_participants))

print(tenure_3cat_analysis)

#Order segmentation
order_segment_analysis <- master %>%
  filter(is_participant == TRUE) %>%
  mutate(
    order_segment = case_when(
      n_orders_pre <= 1 ~ "Low (0-1)",
      n_orders_pre <= 5 ~ "Medium (2-5)",
      n_orders_pre > 5 ~ "High (6+)"
    )
  ) %>%
  group_by(order_segment) %>%
  summarise(
    n_participants     = n(),
    avg_opens          = mean(n_opens, na.rm = TRUE),
    repeat_open_rate   = sum(n_opens > 1) / n() * 100,
    claim_rate         = sum(n_claims > 0) / n() * 100,
    avg_max_streak     = mean(max_streak, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric), ~round(., 1))
  ) %>%
  arrange(desc(n_participants))

print(order_segment_analysis)

#Streak based
streak_segment_analysis <- master %>%
  filter(is_participant == TRUE) %>%
  mutate(
    streak_segment = case_when(
      max_streak <= 1 ~ "Low (1 day)",
      max_streak <= 5 ~ "Medium (2-5 days)",
      max_streak > 5 ~ "High (6+ days)"
    )
  ) %>%
  group_by(streak_segment) %>%
  summarise(
    n_participants     = n(),
    avg_opens          = mean(n_opens, na.rm = TRUE),
    repeat_open_rate   = sum(n_opens > 1) / n() * 100,
    claim_rate         = sum(n_claims > 0) / n() * 100,
    avg_max_streak     = mean(max_streak, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric), ~round(., 1))
  ) %>%
  arrange(desc(n_participants))

print(streak_segment_analysis)



#Q8:Streak Card Analysis # ----
#Step 1: Extract streak reward selections from logging 
streak_selection <- wannagive %>%
  filter(
    event == "collectable_interaction",
    action == "selected",
    !is.na(collectable_value)
  ) %>%
  select(
    unique_customer_id,
    collectable_value,
    timestamp
  ) %>%
  distinct(unique_customer_id, .keep_all = TRUE)

# Quick validation checks
nrow(streak_selection)  # Should be <= number of participants
head(streak_selection)
table(streak_selection$collectable_value)


# Step 2: Map reward codes to streak tiers  
streak_selection <- streak_selection %>%
  mutate(
    selected_tier = case_when(
      collectable_value == "NIEUWJAARSKORTING1" ~ 5,
      collectable_value == "NIEUWJAARSKORTING2" ~ 10,
      collectable_value == "NIEUWJAARSKORTING3" ~ 15,
      collectable_value == "NIEUWJAARSKORTING4" ~ 20,
      collectable_value == "NIEUWJAARSKORTING5" ~ 22,
      TRUE ~ NA_real_
    )
  )

# Check mapping
table(streak_selection$selected_tier, useNA = "always")


# Step 3: Merge selection data into master table -
master_validation <- master %>%
  left_join(
    streak_selection %>% 
      select(unique_customer_id, selected_tier),
    by = "unique_customer_id"
  )

master_validation

# Quick check
summary(master_validation$selected_tier)
table(is.na(master_validation$selected_tier))  # Users who didn’t select

# Test 1: Can users select rewards higher than their FINAL streak? -----
master_validation %>%
  filter(!is.na(selected_tier)) %>%
  summarise(
    total_selectors = n(),
    pct_selected_above_final_streak = mean(selected_tier > final_streak_dec24) * 100,
    pct_selected_above_max_streak = mean(selected_tier > max_streak) * 100
  )

# Test 2: Correlation with selected reward tier ----
validation_data <- master_validation %>%
  filter(!is.na(selected_tier))

cor_max <- cor(validation_data$selected_tier, 
               validation_data$max_streak, 
               use = "complete.obs")

cor_final <- cor(validation_data$selected_tier, 
                 validation_data$final_streak_dec24, 
                 use = "complete.obs")


cor_max
cor_final

# Test 3: Users with LOW final streak but HIGH selected reward ----
edge_cases <- master_validation %>%
  filter(
    !is.na(selected_tier),
    final_streak_dec24 <= 2,
    selected_tier >= 10
  ) %>%
  select(unique_customer_id, max_streak, final_streak_dec24, selected_tier)

# How many such cases exist?
nrow(edge_cases)

# Inspect first few
head(edge_cases, 20)

# Cross-tab 1: Selected Tier vs Max Streak ----
table_max <- master_validation %>%
  filter(!is.na(selected_tier)) %>%
  count(max_streak, selected_tier) %>%
  tidyr::pivot_wider(
    names_from = selected_tier,
    values_from = n,
    values_fill = 0
  )

table_max

# Calculate total users per selected tier from table_max
total_selected_by_tier <- table_max %>%
  summarise(
    tier_5  = sum(`5`,  na.rm = TRUE),
    tier_10 = sum(`10`, na.rm = TRUE),
    tier_15 = sum(`15`, na.rm = TRUE),
    tier_20 = sum(`20`, na.rm = TRUE),
    tier_22 = sum(`22`, na.rm = TRUE)
  )

total_selected_by_tier

# Cross-tab 2: Selected Tier vs Final Streak ----
table_final <- master_validation %>%
  filter(!is.na(selected_tier)) %>%
  count(final_streak_dec24, selected_tier) %>%
  tidyr::pivot_wider(
    names_from = selected_tier,
    values_from = n,
    values_fill = 0
  )

table_final


## True system mapping check  
wannagive %>%
  filter(
    event == "collectable_interaction",
    action == "selected",
    !is.na(collectable_value)
  ) %>%
  group_by(collectable_value) %>%
  summarise(
    avg_logged_streak = mean(current_streak_value, na.rm = TRUE),
    median_logged_streak = median(current_streak_value, na.rm = TRUE),
    min_logged_streak = min(current_streak_value, na.rm = TRUE),
    max_logged_streak = max(current_streak_value, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(avg_logged_streak)

# Define down-selection (chosen tier < max streak milestone proxy)
down_selection_analysis <- master_validation %>%
  filter(!is.na(selected_tier)) %>%
  mutate(
    down_selected = selected_tier < max_streak,
    equal_selected = selected_tier == max_streak,
    over_selected = selected_tier > max_streak
  ) %>%
  summarise(
    total_selectors = n(),
    pct_down_selected = mean(down_selected) * 100,
    pct_equal_selected = mean(equal_selected) * 100,
    pct_over_selected = mean(over_selected) * 100
  )

down_selection_analysis


#Do customers with higher historical basket value select higher-tier coupons?

#Filter pre-campiagn txn
transaction_pre <- transaction %>%
  mutate(date = as.Date(order_local_timestamp)) %>%
  filter(date < "2024-12-03")

#calculate historic Average basket value per customer
historic_abv <- transaction_pre %>%
  group_by(unique_customer_id) %>%
  summarise(
    n_orders_pre = n(),
    total_demand_pre = sum(demand, na.rm = TRUE),
    historic_abv = total_demand_pre / n_orders_pre,
    .groups = "drop"
  )

#merge with validation dataset (Only users who had streak selection)
abv_selection_data <- master_validation %>%
  left_join(historic_abv, by = "unique_customer_id") %>%
  filter(!is.na(selected_tier))  # Only users who selected a streak reward

colnames(abv_selection_data)

summary(abv_selection_data$historic_abv)
table(is.na(abv_selection_data$historic_abv))

#correlation
correlation_abv_tier <- cor(
  abv_selection_data$historic_abv,
  abv_selection_data$selected_tier,
  use = "complete.obs"
)

correlation_abv_tier


abv_by_selected_tier <- abv_selection_data %>%
  group_by(selected_tier) %>%
  summarise(
    n_users = n(),
    n_with_history = sum(!is.na(historic_abv)),
    avg_historic_abv = mean(historic_abv, na.rm = TRUE),
    median_historic_abv = median(historic_abv, na.rm = TRUE),
    avg_orders_pre = mean(n_orders_pre.y, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(selected_tier)

abv_by_selected_tier


abv_downselection <- abv_selection_data %>%
  mutate(
    down_selected = selected_tier < max_streak
  ) %>%
  group_by(down_selected) %>%
  summarise(
    n_users = n(),
    avg_historic_abv = mean(avg_order_value_pre, na.rm = TRUE),
    median_historic_abv = median(avg_order_value_pre, na.rm = TRUE),
    .groups = "drop"
  )

abv_downselection



#Q9: Order Frequency and Demand per customer-----
#Did campaign exposure change behaviour
impact_participants <- master %>%
  group_by(is_participant) %>%
  summarise(
    avg_orders_during = mean(n_orders_during, na.rm = TRUE),
    avg_demand_during = mean(total_demand_during, na.rm = TRUE),
    n_customers = n()
  )

impact_participants


#Do more wannagive days increase orders and demand
# Correlation between engagement and orders
cor_orders <- cor(master$max_streak, master$n_orders_during, use = "complete.obs")

# Correlation between engagement and demand
cor_demand <- cor(master$max_streak, master$total_demand_during, use = "complete.obs")

cor_orders #0.163
cor_demand #0.084

master <- master %>%
  mutate(streak_group = case_when(
    max_streak == 1 ~ "One-day users",
    max_streak <= 5 ~ "Low engagement",
    max_streak <= 15 ~ "Medium engagement",
    TRUE ~ "High engagement"
  ))

#Engagement Groups vs Behaviour
streak_impact <- master %>%
  group_by(streak_group) %>%
  summarise(
    avg_orders_during = mean(n_orders_during, na.rm = TRUE),
    avg_demand_during = mean(total_demand_during, na.rm = TRUE),
    customers = n()
  )

streak_impact

#control self selection
master <- master %>%
  mutate(
    order_lift = n_orders_during - n_orders_pre,
    demand_lift = total_demand_during - total_demand_pre
  )

lift_analysis <- master %>%
  group_by(is_participant) %>%
  summarise(
    avg_order_lift = mean(order_lift, na.rm = TRUE),
    avg_demand_lift = mean(demand_lift, na.rm = TRUE),
    customers = n()
  )

lift_analysis


#Do Wannagive days (streak) influence purchasing?
model_orders <- lm(n_orders_during ~ max_streak, data = master)
summary(model_orders)

model_demand <- lm(total_demand_during ~ max_streak, data = master)
summary(model_demand)


#“Is the increase in orders during the campaign actually caused by the campaign, or just December seasonality?”
colnames(master)

# Define period lengths (adjust if your professor specified exact windows)
days_pre <- as.numeric(as.Date("2024-12-02") - as.Date("2023-12-01")) + 1
days_during <- 22
days_post <- as.numeric(as.Date("2025-12-31") - as.Date("2024-12-25")) + 1

days_pre
days_during
days_post

period_orders_rate <- master %>%
  summarise(
    avg_orders_per_day_pre = mean(n_orders_pre / days_pre, na.rm = TRUE),
    avg_orders_per_day_during = mean(n_orders_during / days_during, na.rm = TRUE),
    avg_orders_per_day_post = mean(n_orders_post / days_post, na.rm = TRUE)
  )

period_orders_rate

period_demand_rate <- master %>%
  summarise(
    avg_demand_per_day_pre = mean(total_demand_pre / days_pre, na.rm = TRUE),
    avg_demand_per_day_during = mean(total_demand_during / days_during, na.rm = TRUE),
    avg_demand_per_day_post = mean(total_demand_post / days_post, na.rm = TRUE)
  )

period_demand_rate

seasonality_adjusted <- master %>%
  group_by(is_participant) %>%
  summarise(
    pre_daily = mean(n_orders_pre / days_pre, na.rm = TRUE),
    during_daily = mean(n_orders_during / days_during, na.rm = TRUE),
    post_daily = mean(n_orders_post / days_post, na.rm = TRUE)
  )

seasonality_adjusted

seasonality_adjusted_demand <- master %>%
  group_by(is_participant) %>%
  summarise(
    pre_daily_demand = mean(total_demand_pre / days_pre, na.rm = TRUE),
    during_daily_demand = mean(total_demand_during / days_during, na.rm = TRUE),
    post_daily_demand = mean(total_demand_post / days_post, na.rm = TRUE)
  )

seasonality_adjusted_demand

# Incremental revenue calculation
incremental_daily_demand <- (4.18 - 3.11) - (2.39 - 2.20)
incremental_per_participant <- incremental_daily_demand * 22
total_incremental <- incremental_per_participant * 16030

cat("Incremental daily demand per participant:", round(incremental_daily_demand, 2), "\n")
cat("Incremental demand per participant (22 days):", round(incremental_per_participant, 2), "\n")
cat("Total incremental demand generated by campaign:", round(total_incremental), "\n")

#Additional Analysis ----
##Cohort Analysis ----
#1. create engagement events table
engagement_events <- wannagive %>%
  # Keep only campaign window
  filter(
    date(timestamp) >= as.Date("2024-12-03"),
    date(timestamp) <= as.Date("2024-12-24")
  ) %>%
  # Keep only engagement-related events (Option B)
  filter(
    event == "participate" |
      event == "collectable_interaction"
  ) %>%
  # Create a clean activity date (daily granularity)
  mutate(
    activity_date = as.Date(timestamp)  # still UTC but consistent daily measure
  ) %>%
  select(
    unique_customer_id,
    event,
    action,
    activity_date,
    timestamp
  )

#checks
nrow(engagement_events)
table(engagement_events$event)
head(engagement_events)
engagement_events %>% count(engagement_events$unique_customer_id)

#2. Create cohort table
cohort_table <- engagement_events %>%
  filter(event == "participate") %>%
  group_by(unique_customer_id) %>%
  summarise(
    cohort_date = min(activity_date),
    .groups = "drop"
  )

nrow(cohort_table)
table(cohort_table$cohort_date)
head(cohort_table)

#User daily activity
user_daily_activity <- engagement_events %>%
  select(unique_customer_id, activity_date) %>%
  distinct()

nrow(user_daily_activity)
user_daily_activity %>%
  count(unique_customer_id, activity_date) %>%
  filter(n > 1)
head(user_daily_activity)

#retention lifecyle
retention_lifecycle <- user_daily_activity %>%
  left_join(cohort_table, by = "unique_customer_id") %>%
  mutate(
    days_since_cohort = as.integer(activity_date - cohort_date)
  ) %>%
  # Keep only activity on or after first participation
  filter(days_since_cohort >= 0)

head(retention_lifecycle)
summary(retention_lifecycle$days_since_cohort)
retention_lifecycle %>%
  filter(days_since_cohort == 0) %>%
  summarise(n_users = n_distinct(unique_customer_id))

#cohort sizes(denominator)
cohort_sizes <- cohort_table %>%
  group_by(cohort_date) %>%
  summarise(
    cohort_size = n(),
    .groups = "drop"
  )

head(cohort_sizes)
summary(cohort_sizes$cohort_size)

#Step 5B: Count Active Users per Cohort per Day (Numerator)
cohort_activity <- retention_lifecycle %>%
  group_by(cohort_date, days_since_cohort) %>%
  summarise(
    active_users = n_distinct(unique_customer_id),
    .groups = "drop"
  )
head(cohort_activity)

#cohort_retention
cohort_retention <- cohort_activity %>%
  left_join(cohort_sizes, by = "cohort_date") %>%
  mutate(
    retention_rate = (active_users / cohort_size) * 100
  )

cohort_retention %>%
  filter(days_since_cohort == 0) %>%
  select(cohort_date, retention_rate) %>%
  head()

head(cohort_retention)
summary(cohort_retention$retention_rate)

#for clean and comparable cohorts
cohort_retention_limited <- cohort_retention %>%
  filter(days_since_cohort <= 14)

cohort_retention_limited <- cohort_retention_limited %>%
  mutate(
    cohort_date = as.factor(cohort_date)
  )

ggplot(cohort_retention_limited, 
       aes(x = days_since_cohort, 
           y = cohort_date, 
           fill = retention_rate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#f7fbff",
    high = "#08306b",
    name = "Retention %"
  ) +
  labs(
    title = "Cohort Retention Heatmap – Wannagive Campaign",
    x = "Days Since First Participation",
    y = "Cohort (First Participate Date)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(face = "bold")
  )

ggplot(cohort_retention_limited, 
       aes(x = days_since_cohort, 
           y = cohort_date, 
           fill = retention_rate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(retention_rate, 1)), size = 2) +
  scale_fill_gradient(
    low = "#f7fbff",
    high = "#08306b",
    name = "Retention %"
  ) +
  labs(
    title = "Cohort Retention Heatmap – Wannagive Campaign",
    x = "Days Since First Participation",
    y = "Cohort (First Participate Date)"
  ) +
  theme_minimal()

overall_retention <- retention_lifecycle %>%
  group_by(days_since_cohort) %>%
  summarise(
    active_users = n_distinct(unique_customer_id),
    .groups = "drop"
  ) %>%
  mutate(
    total_users = n_distinct(cohort_table$unique_customer_id),
    retention_rate = (active_users / total_users) * 100
  )

head(overall_retention)
summary(overall_retention$days_since_cohort)

overall_retention_14 <- overall_retention %>%
  filter(days_since_cohort <= 14)

ggplot(overall_retention_14, 
       aes(x = days_since_cohort, y = retention_rate)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Overall Retention Curve – Wannagive Campaign",
    x = "Days Since First Participation",
    y = "Retention Rate (%)"
  ) +
  theme_minimal()

ggplot(overall_retention_14, 
       aes(x = days_since_cohort, y = retention_rate)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_smooth(se = FALSE, linetype = "dashed") +
  labs(
    title = "Overall Retention Curve – Wannagive Campaign",
    x = "Days Since First Participation",
    y = "Retention Rate (%)"
  ) +
  theme_minimal()

#cohort analysis by segments
table(master$age)
# Create age groups from the existing age band column
master <- master %>%
  mutate(
    age_group = case_when(
      age %in% c("15-19", "20-24") ~ "15-24",
      age %in% c("25-29", "30-34") ~ "25-34",
      age %in% c("35-39", "40-44") ~ "35-44",
      age %in% c("45-49", "50-54") ~ "45-54",
      age %in% c("55-59", "60-64") ~ "55-64",
      age %in% c("65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99") ~ "65+",
      age %in% c("null", "110-114", "120-124") ~ "Unknown",
      TRUE ~ "Unknown"
    )
  )

table(master$age_group)
table(master$gender)

# Step 1: Create a clean master table ONLY for cohort analysis
master_cohort <- master %>%
  mutate(
    # Recode gender properly
    gender_clean = case_when(
      gender == "V" ~ "Female",
      gender == "M" ~ "Male",
      TRUE ~ "Other"
    ),
    
    # Keep your already created age_group but remove Unknown
    age_group_clean = case_when(
      age_group %in% c("15-24", "25-34", "35-44", "45-54", "55-64", "65+") ~ age_group,
      TRUE ~ NA_character_
    )
  ) %>%
  # Remove tiny and invalid groups for stable cohort curves
  filter(
    gender_clean %in% c("Male", "Female"),
    !is.na(age_group_clean)
  ) %>%
  select(unique_customer_id, gender_clean, age_group_clean)

table(master_cohort$gender_clean)
table(master_cohort$age_group_clean)

# Step 2: Attach gender and age to lifecycle data
retention_demo <- retention_lifecycle %>%
  left_join(
    master_cohort,
    by = "unique_customer_id"
  ) %>%
  # Keep only valid segmented users
  filter(
    !is.na(gender_clean),
    !is.na(age_group_clean)
  )

summary(retention_demo$days_since_cohort)
table(retention_demo$gender_clean)
table(retention_demo$age_group_clean)
retention_demo %>%
  summarise(n_unique_users = n_distinct(unique_customer_id))

# Step 3: Cohort size per segment (Day 0 users)
cohort_sizes_demo <- retention_demo %>%
  filter(days_since_cohort == 0) %>%
  group_by(gender_clean, age_group_clean) %>%
  summarise(
    cohort_size = n_distinct(unique_customer_id),
    .groups = "drop"
  )

cohort_sizes_demo

# Step 4: Active users per day by gender and age group
cohort_activity_demo <- retention_demo %>%
  group_by(gender_clean, age_group_clean, days_since_cohort) %>%
  summarise(
    active_users = n_distinct(unique_customer_id),
    .groups = "drop"
  )

# Step 5: Calculate retention %
cohort_retention_demo <- cohort_activity_demo %>%
  left_join(
    cohort_sizes_demo,
    by = c("gender_clean", "age_group_clean")
  ) %>%
  mutate(
    retention_rate = (active_users / cohort_size) * 100
  )

cohort_retention_demo_14 <- cohort_retention_demo %>%
  filter(days_since_cohort <= 14)

#graph
ggplot(cohort_retention_demo_14,
       aes(x = days_since_cohort,
           y = retention_rate,
           color = age_group_clean,
           group = age_group_clean)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ gender_clean) +
  labs(
    title = "Cohort Retention by Gender and Age Group – Wannagive Campaign",
    subtitle = "Retention lifecycle segmented by demographic cohorts",
    x = "Days Since First Participation",
    y = "Retention Rate (%)",
    color = "Age Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

##When do users engage?----
#Timing of engagment
# STEP 1: Filter TRUE card opens + Convert UTC → CET (Netherlands)
time_of_day_opens <- wannagive_campaign %>%
  filter(event == "participate") %>%   # true daily opens
  mutate(
    # Convert from UTC logging time to Netherlands local time (CET)
    timestamp_cet = with_tz(timestamp, tzone = "Europe/Amsterdam"),
    hour_cet = hour(timestamp_cet),
    date_cet = as.Date(timestamp_cet),
    weekday_cet = wday(timestamp_cet, label = TRUE)
  )

# Sanity checks
nrow(time_of_day_opens)
summary(time_of_day_opens$hour_cet)


# STEP 2: Hourly Open Distribution (Core Behavioural Analysis)
hourly_distribution <- time_of_day_opens %>%
  group_by(hour_cet) %>%
  summarise(
    total_opens = n(),
    unique_users = n_distinct(unique_customer_id),
    .groups = "drop"
  ) %>%
  mutate(
    pct_opens = round(total_opens / sum(total_opens) * 100, 2)
  ) %>%
  arrange(hour_cet)

print(hourly_distribution)


# STEP 3: Managerial Time-of-Day Buckets (Best for Slides/Insights)
time_bucket_distribution <- time_of_day_opens %>%
  mutate(
    time_of_day_bucket = case_when(
      hour_cet >= 6  & hour_cet < 12 ~ "Morning (06-11)",
      hour_cet >= 12 & hour_cet < 18 ~ "Afternoon (12-17)",
      hour_cet >= 18 & hour_cet < 24 ~ "Evening (18-23)",
      TRUE ~ "Night (00-05)"
    )
  ) %>%
  group_by(time_of_day_bucket) %>%
  summarise(
    total_opens = n(),
    unique_users = n_distinct(unique_customer_id),
    .groups = "drop"
  ) %>%
  mutate(
    pct_opens = round(total_opens / sum(total_opens) * 100, 1)
  ) %>%
  arrange(desc(total_opens))

print(time_bucket_distribution)


# STEP 4: Peak Engagement Hours (Top 5 - CET)
peak_hours <- hourly_distribution %>%
  arrange(desc(total_opens)) %>%
  slice(1:5)

print(peak_hours)


# STEP 5: Hourly Engagement Curve (Report-Ready, CET)
ggplot(hourly_distribution, aes(x = hour_cet, y = pct_opens)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Time-of-Day Open Distribution — Wannagive Campaign (CET)",
    subtitle = "Based on Participate Events (True Daily Card Opens)",
    x = "Hour of Day (CET - Netherlands Local Time)",
    y = "Percentage of Total Opens (%)"
  ) +
  theme_minimal(base_size = 13)

# STEP 6: Weekday vs Weekend Hourly Pattern (CET)
weekday_time_distribution <- time_of_day_opens %>%
  mutate(
    day_type = if_else(weekday_cet %in% c("Sat", "Sun"), "Weekend", "Weekday")
  ) %>%
  group_by(day_type, hour_cet) %>%
  summarise(
    total_opens = n(),
    .groups = "drop"
  ) %>%
  group_by(day_type) %>%
  mutate(
    pct_opens = total_opens / sum(total_opens) * 100
  )

# Plot: Weekday vs Weekend hourly engagement (CET)
ggplot(weekday_time_distribution, aes(x = hour_cet, y = pct_opens, linetype = day_type)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Hourly Engagement Pattern: Weekday vs Weekend (CET)",
    subtitle = "Wannagive Campaign — Local Dutch Time",
    x = "Hour of Day (CET)",
    y = "Percentage of Opens (%)",
    linetype = "Day Type"
  ) +
  theme_minimal(base_size = 13)


# STEP 7 (Optional but Strong): Opens per Hour per Day (Intensity Check)
opens_per_day_hour <- time_of_day_opens %>%
  group_by(date_cet, hour_cet) %>%
  summarise(
    opens = n(),
    unique_users = n_distinct(unique_customer_id),
    .groups = "drop"
  )

head(opens_per_day_hour)


##Customers who tried to rebuild streaks ----
# STEP 0: Create DAILY ACTIVITY using ONLY PARTICIPATE events
# (CRITICAL for streak analysis)
user_daily_participation <- wannagive_campaign %>%
  filter(event == "participate") %>%
  mutate(activity_date = as.Date(timestamp)) %>%
  select(unique_customer_id, activity_date) %>%
  distinct()

# Sanity check
nrow(user_daily_participation)
n_distinct(user_daily_participation$unique_customer_id)


# STEP 1: Calculate gaps between consecutive participation days
user_streak_gaps <- user_daily_participation %>%
  arrange(unique_customer_id, activity_date) %>%
  group_by(unique_customer_id) %>%
  mutate(
    prev_date = lag(activity_date),
    gap_days = as.numeric(activity_date - prev_date)
  ) %>%
  ungroup()


# STEP 2: User-level streak behaviour metrics
rebuild_flags <- user_streak_gaps %>%
  group_by(unique_customer_id) %>%
  summarise(
    total_active_days = n(),
    
    # Number of streak breaks (gap > 1 day)
    n_breaks = sum(gap_days > 1, na.rm = TRUE),
    
    # Did the user ever break a streak?
    had_streak_break = n_breaks > 0,
    
    # Position of first break
    first_break_position = ifelse(
      had_streak_break,
      which(gap_days > 1)[1],
      NA_integer_
    ),
    
    # TRUE rebuild = break happened AND user came back after break
    tried_to_rebuild = ifelse(
      had_streak_break & !is.na(first_break_position) & first_break_position < total_active_days,
      TRUE,
      FALSE
    ),
    
    .groups = "drop"
  )


# STEP 3: Keep ONLY campaign participants
rebuild_analysis <- rebuild_flags %>%
  left_join(
    master %>% select(unique_customer_id, is_participant),
    by = "unique_customer_id"
  ) %>%
  filter(is_participant == TRUE)


# STEP 4: Behaviourally CORRECT segmentation
# (Fixes the "Never Broke Streak" inflation issue)
rebuild_analysis <- rebuild_analysis %>%
  mutate(
    rebuild_segment = case_when(
      total_active_days == 1 ~ "One-Day Users (No Streak)",
      total_active_days > 1 & n_breaks == 0 ~ "Continuous Users (No Breaks)",
      n_breaks > 0 & tried_to_rebuild == TRUE ~ "Tried to Rebuild Streak",
      n_breaks > 0 & tried_to_rebuild == FALSE ~ "Broke & Dropped Off"
    )
  )


# STEP 5: Final distribution (KEY ANSWER TO YOUR QUESTION)
rebuild_segments <- rebuild_analysis %>%
  count(rebuild_segment) %>%
  mutate(
    pct = round(n / sum(n) * 100, 1)
  ) %>%
  arrange(desc(n))

print(rebuild_segments)


# STEP 6: Headline KPI — Number of customers who tried to rebuild
# (Use this directly in your report)
rebuild_summary <- rebuild_analysis %>%
  summarise(
    total_participants = n(),
    one_day_users = sum(rebuild_segment == "One-Day Users (No Streak)"),
    continuous_users = sum(rebuild_segment == "Continuous Users (No Breaks)"),
    users_broke_streak = sum(n_breaks > 0),
    users_tried_rebuild = sum(tried_to_rebuild),
    pct_tried_rebuild = round(users_tried_rebuild / total_participants * 100, 2)
  )

print(rebuild_summary)



#Self selection
table(master$is_participant)
colnames(master)
master <- master %>%
  mutate(engagement_segment = case_when(
    is_participant == FALSE ~ "Non-participants",
    max_streak == 1 ~ "One-day users",
    max_streak >= 2 & max_streak <= 5 ~ "Low engagement",
    max_streak >= 6 & max_streak <= 14 ~ "Medium engagement",
    max_streak >= 15 ~ "High engagement (loyal core)"
  )
  )

table(master$engagement_segment)

pre_post_engagement <- master %>%
  group_by(engagement_segment) %>%
  summarise(
    n_users = n(),
    
    # PRE CAMPAIGN
    avg_orders_pre = mean(n_orders_pre, na.rm = TRUE),
    avg_demand_pre = mean(total_demand_pre, na.rm = TRUE),
    avg_aov_pre = mean(avg_order_value_pre, na.rm = TRUE),
    
    # POST CAMPAIGN
    avg_orders_post = mean(n_orders_post, na.rm = TRUE),
    avg_demand_post = mean(total_demand_post, na.rm = TRUE),
    avg_aov_post = mean(avg_order_value_post, na.rm = TRUE),
    
    #UPLIFT METRICS
    avg_order_uplift_pct = mean(order_uplift_pct, na.rm = TRUE),
    avg_demand_uplift_pct = mean(demand_uplift_pct, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(engagement_segment)

pre_post_engagement


pre_post_change <- master %>%
  group_by(engagement_segment) %>%
  summarise(
    n_users = n(),
    
    # Absolute behavioural change
    orders_change = mean(n_orders_post - n_orders_pre, na.rm = TRUE),
    demand_change = mean(total_demand_post - total_demand_pre, na.rm = TRUE),
    aov_change = mean(avg_order_value_post - avg_order_value_pre, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(engagement_segment)

pre_post_change

