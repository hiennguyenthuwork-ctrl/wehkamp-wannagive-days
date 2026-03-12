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


#Loading Data and data cleaning----
getwd()
setwd("C:/Users/Dell/Desktop/Groningen/18. Digital Marketing/Data")

wannagive <- read_csv("wannagive_logging.csv") %>%
  clean_names()

coupons <- read_csv("coupons_logging.csv") %>%
  clean_names()

transaction <- read_csv("Transactions.csv") %>%
  clean_names()

customer_info <- read_csv("customer_info.csv") %>%
  clean_names()

colnames(wannagive)
colnames(coupons)
colnames(transaction)
colnames(customer_info)

customer_info <- customer_info %>%  select(-x12)

wannagive <- wannagive %>%
  mutate(across(where(is.character), ~na_if(.x, "null")))

coupons <- coupons %>%
  mutate(across(where(is.character), ~na_if(.x, "null")))

## Dates ----
#Inspecting date changes
glimpse(wannagive)
glimpse(coupons)
glimpse(transaction)
glimpse(customer_info)
str(wannagive$timestamp)
str(coupons$timestamp)
str(transaction$order_local_timestamp)

##checking timezone ----
attr(wannagive$timestamp, "tzone")
attr(coupons$timestamp, "tzone")
attr(transaction$order_local_timestamp, "tzone")

#looking at a few timestamps converted to UTC explicitly
# Show original vs forced-to-UTC
head(data.frame(
  original = wannagive$timestamp[1:6],
  as_utc   = with_tz(wannagive$timestamp[1:6], "UTC")
))

head(data.frame(
  original_1 = coupons$timestamp[1:6],
  as_utc_1   = with_tz(coupons$timestamp[1:6], "UTC")
))

head(data.frame(
  original_2 = transaction$order_local_timestamp[1:6],
  as_utc_2   = with_tz(transaction$order_local_timestamp[1:6], "UTC")
))


###Sanity checks for timezone difference ---
#since output is showing UTC for all timestamp variables, checking to see if there are any weird spikes
timing <- transaction %>%
  mutate(hour = hour(order_local_timestamp)) %>%
  count(hour) %>%
  arrange(hour)
timing
#The distribution is consistent with real human shopping behavior in a Dutch/European market, 
#Peak shopping in the evening and no spikes
#No hour which shows zero orders
#Seems to be recorded cleanly in UTC.
#No timezone changes as of now


# Find 5-10 customers present in all three tables
common_customers <- Reduce(intersect, list(
  unique(wannagive$unique_customer_id),
  unique(coupons$unique_customer_id),
  unique(transaction$unique_customer_id)
))[1:10]

# Inspect timeline for one or two
wannagive %>%
  filter(unique_customer_id %in% common_customers[1:2]) %>%
  select(unique_customer_id, event, timestamp) %>%
  arrange(unique_customer_id, timestamp)

coupons %>%
  filter(unique_customer_id %in% common_customers[1:2]) %>%
  select(unique_customer_id, event_type, code, timestamp) %>%
  arrange(unique_customer_id, timestamp)

transaction %>%
  filter(unique_customer_id %in% common_customers[1:2]) %>%
  select(unique_customer_id, order_id, order_local_timestamp, demand, coupon_discount) %>%
  arrange(unique_customer_id, order_local_timestamp)
#wanna give event comes before or at the same time as related coupons and coupons precede or slign with subsequent orders
#No suspicious jumps

#deeper checks
#Check 1: Hourly Counts Around a DST Start (Spring Forward — Should Show Missing Hour if Local)
# Filter around DST date, extract UTC hour
dst_spring <- transaction %>%
  filter(date(order_local_timestamp) %in% ymd(c("2024-03-30", "2024-03-31", "2024-04-01"))) %>%
  mutate(utc_hour = hour(order_local_timestamp)) %>%
  count(date = date(order_local_timestamp), utc_hour) %>%
  arrange(date, utc_hour)

# Print table — look for gaps (e.g., no row for hour 2 on Mar 31) or low counts
dst_spring

# Plot
ggplot(dst_spring, aes(x = utc_hour, y = n, fill = date)) +
  geom_col(position = "dodge") +
  labs(title = "Orders per UTC Hour Around DST Start (Mar 31 2024)",
       x = "UTC Hour", y = "Order Count") +
  theme_minimal()

#for DST end
dst_fall <- transaction %>%
  filter(date(order_local_timestamp) %in% ymd(c("2024-10-26", "2024-10-27", "2024-10-28"))) %>%
  mutate(utc_hour = hour(order_local_timestamp)) %>%
  count(date = date(order_local_timestamp), utc_hour) %>%
  arrange(date, utc_hour)

dst_fall

ggplot(dst_fall, aes(x = utc_hour, y = n, fill = date)) +
  geom_col(position = "dodge") +
  labs(title = "Orders per UTC Hour Around DST End (Oct 27 2024)",
       x = "UTC Hour", y = "Order Count") +
  theme_minimal()

#Global day length
daily_hours <- transaction %>%
  mutate(date = date(order_local_timestamp)) %>%
  group_by(date) %>%
  summarise(unique_hours = n_distinct(hour(order_local_timestamp)),
            total_orders = n()) %>%
  arrange(unique_hours)

# Look for days with !=24 unique hours
daily_hours %>% filter(unique_hours != 24)

daily_hours


## comparing time ranges across tables ----
wannagive %>% summarise(
  min_time = min(timestamp, na.rm = TRUE),
  max_time = max(timestamp, na.rm = TRUE)
)

coupons %>% summarise(
  min_time = min(timestamp, na.rm = TRUE),
  max_time = max(timestamp, na.rm = TRUE)
)

transaction %>% summarise(
  min_time = min(order_local_timestamp, na.rm = TRUE),
  max_time = max(order_local_timestamp, na.rm = TRUE)
)


##campaign window filtering ----
#data in the wannagive table from 30th November 2024 - 7th January 2025
#where as the wannagive days were from 3rd to 24th December

wannagive %>%
  filter(event == "participate" | (event == "collectable_interaction" & action == "opened")) %>%
  mutate(date = date(timestamp)) %>%
  summarise(
    first_participate = min(date),
    last_participate_open = max(date)
  )

#make pivot table to understand user activity on these days 
#create date column
wannagive_daily <- wannagive %>%
  mutate(date = as_date(timestamp))

pivot_users <- wannagive_daily %>%
  group_by(date, event) %>%
  summarise(unique_users = n_distinct(unique_customer_id), .groups = "drop") %>%
  pivot_wider(
    names_from = event,
    values_from = unique_users,
    values_fill = 0
  ) %>%
  arrange(date)

pivot_users
#this table shows minimal activity before campaign which seems to be internal testing
#Participation related events drop to zero after 24th December - Functional end of campaign
#Analysis window should be restricted to campaign days for funnel measurements

## Create clean campaign dataset ----
wannagive_campaign <- wannagive %>%
  mutate(date = as.Date(timestamp)) %>%
  filter(date >= as.Date("2024-12-03"),
         date <= as.Date("2024-12-24"))

## Unique customer ID consistency check ----
#Check missing customer id
colSums(is.na(wannagive["unique_customer_id"]))
colSums(is.na(coupons["unique_customer_id"]))
colSums(is.na(transaction["unique_customer_id"]))
colSums(is.na(customer_info["unique_customer_id"]))

#check ID format consistency
# Sample IDs from each table
head(wannagive$unique_customer_id, 5)
head(customer_info$unique_customer_id, 5)
head(transaction$unique_customer_id, 5)
head(coupons$unique_customer_id, 5)
#id formats are different - in the customer table id starts with "wehkamp"

#Unique customers per table
id_summary <- data.frame(
  table = c("wannagive", "coupons", "transaction", "customer_info"),
  unique_customers = c(
    n_distinct(wannagive$unique_customer_id),
    n_distinct(coupons$unique_customer_id),
    n_distinct(transaction$unique_customer_id),
    n_distinct(customer_info$unique_customer_id)
  )
)

id_summary

# Check if all customer_info IDs start with "wehkamp:"
mean(grepl("^wehkamp:", customer_info$unique_customer_id))

#Checking string length distribution
wannagive %>%
  mutate(id_length = nchar(unique_customer_id)) %>%
  count(id_length) %>%
  arrange(id_length)

customer_info %>%
  mutate(id_length = nchar(unique_customer_id)) %>%
  count(id_length)

#checking joins between tables
length(intersect(
  unique(wannagive$unique_customer_id),
  unique(customer_info$unique_customer_id)
))

length(intersect(
  unique(wannagive$unique_customer_id),
  unique(transaction$unique_customer_id)
))


length(intersect(
  unique(coupons$unique_customer_id),
  unique(transaction$unique_customer_id)
))

#clean customer_info ID
customer_info_clean <- customer_info %>%
  mutate(
    unique_customer_id = str_remove(unique_customer_id, "^wehkamp:")
  )

#checking joins between tables again
length(intersect(
  unique(wannagive$unique_customer_id),
  unique(customer_info_clean$unique_customer_id)
))
#join between tables successful