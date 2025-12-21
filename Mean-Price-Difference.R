############################################################
# Mean Price Difference vs Bitvavo
#
# Purpose:
# - Compare actual API prices from Bitvavo, Coinbase, Kraken
# - Measure how much worse (in %) Coinbase & Kraken are
#   compared to Bitvavo, on average
#
# Method:
# - For each timestamp × symbol × notional:
#     % difference = (Bitvavo_coins - Exchange_coins) / Bitvavo_coins
# - Aggregate by exchange and take the mean
############################################################

########################
# 0) Libraries & Config
########################

library(tidyverse)
library(lubridate)
library(jsonlite)

json_path <- "data_updated.json"   # adjust if needed
real_venues <- c("bitvavo", "coinbase", "kraken")


#########################################
# 1) Load and clean the data
#########################################

data <- fromJSON(json_path, flatten = TRUE) %>%
  rename_with(~ gsub("^fees\\.taker\\.", "taker", .x)) %>%
  rename_with(~ gsub("^price_without_fee\\.taker\\.", "nofee_taker", .x)) %>%
  mutate(
    datetime = ymd_hms(datetime),
    datetime_min = floor_date(datetime, "minute")
  ) %>%
  filter(source %in% real_venues)


############################################################
# 2) Keep only relevant columns and reshape to long format
############################################################

# We compare all three notionals equally
long <- data %>%
  select(source, symbol, datetime_min,
         taker100, taker500, taker1000) %>%
  pivot_longer(
    cols = starts_with("taker"),
    names_to = "notional",
    values_to = "coins"
  ) %>%
  filter(!is.na(coins)) %>%
  mutate(
    notional = recode(
      notional,
      taker100  = "100 €",
      taker500  = "500 €",
      taker1000 = "1,000 €"
    )
  )


############################################################
# 3) Compute % difference vs Bitvavo per timestamp
############################################################

# First, attach Bitvavo's price to every row
with_bitvavo <- long %>%
  left_join(
    long %>%
      filter(source == "bitvavo") %>%
      select(symbol, datetime_min, notional,
             bitvavo_coins = coins),
    by = c("symbol", "datetime_min", "notional")
  )

# Compute % disadvantage relative to Bitvavo
# Positive value = worse than Bitvavo
diff_vs_bitvavo <- with_bitvavo %>%
  filter(source != "bitvavo") %>%
  mutate(
    pct_worse = (bitvavo_coins - coins) / bitvavo_coins
  )


############################################################
# 4) Aggregate: mean % difference per exchange
############################################################

mean_diff <- diff_vs_bitvavo %>%
  group_by(source) %>%
  summarise(
    mean_pct_worse = mean(pct_worse, na.rm = TRUE),
    median_pct_worse = median(pct_worse, na.rm = TRUE),
    observations = n(),
    .groups = "drop"
  ) %>%
  arrange(mean_pct_worse)


########################
# 5) Output
########################

print(mean_diff)

# Optional: pretty text output
mean_diff %>%
  mutate(
    sentence = sprintf(
      "On average, %s offers a %.3f%% worse price than Bitvavo.",
      str_to_title(source),
      mean_pct_worse * 100
    )
  ) %>%
  pull(sentence)
