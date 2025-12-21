############################################################
# Exchange Fee Comparison Script (Final, Clean Version)
#
# Purpose:
# Compare Bitvavo, Coinbase, Kraken (real API data)
# Estimate Bison, BSDEX, Bitpanda prices by applying fees
#
# Key features:
#  - Proxy price = BEST (max) no-fee price among Bitvavo/Coinbase/Kraken
#  - Estimate coins for Bison / BSDEX / Bitpanda for €100 / €500 / €1,000
#  - For every timestamp × symbol × notional → 1 point to the winner
#  - Final score = sum of points across all three notionals
############################################################

########################
# 0) Libraries & Config
########################

library(tidyverse)
library(lubridate)
library(jsonlite)
library(tidyr)
library(ggplot2)

json_path <- "data_updated.json"   # CHANGE if necessary
real_venues <- c("bitvavo", "coinbase", "kraken")

# Human-readable labels & fixed order
labels_map <- c(
  bitvavo      = "Bitvavo",
  coinbase     = "Coinbase",
  kraken       = "Kraken",
  bison_est    = "Bison (est.)",
  bsdex_est    = "BSDEX (est.)",
  bitpanda_est = "Bitpanda (est.)"
)
all_labels <- unname(labels_map)


#########################################
# 1) Load JSON and clean column names
#########################################

data <- fromJSON(json_path, flatten = TRUE)

data <- data %>%
  rename_with(~ gsub("^fees\\.maker\\.", "maker", .x)) %>%
  rename_with(~ gsub("^fees\\.taker\\.", "taker", .x)) %>%
  rename_with(~ gsub("^price_without_fee\\.maker\\.", "nofee_maker", .x)) %>%
  rename_with(~ gsub("^price_without_fee\\.taker\\.", "nofee_taker", .x)) %>%
  mutate(
    datetime = ymd_hms(datetime),
    datetime_min = floor_date(datetime, "minute")   # never round up
  )


##############################################################
# 2) Build a proxy “true price” from BEST (max) no-fee coins
##############################################################

safe_max <- function(x) {
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

real_only <- data %>% filter(source %in% real_venues)

proxy <- real_only %>%
  group_by(symbol, datetime_min) %>%
  summarise(
    proxy_taker100  = safe_max(nofee_taker100),
    proxy_taker500  = safe_max(nofee_taker500),
    proxy_taker1000 = safe_max(nofee_taker1000),
    .groups = "drop"
  )

# Ensure proxy only covers timestamps where real data exists
proxy_limited <- proxy %>%
  semi_join(real_only %>% distinct(symbol, datetime_min),
            by = c("symbol", "datetime_min"))


#############################################
# 3) Apply venue-specific fee models
#############################################

apply_pct_fee <- function(coins_no_fee, notional_eur, pct) {
  fee_eur <- pct * notional_eur
  coins_no_fee * (1 - fee_eur / notional_eur)
}

mk_estimated_exchanges <- function(proxy_df) {
  
  bison <- proxy_df %>%
    transmute(
      source = "bison_est", symbol, datetime_min,
      taker100  = apply_pct_fee(proxy_taker100,  100,  0.0125),   # 1.25%
      taker500  = apply_pct_fee(proxy_taker500,  500,  0.0125),
      taker1000 = apply_pct_fee(proxy_taker1000, 1000, 0.0125)
    )
  
  bsdex <- proxy_df %>%
    transmute(
      source = "bsdex_est", symbol, datetime_min,
      taker100  = apply_pct_fee(proxy_taker100,  100,  0.0035),   # 0.35%
      taker500  = apply_pct_fee(proxy_taker500,  500,  0.0035),
      taker1000 = apply_pct_fee(proxy_taker1000, 1000, 0.0035)
    )
  
  bitpanda <- proxy_df %>%
    mutate(
      pct = if_else(symbol %in% c("BTCEUR", "ETHEUR"),
                    0.0099,  # BTC & ETH 0.99%
                    0.0249)  # ALGO     2.49%
    ) %>%
    transmute(
      source = "bitpanda_est", symbol, datetime_min,
      taker100  = apply_pct_fee(proxy_taker100,  100,  pct),
      taker500  = apply_pct_fee(proxy_taker500,  500,  pct),
      taker1000 = apply_pct_fee(proxy_taker1000, 1000, pct)
    )
  
  bind_rows(bison, bsdex, bitpanda)
}

est <- mk_estimated_exchanges(proxy_limited)


#####################################
# 4) Combine real data + estimates
#####################################

real_for_cmp <- real_only %>%
  select(source, symbol, datetime_min, starts_with("taker"))

combined <- bind_rows(real_for_cmp, est)


############################################################
# 5) Winner scoring system across ALL notionals
#
# For each:
#   - symbol × datetime_min × notional
# winner gets 1 point.
#
# Each timestamp has 3 scoring rounds:
#   - 100 € winner → 1 point
#   - 500 € winner → 1 point
#   - 1,000 € winner → 1 point
############################################################

# Convert into a long format so we can score 100/500/1000 the same way
combined_long <- combined %>%
  pivot_longer(
    cols = starts_with("taker"),
    names_to = "notional",
    values_to = "coins"
  ) %>%
  mutate(
    # make labels nicer (optional)
    notional = case_when(
      notional == "taker100"  ~ "100 €",
      notional == "taker500"  ~ "500 €",
      notional == "taker1000" ~ "1,000 €"
    )
  )

# 1 point to the winner per notional
score_all <- combined_long %>%
  filter(!is.na(coins)) %>%
  group_by(symbol, datetime_min, notional) %>%
  slice_max(coins, with_ties = FALSE) %>%   # winner
  ungroup() %>%
  count(source) %>%                         # total points
  rename(points = n)

# Add 0 for any exchange that never wins
score_all <- score_all %>%
  complete(source = names(labels_map), fill = list(points = 0)) %>%
  mutate(source_label = recode(source, !!!labels_map),
         source_label = factor(source_label, levels = all_labels))


##########################
# 6) Final Visualization
##########################

ggplot(score_all, aes(x = source_label, y = points, fill = source_label)) +
  geom_col() +
  labs(
    title = "Total points across all trade sizes (100 €, 500 €, 1,000 €)",
    subtitle = "1 point per notional per minute when an exchange is best",
    x = "Exchange",
    y = "Points",
    fill = "Exchange"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
