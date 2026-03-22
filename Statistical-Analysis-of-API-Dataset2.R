############################################################
# Full Results Analysis & Figures
# Sections 7a, 7b, 7c of: "Pricing Transparency in 
# European Crypto Trading"
#
# Produces:
#   - Figure 1: Phase 1 winner scoring (revised)
#   - Figure 2: Phase 2 round-trip costs with CIs
#   - Table X:  Phase 2 descriptive statistics
#   - Table Y:  Pairwise Mann-Whitney U tests
#   - Figure 3: Cross-phase validation (predicted vs actual)
############################################################

########################
# 0) Libraries & Config
########################

library(tidyverse)
library(lubridate)
library(jsonlite)
library(readxl)

json_path  <- "full_API_Dataset.json"           # Phase 1
excel_path <- "Full-Manual-trades-data__1_.xlsx" # Phase 2

real_venues <- c("bitvavo", "coinbase", "kraken")

# Labels and ordered factor for all plots
labels_map <- c(
  bitvavo      = "Bitvavo",
  coinbase     = "Coinbase",
  kraken       = "Kraken",
  bison_est    = "Bison (est.)",
  bsdex_est    = "BSDEX (est.)",
  bitpanda_est = "Bitpanda (est.)"
)

phase2_order <- c("Bitvavo", "BSDEX", "Bison", "Kraken", "Bitpanda", "Coinbase")


############################################################
# ══════════════════════════════════════════════════════════
# PHASE 1 ANALYSIS
# ══════════════════════════════════════════════════════════
############################################################

#########################################
# 1) Load and clean API data
#########################################

data <- fromJSON(json_path, flatten = TRUE) %>%
  rename_with(~ gsub("^fees\\.taker\\.", "taker", .x)) %>%
  rename_with(~ gsub("^fees\\.maker\\.", "maker", .x)) %>%
  rename_with(~ gsub("^price_without_fee\\.taker\\.", "nofee_taker", .x)) %>%
  rename_with(~ gsub("^price_without_fee\\.maker\\.", "nofee_maker", .x)) %>%
  mutate(
    datetime = ymd_hms(datetime),
    datetime_min = floor_date(datetime, "minute")
  )


#########################################
# 2) Cross-exchange ask price convergence
#########################################

# Fee rates used (default retail taker tier)
fee_rates <- c(bitvavo = 0.0025, kraken = 0.0100, coinbase = 0.0149)

# Recover raw ask price: ask = 100*(1-fee) / nofee_taker100
# Because nofee_taker100 = coins_received_after_fee = 100*(1-fee)/ask
convergence <- data %>%
  filter(source %in% real_venues) %>%
  mutate(
    fee_rate = fee_rates[source],
    raw_ask  = 100 * (1 - fee_rate) / nofee_taker100
  ) %>%
  select(source, symbol, datetime_min, raw_ask) %>%
  group_by(symbol, datetime_min) %>%
  filter(n_distinct(source) == 3) %>%
  summarise(
    mean_ask   = mean(raw_ask),
    spread_pct = (max(raw_ask) - min(raw_ask)) / mean(raw_ask) * 100,
    .groups = "drop"
  )

cat("\n=== Cross-Exchange Ask Price Convergence ===\n")
convergence %>%
  group_by(symbol) %>%
  summarise(
    n         = n(),
    mean_spread  = mean(spread_pct),
    median_spread = median(spread_pct),
    sd_spread = sd(spread_pct),
    .groups = "drop"
  ) %>%
  print()


#########################################
# 3) Winner scoring (revised from original)
#########################################

# Build proxy price from best no-fee price across real venues
real_only <- data %>% filter(source %in% real_venues)

safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

proxy <- real_only %>%
  group_by(symbol, datetime_min) %>%
  summarise(
    proxy100  = safe_max(nofee_taker100),
    proxy500  = safe_max(nofee_taker500),
    proxy1000 = safe_max(nofee_taker1000),
    .groups = "drop"
  )

# Estimated venues
apply_pct_fee <- function(coins, notional, pct) coins * (1 - pct)

est <- bind_rows(
  proxy %>% transmute(source = "bison_est", symbol, datetime_min,
                      taker100 = apply_pct_fee(proxy100, 100, 0.0125),
                      taker500 = apply_pct_fee(proxy500, 500, 0.0125),
                      taker1000 = apply_pct_fee(proxy1000, 1000, 0.0125)),
  proxy %>% transmute(source = "bsdex_est", symbol, datetime_min,
                      taker100 = apply_pct_fee(proxy100, 100, 0.0035),
                      taker500 = apply_pct_fee(proxy500, 500, 0.0035),
                      taker1000 = apply_pct_fee(proxy1000, 1000, 0.0035)),
  proxy %>% mutate(pct = if_else(symbol %in% c("BTCEUR","ETHEUR"), 0.0099, 0.0249)) %>%
    transmute(source = "bitpanda_est", symbol, datetime_min,
              taker100 = apply_pct_fee(proxy100, 100, pct),
              taker500 = apply_pct_fee(proxy500, 500, pct),
              taker1000 = apply_pct_fee(proxy1000, 1000, pct))
)

real_cmp <- real_only %>% select(source, symbol, datetime_min, starts_with("taker"))
combined <- bind_rows(real_cmp, est)

# Long format and score
combined_long <- combined %>%
  pivot_longer(cols = starts_with("taker"), names_to = "notional", values_to = "coins") %>%
  mutate(notional = recode(notional, taker100="100 €", taker500="500 €", taker1000="1,000 €"))

score_all <- combined_long %>%
  filter(!is.na(coins)) %>%
  group_by(symbol, datetime_min, notional) %>%
  slice_max(coins, with_ties = FALSE) %>%
  ungroup() %>%
  count(source) %>%
  rename(points = n) %>%
  complete(source = names(labels_map), fill = list(points = 0)) %>%
  mutate(
    source_label = recode(source, !!!labels_map),
    source_label = factor(source_label, levels = unname(labels_map)),
    is_estimated = str_detect(source, "_est")
  )

cat("\n=== Phase 1 Winner Scores ===\n")
print(score_all)

# FIGURE 1: Winner scoring with estimated venues distinguished
fig1 <- ggplot(score_all, aes(x = source_label, y = points, fill = source_label, alpha = is_estimated)) +
  geom_col(colour = "grey30", linewidth = 0.3) +
  scale_alpha_manual(values = c("FALSE" = 1, "TRUE" = 0.5), guide = "none") +
  labs(
    title = "Phase 1: Cross-venue execution price comparison",
    subtitle = "1 point per notional (€100, €500, €1,000) per timestamp when exchange offers best price\nHatched bars = estimated from published fees (lower-bound cost only)",
    x = "Exchange",
    y = "Points",
    fill = "Exchange"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggsave("figure1_phase1_scores.pdf", fig1, width = 8, height = 5)
cat("\nSaved: figure1_phase1_scores.pdf\n")


#########################################
# 4) Mean cost deviation from reference
#########################################

# Effective price per exchange = 100 / coins_after_fee
# Reference = mean effective price across 3 venues

deviation_data <- data %>%
  filter(source %in% real_venues) %>%
  mutate(eff_price = 100 / nofee_taker100) %>%
  select(source, symbol, datetime_min, eff_price) %>%
  group_by(symbol, datetime_min) %>%
  filter(n_distinct(source) == 3) %>%
  mutate(ref_price = mean(eff_price)) %>%
  ungroup() %>%
  mutate(pct_deviation = (eff_price / ref_price - 1) * 100)

cat("\n=== Phase 1: Mean cost deviation from reference (%) ===\n")
deviation_data %>%
  group_by(source) %>%
  summarise(
    mean_dev = mean(pct_deviation),
    sd_dev   = sd(pct_deviation),
    n        = n(),
    .groups  = "drop"
  ) %>%
  arrange(mean_dev) %>%
  print()


############################################################
# ══════════════════════════════════════════════════════════
# PHASE 2 ANALYSIS
# ══════════════════════════════════════════════════════════
############################################################

#########################################
# 5) Load and parse real-money trades
#########################################

raw <- read_xlsx(excel_path, sheet = "Raw Data") %>%
  rename(
    exchange    = Exchange,
    symbol      = Symbol,
    side        = Side,
    dt          = datetime,
    amount_eur  = amount_eur,
    amount_coin = amount_coin,
    coin_price  = Coin_Price,
    fee_eur     = `Fee_€`
  ) %>%
  select(exchange, symbol, side, dt, amount_eur, amount_coin, coin_price, fee_eur)

# Fix the known date error: Coinbase ETHEUR sell recorded as 2024
raw <- raw %>%
  mutate(dt = if_else(year(dt) == 2024, dt + years(1), dt))

cat("\n=== Phase 2: Trade count by exchange ===\n")
raw %>% count(exchange, side) %>% print()


#########################################
# 6) Compute round-trip costs
#########################################

buys  <- raw %>% filter(side == "buy") %>%
  group_by(exchange, symbol) %>% mutate(trade_n = row_number()) %>% ungroup()
sells <- raw %>% filter(side == "sell") %>%
  group_by(exchange, symbol) %>% mutate(trade_n = row_number()) %>% ungroup()

rt <- buys %>%
  inner_join(sells, by = c("exchange", "symbol", "trade_n"), suffix = c("_buy", "_sell")) %>%
  mutate(
    rt_cost     = (amount_eur_sell - amount_eur_buy) / amount_eur_buy,
    rt_cost_pct = rt_cost * 100,
    gap_min     = as.numeric(difftime(dt_sell, dt_buy, units = "mins"))
  )

cat("\n=== Phase 2: Round-trips per exchange ===\n")
rt %>% count(exchange) %>% print()


#########################################
# 7) TABLE X: Descriptive statistics
#########################################

table_x <- rt %>%
  group_by(exchange) %>%
  summarise(
    n        = n(),
    mean_rt  = mean(rt_cost_pct),
    sd_rt    = sd(rt_cost_pct),
    ci_lower = mean_rt - qt(0.975, n() - 1) * sd(rt_cost_pct) / sqrt(n()),
    ci_upper = mean_rt + qt(0.975, n() - 1) * sd(rt_cost_pct) / sqrt(n()),
    .groups  = "drop"
  ) %>%
  mutate(exchange = factor(exchange, levels = phase2_order)) %>%
  arrange(exchange)

cat("\n=== TABLE X: Phase 2 Descriptive Statistics ===\n")
print(table_x, width = Inf)


#########################################
# 8) Per-asset breakdown (indicative)
#########################################

table_x_asset <- rt %>%
  group_by(exchange, symbol) %>%
  summarise(
    n       = n(),
    mean_rt = mean(rt_cost_pct),
    .groups = "drop"
  ) %>%
  mutate(exchange = factor(exchange, levels = phase2_order)) %>%
  arrange(exchange, symbol)

cat("\n=== Per-Asset Breakdown (indicative, small N) ===\n")
print(table_x_asset, width = Inf)


#########################################
# 9) FIGURE 2: Round-trip costs with CIs
#########################################

fig2 <- ggplot(table_x, aes(x = exchange, y = mean_rt)) +
  geom_col(aes(fill = exchange), width = 0.7, alpha = 0.85) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.25, linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f%%", mean_rt)),
            vjust = 1.5, size = 3.5, fontface = "bold", colour = "white") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Phase 2: Mean round-trip cost by platform",
    subtitle = "Error bars = 95% confidence intervals | €100 buy-sell round-trips",
    x = NULL, y = "Round-trip cost (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11)) +
  scale_fill_brewer(palette = "Set2")

ggsave("figure2_phase2_rt_costs.pdf", fig2, width = 8, height = 5)
cat("\nSaved: figure2_phase2_rt_costs.pdf\n")


#########################################
# 10) TABLE Y: Pairwise Mann-Whitney U
#########################################

pairs <- combn(phase2_order, 2, simplify = FALSE)

table_y <- map_dfr(pairs, function(pair) {
  x <- rt %>% filter(exchange == pair[1]) %>% pull(rt_cost)
  y <- rt %>% filter(exchange == pair[2]) %>% pull(rt_cost)
  test <- wilcox.test(x, y, exact = FALSE)
  
  # Rank-biserial correlation (effect size)
  nx <- length(x); ny <- length(y)
  U  <- test$statistic
  r  <- 1 - (2 * U) / (nx * ny)   # rank-biserial
  
  tibble(
    exchange_1 = pair[1],
    exchange_2 = pair[2],
    U          = test$statistic,
    p_value    = test$p.value,
    r_rb       = r,
    sig_nominal  = case_when(
      test$p.value < 0.001 ~ "***",
      test$p.value < 0.01  ~ "**",
      test$p.value < 0.05  ~ "*",
      TRUE                 ~ "n.s."
    ),
    sig_bonferroni = if_else(test$p.value < 0.05/15, "sig", "n.s.")
  )
})

cat("\n=== TABLE Y: Pairwise Mann-Whitney U Tests ===\n")
print(table_y, width = Inf)

# Key finding
cat("\n*** KEY FINDING: Kraken vs Bitpanda is NOT significant ***\n")
table_y %>% filter(exchange_1 == "Kraken", exchange_2 == "Bitpanda") %>% print(width = Inf)


#########################################
# 11) Cross-phase validation
#########################################

# Phase 1 one-way fee rates (applied to order-book prices)
phase1_fee <- tibble(
  exchange    = c("Bitvavo", "Kraken", "Coinbase"),
  oneway_fee  = c(0.25, 1.00, 1.49),
  predicted_rt = 2 * oneway_fee
)

# Actual Phase 2
phase2_actual <- table_x %>%
  select(exchange, actual_rt = mean_rt) %>%
  mutate(actual_rt = abs(actual_rt))  # make positive for comparison

# Join and compute hidden spread
cross_phase <- phase1_fee %>%
  left_join(phase2_actual, by = "exchange") %>%
  mutate(hidden_spread = actual_rt - predicted_rt)

cat("\n=== Cross-Phase Validation (API platforms only) ===\n")
print(cross_phase, width = Inf)

# Also add estimated platforms
est_platforms <- tibble(
  exchange     = c("BSDEX", "Bison", "Bitpanda"),
  oneway_fee   = c(0.35, 1.25, 1.49),  # published fees
  predicted_rt = 2 * oneway_fee
) %>%
  left_join(phase2_actual, by = "exchange") %>%
  mutate(hidden_spread = actual_rt - predicted_rt)

cat("\n=== Cross-Phase Validation (estimated platforms) ===\n")
print(est_platforms, width = Inf)

# FIGURE 3: Predicted vs Actual RT costs
fig3_data <- bind_rows(
  cross_phase %>% mutate(type = "API-observed"),
  est_platforms %>% mutate(type = "Fee-estimated")
) %>%
  mutate(exchange = factor(exchange, levels = phase2_order))

fig3 <- ggplot(fig3_data, aes(x = exchange)) +
  geom_col(aes(y = actual_rt, fill = "Actual (Phase 2)"), width = 0.45, position = position_nudge(x = 0.22)) +
  geom_col(aes(y = predicted_rt, fill = "Predicted (2× Phase 1 fee)"), width = 0.45, position = position_nudge(x = -0.22)) +
  scale_fill_manual(values = c("Actual (Phase 2)" = "#e74c3c", "Predicted (2× Phase 1 fee)" = "#3498db")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Cross-phase validation: predicted vs. actual round-trip costs",
    subtitle = "Gap between bars = hidden retail spread not captured by published fees or order-book data",
    x = NULL, y = "Round-trip cost (%)", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

ggsave("figure3_cross_phase.pdf", fig3, width = 9, height = 5)
cat("\nSaved: figure3_cross_phase.pdf\n")


############################################################
# 12) Summary output
############################################################

cat("\n\n========================================\n")
cat("SUMMARY OF KEY RESULTS FOR SECTION 7\n")
cat("========================================\n\n")

cat("Phase 1:\n")
cat("  - 86 matched timestamps across 3 exchanges, 3 assets\n")
cat("  - Bitvavo lowest effective price at 100% of timestamps\n")
cat("  - Cross-exchange ask convergence: BTC 0.030%, ETH 0.037%, ALGO 0.061%\n\n")

cat("Phase 2:\n")
cat("  - 50 round-trips across 6 platforms, 12 trading days\n")
cat("  - Three-tier structure (after Bonferroni):\n")
cat("    Tier 1: Bitvavo (-0.58%), BSDEX (-0.88%)\n")
cat("    Tier 2: Bison (-2.58%)\n")
cat("    Tier 3: Kraken (-5.81%), Bitpanda (-6.23%), Coinbase (-7.49%)\n")
cat("  - Kraken vs Bitpanda: NOT significant (p=0.589)\n\n")

cat("Cross-phase validation:\n")
cat("  - Bitvavo hidden spread: 0.08% (negligible)\n")
cat("  - Kraken hidden spread:  3.81%\n")
cat("  - Coinbase hidden spread: 4.51%\n")

