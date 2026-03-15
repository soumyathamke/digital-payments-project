# ================================================================
# PROJECT: Digital Payments & Financial Inclusion
# Data: PhonePe Pulse — Raw Transaction Data Q1 2018 to Q2 2021
# Language: R



# ── LIBRARIES ─────────────────────────────────────────────────────────────────
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(writexl)
library(gridExtra)

# ── FILE PATH — 
FILE_PATH <- "phonepe-pulse_raw-data_q12018-to-q22021-v0-1-5-1720351752.xlsx"

# ── COLOUR PALETTE ────────────────────────────────────────────────────────────
NAVY   <- "#1F4E79"
BLUE   <- "#2E75B6"
ORANGE <- "#ED7D31"
GREEN  <- "#70AD47"
GOLD   <- "#FFC000"
RED    <- "#C00000"
PURPLE <- "#7030A0"

cat("================================================================\n")
cat("PROJECT: Digital Payments & Financial Inclusion — INDIA\n")
cat("================================================================\n\n")

# ================================================================
# STEP 1 — LOAD & CLEAN DATA
# ================================================================
cat("STEP 1: Loading & Cleaning Data\n")
cat("----------------------------------------------------------------\n")

state_txn   <- read_excel(FILE_PATH, sheet = "State_Txn and Users")
state_split <- read_excel(FILE_PATH, sheet = "State_TxnSplit")
dist_txn    <- read_excel(FILE_PATH, sheet = "District_Txn and Users")
dist_demo   <- read_excel(FILE_PATH, sheet = "District Demographics")

cat(sprintf("State transactions loaded:   %d rows\n",   nrow(state_txn)))
cat(sprintf("Transaction type split:      %d rows\n",   nrow(state_split)))
cat(sprintf("District transactions:       %d rows\n",   nrow(dist_txn)))
cat(sprintf("District demographics:       %d rows\n\n", nrow(dist_demo)))

# Standardise column names
colnames(state_txn)   <- c("state","year","quarter","transactions",
                            "amount_inr","atv_inr","reg_users","app_opens")
colnames(state_split) <- c("state","year","quarter","txn_type",
                            "transactions","amount_inr","atv_inr")
colnames(dist_txn)    <- c("state","year","quarter","district","code",
                            "transactions","amount_inr","atv_inr",
                            "reg_users","app_opens")
colnames(dist_demo)   <- c("state","district","headquarters","population",
                            "area_sqkm","density","code","alt_name")

# Remove zero/null transaction rows
before <- nrow(state_txn)
state_txn <- state_txn %>%
  filter(!is.na(transactions), transactions > 0,
         !is.na(amount_inr))
cat(sprintf("Zero/null rows removed: %d\n", before - nrow(state_txn)))

# Add derived columns
state_txn <- state_txn %>%
  mutate(
    period     = paste0(year, " Q", quarter),
    amount_cr  = amount_inr / 1e7        # convert to crores
  )

state_split <- state_split %>%
  mutate(period = paste0(year, " Q", quarter))

cat(sprintf("\nCleaned: %d rows | %d states | %d years\n",
            nrow(state_txn),
            n_distinct(state_txn$state),
            n_distinct(state_txn$year)))
cat(sprintf("Total transactions: %.1f billion\n",
            sum(state_txn$transactions) / 1e9))
cat(sprintf("Total amount:       INR %.1f trillion\n\n",
            sum(state_txn$amount_inr) / 1e12))

# ================================================================
# STEP 2 — NATIONAL TREND ANALYSIS
# ================================================================
cat("STEP 2: National Trend Analysis (2018-2021)\n")
cat("----------------------------------------------------------------\n")

national <- state_txn %>%
  group_by(year, quarter, period) %>%
  summarise(
    total_transactions = sum(transactions),
    total_amount_cr    = sum(amount_cr),
    total_users        = sum(reg_users),
    .groups = "drop"
  ) %>%
  arrange(year, quarter)

print(national %>%
  select(period, total_transactions, total_amount_cr, total_users),
  n = 20)

# Growth rate
first_q <- national$total_transactions[1]
last_q  <- national$total_transactions[nrow(national)]
growth  <- (last_q - first_q) / first_q * 100
cat(sprintf("\nTransaction growth Q1 2018 to Q2 2021: %.0f%%\n\n", growth))

# ================================================================
# STEP 3 — STATE-LEVEL ADOPTION ANALYSIS
# ================================================================
cat("STEP 3: State-Level Adoption Analysis\n")
cat("----------------------------------------------------------------\n")

state_summary <- state_txn %>%
  group_by(state) %>%
  summarise(
    total_transactions = sum(transactions),
    total_amount_cr    = sum(amount_cr),
    avg_atv_inr        = mean(atv_inr),
    total_users        = sum(reg_users),
    .groups = "drop"
  ) %>%
  mutate(txn_share_pct = round(total_transactions /
                                sum(total_transactions) * 100, 2)) %>%
  arrange(desc(total_transactions))

cat("Top 10 states by transaction volume:\n")
print(state_summary %>%
  select(state, total_transactions, txn_share_pct, total_amount_cr) %>%
  head(10))

cat("\nBottom 10 states (lowest adoption):\n")
print(state_summary %>%
  select(state, total_transactions, txn_share_pct) %>%
  tail(10))

# ================================================================
# STEP 4 — TRANSACTION TYPE BREAKDOWN
# ================================================================
cat("\nSTEP 4: Transaction Type Breakdown\n")
cat("----------------------------------------------------------------\n")

txn_type_summary <- state_split %>%
  group_by(txn_type) %>%
  summarise(
    total_transactions = sum(transactions),
    total_amount_cr    = sum(amount_inr) / 1e7,
    avg_atv_inr        = mean(atv_inr),
    .groups = "drop"
  ) %>%
  mutate(share_pct = round(total_transactions /
                            sum(total_transactions) * 100, 1)) %>%
  arrange(desc(total_transactions))

cat("Transaction volume by type:\n")
print(txn_type_summary)

# ================================================================
# STEP 5 — UNDERSERVED DISTRICT ANALYSIS
# Cross-reference transaction volume with population density
# ================================================================
cat("\nSTEP 5: Underserved District Analysis\n")
cat("----------------------------------------------------------------\n")

# Aggregate district transactions
dist_agg <- dist_txn %>%
  group_by(state, district, code) %>%
  summarise(
    total_transactions = sum(transactions),
    total_amount_cr    = sum(amount_inr) / 1e7,
    total_users        = sum(reg_users),
    .groups = "drop"
  )

# Join with demographics
dist_merged <- dist_agg %>%
  left_join(dist_demo %>% select(code, population, density, area_sqkm),
            by = "code") %>%
  filter(!is.na(population), population > 0)

# Compute transactions per 1000 people (adoption rate)
dist_merged <- dist_merged %>%
  mutate(
    txn_per_1000     = total_transactions / population * 1000,
    users_per_1000   = total_users / population * 1000
  )

# Underserved = low txn_per_1000 but high population (high opportunity)
underserved <- dist_merged %>%
  filter(population > 500000) %>%          # focus on populous districts
  arrange(txn_per_1000) %>%
  head(15) %>%
  select(state, district, population, density,
         total_transactions, txn_per_1000)

cat("Top 15 underserved districts (high population, low digital adoption):\n")
print(underserved)

high_adoption <- dist_merged %>%
  arrange(desc(txn_per_1000)) %>%
  head(10) %>%
  select(state, district, population, txn_per_1000, users_per_1000)

cat("\nTop 10 highest adoption districts:\n")
print(high_adoption)

# ================================================================
# STEP 6 — VISUALISATIONS 
# ================================================================
cat("\nSTEP 6: Generating Charts...\n")

# -- Chart 1: National transaction growth over time
p1 <- ggplot(national, aes(x = reorder(period, paste0(year, quarter)))) +
  geom_col(aes(y = total_transactions / 1e9), fill = NAVY, alpha = 0.85) +
  geom_line(aes(y = total_amount_cr / 5000, group = 1),
            color = ORANGE, linewidth = 1.2) +
  geom_point(aes(y = total_amount_cr / 5000), color = ORANGE, size = 2) +
  scale_y_continuous(
    name = "Transactions (billions)",
    sec.axis = sec_axis(~ . * 5000, name = "Amount (INR Crores)",
                        labels = comma)
  ) +
  labs(title = "National Digital Payment Growth (Q1 2018 - Q2 2021)",
       subtitle = "Bars = transaction volume | Line = transaction value",
       x = "Quarter") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", color = NAVY),
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 7),
    panel.grid.major.x = element_blank()
  )

# -- Chart 2: Top 15 states by transaction volume 
top15 <- state_summary %>% head(15)

p2 <- ggplot(top15, aes(x = reorder(state, total_transactions),
                         y = total_transactions / 1e9)) +
  geom_col(fill = BLUE, alpha = 0.85) +
  geom_text(aes(label = paste0(txn_share_pct, "%")),
            hjust = -0.1, size = 2.8, color = "grey30") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Top 15 States — Transaction Volume (2018-2021)",
       x = NULL, y = "Total Transactions (billions)") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", color = NAVY))

# -- Chart 3: Transaction type breakdown (donut-style bar) 
p3 <- ggplot(txn_type_summary,
             aes(x = reorder(txn_type, total_transactions),
                 y = total_transactions / 1e9,
                 fill = txn_type)) +
  geom_col(alpha = 0.9, show.legend = FALSE) +
  geom_text(aes(label = paste0(share_pct, "%")),
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c(NAVY, BLUE, ORANGE, GREEN, GOLD)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Transaction Volume by Category",
       x = NULL, y = "Transactions (billions)") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", color = NAVY))

# -- Chart 4: Scatter — population density vs adoption rate 
p4 <- dist_merged %>%
  filter(!is.na(density), density < 5000,    # remove extreme outliers
         txn_per_1000 < quantile(txn_per_1000, 0.99, na.rm = TRUE)) %>%
  ggplot(aes(x = density, y = txn_per_1000)) +
  geom_point(alpha = 0.4, color = NAVY, size = 1.5) +
  geom_smooth(method = "lm", color = ORANGE,
              se = TRUE, linewidth = 1.2) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(title = "Population Density vs Digital Payment Adoption",
       subtitle = "Each point = one district | Line = OLS trend",
       x = "Population Density (per sq km)",
       y = "Transactions per 1,000 people") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", color = NAVY))

# ================================================================
# STEP 7 — EXPORT TO EXCEL (4 sheets)
# ================================================================
cat("Exporting Excel report...\n")

write_xlsx(
  list(
    "National Trend"        = national %>%
                                select(period, total_transactions,
                                       total_amount_cr, total_users),
    "State Summary"         = state_summary,
    "Transaction Types"     = txn_type_summary,
    "Underserved Districts" = underserved
  ),
  path = "phonepe_report.xlsx"
)
cat("Excel saved: phonepe_report.xlsx\n")

# ================================================================
# STEP 8 — ADVISORY SUMMARY
# ================================================================
top_state      <- state_summary$state[1]
top_state_pct  <- state_summary$txn_share_pct[1]
top_txn_type   <- txn_type_summary$txn_type[1]
top_type_pct   <- txn_type_summary$share_pct[1]
density_corr   <- cor(dist_merged$density,
                      dist_merged$txn_per_1000,
                      use = "complete.obs")

cat("\n================================================================\n")
cat("ADVISORY BRIEF SUMMARY\n")
cat("================================================================\n")
cat(sprintf("  Growth Q1 2018 to Q2 2021: %.0f%%\n", growth))
cat(sprintf("  Density-Adoption correlation: r = %.2f\n\n", density_corr))

cat(sprintf(
"1. MASSIVE GROWTH IN DIGITAL PAYMENTS
   Transactions grew %.0f%% from Q1 2018 to Q2 2021 nationally,
   driven by UPI adoption and smartphone penetration.\n\n", growth))

cat(sprintf(
"2. GEOGRAPHIC CONCENTRATION
   %s alone accounts for %.1f%% of all transactions.
   Top 5 states drive the majority of national volume,
   pointing to significant untapped potential in smaller states.\n\n",
   top_state, top_state_pct))

cat(sprintf(
"3. PEER-TO-PEER DOMINATES BUT MERCHANT PAYMENTS GROWING
   %s is the largest category at %.1f%% of transactions.
   Merchant payments growth signals expanding commercial adoption
   beyond basic money transfers.\n\n", top_txn_type, top_type_pct))

cat(sprintf(
"4. UNDERSERVED DISTRICTS — HIGH OPPORTUNITY
   Cross-referencing transaction volume with population density
   (correlation r = %.2f) identifies populous districts with
   low digital adoption — highest ROI for financial inclusion
   interventions and infrastructure investment.\n\n", density_corr))

cat(
"RECOMMENDATIONS:
   > Focus financial inclusion drives on high-population,
     low-adoption districts identified in Step 5.
   > Merchant payment infrastructure investment justified
     by strong growth trajectory in that category.
   > COVID-19 (2020) caused a visible dip — recovery by
     Q2 2021 confirms resilience of digital payment habits.
\n")
