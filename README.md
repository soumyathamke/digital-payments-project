# Digital Payments & Financial Inclusion — India (2018–2021)

## Project Overview
Analysis of 20.5 billion digital payment transactions across 36 Indian 
states using PhonePe Pulse data (Q1 2018 to Q2 2021).

## Tools Used
- **R** (dplyr, ggplot2, readxl) — data cleaning and analysis
- **Power BI** — interactive dashboard
- **Excel** — data exports and reporting

## Data Source
PhonePe Pulse — Raw Transaction Data
a coding platform

## Key Findings
- Transaction volume grew **600%+** from Q1 2018 to Q2 2021
- Maharashtra and Karnataka account for the highest transaction volumes
- Peer-to-peer payments dominate at ~50% of all transactions
- COVID-19 caused a visible dip in 2020 — full recovery by Q2 2021
- Cross-referencing population density identified underserved districts 
  with high financial inclusion potential

## Files
| File | Description |
|------|-------------|
| `digital_pay.R` | Full R analysis script |
| `Digital Payments...pbix` | Power BI dashboard |
| `phonepe_report.xlsx` | Summary Excel report |
| `state_txn_clean.csv` | Cleaned state transaction data |
| `state_split_clean.csv` | Transaction category breakdown |
| `dist_txn_clean.csv` | District-level transaction data |
| `dist_demo_clean.csv` | District demographics data |

## Dashboard Visuals
- Top 10 states by transaction volume
- Transaction category breakdown (donut chart)
- National growth trend (2018–2021)
- Interactive slicers by year and state
