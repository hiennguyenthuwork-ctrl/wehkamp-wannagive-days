# Wehkamp Wannagive Days Campaign Analysis
[![R](https://img.shields.io/badge/R-276DC3?style=flat&logo=r&logoColor=white)](https://www.r-project.org)
![Status](https://img.shields.io/badge/Status-Completed-brightgreen)
![Impact](https://img.shields.io/badge/Revenue_Impact-€310K-green)
---
## Project Overview

Comprehensive end-to-end analysis of Wehkamp’s Wannagive Days app-exclusive campaign (3–24 December 2024). The project evaluated customer engagement, identified a critical streak logic bug, quantified business impact (€310K incremental revenue), and provided concrete recommendations for the next edition.

**Goal**: Understand why the campaign succeeded in short-term revenue but failed in long-term habit formation, and how to improve it.

---

## Business Problem & Objective

Wehkamp wanted to:
- Reward loyal customers and increase app usage during Christmas
- Test a new daily engagement mechanic (streaks + rewards)
- Measure real incremental revenue and long-term loyalty impact

**My task**: Analyze 5 data tables (logging, coupons, orders, customer info) and answer 12 key business questions.

---

## Key Results & Business Impact

**Campaign Performance**
- **16,030** unique participants
- Average daily open rate: **21.8%**
- Median streak length: **1 day** (42.5% of users opened only 1 card)
- Only **5.4%** reached long streaks (≥7 days)

**Business Impact**
!(visualizations/campaign_performance.jpeg)
- Generated **€310,341** incremental revenue (+€0.88 daily lift per participant)
- Orders increased **+38%** and demand **+30%** during the campaign
- Participants outperformed non-participants by **2–3×** in orders & demand
- Post-campaign demand returned to exact baseline → short-term lift only

**Critical Discovery**
- **Streak logic bug** affected 1,844 users (11.5% of participants)
- Caused incorrect streak calculation → over-discount of **€6,827** (34.8% of all campaign discounts came from this bug)

---

## Methodology

1. Data ingestion & cleaning (5 Excel tabs + timezone alignment)
2. Engagement & streak analysis (cohort retention, streak distribution)
3. Bug detection (gap analysis between logged streak vs actual participation)
4. Business impact measurement (pre/during/post comparison + incremental lift)
5. Customer segmentation by engagement level & pre-campaign behavior
6. Statistical validation (ANOVA, Chi-square where applicable)

---

## Tech Stack

- **Language**: R
- **Main packages**: `dplyr`, `ggplot2`, `lubridate`, `tidyr`
- **Tools**: RStudio, Git, Excel

---

## Project Structure
Wehkamp-Wannagive-Days/
├── README.md
├── data/
│   └── raw_data_description.md
├── scripts/
│   ├── 01_data_cleaning.R
│   ├── 02_streak_calculation.R
│   ├── 03_bug_detection.R
│   └── 04_business_impact.R
├── reports/
│   ├── Final_Presentation.pdf
│   └── Executive_Summary.pdf
├── visualizations/
│   ├── retention_curve.png
│   ├── streak_distribution.png
│   └── daily_open_rate.png
└── requirements/
└── renv.lock

## How to Run the Code

```bash
git clone https://github.com/hiennguyenthuwork-ctrl/wehkamp-wannagive-days.git
cd Wehkamp-Wannagive-Days

# Restore R environment
Rscript -e "renv::restore()"

# Run analysis
Rscript scripts/01_data_cleaning.R
