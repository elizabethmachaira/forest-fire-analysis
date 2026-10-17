# Statistical Analysis of Forest Fire Occurrence and Burned Area

A statistical investigation into the factors influencing forest fire occurrence and burned area extent, using the UCI Forest Fires dataset. The analysis covers frequency analysis, non-parametric hypothesis testing, and regression modelling.

## Overview

The study addresses two questions:
1. Is fire occurrence (Fire / No Fire) associated with temporal factors (day of week, month, weekend)?
2. What weather and fire-index variables predict the burned area?

## Contents

| File | Description |
|---|---|
| `forest_fires.R` | Full analysis: EDA, hypothesis tests, GLM, variable selection |

## Methods

**Part 1 — Exploratory Analysis**
- Descriptive statistics; fire/no-fire frequency by day, month, and weekend
- Variable transformations: `log(area+1)`, `FFMC³`, `log(ISI+1)`, `rain^(1/3)` to correct skew

**Part 2 — Hypothesis Testing**
- Chi-square tests: association between fire occurrence and day / month / weekend
- Shapiro-Wilk test for normality; Levene and Brown-Forsythe tests for homogeneity of variances
- Kruskal-Wallis test with ε² effect size; Dunn's post-hoc test (BH correction)

**Part 3 — Regression Modelling**
- GLM (Gaussian family) on log-transformed area
- Hosmer-Lemeshow goodness-of-fit; Nagelkerke pseudo-R²; VIF for multicollinearity
- Stepwise variable selection (likelihood ratio test)

## Stack

R · tidyverse · ggplot2 · jmv · car · rcompanion · FSA · ResourceSelection
