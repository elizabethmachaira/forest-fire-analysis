# Statistical Analysis of Forest Fire Occurrence and Burned Area

A statistical investigation into the factors influencing forest fire occurrence and the extent of burned area, using the UCI Forest Fires dataset.

## Overview

This project applies regression and statistical inference to understand how weather conditions (temperature, humidity, wind, rain) and spatial factors (FFMC, DMC, DC, ISI indices) relate to fire size.

Key questions explored:
- Which variables most strongly predict burned area?
- How do fire weather indices interact with environmental conditions?
- Can we build a reliable model for burned area given high skew in the response variable?

## Contents

| File | Description |
|---|---|
| `forest_fires.R` | Full analysis: EDA, modelling, inference |

## Methods

- Exploratory data analysis and visualisation
- Log-transformation of burned area to handle skew
- Multiple linear regression and model selection
- Residual diagnostics and hypothesis testing

## Stack

R · ggplot2 · dplyr
