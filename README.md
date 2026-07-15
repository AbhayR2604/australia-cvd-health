# Australia CVD Health Dashboard

## Project Overview

This project is an interactive R Shiny dashboard that visualises cardiovascular disease-related health patterns across Australia. The dashboard explores state-level cardiovascular disease prevalence, health condition trends, age and gender differences, and risk factor patterns using Australian health survey data.

The project demonstrates data wrangling, interactive dashboard development, geospatial visualisation, and health data storytelling using R.

## Objectives

The dashboard was designed to:

1. Visualise cardiovascular disease prevalence across Australian states and territories.
2. Compare health patterns by age group and gender.
3. Explore trends in selected health conditions over time.
4. Display risk factor patterns using interactive visualisations.
5. Present health data in an accessible and user-friendly dashboard format.

## Tools and Technologies

- R
- R Shiny
- tidyverse
- dplyr
- ggplot2
- plotly
- leaflet
- sf
- readxl
- HTML/CSS dashboard components
- Geospatial visualisation

## Key Features

### 1. Interactive Dashboard

The project uses R Shiny to create an interactive dashboard interface where users can explore different cardiovascular disease-related health indicators.

### 2. Choropleth Map

A state-level choropleth map is used to visualise cardiovascular disease prevalence across Australia, making regional differences easier to understand.

### 3. Trend Visualisation

The dashboard includes trend-based visualisations to show how selected health conditions vary over time.

### 4. Age and Gender Analysis

The dashboard explores how cardiovascular disease-related patterns differ across age groups and gender categories.

### 5. Risk Factor Heatmap

A heatmap is used to compare health-related risk factors across different groups, helping identify patterns and areas of concern.

## Skills Demonstrated

- Data wrangling and preprocessing
- R Shiny dashboard development
- Interactive data visualisation
- Geospatial mapping
- Health data analysis
- Exploratory data analysis
- Data storytelling
- Dashboard design
- Cleaning and transforming Excel-based datasets
- Preparing cleaned CSV files for visualisation

## Files in This Repository

```text
australia-cvd-health-dashboard/
│
├── README.md
├── app.R
├── data_wrangling.R
├── Data/
│   ├── cvd_prevalence_by_state.csv
│   ├── conditions_trend_australia.csv
│   └── aus_states.geojson
│
├── National-Health-Survey-2022/
│   └── Raw Excel files used for data preparation
│
└── Screenshots/
    ├── age_gender_divide.png
    ├── choropleth_map.png
    ├── risk_heatmap.png
    └── trend_plot.png
