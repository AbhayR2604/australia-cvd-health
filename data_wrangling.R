# Load libraries
library(tidyverse)
library(readxl)
library(janitor)
library(tibble)
library(stringr)

# Reading the file which has our statewise Prevalence data

cvd_raw <- read_excel(
  path = "National-Health-Survey-2022/NHSDC02.xlsx",
  sheet = 2,
  range = "A17:J29",
  col_names = FALSE
) %>%
  clean_names()

colnames(cvd_raw) <- c("condition","NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "AUS")

# Filtering the rows which has our CVD data
cvd_row <- cvd_raw %>%
  filter(condition == "Heart, stroke and vascular disease(g)")

cvd_long <- cvd_row %>%
  pivot_longer(cols = -condition, names_to = "state_code", values_to = "estimate_000") %>%
  select(state_code, estimate_000)

# Extract population row and pivot
population_row <- cvd_raw %>%
  filter(condition == "Total persons, all ages")

population_long <- population_row %>%
  pivot_longer(cols = -condition, names_to = "state_code", values_to = "total_population_000") %>%
  select(state_code, total_population_000)

# Merge estimates with population and calculate prevalence
cvd_by_state <- cvd_long %>%
  left_join(population_long, by = "state_code") %>%
  mutate(
    estimate_000 = as.numeric(estimate_000),
    total_population_000 = as.numeric(total_population_000),
    cvd_prevalence_percent = round((estimate_000 / total_population_000) * 100, 2)
  )

# Add state names and geographic coordinates
state_coords <- tibble::tribble(
  ~state_code, ~state,               ~latitude, ~longitude,
  "NSW", "New South Wales",          -33.87,  151.21,
  "VIC", "Victoria",                 -37.81,  144.96,
  "QLD", "Queensland",               -27.47,  153.03,
  "SA",  "South Australia",          -34.93,  138.60,
  "WA",  "Western Australia",        -31.95,  115.86,
  "TAS", "Tasmania",                 -42.88,  147.33,
  "NT",  "Northern Territory",       -12.46,  130.85,
  "ACT", "Australian Capital Territory", -35.28, 149.13,
  "AUS", "Australia",                -25.2744,  133.7751  # Central reference point
)

# Merge with your existing cvd_by_state
geo_cvd_long <- cvd_by_state %>%
  left_join(state_coords, by = "state_code")

# write_csv(geo_cvd_long, "data/cvd_prevalence_by_state.csv")

### Building our second visualisation in the top panel

# Step 1: Read the file
national_raw <- read_excel(
  path = "National-Health-Survey-2022/NHSDC01.xlsx",
  sheet = 2,
  range = "A17:H29",
  col_names = FALSE
)

# Step 2: Set and normalize year columns
colnames(national_raw) <- c(
  "condition", "2001", "2004_05", "2007_08", "2011_12", "2014_15", "2017_18", "2022"
)

# Step 3: Rename for consistent single-year representation
colnames(national_raw) <- c(
  "condition", "2001", "2005", "2008", "2012", "2015", "2018", "2022"
)

# Step 4: Filter target conditions
national_trend_filtered <- national_raw %>%
  filter(condition %in% c(
    "Heart, stroke and vascular disease(g)",
    "Hypertension(h)",
    "Diabetes mellitus(f)"
  ))

# 🔧 Step 5: Convert all year columns to character
national_trend_filtered <- national_trend_filtered %>%
  mutate(across(-condition, as.character))

# Step 6: Pivot and clean
national_trend_long <- national_trend_filtered %>%
  pivot_longer(
    cols = -condition,
    names_to = "year",
    values_to = "estimate_000"
  ) %>%
  mutate(
    condition = case_when(
      str_detect(condition, "Heart, stroke") ~ "CVD",
      str_detect(condition, "Hypertension") ~ "High blood pressure",
      str_detect(condition, "Diabetes") ~ "Diabetes"
    ),
    year = as.integer(year),
    estimate_000 = as.numeric(estimate_000)
  )

# Fixing the NA value for Diabetes in the year 2005
# Step 1: Pull diabetes values for 2001 and 2008
diabetes_2001 <- national_trend_long %>%
  filter(condition == "Diabetes", year == 2001) %>%
  pull(estimate_000)

diabetes_2008 <- national_trend_long %>%
  filter(condition == "Diabetes", year == 2008) %>%
  pull(estimate_000)

# Step 2: Calculate average annual increase
annual_increase <- (diabetes_2008 - diabetes_2001) / (2008 - 2001)

# Step 3: Estimate 2005
diabetes_2005_est <- diabetes_2001 + (2005 - 2001) * annual_increase

# Step 4: Update the NA in the original dataset
national_trend_long <- national_trend_long %>%
  mutate(estimate_000 = case_when(
    condition == "Diabetes" & year == 2005 & is.na(estimate_000) ~ diabetes_2005_est,
    TRUE ~ estimate_000
  ))

# Creating a new column state and giving it the value 'national' which will help us later

national_trend_long <- national_trend_long %>%
  mutate(state = "National")

# Calculating the Percentage prevalence of each condition

national_raw <- national_raw %>%
  mutate(across(-condition, as.character))

# Step 1: Extract population data from NSW sheet
national_population <- national_raw %>%
  filter(condition == "Total persons, all ages") %>%
  pivot_longer(-condition, names_to = "year", values_to = "population_000") %>%
  mutate(
    year = as.integer(year),
    population_000 = as.numeric(population_000)
  ) %>%
  select(-condition)

# Step 2: Merge with estimates and calculate prevalence %
national_trend_long <- national_trend_long %>%
  left_join(national_population, by = "year") %>%
  mutate(
    prevalence_percent = round((estimate_000 / population_000) * 100, 2)
  )
#------------------------------------South Australia----------------------------
# Creating the data for South Australia

sa_raw <- read_excel(
  path = "National-Health-Survey-2022/SAus_health.xlsx",
  sheet = 2,
  range = "A17:H29",
  col_names = FALSE
)

colnames(sa_raw) <- c(
  "condition", "2001", "2004_05", "2007_08", "2011_12", "2014_15", "2017_18", "2022"
)

colnames(sa_raw) <- c(
  "condition", "2001", "2005", "2008", "2012", "2015", "2018", "2022"
)

sa_State_filtered <- sa_raw %>%
  filter(condition %in% c(
    "Heart, stroke and vascular disease(g)",
    "Hypertension(h)",
    "Diabetes mellitus(f)"
  ))


sa_State_filtered <- sa_State_filtered %>%
  mutate(across(-condition, as.character))

sa_State_long <- sa_State_filtered %>%
  pivot_longer(
    cols = -condition,
    names_to = "year",
    values_to = "estimate_000"
  ) %>%
  mutate(
    condition = case_when(
      str_detect(condition, "Heart, stroke") ~ "CVD",
      str_detect(condition, "Hypertension") ~ "High blood pressure",
      str_detect(condition, "Diabetes") ~ "Diabetes"
    ),
    year = as.integer(year),
    estimate_000 = as.numeric(estimate_000)
  )

sa_State_long <- sa_State_long %>%
  mutate(state = "South Australia")

#--------------------------------Queensland-------------------------------------
# Creating the dataset for Queensland
qld_raw <- read_excel(
  path = "National-Health-Survey-2022/Queensland_health.xlsx",
  sheet = 2,
  range = "A17:H29",
  col_names = FALSE
)

colnames(qld_raw) <- c(
  "condition", "2001", "2004_05", "2007_08", "2011_12", "2014_15", "2017_18", "2022"
)

colnames(qld_raw) <- c(
  "condition", "2001", "2005", "2008", "2012", "2015", "2018", "2022"
)

qld_State_filtered <- qld_raw %>%
  filter(condition %in% c(
    "Heart, stroke and vascular disease(g)",
    "Hypertension(h)",
    "Diabetes mellitus(f)"
  ))


qld_State_filtered <- qld_State_filtered %>%
  mutate(across(-condition, as.character))

qld_State_long <- qld_State_filtered %>%
  pivot_longer(
    cols = -condition,
    names_to = "year",
    values_to = "estimate_000"
  ) %>%
  mutate(
    condition = case_when(
      str_detect(condition, "Heart, stroke") ~ "CVD",
      str_detect(condition, "Hypertension") ~ "High blood pressure",
      str_detect(condition, "Diabetes") ~ "Diabetes"
    ),
    year = as.integer(year),
    estimate_000 = as.numeric(estimate_000)
  )

qld_State_long <- qld_State_long %>%
  mutate(state = "Queensland")
#------------------------------------------------------------------------------

#------------------------------VICTORIA----------------------------------------

print("Reading VIC health data...")

# Creating the dataset for Victoria

vic_raw <- read_excel(
  path = "National-Health-Survey-2022/VIC_health.xlsx",
  sheet = 2,
  range = "A17:H29",
  col_names = FALSE
)

colnames(vic_raw) <- c(
  "condition", "2001", "2004_05", "2007_08", "2011_12", "2014_15", "2017_18", "2022"
)

colnames(vic_raw) <- c(
  "condition", "2001", "2005", "2008", "2012", "2015", "2018", "2022"
)

vic_State_filtered <- vic_raw %>%
  filter(condition %in% c(
    "Heart, stroke and vascular disease(g)",
    "Hypertension(h)",
    "Diabetes mellitus(f)"
  ))


vic_State_filtered <- vic_State_filtered %>%
  mutate(across(-condition, as.character))

vic_State_long <- vic_State_filtered %>%
  pivot_longer(
    cols = -condition,
    names_to = "year",
    values_to = "estimate_000"
  ) %>%
  mutate(
    condition = case_when(
      str_detect(condition, "Heart, stroke") ~ "CVD",
      str_detect(condition, "Hypertension") ~ "High blood pressure",
      str_detect(condition, "Diabetes") ~ "Diabetes"
    ),
    year = as.integer(year),
    estimate_000 = as.numeric(estimate_000)
  )

vic_State_long <- vic_State_long %>%
  mutate(state = "Victoria")
#----------------------------------------------------------------------------

#-------------------------New South Wales-------------------------------------------

# Creating the dataset for New South Wales
nsw_raw <- read_excel(
  path = "National-Health-Survey-2022/NSW_health.xlsx",
  sheet = 2,
  range = "A17:H29",
  col_names = FALSE
)

colnames(nsw_raw) <- c(
  "condition", "2001", "2004_05", "2007_08", "2011_12", "2014_15", "2017_18", "2022"
)

colnames(nsw_raw) <- c(
  "condition", "2001", "2005", "2008", "2012", "2015", "2018", "2022"
)

nsw_State_filtered <- nsw_raw %>%
  filter(condition %in% c(
    "Heart, stroke and vascular disease(g)",
    "Hypertension(h)",
    "Diabetes mellitus(f)"
  ))


nsw_State_filtered <- nsw_State_filtered %>%
  mutate(across(-condition, as.character))

nsw_State_long <- nsw_State_filtered %>%
  pivot_longer(
    cols = -condition,
    names_to = "year",
    values_to = "estimate_000"
  ) %>%
  mutate(
    condition = case_when(
      str_detect(condition, "Heart, stroke") ~ "CVD",
      str_detect(condition, "Hypertension") ~ "High blood pressure",
      str_detect(condition, "Diabetes") ~ "Diabetes"
    ),
    year = as.integer(year),
    estimate_000 = as.numeric(estimate_000)
  )
nsw_State_long <- nsw_State_long %>%
  mutate(state = "New South Wales")

#-----------------------------Western Australia---------------------------------

# Creating a dataset for Western Australia

wa_raw <- read_excel(
  path = "National-Health-Survey-2022/WA_health.xlsx",
  sheet = 2,
  range = "A17:H29",
  col_names = FALSE
)

colnames(wa_raw) <- c(
  "condition", "2001", "2004_05", "2007_08", "2011_12", "2014_15", "2017_18", "2022"
)

colnames(wa_raw) <- c(
  "condition", "2001", "2005", "2008", "2012", "2015", "2018", "2022"
)

wa_State_filtered <- wa_raw %>%
  filter(condition %in% c(
    "Heart, stroke and vascular disease(g)",
    "Hypertension(h)",
    "Diabetes mellitus(f)"
  ))


wa_State_filtered <- wa_State_filtered %>%
  mutate(across(-condition, as.character))

wa_State_long <- wa_State_filtered %>%
  pivot_longer(
    cols = -condition,
    names_to = "year",
    values_to = "estimate_000"
  ) %>%
  mutate(
    condition = case_when(
      str_detect(condition, "Heart, stroke") ~ "CVD",
      str_detect(condition, "Hypertension") ~ "High blood pressure",
      str_detect(condition, "Diabetes") ~ "Diabetes"
    ),
    year = as.integer(year),
    estimate_000 = as.numeric(estimate_000)
  )
wa_State_long <- wa_State_long %>%
  mutate(state = "Western Australia")
#------------------------------------------------------------------------


#--------------------------------Tasmania---------------------------------------

# Creating our dataset for Tasmania
tas_raw <- read_excel(
  path = "National-Health-Survey-2022/Tas_health.xlsx",
  sheet = 2,
  range = "A17:H29",
  col_names = FALSE
)

colnames(tas_raw) <- c(
  "condition", "2001", "2004_05", "2007_08", "2011_12", "2014_15", "2017_18", "2022"
)

colnames(tas_raw) <- c(
  "condition", "2001", "2005", "2008", "2012", "2015", "2018", "2022"
)

tas_State_filtered <- tas_raw %>%
  filter(condition %in% c(
    "Heart, stroke and vascular disease(g)",
    "Hypertension(h)",
    "Diabetes mellitus(f)"
  ))


tas_State_filtered <- tas_State_filtered %>%
  mutate(across(-condition, as.character))

tas_State_long <- tas_State_filtered %>%
  pivot_longer(
    cols = -condition,
    names_to = "year",
    values_to = "estimate_000"
  ) %>%
  mutate(
    condition = case_when(
      str_detect(condition, "Heart, stroke") ~ "CVD",
      str_detect(condition, "Hypertension") ~ "High blood pressure",
      str_detect(condition, "Diabetes") ~ "Diabetes"
    ),
    year = as.integer(year),
    estimate_000 = as.numeric(estimate_000)
  )
tas_State_long <- tas_State_long %>%
  mutate(state = "Tasmania")
#------------------------------Northern Territory-------------------------------

# Creating the dataset for Northern Territory

nt_raw <- read_excel(
  path = "National-Health-Survey-2022/northernTerritory_health.xlsx",
  sheet = 2,
  range = "A17:H29",
  col_names = FALSE
)

colnames(nt_raw) <- c(
  "condition", "2001", "2004_05", "2007_08", "2011_12", "2014_15", "2017_18", "2022"
)

colnames(nt_raw) <- c(
  "condition", "2001", "2005", "2008", "2012", "2015", "2018", "2022"
)

nt_State_filtered <- nt_raw %>%
  filter(condition %in% c(
    "Heart, stroke and vascular disease(g)",
    "Hypertension(h)",
    "Diabetes mellitus(f)"
  ))


nt_State_filtered <- nt_State_filtered %>%
  mutate(across(-condition, as.character))

nt_State_long <- nt_State_filtered %>%
  pivot_longer(
    cols = -condition,
    names_to = "year",
    values_to = "estimate_000"
  ) %>%
  mutate(
    condition = case_when(
      str_detect(condition, "Heart, stroke") ~ "CVD",
      str_detect(condition, "Hypertension") ~ "High blood pressure",
      str_detect(condition, "Diabetes") ~ "Diabetes"
    ),
    year = as.integer(year),
    estimate_000 = as.numeric(estimate_000)
  )

nt_State_long <- nt_State_long %>%
  mutate(state = "Northern Territory")

#------------------------------------------------------------------------------
# Function to replace all na values for diabetes for the year 2005

# This function takes a state-level dataframe and fills 2005 diabetes values using linear interpolation
fill_diabetes_2005 <- function(df) {
  df <- df %>%
    arrange(condition, year)
  
  # Identify the diabetes data
  diabetes_data <- df %>% filter(condition == "Diabetes")
  
  if (any(is.na(diabetes_data$estimate_000) & diabetes_data$year == 2005)) {
    # Extract 2001 and 2008 values
    est_2001 <- diabetes_data %>% filter(year == 2001) %>% pull(estimate_000)
    est_2008 <- diabetes_data %>% filter(year == 2008) %>% pull(estimate_000)
    
    # If both values are available, interpolate
    if (!is.na(est_2001) && !is.na(est_2008)) {
      interpolated <- round(est_2001 + (est_2008 - est_2001) / 2, 1)
      
      # Replace NA in 2005 with interpolated value
      df <- df %>%
        mutate(
          estimate_000 = if_else(
            condition == "Diabetes" & year == 2005 & is.na(estimate_000),
            interpolated,
            estimate_000
          )
        )
    }
  }
  
  return(df)
}

#--------------------------------------------------------------------------
# Applying the function to each state-level dataframe
vic_State_long <- fill_diabetes_2005(vic_State_long)
qld_State_long <- fill_diabetes_2005(qld_State_long)
sa_State_long <- fill_diabetes_2005(sa_State_long)
nsw_State_long <- fill_diabetes_2005(nsw_State_long)
wa_State_long <- fill_diabetes_2005(wa_State_long)
tas_State_long <- fill_diabetes_2005(tas_State_long)
nt_State_long  <- fill_diabetes_2005(nt_State_long)
#--------------------------------------------------------------------------

# Calculating the prevalence Percentage of each of the conditions
# Victoria

vic_raw <- vic_raw %>%
  mutate(across(-condition, as.character))

# Step 1: Extract population data from NSW sheet
vic_population <- vic_raw %>%
  filter(condition == "Total persons, all ages") %>%
  pivot_longer(-condition, names_to = "year", values_to = "population_000") %>%
  mutate(
    year = as.integer(year),
    population_000 = as.numeric(population_000)
  ) %>%
  select(-condition)

# Step 2: Merge with estimates and calculate prevalence %
vic_State_long <- vic_State_long %>%
  left_join(vic_population, by = "year") %>%
  mutate(
    prevalence_percent = round((estimate_000 / population_000) * 100, 2)
  )

# Queensland

qld_raw <- qld_raw %>%
  mutate(across(-condition, as.character))

# Step 1: Extract population data from NSW sheet
qld_population <- qld_raw %>%
  filter(condition == "Total persons, all ages") %>%
  pivot_longer(-condition, names_to = "year", values_to = "population_000") %>%
  mutate(
    year = as.integer(year),
    population_000 = as.numeric(population_000)
  ) %>%
  select(-condition)

# Step 2: Merge with estimates and calculate prevalence %
qld_State_long <- qld_State_long %>%
  left_join(qld_population, by = "year") %>%
  mutate(
    prevalence_percent = round((estimate_000 / population_000) * 100, 2)
  )

# South Australia

sa_raw <- sa_raw %>%
  mutate(across(-condition, as.character))

# Step 1: Extract population data from NSW sheet
sa_population <- sa_raw %>%
  filter(condition == "Total persons, all ages") %>%
  pivot_longer(-condition, names_to = "year", values_to = "population_000") %>%
  mutate(
    year = as.integer(year),
    population_000 = as.numeric(population_000)
  ) %>%
  select(-condition)

# Step 2: Merge with estimates and calculate prevalence %
sa_State_long <- sa_State_long %>%
  left_join(sa_population, by = "year") %>%
  mutate(
    prevalence_percent = round((estimate_000 / population_000) * 100, 2)
  )

# New South Wales

# Step 1: Extract population data from NSW sheet

nsw_raw <- nsw_raw %>%
  mutate(across(-condition, as.character))

nsw_population <- nsw_raw %>%
  filter(condition == "Total persons, all ages") %>%
  pivot_longer(-condition, names_to = "year", values_to = "population_000") %>%
  mutate(
    year = as.integer(year),
    population_000 = as.numeric(population_000)
  ) %>%
  select(-condition)

# Step 2: Merge with estimates and calculate prevalence %
nsw_State_long <- nsw_State_long %>%
  left_join(nsw_population, by = "year") %>%
  mutate(
    prevalence_percent = round((estimate_000 / population_000) * 100, 2)
  )
# Tasmania

tas_raw <- tas_raw %>%
  mutate(across(-condition, as.character))

# Step 1: Extract population data from NSW sheet
tas_population <- tas_raw %>%
  filter(condition == "Total persons, all ages") %>%
  pivot_longer(-condition, names_to = "year", values_to = "population_000") %>%
  mutate(
    year = as.integer(year),
    population_000 = as.numeric(population_000)
  ) %>%
  select(-condition)

# Step 2: Merge with estimates and calculate prevalence %
tas_State_long <- tas_State_long %>%
  left_join(tas_population, by = "year") %>%
  mutate(
    prevalence_percent = round((estimate_000 / population_000) * 100, 2)
  )

# Western Australia

wa_raw <- wa_raw %>%
  mutate(across(-condition, as.character))

# Step 1: Extract population data from NSW sheet
wa_population <- wa_raw %>%
  filter(condition == "Total persons, all ages") %>%
  pivot_longer(-condition, names_to = "year", values_to = "population_000") %>%
  mutate(
    year = as.integer(year),
    population_000 = as.numeric(population_000)
  ) %>%
  select(-condition)

# Step 2: Merge with estimates and calculate prevalence %
wa_State_long <- wa_State_long %>%
  left_join(wa_population, by = "year") %>%
  mutate(
    prevalence_percent = round((estimate_000 / population_000) * 100, 2)
  )

# Northern Territory 

nt_raw <- nt_raw %>%
  mutate(across(-condition, as.character))

# Step 1: Extract population data from NSW sheet
nt_population <- nt_raw %>%
  filter(condition == "Total persons, all ages") %>%
  pivot_longer(-condition, names_to = "year", values_to = "population_000") %>%
  mutate(
    year = as.integer(year),
    population_000 = as.numeric(population_000)
  ) %>%
  select(-condition)

# Step 2: Merge with estimates and calculate prevalence %
nt_State_long <- nt_State_long %>%
  left_join(nt_population, by = "year") %>%
  mutate(
    prevalence_percent = round((estimate_000 / population_000) * 100, 2)
  )

# --------------------------------------------------------------------------------
# Joining the data of all the states to create one state-wise data

state_trend_long <- bind_rows(
  vic_State_long,
  qld_State_long,
  sa_State_long,
  nsw_State_long,
  wa_State_long,
  tas_State_long,
  nt_State_long
)

#--------------------------------------------------------------------------


#---------------------------------------------------------------------------
# combining the statewise and national trend into one complete dataset

complete_trend_data <- bind_rows(state_trend_long, national_trend_long)

# write_csv(complete_trend_data, "data/conditions_trend_australia.csv")

#___________________________________________________________________________
# Preparing our risk factor dataset

# Preparing our smoking data

# Defining our headers
colnames <- c(
  "Condition", "15–17", "18–24", "25–34", "35–44", "45–54", "55–64", "65+"
)

# Gathering our smoking cigarette data
cig_data <- read_excel(
  path = "National-Health-Survey-2022/NHSDC14.xlsx",
  sheet = 2,
  range = "A12:H16",
  col_names = FALSE
)
colnames(cig_data) <- colnames

# Gathering our e-cig/vaping data

ecig_data <- read_excel(
  path = "National-Health-Survey-2022/NHSDC14.xlsx",
  sheet = 2,
  range = "A32:H36",
  col_names = FALSE
)
colnames(ecig_data) <- colnames

# Step 1: Extract relevant rows for smoking totals
cig_total <- cig_data %>%
  filter(Condition == "Total current smoker(c)") %>%
  select(-Condition)

ecig_total <- ecig_data %>%
  filter(Condition == "Total used an e-cigarette / vaping device") %>%
  select(-Condition)

# Step 2: Convert to numeric and add values column-wise
total_smokers <- map2_dfr(cig_total, ecig_total, ~ as.numeric(.x) + as.numeric(.y))

# Step 3: Assign column names back
colnames(total_smokers) <- colnames[-1]  # Drop 'Condition'

# Step 4: Pivot to long format
smoking_long <- total_smokers %>%
  pivot_longer(cols = everything(), names_to = "Age_Group", values_to = "Total_Smokers_000")

# Calculating teh risk percent
# Step 1: Extract population data from cigarette dataset only
population_row <- cig_data %>%
  filter(Condition == "Total persons aged 15 years and over") %>%
  select(-Condition) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Age_Group",
    values_to = "Population_000"
  ) %>%
  mutate(Population_000 = as.numeric(Population_000))

# Step 2: Calculate smoking prevalence using this population
smoking_long <- smoking_long %>%
  left_join(population_row, by = "Age_Group") %>%
  mutate(
    risk_percent = round((Total_Smokers_000 / Population_000) * 100, 2),
    Risk_Factor = "Smoking",
    State = "National"
  )
#----------------------- Inactivity Data ---------------------------------
#  Read the inactivity data
inactivity_data <- read_excel(
  path = "National-Health-Survey-2022/NHSDC11.xlsx",
  sheet = 2,
  range = "A10:H12",
  col_names = FALSE
)

# Assign column names
colnames(inactivity_data) <- colnames

# Extract inactive population
inactive_row <- inactivity_data %>%
  filter(str_detect(Condition, "Did not meet guidelines")) %>%
  pivot_longer(-Condition, names_to = "Age_Group", values_to = "Inactive_000") %>%
  mutate(Inactive_000 = as.numeric(Inactive_000))

# Extract population totals
population_row <- inactivity_data %>%
  filter(str_detect(Condition, "Total persons 15 years and over")) %>%
  pivot_longer(-Condition, names_to = "Age_Group", values_to = "Population_000") %>%
  mutate(Population_000 = as.numeric(Population_000)) %>%
  select(-Condition)

# Calculate risk percent and add metadata
inactivity_long <- inactive_row %>%
  left_join(population_row, by = "Age_Group") %>%
  mutate(
    risk_percent = round((Inactive_000 / Population_000) * 100, 2),
    Risk_Factor = "Inactivity",
    State = "National"
  ) %>%
  select(Age_Group, Inactive_000, Population_000, risk_percent, Risk_Factor, State)

#---------------------------- Alcohol Consumption ------------------------------

# Read Alcohol data

alcohol_data <- read_excel(
  path = "National-Health-Survey-2022/NHSDC07.xlsx",
  sheet = 2,
  range = "A11:H20",
  col_names = FALSE
)


colnames(alcohol_data) <- colnames

# Extract relevant rows for risk and population
alcohol_risk_row <- alcohol_data %>%
  filter(Condition == "Total exceeded guideline") %>%
  pivot_longer(-Condition, names_to = "Age_Group", values_to = "Alcohol_000") %>%
  mutate(Alcohol_000 = as.numeric(Alcohol_000))

alcohol_population_row <- alcohol_data %>%
  filter(Condition == "Total persons, 15 years and over(h)") %>%
  pivot_longer(-Condition, names_to = "Age_Group", values_to = "Population_000") %>%
  mutate(Population_000 = as.numeric(Population_000)) %>%
  select(-Condition)

# Calculate risk percent and add metadata
alcohol_long <- alcohol_risk_row %>%
  left_join(alcohol_population_row, by = "Age_Group") %>%
  mutate(
    risk_percent = round((Alcohol_000 / Population_000) * 100, 2),
    Risk_Factor = "Alcohol consumption",
    State = "National"
  ) %>%
  select(Age_Group, Alcohol_000, Population_000, risk_percent, Risk_Factor, State)

# Creating a function to gather the alcohol data of each state

process_state_alcohol_data <- function(file_path, state_name, sheet = 22, range = "A11:H20") {
  # Define column names
  colnames <- c("Condition", "15–17", "18–24", "25–34", "35–44", "45–54", "55–64","65+")
  
  # Step 1: Read and assign column names immediately
  alcohol_data <- read_excel(
    path = file_path,
    sheet = sheet,
    range = range,
    col_names = FALSE
  )
  colnames(alcohol_data) <- colnames
  
  # Ensure uniform data types and trim conditions
  alcohol_data <- alcohol_data %>%
    mutate(across(-Condition, as.character)) %>%
    mutate(Condition = str_trim(Condition))
  
  # Step 2: Extract rows
  alcohol_risk <- alcohol_data %>%
    filter(str_detect(Condition, fixed("Total exceeded guideline"))) %>%
    pivot_longer(-Condition, names_to = "Age_Group", values_to = "Alcohol_000") %>%
    mutate(Alcohol_000 = as.numeric(Alcohol_000))
  
  alcohol_population <- alcohol_data %>%
    filter(Condition == "Total persons, 18 years and over(h)") %>%
    pivot_longer(-Condition, names_to = "Age_Group", values_to = "Population_000") %>%
    mutate(Population_000 = as.numeric(Population_000)) %>%
    select(-Condition)
  
  # Step 3: Join and calculate risk
  alcohol_state_long <- alcohol_risk %>%
    left_join(alcohol_population, by = "Age_Group") %>%
    mutate(
      risk_percent = round((Alcohol_000 / Population_000) * 100, 2),
      Risk_Factor = "Alcohol consumption",
      State = state_name
    ) %>%
    select(Age_Group, Alcohol_000, Population_000, risk_percent, Risk_Factor, State)
  
  return(alcohol_state_long)
}

nsw_alcohol_long <- process_state_alcohol_data(
  file_path = "National-Health-Survey-2022/NSW_health.xlsx",
  state_name = "New South Wales"
)

vic_alcohol_long <- process_state_alcohol_data(
  file_path = "National-Health-Survey-2022/VIC_health.xlsx",
  state_name = "Victoria"
)

sa_alcohol_long <- process_state_alcohol_data(
  file_path = "National-Health-Survey-2022/SAus_health.xlsx",
  state_name = "South Australia"
)

qld_alcohol_long <- process_state_alcohol_data(
  file_path = "National-Health-Survey-2022/Queensland_health.xlsx",
  state_name = "Queensland"
)

wa_alcohol_long <- process_state_alcohol_data(
  file_path = "National-Health-Survey-2022/WA_health.xlsx",
  state_name = "Western Australia"
)

process_alcohol_data_with_asterisk <- function(file_path, state_name, sheet = 21, range = "A11:H20") {
  # Step 1: Define column names manually (missing 65+ in these sheets)
  colnames <- c("Condition", "15–17", "18–24", "25–34", "35–44", "45–54", "55–64","65+")
  
  # Step 2: Read data and assign column names
  alcohol_data <- read_excel(
    path = file_path,
    sheet = sheet,
    range = range,
    col_names = FALSE
  )
  colnames(alcohol_data) <- colnames
  
  # Step 3: Standardise types and clean asterisk-marked values
  alcohol_data <- alcohol_data %>%
    mutate(across(-Condition, as.character)) %>%
    mutate(Condition = str_trim(Condition))
  
  # Step 4: Extract alcohol risk row and clean values
  alcohol_risk <- alcohol_data %>%
    filter(str_detect(Condition, "Total exceeded guideline")) %>%
    pivot_longer(-Condition, names_to = "Age_Group", values_to = "Alcohol_000") %>%
    mutate(
      Alcohol_000 = as.numeric(str_remove(Alcohol_000, "\\*"))
    )
  
  # Step 5: Extract population row and clean values
  alcohol_population <- alcohol_data %>%
    filter(str_detect(Condition, "Total persons, 18 years and over")) %>%
    pivot_longer(-Condition, names_to = "Age_Group", values_to = "Population_000") %>%
    mutate(
      Population_000 = as.numeric(str_remove(Population_000, "\\*"))
    ) %>%
    select(-Condition)
  
  # Step 6: Join and calculate risk percentage
  alcohol_state_long <- alcohol_risk %>%
    left_join(alcohol_population, by = "Age_Group") %>%
    mutate(
      risk_percent = round((Alcohol_000 / Population_000) * 100, 2),
      Risk_Factor = "Alcohol consumption",
      State = state_name
    ) %>%
    select(Age_Group, Alcohol_000, Population_000, risk_percent, Risk_Factor, State)
  
  return(alcohol_state_long)
}

tas_alcohol_long <- process_alcohol_data_with_asterisk(
  file_path = "National-Health-Survey-2022/Tas_health.xlsx",
  state_name = "Tasmania",
  sheet = 22
)

nt_alcohol_long <- process_alcohol_data_with_asterisk(
  file_path = "National-Health-Survey-2022/northernTerritory_health.xlsx",
  state_name = "Northern Territory",
  sheet = 22
)

#---------------------- State-level Inactivity data -------------------------------

# Creating a function to clean and gather all the smoking data

process_state_inactivity_data <- function(file_path, state_name, sheet = 38, range = "A10:H12") {
  # Define column names
  colnames <- c("Condition", "15–17", "18–24", "25–34", "35–44", "45–54", "55–64", "65+")
  
  # Step 1: Read and assign column names
  inactivity_data <- read_excel(
    path = file_path,
    sheet = sheet,
    range = range,
    col_names = FALSE
  )
  colnames(inactivity_data) <- colnames
  
  # Step 2: Convert all values to character for consistent processing
  inactivity_data <- inactivity_data %>%
    mutate(across(-Condition, as.character)) %>%
    mutate(Condition = str_trim(Condition))
  
  # Step 3: Extract risk data and clean numeric strings
  inactivity_risk <- inactivity_data %>%
    filter(str_detect(Condition, fixed("Did not meet guidelines"))) %>%
    pivot_longer(-Condition, names_to = "Age_Group", values_to = "Inactivity_000") %>%
    mutate(Inactivity_000 = as.numeric(str_remove_all(Inactivity_000, "\\*|\\s")))
  
  # Step 4: Extract population and clean numeric strings
  inactivity_population <- inactivity_data %>%
    filter(str_detect(Condition, fixed("Total persons 15 years and over"))) %>%
    pivot_longer(-Condition, names_to = "Age_Group", values_to = "Population_000") %>%
    mutate(Population_000 = as.numeric(str_remove_all(Population_000, "\\*|\\s"))) %>%
    select(-Condition)
  
  # Step 5: Join and calculate risk percent
  inactivity_state_long <- inactivity_risk %>%
    left_join(inactivity_population, by = "Age_Group") %>%
    mutate(
      risk_percent = round((Inactivity_000 / Population_000) * 100, 2),
      Risk_Factor = "Inactivity",
      State = state_name
    ) %>%
    select(Age_Group, Inactivity_000, Population_000, risk_percent, Risk_Factor, State)
  
  return(inactivity_state_long)
}

qld_inactivity_long <- process_state_inactivity_data(
  file_path = "National-Health-Survey-2022/Queensland_health.xlsx",
  state_name = "Queensland",
  sheet = 38
)

vic_inactivity_long <- process_state_inactivity_data(
  file_path = "National-Health-Survey-2022/VIC_health.xlsx",
  state_name = "Victoria",
  sheet = 38
)

nsw_inactivity_long <- process_state_inactivity_data(
  file_path = "National-Health-Survey-2022/NSW_health.xlsx",
  state_name = "New South Wales",
  sheet = 38
)

wa_inactivity_long <- process_state_inactivity_data(
  file_path = "National-Health-Survey-2022/WA_health.xlsx",
  state_name = "Western Australia",
  sheet = 38
)

tas_inactivity_long <- process_state_inactivity_data(
  file_path = "National-Health-Survey-2022/Tas_health.xlsx",
  state_name = "Tasmania",
  sheet = 38
)

nt_inactivity_long <- process_state_inactivity_data(
  file_path = "National-Health-Survey-2022/northernTerritory_health.xlsx",
  state_name = "Northern Territory",
  sheet = 38
)

sa_inactivity_long <- process_state_inactivity_data(
  file_path = "National-Health-Survey-2022/SAus_health.xlsx",
  state_name = "South Australia",
  sheet = 38
)
#----------------------- State Level Smoking Data ------------------------------

# Lets create a function to gather the smoking data

process_state_smoking_data <- function(file_path, state_name, sheet = 58, range = "A12:H23") {
  # Step 1: Define consistent column names
  colnames <- c("Condition", "15–17", "18–24", "25–34", "35–44", "45–54", "55–64", "65+")
  
  # Step 2: Read and label data
  smoking_data <- read_excel(
    path = file_path,
    sheet = sheet,
    range = range,
    col_names = FALSE
  )
  colnames(smoking_data) <- colnames
  
  # Step 3: Clean values and trim condition labels
  smoking_data <- smoking_data %>%
    mutate(across(-Condition, as.character)) %>%
    mutate(Condition = str_trim(Condition))
  
  # Step 4: Extract and clean relevant rows
  cig_smokers <- smoking_data %>%
    filter(str_detect(Condition, "Total current smoker")) %>%
    pivot_longer(-Condition, names_to = "Age_Group", values_to = "Cig_Smokers_000") %>%
    mutate(Cig_Smokers_000 = as.numeric(str_remove_all(Cig_Smokers_000, "\\*|\\s")))
  
  ecig_smokers <- smoking_data %>%
    filter(str_detect(Condition, "Total used an e–cigarette")) %>%
    pivot_longer(-Condition, names_to = "Age_Group", values_to = "Ecig_Smokers_000") %>%
    mutate(Ecig_Smokers_000 = as.numeric(str_remove_all(Ecig_Smokers_000, "\\*|\\s")))
  
  population <- smoking_data %>%
    filter(str_detect(Condition, "Total persons, 15 years and over")) %>%
    pivot_longer(-Condition, names_to = "Age_Group", values_to = "Population_000") %>%
    mutate(Population_000 = as.numeric(str_remove_all(Population_000, "\\*|\\s"))) %>%
    select(-Condition)
  
  # Step 5: Combine and calculate total smokers and prevalence
  smoking_state_long <- cig_smokers %>%
    left_join(ecig_smokers, by = "Age_Group") %>%
    left_join(population, by = "Age_Group") %>%
    mutate(
      Total_Smokers_000 = Cig_Smokers_000 + Ecig_Smokers_000,
      risk_percent = round((Total_Smokers_000 / Population_000) * 100, 2),
      Risk_Factor = "Smoking",
      State = state_name
    ) %>%
    select(Age_Group, Cig_Smokers_000, Ecig_Smokers_000, Total_Smokers_000,
           Population_000, risk_percent, Risk_Factor, State)
  
  return(smoking_state_long)
}

qld_smoking_long <- process_state_smoking_data(
  file_path = "National-Health-Survey-2022/Queensland_health.xlsx",
  state_name = "Queensland",
  sheet = 58
)

vic_smoking_long <- process_state_smoking_data(
  file_path = "National-Health-Survey-2022/VIC_health.xlsx",
  state_name = "Victoria",
  sheet = 58
)

nsw_smoking_long <- process_state_smoking_data(
  file_path = "National-Health-Survey-2022/NSW_health.xlsx",
  state_name = "New South Wales",
  sheet = 58
)

wa_smoking_long <- process_state_smoking_data(
  file_path = "National-Health-Survey-2022/WA_health.xlsx",
  state_name = "Western Australia",
  sheet = 58
)

tas_smoking_long <- process_state_smoking_data(
  file_path = "National-Health-Survey-2022/Tas_health.xlsx",
  state_name = "Tasmania",
  sheet = 58
)

nt_smoking_long <- process_state_smoking_data(
  file_path = "National-Health-Survey-2022/northernTerritory_health.xlsx",
  state_name = "Northern Territory",
  sheet = 58
)

sa_smoking_long <- process_state_smoking_data(
  file_path = "National-Health-Survey-2022/SAus_health.xlsx",
  state_name = "South Australia",
  sheet = 58
)

# --------------- Consolidated smoking Data of the states ---------------------
state_smoking_dummy <- bind_rows(
  nsw_smoking_long,
  qld_smoking_long,
  vic_smoking_long,
  wa_smoking_long,
  tas_smoking_long,
  nt_smoking_long,
  sa_smoking_long
)

statelevel_smoking_data <- state_smoking_dummy %>%
  select(Age_Group, risk_percent, Risk_Factor, State) %>%
  rename(
    Age = Age_Group,
    Risk_Percent = risk_percent
  )

# ----------------- Consolidated alcohol data of the states --------------------

statelevel_alcohol_data <- bind_rows(
  nsw_alcohol_long,
  vic_alcohol_long,
  wa_alcohol_long,
  qld_alcohol_long,
  sa_alcohol_long,
  tas_alcohol_long,
  nt_alcohol_long
)

statelevel_alcohol_data <- statelevel_alcohol_data %>%
  select(Age_Group, risk_percent, Risk_Factor, State) %>%
  rename(
    Age = Age_Group,
    Risk_Percent = risk_percent
  )

#-------------------- Consolidated Inactivity Data of the states ----------------

statelevel_inactivity_data <- bind_rows(
  nsw_inactivity_long,
  vic_inactivity_long,
  sa_inactivity_long,
  wa_inactivity_long,
  nt_inactivity_long,
  qld_inactivity_long,
  tas_inactivity_long
)

statelevel_inactivity_data <- statelevel_inactivity_data %>%
  select(Age_Group, risk_percent, Risk_Factor, State) %>%
  rename(
    Age = Age_Group,
    Risk_Percent = risk_percent
  )


#---------------- Consolidated Risk Factor Data - State ------------------------

statelevel_riskfactor_data <- bind_rows(
  statelevel_smoking_data,
  statelevel_alcohol_data,
  statelevel_inactivity_data
)

# ----------------- Consolidated Risk Factor Data - National -------------------

National_inactivity_data <- inactivity_long %>%
  select(Age_Group, risk_percent, Risk_Factor, State) %>%
  rename(
    Age = Age_Group,
    Risk_Percent = risk_percent
  )

National_smoking_data <- smoking_long %>%
  select(Age_Group, risk_percent, Risk_Factor, State) %>%
  rename(
    Age = Age_Group,
    Risk_Percent = risk_percent
  )
  
National_alcohol_data <- alcohol_long %>%
  select(Age_Group, risk_percent, Risk_Factor, State) %>%
  rename(
    Age = Age_Group,
    Risk_Percent = risk_percent
  )

National_riskfactor_data <- bind_rows(
  National_smoking_data,
  National_alcohol_data,
  National_inactivity_data
)

#---------------- lets prepare our CVD Data for the Nation -------------------

# Creating a function to calculate the CVD prevalence and the prepare our national cvd data
process_state_cvd_data <- function(file_path, state_name, sheet = 2, range = "A92:H151") {
  # Define column names
  colnames <- c("Condition", "0–14", "15–24", "25–34", "35–44", "45–54", "55–64", "65+")
  
  # Step 1: Read data
  cvd_data <- readxl::read_excel(
    path = file_path,
    sheet = sheet,
    range = range,
    col_names = FALSE
  )
  colnames(cvd_data) <- colnames
  
  # Step 2: Standardise text and remove asterisks
  cvd_data <- cvd_data %>%
    dplyr::mutate(across(-Condition, as.character)) %>%
    dplyr::mutate(across(-Condition, ~ stringr::str_remove_all(., "\\*"))) %>%
    dplyr::mutate(across(-Condition, as.numeric)) %>%
    dplyr::mutate(Condition = stringr::str_trim(Condition))
  
  # Step 3: Extract relevant rows
  cases <- cvd_data %>%
    dplyr::filter(Condition == "Total diseases of the circulatory system") %>%
    tidyr::pivot_longer(-Condition, names_to = "Age_Group", values_to = "CVD_000") %>%
    dplyr::select(-Condition)
  
  population <- cvd_data %>%
    dplyr::filter(Condition == "Total persons, all ages(x)") %>%
    tidyr::pivot_longer(-Condition, names_to = "Age_Group", values_to = "Population_000") %>%
    dplyr::select(-Condition)
  
  # Step 4: Calculate prevalence
  cvd_state_long <- cases %>%
    dplyr::left_join(population, by = "Age_Group") %>%
    dplyr::mutate(
      risk_percent = round((CVD_000 / Population_000) * 100, 2),
      Risk_Factor = "CVD",
      State = state_name
    ) %>%
    dplyr::select(Age_Group, CVD_000, Population_000, risk_percent, Risk_Factor, State)
  
  return(cvd_state_long)
}

National_cvd_age <- process_state_cvd_data(
  file_path = "National-Health-Survey-2022/NHSDC03.xlsx",
  state_name = "National"
)

glimpse(National_cvd_age)
glimpse(National_riskfactor_data)

# Preparing our national CVD and risk factor dataset

National_cvd_age <- National_cvd_age %>%
  filter(Age_Group != "0–14")

National_riskfactor_data <- National_riskfactor_data %>%
  filter(Age != "15–17")

# Renaming the age category in cvd_age
National_cvd_age <- National_cvd_age %>%
  mutate(Age_Group = ifelse(Age_Group == "15–24", "18–24", Age_Group))

# Removing redundant columns
National_cvd_age <- National_cvd_age %>%
  select(-Risk_Factor, -State)

# Renaming columns for consistency
National_cvd_age <- National_cvd_age %>%
  rename(
    Age = Age_Group,
    CVD_Prevalence = risk_percent
  )

# Joining to create one master risk factor table
National_riskfactor_data <- National_riskfactor_data %>%
  left_join(National_cvd_age %>% select(Age, CVD_Prevalence), by = "Age")

# Finding the threshold values to perform the classification

summary(National_riskfactor_data$CVD_Prevalence)

National_riskfactor_data %>%
  group_by(Risk_Factor) %>%
  summarise(
    Min = min(Risk_Percent, na.rm = TRUE),
    Q1 = quantile(Risk_Percent, 0.25, na.rm = TRUE),
    Median = median(Risk_Percent, na.rm = TRUE),
    Q3 = quantile(Risk_Percent, 0.75, na.rm = TRUE),
    Max = max(Risk_Percent, na.rm = TRUE),
    .groups = "drop"
  )

# Performing the classifications based on the above threshold values inferred

classified_national_riskfactor_data <- National_riskfactor_data %>%
  mutate(
    # Risk Level based on Risk Factor–specific breakpoints
    Risk_Level = case_when(
      Risk_Factor == "Smoking" & Risk_Percent < 22.6 ~ "Low",
      Risk_Factor == "Smoking" & Risk_Percent <= 35.7 ~ "Medium",
      Risk_Factor == "Smoking" & Risk_Percent > 35.7 ~ "High",
      
      Risk_Factor == "Alcohol consumption" & Risk_Percent < 25.2 ~ "Low",
      Risk_Factor == "Alcohol consumption" & Risk_Percent <= 29.0 ~ "Medium",
      Risk_Factor == "Alcohol consumption" & Risk_Percent > 29.0 ~ "High",
      
      Risk_Factor == "Inactivity" & Risk_Percent < 69.8 ~ "Low",
      Risk_Factor == "Inactivity" & Risk_Percent <= 81.2 ~ "Medium",
      Risk_Factor == "Inactivity" & Risk_Percent > 81.2 ~ "High",
      
      TRUE ~ NA_character_
    ),
    
    # CVD Level classification
    CVD_Level = case_when(
      CVD_Prevalence < 5.56 ~ "Low",
      CVD_Prevalence <= 33.28 ~ "Medium",
      CVD_Prevalence > 33.28 ~ "High",
      TRUE ~ NA_character_
    ),
    
    # Optional: Composite Risk (combined assessment)
    Composite_Risk = case_when(
      Risk_Level == "High" & CVD_Level == "High" ~ "Critical",
      Risk_Level == "High" & CVD_Level == "Medium" ~ "High",
      Risk_Level == "Medium" & CVD_Level == "High" ~ "High",
      Risk_Level == "Medium" & CVD_Level == "Medium" ~ "Medium",
      Risk_Level == "Low" & CVD_Level == "Low" ~ "Low",
      TRUE ~ "Moderate"
    )
  )

# -------------- Building Visual 4 ------------------------------------------

# Creating a function to read our data and prepare the dataset

process_cvd_bp_persons <- function(file_path) {
  # Define column names
  colnames <- c("Condition", "0–14", "15–24", "25–34", "35–44", "45–54", "55–64", "65+")
  
  # Read and clean the data
  health_data <- read_excel(
    path = file_path,
    sheet = 2,
    range = "A80:H151",
    col_names = FALSE
  )
  colnames(health_data) <- colnames
  
  health_data <- health_data %>%
    mutate(across(-Condition, ~ str_trim(str_remove_all(as.character(.x), "\\*")))) %>%
    mutate(across(-Condition, ~ suppressWarnings(as.numeric(str_replace_all(.x, ",", ""))))) %>%
    mutate(Condition = str_trim(Condition))
  
  # Extract CVD and High BP data
  extract_condition <- function(condition_name, label) {
    health_data %>%
      filter(str_detect(Condition, fixed(condition_name))) %>%
      pivot_longer(-Condition, names_to = "Age", values_to = "Estimate_000") %>%
      mutate(
        Condition = label,
        Sex = "Persons"
      )
  }
  
  cvd_data <- extract_condition("Total heart, stroke and vascular diseases", "CVD")
  bp_data <- extract_condition("Hypertension(n)", "High BP")
  
  # Extract population
  population <- health_data %>%
    filter(str_detect(Condition, fixed("Total persons, all ages(x)"))) %>%
    pivot_longer(-Condition, names_to = "Age", values_to = "Population_000") %>%
    mutate(
      Population_000 = as.numeric(Population_000)
    ) %>%
    select(-Condition)
  
  # Join and calculate prevalence
  bind_rows(cvd_data, bp_data) %>%
    left_join(population, by = "Age") %>%
    mutate(
      Prevalence = round((Estimate_000 / Population_000) * 100, 2)
    ) %>%
    select(Age, Condition, Estimate_000, Population_000, Prevalence, Sex)
}

process_cvd_bp_male <- function(file_path, sheet = 6) {
  colnames <- c("Condition", "0–14", "15–24", "25–34", "35–44", "45–54", "55–64", "65+")
  
  health_data <- read_excel(file_path, sheet = sheet, range = "A80:H151", col_names = FALSE)
  colnames(health_data) <- colnames
  
  health_data <- health_data %>%
    mutate(across(-Condition, ~ str_trim(str_remove_all(as.character(.x), "\\*")))) %>%
    mutate(across(-Condition, ~ as.numeric(str_replace_all(.x, ",", "")))) %>%
    mutate(Condition = str_trim(Condition))
  
  extract_condition <- function(condition_name, label) {
    health_data %>%
      filter(str_detect(Condition, fixed(condition_name))) %>%
      pivot_longer(-Condition, names_to = "Age", values_to = "Estimate_000") %>%
      mutate(Condition = label, Sex = "Male")
  }
  
  cvd_data <- extract_condition("Total heart, stroke and vascular diseases", "CVD")
  bp_data <- extract_condition("Hypertension(o)", "High BP")
  
  population <- health_data %>%
    filter(str_detect(Condition, "Total males, all ages\\(y\\)")) %>%
    pivot_longer(-Condition, names_to = "Age", values_to = "Population_000") %>%
    mutate(Population_000 = as.numeric(str_replace_all(Population_000, ",", ""))) %>%
    select(-Condition)
  
  bind_rows(cvd_data, bp_data) %>%
    left_join(population, by = "Age") %>%
    mutate(Prevalence = round((Estimate_000 / Population_000) * 100, 2)) %>%
    select(Age, Condition, Estimate_000, Population_000, Prevalence, Sex)
}

process_cvd_bp_female <- function(file_path, sheet = 10) {
  colnames <- c("Condition", "0–14", "15–24", "25–34", "35–44", "45–54", "55–64", "65+")
  
  health_data <- read_excel(file_path, sheet = sheet, range = "A80:H151", col_names = FALSE)
  colnames(health_data) <- colnames
  
  health_data <- health_data %>%
    mutate(across(-Condition, ~ str_trim(str_remove_all(as.character(.x), "\\*")))) %>%
    mutate(across(-Condition, ~ as.numeric(str_replace_all(.x, ",", "")))) %>%
    mutate(Condition = str_trim(Condition))
  
  extract_condition <- function(condition_name, label) {
    health_data %>%
      filter(str_detect(Condition, fixed(condition_name))) %>%
      pivot_longer(-Condition, names_to = "Age", values_to = "Estimate_000") %>%
      mutate(Condition = label, Sex = "Female")
  }
  
  cvd_data <- extract_condition("Total heart, stroke and vascular diseases", "CVD")
  bp_data <- extract_condition("Hypertension(o)", "High BP")
  
  population <- health_data %>%
    filter(str_detect(Condition, "Total females, all ages\\(y\\)")) %>%
    pivot_longer(-Condition, names_to = "Age", values_to = "Population_000") %>%
    mutate(Population_000 = as.numeric(str_replace_all(Population_000, ",", ""))) %>%
    select(-Condition)
  
  bind_rows(cvd_data, bp_data) %>%
    left_join(population, by = "Age") %>%
    mutate(Prevalence = round((Estimate_000 / Population_000) * 100, 2)) %>%
    select(Age, Condition, Estimate_000, Population_000, Prevalence, Sex)
}


# For Persons
persons_cvd_bp <- process_cvd_bp_persons("National-Health-Survey-2022/NHSDC03.xlsx")

# For Males
males_cvd_bp <- process_cvd_bp_male(
  file_path = "National-Health-Survey-2022/NHSDC03.xlsx",
  sheet = 6
)

# Process CVD & High BP data for Females
females_cvd_bp <- process_cvd_bp_female(
  file_path = "National-Health-Survey-2022/NHSDC03.xlsx",
  sheet = 10
)

# Combining all the datasets to form one big tables

cvd_bp_age_combined <- bind_rows(persons_cvd_bp, males_cvd_bp, females_cvd_bp)









