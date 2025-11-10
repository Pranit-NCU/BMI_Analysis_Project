# ---
# 01-LOAD-AND-CLEAN.R
#
# This is "munge" script.
# Job of this Munge File is:
# 1. Load two RAW csv files.
# 2. Clean and merge them.
# 3. Create the two FINAL, CLEAN Datasets for OUR Analysis.
# 
# You only need to run this script ONCE, or anytime the raw data changes.
# ---

# ---- 1. load Libraries -----
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(kableExtra)

cat("--- Starting Munge Script ---\n")

# ---- 2.Define File Path Load Dataset ----
bmi <- read.csv('D:/Newcastle _Coursework/11300607/NCD_RisC_CRISPDM_Analysis/data/NCD_RisC_Lancet_2024_BMI_age_standardised_country.csv')
diab <- read.csv('D:/Newcastle _Coursework/11300607/NCD_RisC_CRISPDM_Analysis/data/NCD_RisC_Lancet_2024_Diabetes_age_standardised_countries.csv')

# --- BMI Dataset ---

cat("Dimensions:", nrow(bmi), "rows,", ncol(bmi), "columns\n")
cat("Year range:", min(bmi$Year), "to", max(bmi$Year), "\n")
cat("Sex :", unique(bmi$Sex), "\n")
cat("Number of countries/regions:", length(unique(bmi$Country.Region.World)), "\n\n")

# ---- Diabetes Dataset ----
cat("Diabetes Dataset:\n")
cat("Dimensions:", nrow(diab), "rows,", ncol(diab), "columns\n")
cat("Year range:", min(diab$Year), "to", max(diab$Year), "\n")
cat("Sex:", unique(diab$Sex), "\n")
cat("Number of countries/regions:", length(unique(diab$Country.Region.World)), "\n")


# ---- 3.Preparing Dataset ----
# ----- Filtering and Cleaning---

# Filter for India and UK, all years (1990-2022)

bmi_countries <- bmi |>
filter(Country.Region.World %in% c("India", "United Kingdom")) |>
select(Year, Sex, Country = Country.Region.World,
Obesity = Prevalence.of.BMI..30.kg.m...obesity.)

diab_countries <- diab |>
filter(Country.Region.World %in% c("India", "United Kingdom")) |>
select(Year, Sex, Country = Country.Region.World,
Diabetes = Prevalence.of.diabetes..18..years.)

# Filter for Male or Female 30+ years old

Gender_Filter <- 'Men'
bmi_munged <- bmi |> filter(Sex == Gender_Filter) |> select(Year, Sex, `Country.Region.World`, `Prevalence.of.BMI..30.kg.m...obesity.`, `Prevalence.of.BMI..30.kg.m...obesity..lower.95..uncertainty.interval`, `Prevalence.of.BMI..30.kg.m...obesity..upper.95..uncertainty.interval`)

Diabetes_munged <- Diabetes |> filter(Sex == Gender_Filter) |> select(`Country.Region.World`, Sex, Year, `Proportion.of.people.with.diabetes.who.were.treated..30..years.`, `Proportion.of.people.with.diabetes.who.were.treated..30..years..lower.95..uncertainty.interval`, `Proportion.of.people.with.diabetes.who.were.treated..30..years..upper.95..uncertainty.interval`)

# --- Merge BMI and Diabetes for India and UK
merged_countries <- inner_join(bmi_countries, diab_countries,
by = c("Year", "Sex", "Country"))

cat("Merged data for India and UK:\n")
print(head(merged_countries, 10))
cat("\nDimensions:", nrow(merged_countries), "rows\n")

# ---- Global Average Calculation ----

bmi_global <- bmi %>%
group_by(Year, Sex) %>%
summarise(Obesity = mean(Prevalence.of.BMI..30.kg.m...obesity., na.rm=TRUE),
.groups='drop')

diab_global <- diab %>%
group_by(Year, Sex) %>%
summarise(Diabetes = mean(Prevalence.of.diabetes..18..years., na.rm=TRUE),
.groups='drop')

# ---- Merge global obesity and diabetes ----

merged_global <- inner_join(bmi_global, diab_global, by = c("Year", "Sex"))

cat("Global mean obesity and diabetes by year and sex:\n")
print(head(merged_global, 11))

# ---- Merge global obesity and diabetes for sex 30+ years old ----

joined_bmi_diabetes <- full_join(bmi_munged, Diabetes_munged, by = c("Year", "Sex", "Country.Region.World"))
names(joined_bmi_diabetes)

# ---- Summary of global obesity and diabetes for sex 30+ years old ----

joined_bmi_diabetes |>
  summarise(across(where(is.numeric),
                   list(mean = mean, sd = sd, min = min, median = median, max = max),
                   .names = "{.col}_{.fn}")) |>
  kable() |>
  kable_styling()

