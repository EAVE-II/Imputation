######################################################################
## Code author: Steven Kerr steven.kerr@ed.ac.uk
## Description: Load in all data and clean it
##              01_data_cleaning and 02-data_prep create a cohort dataframe
##              that is saved as df_cohort.rds in the data folder.
######################################################################

library(tidyverse)

Location <- "/conf/"

setwd("/conf/EAVE/GPanalysis/analyses/imputation")

# R does't have a built-in mode function
Mode <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA)
  } else {
    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
  }
}

## Demographics

# This is used to correct chi number mismatches
EAVE_LINKNO_refresh <- readRDS(paste0(Location, "EAVE/GPanalysis/data/EAVE_LINKNO_refresh.rds"))

EAVE_cohort <- readRDS(paste0(Location, "EAVE/GPanalysis/outputs/temp/Cohort_Demog_Endpoints_Dates2021-07-28.rds")) %>%
  dplyr::select(
    EAVE_LINKNO,
    sex = Sex,
    age = ageYear,
    urban_rural_6cat = ur6_2016_name,
    simd2020_sc_quintile,
    data_zone = DataZone
  ) %>%
  filter(!duplicated(EAVE_LINKNO)) %>%
  mutate(
    sex = recode_factor(sex, F = "Female", M = "Male"),
    urban_rural_6cat = case_when(
      urban_rural_6cat == "1 Large Urban Areas" ~ "Large Urban Areas",
      urban_rural_6cat == "2 Other Urban Areas" ~ "Other Urban Areas",
      urban_rural_6cat == "3 Accessible Small Towns" ~ "Accessible Small Towns",
      urban_rural_6cat == "4 Remote Small Towns" ~ "Remote Small Towns",
      urban_rural_6cat == "5 Accessible Rural" ~ "Accessible Rural",
      urban_rural_6cat == "6 Remote Rural" ~ "Remote Rural"
    ),
    urban_rural_6cat = fct_relevel(
      urban_rural_6cat,
      "Large Urban Areas",
      "Other Urban Areas",
      "Accessible Small Towns",
      "Remote Small Towns",
      "Accessible Rural",
      "Remote Rural"
    ),
    urban_rural_2cat = case_when(
      urban_rural_6cat %in% c("Large Urban Areas", "Other Urban Areas") ~ "Urban",
      urban_rural_6cat %in% c("Accessible Small Towns", "Remote Small Towns", "Accessible Rural", "Remote Rural") ~ "Rural"
    ),
    # Confusingly, 1 is most deprived and 5 is least deprived
    simd2020_sc_quintile = fct_recode(
      as.character(simd2020_sc_quintile), 
      `1 - Most deprived` = '1',
      `5 - Least deprived` = '5'
    )
  ) %>%
  # Original extract was in 2020 - add 2 to age
  mutate(age = age + 2)

## Clinical characteristics

# QCovid risk groups
# This contains records for everyone who either does have a QCovid risk group,
# or *could* have one, but their value is missing.
# Note n_risk_gps counts clinical risk groups, but does not count BMI, ethnicity, 
# learning category and housing category
qcovid_feb22 <- readRDS(paste0(Location, "EAVE/GPanalysis/data/cleaned_data/QCOVID_feb22.rds")) %>%
  dplyr::select(-Age, -Sex) %>%
  mutate(Q_BMI = as.numeric(Q_BMI)) %>%
  # Im taking 10 to be the lowest feasible human BMI, and 100 the largest
  # There are lots of odd values for BMI, which probably happens because issues
  # with units that calculation is done in.
  mutate(
    Q_BMI = ifelse(Q_BMI < 10 | Q_BMI > 100, NA_real_, Q_BMI),
    # Ethnicity coding taken from QCovid algorithm
    Q_ETHNICITY = case_when(
      Q_ETHNICITY %in% c(1, 2, 3) ~ "White",
      Q_ETHNICITY %in% c(4, 5, 6, 7) ~ "Mixed",
      Q_ETHNICITY %in% c(8, 9, 10, 11, 15) ~ "Asian",
      Q_ETHNICITY %in% c(12, 13, 14) ~ "Black",
      Q_ETHNICITY == 16 ~ "Other",
      TRUE ~ 'Unknown'
    ),
    Q_HOME_CAT = case_when(
      Q_HOME_CAT == 1 ~ "Care home",
      Q_HOME_CAT == 2 ~ "Homeless"
    ),
    Q_LEARN_CAT = case_when(
      Q_LEARN_CAT == 1 ~ "Learning disability",
      Q_LEARN_CAT == 2 ~ "Down's syndrome"
    ),
    Q_DIAG_CKD_LEVEL = case_when(
      Q_DIAG_CKD_LEVEL == 0 ~ "No CKD",
      Q_DIAG_CKD_LEVEL == 3 ~ "CKD 3",
      Q_DIAG_CKD_LEVEL == 4 ~ "CKD 4",
      Q_DIAG_CKD_LEVEL == 5 ~ "CKD 5"
    )
  ) %>%
  # Drop diabetes 1 and 2 cateogries because they are not useable
  dplyr::select(-Q_DIAG_DIABETES_1, -Q_DIAG_DIABETES_2)

# Get old QCovid risk groups. We will use this for diabetes, because diabetes grouping
# from more recent QCovid extract are not useable, as well as replacing other variable
# values for people that are not in the newer extract
qcovid_old <- readRDS(paste0(Location, "EAVE/GPanalysis/progs/CR/Vaccine/output/temp/Qcovid.rds")) %>%
  dplyr::select(
    -Sex,
    -ageYear,
    -simd2020_sc_quintile,
    -DataZone,
    -ur6_2016_name,
    -age_gp,
    -EAVE_Smoke,
    -EAVE_BP,
    -eave_weight,
    -n_risk_gps,
    -bmi_impute) %>%
  mutate(Q_BMI = as.numeric(Q_BMI)) %>%
  # Im taking 10 to be the lowest feasible human BMI, and 100 the largest
  # There are lots of odd values for BMI, which probably happens because issues
  # with units that calculation is done in.
  mutate(
    Q_BMI = ifelse(Q_BMI < 10 | Q_BMI > 100, NA_real_, Q_BMI),
    Q_HOME_CAT = case_when(
      Q_HOME_CAT == 1 ~ "Care home",
      Q_HOME_CAT == 2 ~ "Homeless"
    ),
    Q_LEARN_CAT = case_when(
      Q_LEARN_CAT == 1 ~ "Learning disability",
      Q_LEARN_CAT == 2 ~ "Down's syndrome"
    ),
    Q_DIAG_CKD_LEVEL = case_when(
      Q_DIAG_CKD_LEVEL == 0 ~ "No CKD",
      Q_DIAG_CKD_LEVEL == 3 ~ "CKD 3",
      Q_DIAG_CKD_LEVEL == 4 ~ "CKD 4",
      Q_DIAG_CKD_LEVEL == 5 ~ "CKD 5"
    )
  )

# Ethnicity
ethnicity <- readRDS(paste0(Location, "EAVE/GPanalysis/data/lookups/EAVE_Ethnicity_2022.rds")) %>%
  mutate(
    ethnicity_18cat = ifelse(ethnic_code_desc == 'Not Known', NA, ethnic_code_desc) %>% 
      factor()
  ) %>%
  dplyr::select(-ethnic_code, -ethnic_code_desc) %>%
  data.frame()
