######################################################################
## Code author: Steven Kerr steven.kerr@ed.ac.uk
## Description: Create study dataframe, adding derived variables.
##              Saves df_cohort in ./data folder
##              01_data_cleaning is sourced.
##              01_data_cleaning and 02-data_prep create a cohort dataframe
##              that is saved as df_cohort.rds in the data folder.
##              Therefore they only need to be run once. You can skip to 
##              03_load_data.
######################################################################

library(tidyverse)

setwd("/conf/EAVE/GPanalysis/analyses/imputation")

# source("./code/01_data_cleaning.R")

#### Create cohort dataframe with all required variables by merging together individual datasets

df_cohort <- EAVE_LINKNO_refresh %>%
  #For testing
  #sample_n(10000) %>%
  select(EAVE_LINKNO_old, 
         EAVE_LINKNO,
         EAVE_LINKNO_change,
         chili_checked,
         unvalidatedCHI_flag,
  ) %>%
  filter(unvalidatedCHI_flag!= 1, !duplicated(EAVE_LINKNO))


df_cohort <- df_cohort %>% 
  # Join by EAVE_LINKNO_old, since data pre-dates December 2020
  left_join(EAVE_cohort, by = c("EAVE_LINKNO_old" = "EAVE_LINKNO"))

# Add risk groups
df_cohort <- qcovid_feb22 %>%
  select(-n_risk_gps) %>%
  right_join(df_cohort, by = "EAVE_LINKNO")

# Add diabetes from old qcovid extract, because it is not reliable in newer qcovid extract
# Join by EAVE_LINKNO_old, since data pre-dates December 2021
df_cohort <- left_join(
  df_cohort,
  qcovid_old %>% 
    select(EAVE_LINKNO, Q_DIAG_DIABETES_1, Q_DIAG_DIABETES_2), 
  by = c("EAVE_LINKNO_old" = "EAVE_LINKNO"))

# If they aren't present in the new qcovid extract, take their data from the old qcovid extract  
df_cohort <- left_join(
  df_cohort,
  qcovid_old %>% 
    filter(!EAVE_LINKNO %in% qcovid_feb22$EAVE_LINKNO) %>%
    rename(EAVE_LINKNO_old = EAVE_LINKNO)
)

df_cohort <- df_cohort %>%
  mutate(
    Q_HOME_CAT = ifelse(is.na(Q_HOME_CAT), "Neither", Q_HOME_CAT) %>%
      factor() %>%
      fct_relevel("Neither"),
    Q_LEARN_CAT = ifelse(is.na(Q_LEARN_CAT), "Neither", Q_LEARN_CAT) %>%
      factor() %>%
      fct_relevel("Neither"),
    Q_DIAG_CKD_LEVEL = ifelse(is.na(Q_DIAG_CKD_LEVEL), "No CKD", Q_DIAG_CKD_LEVEL) %>%
      factor() %>%
      fct_relevel("No CKD")
  ) 


# # Join by EAVE_LINKNO_old, since data pre-dates December 2021
# df_cohort <- left_join(df_cohort, qcovid_diabetes, by = c("EAVE_LINKNO_old" = "EAVE_LINKNO"))

# Add ethnicity
df_cohort <- df_cohort %>%
  left_join(ethnicity, by = "EAVE_LINKNO")

q_names <- grep("Q", names(df_cohort), value = TRUE)

# Create a list of cols where NA means 0
cols = setdiff(q_names, c("Q_BMI", "Q_ETHNICITY", "Q_HOME_CAT", "Q_LEARN_CAT", "Q_DIAG_CKD_LEVEL")) 

df_cohort <- mutate_at(
  df_cohort, 
  cols,
  ~ as.numeric(.))

df_cohort <- mutate_at(
  df_cohort, 
  cols, ~ 
    case_when(
      is.na(.) ~ 0,
      TRUE ~ .
    )
  )

# Add number of risk groups
# This has to be recalculated because we fused older and newer qcovid risk groups together
df_cohort <- df_cohort %>%
  mutate(Q_CKD_bin = ifelse(Q_DIAG_CKD_LEVEL == 'No CKD', 0, 1)
  ) 

df_cohort <- df_cohort %>%
  mutate(
    n_risk_gps = 
      select(df_cohort, 
             c('Q_CKD_bin', setdiff(q_names, c("Q_BMI", "Q_BMI_imputed", "Q_ETHNICITY", "Q_HOME_CAT", "Q_LEARN_CAT", "Q_DIAG_CKD_LEVEL")))
      ) %>%
      rowSums()
  ) %>%
  select(-Q_CKD_bin) %>%
  mutate(n_risk_gps_cat =
           cut(n_risk_gps, 
               breaks = c(0, 1, 2, 3, 5, Inf),
               labels = c('0', '1', '2', '3-4', '5+'),
               right = FALSE
           )
  )


df_cohort <- data.frame(df_cohort)

# Check duplicates
length(unique(df_cohort$EAVE_LINKNO))

# Check NA
sapply(df_cohort, function(x) sum(is.na(x)))

# Save out
#saveRDS(df_cohort, './data/df_cohort.rds')
