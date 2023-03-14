######################################################################
## Code author: Steven Kerr steven.kerr@ed.ac.uk
## Description: Impute BMI for EAVE cohort
######################################################################

#Libraries
library(tidyverse)
library(mice)
library(janitor)

setwd("/conf/EAVE/GPanalysis/analyses/imputation")

# Variables to be used in imputation
vars = c('EAVE_LINKNO', 'sex', 'age', 'urban_rural_6cat', 'simd2020_sc_quintile', 'ethnicity_18cat', setdiff(q_names, 'Q_ETHNICITY'))

# List of binary variables
bin_vars <- setdiff(q_names, c("Q_BMI", "Q_HOME_CAT", "Q_LEARN_CAT", "Q_DIAG_CKD_LEVEL", "Q_ETHNICITY"))

# Create list of methods to be used in imputation
create_method <- function(df, exclude_vars){
  method <- as.data.frame( sapply(df, nlevels) )
  
  method[ method[,1] > 2, ] <- 'polyreg'
  method[ exclude_vars, ] <- ''
  method[ method[,1] ==0 , ] <- 'pmm'
  method[ method[,1] ==1 , ] <- 'pmm'
  method[ method[,1] ==2 , ] <- 'logreg'
  
  method <- dplyr::pull(method, colnames(method))
}

# Creates a table that compares frequency of categorial variables
# for original and imputed data
create_freq_table <- function(df, df_imp, vars){
  
  for (var in vars){
  
    col = df[, var]
    imp_col = df_imp[, var]
    
    table = tabyl(col)
    names(table)[1] = var
    table = table[1:nrow(table)-1, ]
    
    missing_indices = which(is.na(col))
    
    table2 = imp_col[missing_indices] %>%
      tabyl() 
    
    names(table2)[1] = var
    
    table2 = table2 %>%
      rename(
        n_imputed = n, 
        percent_imputed = percent
      )
    
    output = table %>%
      select(c(var, 'n', 'percent')) %>%
      full_join(table2) %>%
      mutate(
        n_imputed = replace_na(n_imputed, 0),
        percent_imputed = replace_na(percent_imputed, 0)
      )
    
    write.csv(output, paste0('./output/', var, '_frequency_table.csv'))
  }
}

df = df_cohort %>%
  # For testing
  #sample_n(10000) %>%
  select(all_of(vars)) %>%
  mutate_at(bin_vars, factor)

# Carry out imputation
set.seed(123)

# In the first round of imputation, we will impute ethnicity, urban_rural_6cat and simd2020_sc_quintile
# We will not impute Q_BMI
exclude_vars = c('EAVE_LINKNO', 'age', 'sex', 'Q_BMI')
method <- create_method(df, exclude_vars)

predictorMatrix = quickpred(df, exclude = exclude_vars)
predictorMatrix[, 'EAVE_LINKNO'] = 0

imputation <- mice(
  df, 
  m = 1 ,
  maxit =10, 
  predictorMatrix = predictorMatrix, 
  method = method)

df_imp1 = complete(imputation, 1)

create_freq_table(df, df_imp1, c('ethnicity_18cat', 'urban_rural_6cat', 'simd2020_sc_quintile'))



# BMI is qualitiatively different for people under 20
# https://www.cdc.gov/healthyweight/assessing/bmi/adult_bmi/index.html
df_children = df_imp1 %>%
  filter(age < 20)

df_men = df_imp1 %>%
  filter(sex == 'Male', age >= 20)

df_women = df_imp1 %>%
  filter(sex == 'Female', age >= 20)


exclude_vars = c('EAVE_LINKNO', 'age', 'sex')
method <- create_method(df_imp1, exclude_vars)

predictorMatrix = quickpred(df_imp1, exclude = exclude_vars)
predictorMatrix[, 'EAVE_LINKNO'] = 0

# Impute BMI in children
imputation_children <- mice(
  df_children, 
  m = 1 ,
  maxit =10, 
  predictorMatrix = predictorMatrix, 
  method = method)

df_children_imp = complete(imputation_children, 1)

densityplot(imputation_children)
ggsave('./output/children_bmi_pdf.png')

create_freq_table(df_children, df_children_imp, 'Q_BMI')


# Impute BMI in men
imputation_men <- mice(
  df_men, 
  m = 1 ,
  maxit =10, 
  predictorMatrix = predictorMatrix, 
  method = method)

df_men_imp = complete(imputation_men, 1)

densityplot(imputation_men)
ggsave('./output/men_bmi_pdf.png')


# Impute BMI in women
imputation_women <- mice(
  df_women, 
  m = 1 ,
  maxit =10, 
  predictorMatrix = predictorMatrix, 
  method = method)

df_women_imp = complete(imputation_women, 1)

densityplot(imputation_women)
ggsave('./output/women_bmi_pdf.png')



df_imp2 = df_children_imp %>%
  bind_rows(df_men_imp) %>%
  bind_rows(df_women_imp) %>%
  select(EAVE_LINKNO, sex, age, urban_rural_6cat, simd2020_sc_quintile, ethnicity_18cat, Q_BMI)

sapply(df_imp2, function(x) sum(is.na(x)))

# Save imputed values
saveRDS(df_imp2, paste0('./data/df_imp.rds'))
