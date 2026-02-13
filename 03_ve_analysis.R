# 03_ve_analysis.R
# Project: Nepal Typhoid TND
# Description: This script performs the primary Vaccine Effectiveness (VE)
#              analysis using conditional logistic regression, including
#              stratification by age and time since vaccination.
# Author: [Nepal Typhoid TND Team]

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rio, tidyverse, gtsummary, flextable, janitor, here, survival)

# 1. Import Cleaned Dataset -----------------------------------------------
clean_data_path <- here("data", "clean", "Clean_TND_DATA.rds")

if (!file.exists(clean_data_path)) {
    stop(
        "Cleaned data file not found at: ", clean_data_path,
        "\nPlease run '01_data_cleaning.R' first."
    )
}

dt_ve <- readRDS(clean_data_path)

# 2. Preparation for Analysis ---------------------------------------------
dt_ve <- dt_ve %>%
    mutate(
        casecontrol = recode(casecontrol, `1` = 1, `2` = 0),
        vacc_yn_na = recode_factor(vacc_yn, `0` = "No", `1` = "Yes", `77` = NA_character_),
        vacc_yn_na = fct_relevel(vacc_yn_na, "No"),
        # Age category for interaction analysis
        age_cat_under5 = factor(ifelse(age_at_blood_yr < 5, "under 5 yrs", ">5 yrs"),
            levels = c("under 5 yrs", ">5 yrs")
        )
    )

# 3. Overall Vaccine Effectiveness ---------------------------------------
# Adjusted for age and gender
model_overall <- clogit(casecontrol ~ vacc_yn_na + age_at_blood_yr + gender + strata(strata),
    data = dt_ve
)
summary(model_overall)

# 4. Stratified Analysis by Age -------------------------------------------

# Under 5 years
dt_u5 <- dt_ve %>% filter(age_cat_under5 == "under 5 yrs")
model_u5 <- clogit(casecontrol ~ vacc_yn_na + age_at_blood_yr + gender + strata(strata),
    data = dt_u5
)
summary(model_u5)

# Over 5 years
dt_ov5 <- dt_ve %>% filter(age_cat_under5 == ">5 yrs")
model_ov5 <- clogit(casecontrol ~ vacc_yn_na + age_at_blood_yr + gender + strata(strata),
    data = dt_ov5
)
summary(model_ov5)

# 5. Analysis by Time Since Vaccination (Stratified) ---------------------
# Time since vaccination/eligibility split at 2 years
dt_ve <- dt_ve %>%
    mutate(
        vacc_time_cat = case_when(
            vacc_to_blood_eli_yr < 2 ~ "<2 years",
            vacc_to_blood_eli_yr >= 2 ~ ">=2 years",
            TRUE ~ NA_character_
        ),
        vacc_time_cat = factor(vacc_time_cat, levels = c("<2 years", ">=2 years"))
    )

# < 2 Years post-eligibility
dt_u2_eli <- dt_ve %>% filter(vacc_time_cat == "<2 years")
model_u2_eli <- clogit(casecontrol ~ vacc_yn_na + age_at_blood_yr + gender + strata(strata),
    data = dt_u2_eli
)
summary(model_u2_eli)

# >= 2 Years post-eligibility
dt_ov2_eli <- dt_ve %>% filter(vacc_time_cat == ">=2 years")
model_ov2_eli <- clogit(casecontrol ~ vacc_yn_na + age_at_blood_yr + gender + strata(strata),
    data = dt_ov2_eli
)
summary(model_ov2_eli)

# 6. Interaction Analysis (Optional/Exploratory) --------------------------
# Interaction between vaccine and age
model_age_int <- clogit(casecontrol ~ vacc_yn_na * age_cat_under5 + age_at_blood_yr + gender + strata(strata),
    data = dt_ve
)
summary(model_age_int)

# Interaction between vaccine and time since vaccination
model_time_int <- clogit(casecontrol ~ vacc_yn_na * vacc_time_cat + age_at_blood_yr + gender + strata(strata),
    data = dt_ve
)
summary(model_time_int)

message("VE analysis complete.")
