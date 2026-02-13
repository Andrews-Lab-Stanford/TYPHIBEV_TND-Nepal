# 01_data_cleaning.R
# Project: Nepal Typhoid TND
# Description: This script cleans the raw TND data, performs record corrections,
#              calculates age at blood collection, and defines vaccine eligibility
#              time for participants.
# Author: [Nepal Typhoid TND Team]

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rio, tidyverse, gtsummary, flextable, janitor, here, naniar)

# Note on Data: Raw data is expected to be in the 'data/raw' directory.
# Disidentified data should be downloaded from the Dryad repository.

# 1. Import Data ----------------------------------------------------------
# Replace the file name with the actual disidentified data file name
raw_data_path <- here("data", "raw", "TND_DATA_disidentified.csv")

if (!file.exists(raw_data_path)) {
    stop(
        "Raw data file not found at: ", raw_data_path,
        "\nPlease ensure the data is in the 'data/raw/' directory."
    )
}

dt_raw <- read.csv(raw_data_path)

# 2. Initial Cleaning and Strata Assignment -------------------------------
dt <- dt_raw %>%
    clean_names() %>%
    rename(casecontrol = assignment) %>%
    mutate(strata = case_when(
        casecontrol == 1 ~ record_id,
        casecontrol == 2 ~ control_id_match
    ))

# 3. Correcting Vaccine Source Information -------------------------------
dt <- dt %>%
    mutate(
        vacc_card_yn = case_when(
            record_id == 119 ~ 0,
            record_id == 10 ~ 1,
            TRUE ~ vacc_card_yn
        ),
        vacc_yes_confirm_1 = case_when(
            record_id == 119 ~ 1,
            TRUE ~ vacc_yes_confirm_1
        ),
        vacc_type = case_when(vacc_type == 2 ~ 1, TRUE ~ vacc_type),
        vacc_source = case_when(
            vacc_card_yn == 1 ~ 1,
            vacc_yes_confirm_2 == 1 ~ 2,
            vacc_yes_confirm_99 == 1 ~ 2,
            vacc_yes_confirm_1 == 1 ~ 3,
            vacc_yn == 0 ~ 0,
            vacc_yn == 77 ~ 77
        )
    )

# 4. Calculating Age at Blood Collection ----------------------------------
dt <- dt %>%
    mutate(
        spec_coll_date = as.Date(spec_coll_date, format = "%m/%d/%Y"),
        dob = as.Date(dob, format = "%m/%d/%Y"),
        age_at_blood = as.numeric(difftime(spec_coll_date, dob, units = "days")),
        age_at_blood_yr = age_at_blood / 365
    )

# 5. Imputing/Correcting Vaccination Dates --------------------------------
dt <- dt %>%
    mutate(
        vacc_date_comp = ifelse(vacc_date %in% c("", " ", "NA", "N/A"), NA, vacc_date),
        vacc_date_comp = as.Date(vacc_date_comp, format = "%m/%d/%Y")
    )

dt <- dt %>%
    mutate(
        vacc_date_comp = case_when(
            record_id == 102 ~ as.Date("2024-02-09"),
            record_id == 103 ~ as.Date("2022-04-29"),
            record_id == 109 ~ as.Date("2022-04-14"),
            record_id == 110 ~ as.Date("2022-04-30"),
            record_id == 111 ~ as.Date("2022-04-19"),
            record_id == 112 ~ as.Date("2022-04-11"),
            record_id == 119 ~ as.Date("2022-04-25"),
            record_id == 125 ~ as.Date("2022-04-09"),
            record_id == 130 ~ as.Date("2022-04-12"),
            record_id == 131 ~ as.Date("2022-04-26"),
            record_id == 132 ~ as.Date("2022-04-21"),
            record_id == 135 ~ as.Date("2022-04-10"),
            record_id == 136 ~ as.Date("2022-04-07"),
            record_id == 138 ~ as.Date("2022-04-16"),
            record_id == 140 ~ as.Date("2022-04-10"),
            record_id == 142 ~ as.Date("2022-04-08"),
            record_id == 144 ~ as.Date("2022-04-17"),
            record_id == 145 ~ as.Date("2023-08-04"),
            record_id == 150 ~ as.Date("2022-04-20"),
            record_id == 151 ~ as.Date("2022-04-22"),
            record_id == 153 ~ as.Date("2022-04-10"),
            record_id == 154 ~ as.Date("2022-04-12"),
            record_id == 91 ~ as.Date("2022-04-27"),
            record_id == 92 ~ as.Date("2022-04-11"),
            record_id == 97 ~ as.Date("2022-04-18"),
            record_id == 98 ~ as.Date("2022-04-13"),
            record_id == 100 ~ as.Date("2023-10-16"),
            record_id == 101 ~ as.Date("2022-04-28"),
            record_id == 104 ~ as.Date("2022-04-13"),
            record_id == 108 ~ as.Date("2022-04-18"),
            record_id == 129 ~ as.Date("2023-05-30"),
            record_id == 47 ~ as.Date("2022-04-15"),
            TRUE ~ vacc_date_comp
        )
    )

# 6. Defining Vaccine Eligibility Time ------------------------------------
# Mass vaccination campaign start date
vaccination_date <- as.Date("2022-04-07")

# Calculating age in days at the time of mass vaccination
dt <- dt %>%
    mutate(age_at_massv = as.numeric(vaccination_date - dob))

# Assigning vaccination eligibility date for unvaccinated participants
dt <- dt %>%
    mutate(
        vacc_date_comp_eli = case_when(
            vacc_yn == 0 & age_at_massv > 445 ~ as.Date("2022-04-15"),
            vacc_yn == 0 & age_at_massv <= 445 ~ dob + 445,
            TRUE ~ vacc_date_comp
        )
    )

# Time since eligibility/vaccination for all
dt <- dt %>%
    mutate(
        vacc_to_blood_eli = as.numeric(difftime(spec_coll_date, vacc_date_comp_eli, units = "days")),
        vacc_to_blood_eli_yr = vacc_to_blood_eli / 365
    )

# Time since vaccination for binary comparison
dt <- dt %>%
    mutate(
        vacc_to_blood = as.numeric(difftime(spec_coll_date, vacc_date_comp, units = "days")),
        vacc_to_blood_yr = vacc_to_blood / 365
    )

# 7. Final Filtering and Export -------------------------------------------
dt_clean <- dt %>%
    mutate(remove = tidyr::replace_na(remove, 0)) %>%
    filter(remove == 0)

# Save cleaned dataset for subsequent analysis scripts
# Ensure the 'data/clean' directory exists
dir.create(here("data", "clean"), recursive = TRUE, showWarnings = FALSE)
saveRDS(dt_clean, here("data", "clean", "Clean_TND_DATA.rds"))

message("Data cleaning complete. Cleaned file saved to data/clean/Clean_TND_DATA.rds")
