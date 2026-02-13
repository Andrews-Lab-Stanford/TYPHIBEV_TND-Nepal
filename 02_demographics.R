# 02_demographics.R
# Project: Nepal Typhoid TND
# Description: This script generates demographic tables (Table 1) and risk factor
#              summary tables (Table 2) using the cleaned TND dataset.
# Author: [Nepal Typhoid TND Team]

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rio, tidyverse, gtsummary, flextable, janitor, here, survival, broom)

# 1. Import Cleaned Dataset -----------------------------------------------
clean_data_path <- here("data", "clean", "Clean_TND_DATA.rds")

if (!file.exists(clean_data_path)) {
    stop(
        "Cleaned data file not found at: ", clean_data_path,
        "\nPlease run '01_data_cleaning.R' first."
    )
}

dt <- readRDS(clean_data_path)

# 2. Table 1: Participant Characteristics ---------------------------------
dt_char <- dt %>%
    select(casecontrol, strata, age_at_blood_yr, gender, ethnicity, house_material, ed_pat, ed_mat) %>%
    mutate(
        gender = recode_factor(gender, `1` = "Male", `2` = "Female"),
        casecontrol_label = recode_factor(casecontrol, `1` = "Case", `2` = "Control"),
        ethnicity = recode_factor(ethnicity,
            `1` = "Brahmin/Chetrri", `2` = "Janjati",
            `3` = "Newars", `4` = "Dalit", `5` = "Madhesi"
        ),
        ed_pat = recode_factor(ed_pat,
            `0` = "No Formal Education", `1` = "Primary",
            `2` = "Secondary", `3` = "Post Secondary or Higher", `99` = "No Formal Education"
        ),
        ed_mat = recode_factor(ed_mat,
            `0` = "No Formal Education", `1` = "Primary",
            `2` = "Secondary", `3` = "Post Secondary or Higher", `99` = "No Formal Education"
        ),
        house_material = recode_factor(house_material,
            `1` = "Cement with brick/stone",
            `2` = "Mud with brick/stone", `3` = "Tin", `4` = "Wood", `5` = "Wood with mud"
        ),
        age_cat = case_when(
            age_at_blood_yr < 5 ~ "15m-<5yrs",
            age_at_blood_yr < 10 ~ "5-<10yrs",
            age_at_blood_yr >= 10 ~ ">=10yrs"
        ),
        age_cat = factor(age_cat, levels = c("15m-<5yrs", "5-<10yrs", ">=10yrs"), ordered = TRUE)
    )

table1 <- dt_char %>%
    select(casecontrol_label, age_cat, age_at_blood_yr, gender, ethnicity, ed_pat, ed_mat, house_material) %>%
    tbl_summary(
        by = casecontrol_label,
        statistic = list(
            all_categorical() ~ "{n} ({p}%)",
            all_continuous() ~ "{median} ({p25}, {p75})"
        ),
        digits = all_continuous() ~ 2,
        label = list(
            age_cat ~ "Age Categories",
            age_at_blood_yr ~ "Age at blood collection (years)",
            gender ~ "Gender",
            ethnicity ~ "Ethnicity",
            ed_pat ~ "Father's Education",
            ed_mat ~ "Mother's Education",
            house_material ~ "Housing Material"
        )
    ) %>%
    bold_labels()

print(table1)

# 3. Table 2: Risk Factors and Vaccination -------------------------------
dt_risk <- dt %>%
    mutate(
        casecontrol_label = recode_factor(casecontrol, `1` = "Case", `2` = "Control"),
        handwash_food = recode_factor(handwash_food, `1` = "All of time", `2` = "Some of time", `3` = "Never or Rarely"),
        handwash_latrine = recode_factor(handwash_latrine, `1` = "All of time", `2` = "Some of time", `3` = "Never or Rarely"),
        house_purify = recode_factor(house_purify, `1` = "All of time", `2` = "Some of time", `3` = "None of time"),
        vacc_source_label = recode_factor(vacc_source,
            `1` = "Vaccine Card", `2` = "School/Health Record",
            `3` = "Parent/Guardian Confirmation", `0` = "No Vaccination",
            `77` = "Don't Know"
        ),
        across(c(contact_yn, abx_hx, travel, vacc_yn), ~ recode_factor(.x, `1` = "Yes", `0` = "No", `77` = "Don't Know")),
        toilet_types = case_when(
            house_toilets_type_1 == 1 ~ "Toilet with flush",
            house_toilets_type_2 == 1 ~ "Toilet with flush",
            house_toilets_type_3 == 1 ~ "Toilet without flush",
            house_toilets_type_4 == 1 ~ "Open defecation"
        )
    )

table2 <- dt_risk %>%
    select(casecontrol_label, house_purify, toilet_types, handwash_food, handwash_latrine, vacc_yn, vacc_source_label) %>%
    tbl_summary(
        by = casecontrol_label,
        statistic = list(all_categorical() ~ "{n} ({p}%)"),
        label = list(
            house_purify ~ "Water purification practice",
            toilet_types ~ "Type of toilet used",
            handwash_food ~ "Handwashing before eating",
            handwash_latrine ~ "Handwashing after latrine use",
            vacc_yn ~ "TCV Vaccination",
            vacc_source_label ~ "Source of vaccine confirmation"
        )
    ) %>%
    bold_labels()

print(table2)

# Save tables if needed
# dir.create(here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
# table1 %>% as_flex_table() %>% save_as_docx(path = here("outputs", "tables", "Table1.docx"))
# table2 %>% as_flex_table() %>% save_as_docx(path = here("outputs", "tables", "Table2.docx"))

message("Demographic and risk factor tables generated.")
