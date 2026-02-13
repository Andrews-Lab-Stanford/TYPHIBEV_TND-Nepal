# TYPHIBEV_TND-Nepal
Code for the manuscript "Effectiveness of the TYPHIBEV®(Vi-CRM197 conjugate) vaccine introduction in Nepal: a test-negative, case-control study"

This repository contains the R code for the analysis of the Nepal Typhoid TCV Vaccine Effectiveness study using a Test-Negative Design.
## Project Structure
- `01_data_cleaning.R`: Imports raw data, performs record corrections, and prepares the dataset for analysis.
- `02_demographics.R`: Generates Table 1 (Participant Characteristics) and Table 2 (Risk Factors).
- `03_ve_analysis.R`: Performs the primary Vaccine Effectiveness analysis and stratified analyses.
- `04_figures.R`: Generates the figures used in the manuscript (e.g., Forest Plot).

### Data Requirements
To comply with data privacy regulations, the raw datasets are not included in this repository. 
1. Obtain the disidentified dataset from the [Dryad Repository URL].
2. Create a folder named `data/raw/` in the project root.
3. Place the disidentified CSV file in `data/raw/` and name it `TND_DATA_disidentified.csv`.
   
### Dependencies
The scripts use the `pacman` package to manage dependencies. Ensure you have R installed and then run the scripts in order. The following packages will be automatically installed if missing:
- `tidyverse`
- `gtsummary`
- `flextable`
- `janitor`
- `here`
- `survival`
- `rio`
- `naniar`
- `broom`

## Usage
Run the scripts sequentially:
1. `01_data_cleaning.R` should be run first to generate the cleaned dataset in `data/clean/`.
2. Subsequent scripts `02`, `03`, and `04` can then be run in any order.
   
## Citation
Please cite the following paper if you use this code:
[Insert Publication Citation Here]
