# Replication: Schooling and Labor Market Consequences (Duflo, 2001)

**Author:** Arno Golkar  
**Date:** February 2026  
**Method:** Difference-in-Differences (DiD) & Instrumental Variables (IV)  
**Software:** R (fixest, data.table, haven)

## 1. Overview
This repository contains a high-fidelity replication of Esther Duflo’s seminal paper, *“Schooling and Labor Market Consequences of School Construction in Indonesia: Evidence from an Unusual Policy Experiment”* (AER, 2001). 

The study evaluates the **INPRES program**, one of the largest school construction initiatives in history, which built over 61,000 primary schools between 1973 and 1978.

## 2. Reproduction Policy & Data Sources
To ensure universal reproducibility and adhere to the **TIER Protocol**, this repository does not require manual data handling. 

* **Data Source:** The `01_Data_Cleaning.R` script programmatically retrieves the microdata from the Harvard Dataverse (Ashraf et al., 2020).
* **Rationale:** This source utilizes the cleaned files further refined by Roodman (2022), which correct for historical regency boundary splits and coding inconsistencies present in the original 2001 public release.

## 3. Identification Strategy
The identification relies on a **Difference-in-Differences (DiD)** framework exploiting:
1.  **Spatial Variation:** Intensity of school construction across 260 birth districts.
2.  **Cohort Variation:** Exposure levels of the "Young" cohort (born 1968-1972) vs. the "Old" cohort (born 1950-1962).

## 4. Key Results

### Impact on Education (Table 3)
My replication confirms a statistically significant increase in educational attainment.
* **Coefficient:** ~0.160 (Standard Errors clustered by birth district).
* **Interpretation:** Each additional school built per 1,000 children resulted in an average gain of **0.16 years of schooling** for the exposed cohort.

### Labor Market Consequences (Tables 4 & 5)
* **First Stage:** The F-statistic (33.94) indicates a highly robust instrument.
* **IV Estimates:** While the program significantly boosted education, raw wage estimates are sensitive to baseline district controls (Enrollment 1971), reflecting the program's targeting of historically disadvantaged regions.

## 5. Repository Structure
* `Scripts/01_Data_Cleaning.R`: Automates data download and analytic sample construction.
* `Scripts/02_Analysis_Education.R`: Executes the primary Education DiD regressions.
* `Scripts/03_Analysis_Wages.R`: Executes the Reduced Form and IV wage regressions.
* `Analysis_Data/`: (Auto-generated) Contains the processed `.rds` analytic sample.
* `Output/`: (Auto-generated) Contains formatted Markdown tables of the results.

## 6. Instructions for Replication
1. Ensure R and the `pacman` package are installed.
2. Run `Scripts/01_Data_Cleaning.R` to fetch and prepare the data.
3. Execute scripts `02` and `03` to generate the replication tables in the `Output/` folder.