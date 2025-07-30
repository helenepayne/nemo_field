# Nemophila menziesii Field Project

#### Mazer Lab 2020–2023

#### Summary

This repository contains data and R code for the Mazer Lab *Nemophila menziesii* project evaluating Fisher’s Fundamental Theorem of Natural Selection (FFTNS) and patterns of natural selection. It includes data and analysis from a two-year field experiment conducted at four wild populations of *N. menziesii*: Angelo Coast Range Reserve, Bodega Bay Marine Lab, Blue Oak Ranch Reserve, and Hastings Natural History Reservation.

### Project Description

This project quantifies mean fitness (W), additive genetic variance in fitness (Va(W)), and the predictive accuracy of FTNS in multiple populations of *Nemophila menziesii* spanning a latitudinal and aridity gradient. Using pedigreed seeds planted in replicated field experiments over two years, we estimated breeding values and compared predicted versus realized rates of adaptation. Aster models were used to account for sequential life-history stages (e.g., survival, flowering, fruiting, and seed production) to provide robust estimates of fitness components and variance.

The repository includes raw and processed data, analysis scripts, and organized outputs to enable reproducibility and further exploration of evolutionary dynamics in natural populations.

### Project Use

Up-to-date R and RStudio software is recommended to run and reproduce analyses in this project. Most data are stored as `.csv` files and were manipulated primarily within R. The `here` package is used for relative file paths. Some data cleaning was performed in external software (e.g., Excel) and annotated accordingly in the code.

**Recommended versions:**
- R version: 4.2.0 or later
- RStudio version: 2022.07 or later

### File Organization

**Macro-structure:**  
Folders within this repository primarily contain data or code:  
- `analysis_code` – R scripts and R Markdown files for data analysis  
- `climate_data` – Environmental data from field sites  
- `organizing_code` – Code for merging and organizing raw datasets  
- `data_sheets` – Raw and cleaned data sheets for each population  
- `Aster`, `Analysis`, `Calculating Va and h2`, `Data Organization` – Additional project-specific folders with relevant scripts and datasets  

**Micro-structure:**  
Each `.Rmd` file typically includes:
1. A header loading packages and describing the document’s purpose,
2. Code to read and prepare data,
3. Analysis steps (e.g., aster model fitting, fitness calculations),
4. Output tables and figures.

### Credits

**Project conceptualization:** Susan Mazer, Helen Payne, Devin Gamble  
**Data collection:** Helen Payne, Devin Gamble, Lisa Kim, Lynn Jung, Riki Radliff, Ally Whitzel  
**Data curation:** Helen Payne, Devin Gamble  
**Programming and analysis:** Helen Payne, Devin Gamble  

### Acknowledgements

We thank the UC Natural Reserve System for site access and logistical support. This work was made possible through the collaboration of the Mazer Lab at UC Santa Barbara and field assistance from dedicated team members.

### License

This project is licensed under the MIT License.
