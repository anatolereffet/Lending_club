**Econometrics Project IESEG**  
Discrimination in access to credit
==============================  

**Project:**  This main goal of the project was to combine scientific research methods developed in class with statistical methods to create a first short research paper on any relevant subject on a given dataset.

This dataset originates from [Lending Club's](https://www.lendingclub.com) website, the extraction process has been discontinued but it can be found on global platforms such as **Kaggle**. The data used is a truncated part as we only take into account the accepted loans and filter out the rejected loans.

**Goals:**   

Show how do social critera such as social statuts and place of residence might influence access to credit by checking two main hypothesis. 

    H1 - The higher the social status, the more likely it is that the credit interest rate will drop
    H2 - The interest of the loan granted is higher for rural dwellers than for city dwellers


**Main Results :**

Our results were inconclusive as they've failed major statistical checkups such as OLS multicollinearity or linearity assumptions, failing as well during robustness tests.

This could be linked to the encoding we've chosen to realize on categories of workers. 

Code
------------  
All code can be found in: 
* **_/src_** folder:
    - `data_analysis.R`: Summary statistics
    - `data_cleaning.R`: Dataset cleaning, formatting, filtering.
    - `feature_selection.R`: Feature selection using Recursive Feature Elimination (**RFE**)
* **_/src/models_** folder:
    - `script_model_project.R` : Modelling
* **_/Data/Clean_** folder:
    - `add_econ_variables.xlsx` : Three macro-economic variables per state
    - `dictionary_big_city_census.xlsx` : 500 biggest cities in terms of population census 2010 database 
    - `dictionary_emp_title.xlsx` : Hand-Labelled emp_title biggest occurences in Lending Club according to 1 = blue collar, 2 = white collar, 3 = pink collar.

<br>

Project Architecture
------------

```markdown
├── README.md
├── Data
│   ├── Clean
│   │   ├── add_econ_variables.xlsx
│   │   ├── dictionary_big_cities_census.xlsx
│   │   └── dictionary_emp_title.xlsx
│   └── Raw
│       ├── dict_colonne_encode.xlsx
│       ├── dictionnaire_de_donnees_Lending_Club.xlsx
│       └── raw_emp_title_classification.xlsx
└── src <- pipeline of best identified pre-processing and model
    ├── models
    │   ├── script_model_project.R
    ├── plots
    │   ├──both_models.html
    │   ├──interest_rate_histogram.png
    │   ├──robustness.html
    │   └──model
    │        ├──RFE_5000sample_intrate.png
    │        ├──corr_reg_linear_assump.png
    │        └──lm_geolocalisation_linear_assump.png
    ├──corr_matrix.html
    ├──data_analysis.R
    ├──data_cleaning.R
    └──feature_selection.R
```