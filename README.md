# Impact of large-scale food fortification programs on micronutrient inadequacies

This GitHub repository contains the data and code for the following paper nearing submission:

* Friesen VM, Free CM, Adams KP, Bai Y, Costlow L, Dewey K,  Masters WA, Mbuya MNN,  Nordhagen S, Vasta F, Beal T. Impact of large-scale food fortification programs on micronutrient inadequacies and their implementation costs: a modelling analysis. *Near submission* to _The Lancet_.

## Repository structure

The repository is organized using the following structure:

* data.............. folder containing input data for the analysis
* code............. folder containing code to conduction the analysis
* figures.......... folder containing figures for the manuscript
* output........... folder containing output generated in the analysis
* tables........... folder containing tables for the manuscript

The data files containing the final output (i.e., prevalence of inadequate intakes by country-sex-age group under different fortification scenarios) are located here:

* output/fortification_scenario_output_final.csv (for anyone)
* output/fortification_scenario_output_final.Rds (for R progammers)

Please email Valerie Friesen (vfriesen@gainhealth.org) if you have any questions about the paper and Chris Free (cfree14@gmail.com) if you have any questions about the data, code, and/or repository.

## Analysis

The analysis is performed by the following scripts in the code folder:

* **Step1_clean_data.R:** This script cleans the GFDx data.
* **Step2_impute_data_sens.R:** This scripts imputes missing values within the GFDx data.
* **Step3_merge_data.R:** This script merges the intake distributions from Passarelli et al. (2024) and the GFDx data and sets up the fortification scenarios.
* **Step4_calculate_inadequacies.R:** This script derives the prevalence of inadeqauete intakes under the fortification scenarios.
* **Step4a_calculate_inadequacies_sens_calcium.R:** This script derives the prevalence of inadeqauete intakes under the fortification scenarios when excluding fortified calcium from wheat flour.
* **Step4b_calculate_inadequacies_sens_imputation.R:** This script derives the prevalence of inadeqauete intakes under the fortification scenarios when ignoring proxy values in the GFDx imputation.
* **Step5_calculate_inadequacies_iron.R:** This script rederives the prevalence of inadeqauete intakes **for iron** under the fortification scenarios to use a log-normal requirement distribution. Although we only use the log-normal requirement distirbution for reproductive age women, this does this for all age groups and the relevant groups are parsed in the next step.
* **Step6_merge_inadequate_intakes.R:** This script replaces the iron distributions from Step 4 with the appropriate iron distributions derived in Step 5.

## Related resources

The paper leans heaviliy on data and code developed by Passarelli et al. (2021), Passarelli et al. (2024), and the associated nutriR R package. Links to these papers, the nutriR package, and the nutriR package vignette are provided below:

1. Passarelli S, Free CM,  Shepon A, Beal T, Batis C, Golden CD (2024) Global estimation of dietary micronutrient inadequacies: a modeling analysis. The Lancet Global Health 12(10): e1590-e1599. [[GitHub repository]](https://github.com/cfree14/global_intake_inadequacies/) [[link]](https://www.sciencedirect.com/science/article/pii/S2214109X24002766))
2. Passarelli S, Free CM, Allen LH, Batis C, Beal T, Biltoft-Jensen AP, Bromage S, Cao L, Castellanos-Guitiérrez A, Christensen T, Crispim SP, Dekkers A, De Ridder K, Kronsteiner-Gicevic S, Lee C, Li Y, Moursi M, Moyersoen I, Schmidhuber J, Shepon A, Viana DF, Golden CD (2022) Estimating national and sub-national habitual nutrient intake distributions of global diets. _The American Journal of Clinical Nutrition_ 116(2): 551-560. [[GitHub repository]](https://github.com/cfree14/subnational_nutrient_distributions/) [[link]](https://academic.oup.com/ajcn/article/116/2/551/6605334)
3. Free CM, Passarelli S, Allen LH, Batis C, Beal T, Biltoft-Jensen AP, Bromage S, Cao L, Castellanos-Guitiérrez A, Christensen T, Crispim SP, Dekkers A, De Ridder K, Kronsteiner-Gicevic S, Lee C, Li Y, Moursi M, Moyersoen I, Schmidhuber J, Shepon A, Viana DF, Golden CD (2021) nutriR: Nutritional intake functions for R. Available at: https://github.com/cfree14/nutriR
4. A vignette illustrating the functionality of the "nutriR" package is available here: https://chrismfree.com/wp-content/uploads/nutriR-vignette.html

