# CompAREdesign Package Testing Repository

**⚠️ This repository is under construction. Updates will be made regularly. ⚠️**

This repository contains the validation tests for the **CompAREdesign** R package, specifically for evaluating the functionality and accuracy of the **time-to-event** functions.

## Repository Contents

This repository includes the following folders:

- **`data`**: The output of the validation tests in Rdata format
- **`figures`**: The figures resulting for the validation tests.
- **`scripts`**: A directory containing the R scripts used to perform the validation tests.
- **`table`**: A table summarizing the test results in a structured format (CSV file).

## Testing Overview

The validation tests cover **72,576 different scenarios**, systematically evaluating the accuracy of the **time-to-event** functions implemented in the package. The tests focus on:

- Functionality verification across different parameter settings.
- Identification of numerical instability issues.
- Execution time assessment.

## Key Findings

- Minor precision corrections were required to address numerical errors caused by values **close to zero** in certain denominators.
- The functions proved **robust**, with only **0.06%** of cases yielding unstable results.
- The total execution time for all scenarios was **47.26 hours**.

Next, we will provide the graphical outputs of the test. You have the complete results in `table/validation_results.csv`

## ARE results

![ARE_prob](figures/check_are_prob.png)
![ARE_HR](figures/check_are_HR.png)
![ARE_beta](figures/check_are_beta.png)
![ARE_rho](figures/check_are_rho.png)

## Sample size results

![SS_prob](figures/check_ss_prob.png)
![SS_HR](figures/check_ss_HR.png)
![SS_beta](figures/check_ss_beta.png)
![SS_rho](figures/check_ss_rho.png)

## Effect size results

![gAHR_beta](figures/check_gAHR_prob.png)
![gAHR_beta](figures/check_gAHR_HR.png)
![gAHR_beta](figures/check_gAHR_beta.png)
![gAHR_beta](figures/check_gAHR_rho.png)


## Usage Instructions

To reproduce the tests, clone this repository and run the scripts in the `scripts/` directory. 


