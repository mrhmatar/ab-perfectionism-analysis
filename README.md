# Analysis of Attention Bias in Perfectionism

[![DOI](https://zenodo.org/badge/XXXXXXX.svg)](https://doi.org/10.5281/zenodo.XXXXXXX)

This repository contains the R code used for the analyses reported in:

Matar, M., Delor, B., Douilliez, C., & Philippot, P. (2026). *Attention Bias and Emotional Reactivity in Perfectionism: Eye-Tracking and Experimental Evidence for Context-Dependent Effects.*

The project includes analysis scripts for two eye-tracking studies in which perfectionism-related attention bias was measured (Study 1) and experimentally manipulated (Study 2).

---

## Repository contents

### Main analyses

- `AB_Perfectionism_Analysis_1.Rmd`  
  R Markdown file containing the main analyses for Study 1.

- `AB_Perfectionism_Analysis_2.Rmd`  
  R Markdown file containing the main analyses for Study 2.

### Supplementary analyses

- `AB_Perfectionism_Study1_Sup_Ctrl.R`  
  Study 1 supplementary analyses controlling for anxiodepressive symptoms.

- `AB_Perfectionism_Study1_Sup_Fctr.R`  
  Study 1 supplementary analyses using factor-coded semantic category predictors instead of contrast-coded predictors.

- `AB_Perfectionism_Study2_Sup_t1.R`  
  Supplementary analyses examining pre-ABM attentional patterns in the Study 2 subsample.

### Data preparation files

- `AB_Perfectionism_Study1_Preparation.R`  
- `AB_Perfectionism_Study2_Preparation.R`

These scripts reproduce the data preparation steps used in the main analyses and are automatically sourced by the supplementary analysis scripts. They must remain in the project directory but do not need to be run manually.

### Data and figures

- `AB_Perfectionism_Data/`  
  Folder containing all data files required to run the analyses.

- `images/`  
  Folder containing the figures used in the R Markdown files (in `.svg` format).

---

## How to use this repository

To run the analyses:

1. Download the entire `AB_Perfectionism_Analysis` folder.
2. Unzip the folder if needed.
3. Open RStudio.
4. Create an R Project associated with the `AB_Perfectionism_Analysis` folder, or open the existing `.Rproj` file if provided.
5. Run the desired analysis file from within the project.

---

## Running the main analyses

The main analyses are contained in the following R Markdown files:

- `AB_Perfectionism_Analysis_1.Rmd`
- `AB_Perfectionism_Analysis_2.Rmd`

Each file includes both data preparation and statistical analyses for the corresponding study.

---

## Running the supplementary analyses

The supplementary analyses can be run from the following scripts:

- `AB_Perfectionism_Study1_Sup_Ctrl.R`
- `AB_Perfectionism_Study1_Sup_Fctr.R`
- `AB_Perfectionism_Study2_Sup_t1.R`

These scripts automatically source the required data preparation files, so no separate manual preparation step is necessary.

---

## Notes

- Analyses were conducted using R (version 4.4.1). Package dependencies are loaded within each script.
- The repository is organized as a self-contained R project. File paths assume that the working directory is set to the project root.
- To ensure reproducibility, the folder structure should be preserved exactly as provided.
- This is the first public release of the analysis code. Development and prior versions were maintained privately before creation of this repository.

---

## Citation

If you use this code, please cite this repository as:

```bibtex
@misc{matar2026abperfectionism,
  author       = {Matar, M.},
  title        = {ab-perfectionism-analysis: Analysis of Attention Biases in Perfectionism},
  year         = {2026},
  howpublished = {\url{https://github.com/<your-username>/ab-perfectionism-analysis}},
  note         = {GitHub repository}
}
