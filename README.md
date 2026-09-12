# Analysis of Attention Bias in Perfectionism

This repository contains the R code and supporting materials associated with:

> Matar, M., Delor, B., Douilliez, C., & Philippot, P. (2026). *Attention bias and emotional reactivity in perfectionism: Eye-tracking and experimental evidence for context-dependent effects.* Manuscript in preparation.

## Analysis reports

The complete annotated analyses are available in the knitted HTML reports:

- [Study 1 analysis report](reports/AB_Perfectionism_Analysis_1.html)
- [Study 2 analysis report](reports/AB_Perfectionism_Analysis_2.html)

These reports contain the descriptive statistics, statistical models, model comparisons, diagnostic plots, estimated trends, planned comparisons, and substantive figures used to interpret the findings.

The corresponding `.Rmd` files contain the source code used to generate the reports. Streamlined `.R` scripts are also provided for rerunning the analyses and saving the principal outputs.

## Overview

Attention biases have been proposed as a mechanism linking perfectionism to psychological distress, but evidence concerning their nature and causal role remains limited. This research used eye tracking and attention-bias modification to examine how perfectionism relates to attention toward success-related, failure-related, positive, negative, and neutral information.

### Study 1

Study 1 examined associations between perfectionism and attentional allocation in a community sample of 202 participants. Eye movements were recorded during a free-viewing change-detection task.

The analyses examined:

- probability of first or second fixation;
- proportional latency to first fixation;
- dwell-time proportions during early, intermediate, and late viewing periods;
- dwell time across the whole trial; and
- response time.

### Study 2

Study 2 included 92 participants selected from the Study 1 participant pool. Participants completed a gaze-contingent attention-bias modification procedure that trained attention toward success-related, failure-related, or neutral stimuli.

The analyses tested whether the manipulation altered attentional allocation and whether attention-bias modification condition influenced the association between perfectionism and emotional responses to a subsequent failure experience.

## Analysis strategy

Attentional outcomes were analyzed using generalized linear mixed-effects models fitted with `glmmTMB`.

- Eye-tracking proportions were modeled using beta mixed models with a logit link.
- Strictly positive, right-skewed response times were modeled using gamma mixed models with a log link.
- Participant was included as a random intercept.
- An uncorrelated participant-specific random slope for the target-category indicator was retained when supported.
- Mean-centered block or trial presentation order was included as a fixed covariate to control for average linear change across the task.
- Rule awareness was included in the beta-model dispersion submodel.

Semantic category was represented as a five-level factor comprising neutral, positive, negative, success-related, and failure-related stimuli. Neutral was specified as the reference category.

For supported perfectionism-by-category interactions, `emmeans::emtrends()` estimated the perfectionism slope separately within each semantic category. Planned comparisons among these slopes tested four theoretically motivated distinctions:

1. positive versus negative emotional valence;
2. success-related versus failure-related content;
3. performance-irrelevant versus performance-relevant content; and
4. neutral versus non-neutral content.

Study 2 manipulation checks used baseline-adjusted models comparing post-manipulation attentional outcomes across the three experimental conditions. Positive-mood outcomes and approximately symmetric negative-mood outcomes were analyzed using linear models. A Tweedie model was used for post-Tangram negative mood because this outcome included meaningful zero values together with a right-skewed positive distribution and the Tweedie specification provided better fit than the corresponding Gaussian model.

## Repository contents

```text
AB_Perfectionism_Analysis/
├── README.md
├── CITATION.cff
├── LICENSE
├── AB_Perfectionism_Analysis.Rproj
├── AB_Perfectionism_Analysis_1.Rmd
├── AB_Perfectionism_Analysis_2.Rmd
├── AB_Perfectionism_Study1_Preparation.R
├── AB_Perfectionism_Study1_Analysis.R
├── AB_Perfectionism_Study1_Sup_Ctrl.R
├── AB_Perfectionism_Study2_Preparation.R
├── AB_Perfectionism_Study2_Analysis.R
├── AB_Perfectionism_Study2_Sup_t1.R
├── reports/
│   ├── AB_Perfectionism_Analysis_1.html
│   └── AB_Perfectionism_Analysis_2.html
└── AB_Perfectionism_Data/
    └── README.md
```

## File descriptions

### Knitted analysis reports

#### `reports/AB_Perfectionism_Analysis_1.html`

Complete knitted Study 1 report containing:

- sample characteristics and descriptive statistics;
- model specifications and fit comparisons;
- diagnostic plots;
- estimated category-specific perfectionism trends;
- planned theoretical comparisons; and
- interaction figures.

#### `reports/AB_Perfectionism_Analysis_2.html`

Complete knitted Study 2 report containing:

- sample characteristics and descriptive statistics;
- task-performance and control analyses;
- attention-bias modification checks;
- primary mood analyses;
- model comparisons and diagnostics; and
- interaction figures.

The HTML files are generated from the corresponding `.Rmd` documents. They allow readers to examine the complete analytical workflow without installing R or obtaining access to the participant-level data.

### Main analysis reports

#### `AB_Perfectionism_Analysis_1.Rmd`

Source R Markdown document used to generate the complete annotated Study 1 report. It contains the data-preparation steps, statistical models, model comparisons, diagnostics, follow-up analyses, and figures.

#### `AB_Perfectionism_Analysis_2.Rmd`

Source R Markdown document used to generate the complete annotated Study 2 report. It contains the data-preparation steps, manipulation checks, primary analyses, model comparisons, diagnostics, and figures.

### Data-preparation scripts

#### `AB_Perfectionism_Study1_Preparation.R`

Imports and prepares the Study 1 questionnaire, eye-tracking, and response-time data. It defines the task-completer sample, derives the analysis variables, centers the time predictors, and prepares the semantic-category factor.

#### `AB_Perfectionism_Study2_Preparation.R`

Imports and prepares the Study 2 questionnaire, experimental, eye-tracking, and response-time data. It defines the task-completer sample, derives the experimental-condition and time variables, and prepares the baseline and post-manipulation outcomes.

The preparation scripts are sourced automatically by the corresponding analysis scripts. They should remain in the project directory and do not need to be run separately.

### Main analysis scripts

#### `AB_Perfectionism_Study1_Analysis.R`

Runs the Study 1 primary models and produces the model comparisons, diagnostics, estimated trends, planned comparisons, and interaction figures.

#### `AB_Perfectionism_Study2_Analysis.R`

Runs the Study 2 attention-bias modification checks and the primary analyses of emotional responses to the failure task.

### Supplementary analyses

#### `AB_Perfectionism_Study1_Sup_Ctrl.R`

Repeats the Study 1 attentional analyses while controlling for depressive and anxiety symptoms.

#### `AB_Perfectionism_Study2_Sup_t1.R`

Tests perfectionism-related attentional patterns in the Study 2 subsample using only the pre-manipulation assessment.

### Data documentation

#### `AB_Perfectionism_Data/README.md`

Describes the data files and variables required to reproduce the analyses, together with the principal exclusion and preprocessing procedures.

The participant-level data are not included in the public repository. Researchers who obtain the data from the corresponding author should place the supplied files in an `AB_Perfectionism_Data/` directory at the repository root. This preserves the relative paths used by the analysis scripts.

## Viewing the HTML reports

The HTML reports are self-contained and can be downloaded and opened in a web browser without installing R.

GitHub may display an HTML file as source code or offer it as a download rather than rendering it as a webpage. Readers can download the repository and open either of the following files locally:

```text
reports/AB_Perfectionism_Analysis_1.html
reports/AB_Perfectionism_Analysis_2.html
```

The reports may also be published through GitHub Pages to provide browser-accessible versions.

## How to use this repository

### Viewing the completed analyses

Readers who only wish to examine the analyses can download and open the knitted HTML reports. Access to the participant-level data is not required to view the reported output.

### Reproducing the analyses

1. Download or clone the repository:

```bash
git clone https://github.com/mrhmatar/ab-perfectionism-analysis.git
```

2. Open `AB_Perfectionism_Analysis.Rproj` in RStudio.

3. Preserve the repository’s directory structure. The scripts use paths relative to the project root.

4. Request the deidentified data from the corresponding author.

5. Place the supplied data files in:

```text
AB_Perfectionism_Data/
```

6. Run or knit the relevant R Markdown document:

```text
AB_Perfectionism_Analysis_1.Rmd
AB_Perfectionism_Analysis_2.Rmd
```

Alternatively, run the corresponding preparation and analysis scripts:

```text
AB_Perfectionism_Study1_Preparation.R
AB_Perfectionism_Study1_Analysis.R

AB_Perfectionism_Study2_Preparation.R
AB_Perfectionism_Study2_Analysis.R
```

The main and supplementary analysis scripts source their corresponding preparation scripts automatically.

## Knitting the analysis reports

The reports should be generated as self-contained HTML files so that their figures and other resources are embedded in the output.

The R Markdown YAML header should include:

```yaml
output:
  html_document:
    self_contained: true
```

The reports can be rendered from R with:

```r
rmarkdown::render(
  input = "AB_Perfectionism_Analysis_1.Rmd",
  output_file = file.path(
    "reports",
    "AB_Perfectionism_Analysis_1.html"
  )
)
```

and:

```r
rmarkdown::render(
  input = "AB_Perfectionism_Analysis_2.Rmd",
  output_file = file.path(
    "reports",
    "AB_Perfectionism_Analysis_2.html"
  )
)
```

If the reports are not self-contained, R Markdown may generate accompanying directories such as:

```text
AB_Perfectionism_Analysis_1_files/
AB_Perfectionism_Analysis_2_files/
```

Those directories would also need to be included for the HTML reports to display correctly.

## Software requirements

The analyses were developed using R 4.4.1.

The principal R packages include:

- `glmmTMB` for generalized linear mixed-effects models;
- `emmeans` for estimated category-specific trends and planned comparisons;
- `ggplot2` and `ggeffects` for model-based figures;
- `dplyr`, `tidyr`, and related packages for data preparation;
- `psych` for descriptive and psychometric analyses;
- `DHARMa` and supporting packages for model diagnostics; and
- `rmarkdown` for generating the annotated analysis reports.

Package dependencies are loaded within the scripts. A future version of the repository may include an `renv.lock` file to preserve the exact package versions.

## Reproducibility notes

- The repository is organized as a self-contained R project.
- File paths assume that the working directory is the project root.
- The directory structure should be preserved.
- Preparation scripts are sourced automatically by the corresponding analysis scripts.
- The knitted HTML reports provide the most accessible record of the completed analyses.
- The `.Rmd` files provide the complete reproducible source documents.
- The `.R` scripts provide streamlined versions for rerunning analyses and saving figures.
- The participant-level data must be obtained separately from the corresponding author.
- This is the first public release of the analysis code. Development and earlier versions were maintained privately before creation of this repository.

## Data availability

The participant-level data are not publicly available. Deidentified data may be obtained from the corresponding author upon reasonable request, subject to applicable ethical, institutional, and data-protection requirements.

The public repository contains the analysis code, knitted reports, and documentation needed to describe the analytical workflow, but it does not contain participant-level data.

## Citing the associated paper

If you use these materials in academic work, please cite the associated paper:

```bibtex
@unpublished{matar2026attention,
  author = {Matar, M. and Delor, B. and Douilliez, C. and Philippot, P.},
  title  = {Attention Bias and Emotional Reactivity in Perfectionism:
            Eye-Tracking and Experimental Evidence for Context-Dependent Effects},
  year   = {2026},
  note   = {Manuscript in preparation}
}
```

This citation should be updated when a preprint or published article becomes available.

## Citing this repository

The repository can be cited independently of the associated paper:

```bibtex
@software{matar2026abperfectionism,
  author  = {Matar, M.},
  title   = {ab-perfectionism-analysis:
             Analysis of Attention Biases in Perfectionism},
  year    = {2026},
  version = {1.0.0},
  url     = {https://github.com/mrhmatar/ab-perfectionism-analysis},
  note    = {Analysis code, knitted reports, and supporting materials}
}
```

For a version-specific and permanently archived citation, cite the corresponding GitHub release or Zenodo record once a DOI is available:

```bibtex
@software{matar2026abperfectionism_zenodo,
  author    = {Matar, M.},
  title     = {ab-perfectionism-analysis:
               Analysis of Attention Biases in Perfectionism},
  year      = {2026},
  version   = {1.0.0},
  publisher = {Zenodo},
  doi       = {[ZENODO DOI]},
  url       = {https://doi.org/[ZENODO DOI]}
}
```

Citation metadata are also provided in `CITATION.cff`.

## License

The analysis code is released under the [MIT License / GNU General Public License v3.0].

The participant-level data are not distributed under this license. Manuscript materials and other supporting files may be subject to separate reuse conditions.
