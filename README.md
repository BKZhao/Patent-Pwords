# Unintended Consequence of Promotional Words in Patent Evaluation

This repository contains the replication code and analysis scripts for the research paper "Unintended Consequence of Promotional Words in Patent Evaluation."

## Overview

This project investigates how the use of promotional or "hype" words in patent applications affects patent evaluation outcomes, including grant decisions, appeals, citations, and novelty assessments.

## Project Structure

```
patent_words/
├── Data/                           # Data directory (not included in repository)
│   ├── patent_reg_data.csv
│   ├── appeal_reg_data.csv
│   ├── citation_3_reg_data.csv
│   ├── citation_5_reg_data.csv
│   └── claim_reg_data.csv
├── 00_Crobachs_Alpha.ipynb        # Cronbach's Alpha reliability analysis
├── 01_patent_reg.do                # Patent grant decision regression analysis
├── 02_appeal_reg.do                # Patent appeal regression analysis
├── 03_citation_3y_reg.do           # 3-year citation analysis
├── 04_citation_5y_reg.do           # 5-year citation analysis
├── cal_concreteness.ipynb          # Calculate text concreteness scores
├── cal_novelty.ipynb               # Calculate patent novelty metrics
├── cal_text_features.ipynb         # Extract text features from patent documents
├── get_ptab_data.ipynb             # Retrieve PTAB (Patent Trial and Appeal Board) data
├── pred_gender.ipynb               # Predict gender of inventors/examiners/attorneys
├── PSM_Appeal.ipynb                # Propensity Score Matching for appeal analysis
├── Figure1_Plot.R                  # Generate Figure 1
├── Figure2_Plot.R                  # Generate Figure 2
├── Figure3_Plot.R                  # Generate Figure 3
└── Figure4_Plot.R                  # Generate Figure 4
```

## Requirements

### Python Dependencies
- pandas
- numpy
- scipy
- pingouin
- jupyter

### R Dependencies
- ggplot2
- dplyr
- (other packages as needed for plotting)

### Statistical Software
- Stata (version 14 or higher recommended)

## Data

Due to size and privacy considerations, the data files are not included in this repository. The analysis uses the following datasets:

- `patent_reg_data.csv`: Main patent application data with text features
- `appeal_reg_data.csv`: Patent appeal decisions data
- `citation_3_reg_data.csv`: 3-year forward citation data
- `citation_5_reg_data.csv`: 5-year forward citation data
- `claim_reg_data.csv`: Patent claim-level data

## Analysis Pipeline

### 1. Data Preparation
Run the following notebooks to prepare the data:
```bash
jupyter notebook cal_text_features.ipynb
jupyter notebook cal_concreteness.ipynb
jupyter notebook cal_novelty.ipynb
jupyter notebook get_ptab_data.ipynb
jupyter notebook pred_gender.ipynb
```

### 2. Reliability Analysis
```bash
jupyter notebook 00_Crobachs_Alpha.ipynb
```
This notebook performs Cronbach's Alpha analysis to validate the reliability of the hype words measure, iteratively screening words to achieve acceptable internal consistency (α ≥ 0.59).

### 3. Main Regression Analyses
Run the Stata do-files in order:
```stata
do 01_patent_reg.do
do 02_appeal_reg.do
do 03_citation_3y_reg.do
do 04_citation_5y_reg.do
```

### 4. Propensity Score Matching
```bash
jupyter notebook PSM_Appeal.ipynb
```

### 5. Generate Figures
```R
Rscript Figure1_Plot.R
Rscript Figure2_Plot.R
Rscript Figure3_Plot.R
Rscript Figure4_Plot.R
```

## Key Variables

### Hype Words
The analysis examines 139 promotional words commonly used in grant applications, including:
- broad, important, novel, innovative, revolutionary
- significant, critical, essential, groundbreaking
- (see `00_Crobachs_Alpha.ipynb` for complete list)

After reliability screening, 135 words were retained (dropped: 'first', 'key', 'top', 'quality').

### Dependent Variables
- Patent grant decision
- Appeal outcomes
- Forward citations (3-year and 5-year)
- Patent novelty

### Control Variables
- Text features: readability (Flesch Reading Ease), concreteness, novelty
- Patent characteristics: CPC classification, number of claims, number of inventors
- Applicant characteristics: prior applications, prior patents, entity type
- Examiner characteristics: experience, gender
- Attorney characteristics: experience, gender
- Team characteristics: gender composition

## Citation

If you use this code or data in your research, please cite:

```
[Citation information to be added upon publication]
```

## License

[Add appropriate license]

## Contact

For questions or issues, please open an issue on this repository or contact [your contact information].

## Acknowledgments

[Add acknowledgments as appropriate]
