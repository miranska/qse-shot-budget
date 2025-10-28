# Quantum Shot Budget Demo

[![arXiv](https://img.shields.io/badge/arXiv-2510.22418-b31b1b.svg)](https://arxiv.org/abs/2510.22418)
[![R ≥ 4.4](https://img.shields.io/badge/R-%E2%89%A5%204.4-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Live Demo — Posit Connect](https://img.shields.io/badge/Live%20Demo-Posit%20Connect-1f6feb)](https://019a1358-9083-cdd0-9cc9-c4ccb9171663.share.connect.posit.cloud/)


Interactive R Shiny application for exploring shot budgeting in quantum program testing.  
This demo accompanies the [paper](https://arxiv.org/abs/2510.22418) *The Cost of Certainty: Shot Budgets in Quantum Program Testing*.

## Overview

Quantum program testing is costly: each measurement (shot)consumes valuable quantum hardware resources.  
This app illustrates how different testing strategies (namely, inverse, swap, and chi-square tests) translate theoretical distinguishability bounds into concrete shot requirements, both for individual functions and at the program level.

## Dependencies

The app requires R ($\geq 4.4$) and the following packages:
```
shiny
bslib
ggplot2
dplyr
tidyr
purrr
pwr
```

## Running the App

### Option A - Live Demo

Open the hosted app (note: free account, may be unavailable if quota is exceeded): [Launch in Posit Connect Cloud](https://019a1358-9083-cdd0-9cc9-c4ccb9171663.share.connect.posit.cloud/).

### Option B - R console / RStudio
Clone this repository and run:
```r
source("setup.R")
shiny::runApp("app.R")
```
Or, in RStudio, open `app.R` and click *Run App*.

### Option C - Command Line
Run directly from your shell with a fixed host/port (useful for containers/servers):
```bash
# From the repo root:
Rscript -e "source('setup.R'); shiny::runApp('app.R')"
```
Then open the link reported by the app.

## Repository Structure

```
app.R        # Main Shiny app
README.md    # This file
setup.R      # Setup script
```

## Citation
If you use or study the code, please cite it as follows.
```bibtex
@article{miranskyy2025cost,
  title={The Cost of Certainty: Shot Budgets in Quantum Program Testing},
  author={Andriy Miranskyy},
  year={2025},
  journal={arXiv preprint arXiv:2510.22418},
  doi={10.48550/arXiv.2510.22418}
}
```

## Contact us
If you found a bug or came up with a new feature -- 
please open an [issue](https://github.com/miranska/qse-shot-budget/issues) 
or [pull request](https://github.com/miranska/qse-shot-budget/pulls).
