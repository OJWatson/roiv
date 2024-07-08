
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-4.3.3-brightgreen.svg)](https://cran.r-project.org/)
[![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/)

## Research compendium for Return on Investment from Vaccines

This is a working R compendium (think R package but for reproducible
analysis). The analysis directory contains R scripts used to generate
the results.

### Installation

    git clone https://github.com/OJWatson/roiv.git
    cd roiv
    open roiv.Rproj
    devtools::install_deps()

### Overview

The structure within analysis is as follows:

    analysis/
        |
        ├── 00_xxx /             # analysis scripts used for generating figures
        ├── 01_xxx /             # scripts are numbered by order to run these in
        |
        ├── plots/              # location of figures produced by the analysis scripts
        |
        ├── data/
        │   ├── DO-NOT-EDIT-ANY-FILES-IN-HERE-BY-HAND
        │   ├── raw_data/       # data obtained from elsewhere
        │   └── derived_data/   # data generated during the analysis
        |
        ├── tables/             # location of tables produced by the analysis scripts

### Compendium DOI:

<https://doi.org/XXX>

The files at the URL above will generate the results as found in the
publication.

### The R package

This repository is organized as an R package. There are no/negligable R
functions exported in this package - the majority of the R code is in
the analysis and src directory. The R package structure is here to help
manage dependencies, to take advantage of continuous integration, and so
we can keep file and data management simple.

To download the package source as you see it on GitHub, for offline
browsing, use this line at the shell prompt (assuming you have Git
installed on your computer):

``` r
git clone https://github.com/OJWatson/roiv.git
```

Once the download is complete, open the `roiv.Rproj` in RStudio to begin
working with the package and compendium files. We will endeavour to keep
all package dependencies required listed in the DESCRIPTION.

### Licenses

Code: [MIT](http://opensource.org/licenses/MIT) year: 2024, copyright
holder: OJ Watson & Hallie Benjamin

Data: [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse
