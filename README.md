
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EphysData R Package

## Introduction

The `EphysData` package provides a set of tools and classes for working
with electrophysiological data in R. This package is designed to
streamline the process of importing and preprocessing
electrophysiological data, making it easier for researchers and analysts
to work with complex datasets.

## Installation

You can install the development version of EPhysData like so:

``` r
remotes::install_github("moritzlindner/EphysData")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# Load the EphysData package
library(EPhysData)
#> Lade nötiges Paket: units
#> udunits database from /usr/share/xml/udunits/udunits2.xml

# Create an example EPhysData object
data <- makeExampleEPhysData()

# Get data
GetData(data, Raw = T)
#> Units: [mV]
#>           rep1       rep2      rep3
#> [1,] 0.1294224 0.04110528 0.2898242
#> [2,] 0.3116469 0.05032098 0.8458965
#> [3,] 0.5748132 0.99836499 0.2918312
#> [4,] 0.9257058 0.15571426 0.7363647

# Get time trace data
TimeTrace(data)
#> Units: [s]
#> [1] 1 2 3 4

# Set Averaging and function for the data
FilterFunction(data)<-function(x){x/max(x)}
GetData(data, Raw = F)
#> Units: [mV]
#>           rep1       rep2      rep3
#> [1,] 0.1398094 0.04117260 0.3426238
#> [2,] 0.3366587 0.05040339 1.0000000
#> [3,] 0.6209459 1.00000000 0.3449964
#> [4,] 1.0000000 0.15596927 0.8705140
AverageFunction(data)<-function(x){mean(x)}
GetData(data, Raw = F)
#> Units: [mV]
#>      rep1      rep2      rep3 
#> 0.5243535 0.3118863 0.6395335

# Create an EPhysSet object
ephysSet <-
  newEPhysSet(
    Data = list(
      makeExampleEPhysData(),
      makeExampleEPhysData(),
      makeExampleEPhysData(),
      makeExampleEPhysData()
    ),
    Metadata = data.frame(StepID = c("A1", "A2", "A3", "A4"))
  )
ephysSet
#> An object of class EPhysSet 
#> Data Items:
#>  4
#> Metadata:
#>  StepID
#> Size: 33.7 Kb
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
