
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EPhysData R Package

## Introduction

The `EPhysData` package provides a set of tools and classes for working
with electrophysiological data in R. This package is designed to
streamline the process of importing and preprocessing
electrophysiological data, making it easier for researchers and analysts
to work with complex datasets.

There are two classes for storing the data. The `EPhysData` class stores
data from a single recording (possibly with repeated measurements). It
is usually generated from imported raw data using `newEPhysSet()`.
Information on how to filter or average the data contained in this class
can be stored in it, but the data is left unchanged. The `EPhysSet`
class contains a collection of associated `EPhysData` classes (e.g. data
from different channels recorded in parallel or in response to
increasing stimulus intensities). Each `EPhysData` stored is associated
with metadata describing the nature of the particular recodring. This
class can be subsetted, extended or modified in other ways.

## Installation

You can install the development version of EPhysData like so:

``` r
if (!requireNamespace("remotes", quietly = TRUE)){
  install.packages("remotes")
}
#remotes::install_github("moritzlindner/EPhysData")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# Load the EPhysData package
library(EPhysData)
#> Lade nötiges Paket: units
#> udunits database from /usr/share/xml/udunits/udunits2.xml
#> Warning: vorhergehender Import 'lifecycle::last_warnings' durch
#> 'rlang::last_warnings' während des Ladens von 'pillar' ersetzt
#> Warning: vorhergehender Import 'lifecycle::last_warnings' durch
#> 'rlang::last_warnings' während des Ladens von 'tibble' ersetzt

# Create an example EPhysData object
data <- makeExampleEPhysData()

# Get data
GetData(data, Raw = T)
#> Units: [mV]
#>            rep1       rep2       rep3
#> [1,] 0.01443368 0.52238732 0.17180403
#> [2,] 0.36757403 0.31063893 0.48172521
#> [3,] 0.97070700 0.01468193 0.31016890
#> [4,] 0.70255007 0.47099978 0.02797219

# Get time trace data
TimeTrace(data)
#> Units: [s]
#> [1] 1 2 3 4

# Set Averaging and function for the data
FilterFunction(data)<-function(x){x/max(x)}
GetData(data, Raw = F)
#> Units: [mV]
#>            rep1       rep2       rep3
#> [1,] 0.01486924 1.00000000 0.35664323
#> [2,] 0.37866630 0.59465251 1.00000000
#> [3,] 1.00000000 0.02810544 0.64387101
#> [4,] 0.72375091 0.90162944 0.05806669
AverageFunction(data)<-function(x){mean(x)}
GetData(data, Raw = F)
#> Units: [mV]
#>           [,1]      [,2]      [,3]     [,4]
#> [1,] 0.4571708 0.6577729 0.5573255 0.561149

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

# Documentation
