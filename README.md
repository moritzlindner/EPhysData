
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
remotes::install_github("moritzlindner/EPhysData")
```

Note that `EPhysData` recommends the github-deposited R Package
`EPhysMethods`. Installation usually works automatically. Updating,
however may fail. If this is the case, update manually using the
following line of code:

``` r
remotes::install_github("moritzlindner/EPhysMethods")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# Load the EPhysData package
library(EPhysData)

# Create an example EPhysData object
data <- makeExampleEPhysData()

# Get data
GetData(data, Raw = T)

# Get time trace data
TimeTrace(data)

# Set Averaging and function for the data
FilterFunction(data)<-function(x){x/max(x)}
GetData(data, Raw = F)
#> Warning in GetData(data, Raw = F): Averaging function function (x) { x} returns more than a single value per time point. Has a valid function been
#> set? Try e.g.: AverageFunction(X)<-mean
AverageFunction(data)<-function(x){mean(x)}
GetData(data, Raw = F)

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
```

## Documentation

Please see the `EPhysData-package`-page accessible via the package’s
help index for a systematic description of the functions and methods
included into the package.
