# OCCAM-data-R
R tools to generate data for use in Portland State University's Discrete Multivariate Modeling (DMM) tool, OCCAM.

This code/project is released as Open Source Software under the MIT License (see LICENSE.txt).

## About OCCAM

OCCAM is a tool built and provisioned by the Systems Science Department at Portland State University (https://www.pdx.edu/sysc/home).  It's a tool that can perform various kinds of Discrete Multivariate Modeling (DMM).

* Link to PSU's DMM Page: https://www.pdx.edu/sysc/research-discrete-multivariate-modeling
* Lik to OCCAM: http://dmm.sysc.pdx.edu/

## About this R Code

This is a basic R function that will take a dataframe made of categorical variables (e.g. factors) and generate a file appropriate for input into OCCAM.  The code contains the function and a short piece of code that will build dataframe of categorical data and demonstrate how to generate the OCCAM data file from that dataframe.

There is also a vignette demonstrating the loading of a dataset and preparing it for use in OCCAM.  Some rudimentary results from running that data through OCCAM are shown and discussed.

### Example File:

OCCAM expects data in format like the following example.  See OCCAM's manual for more details.

    # OCCAM DATA FILE
    
    :nominal
    Color, 7, 1, a
    Gender, 2, 1, b
    Size, 4, 1, c
    
    :no-frequency
    
    :data
    4 1 1
    6 1 3
    4 2 4
    7 1 4
    7 2 3
    1 1 4
    2 2 2
    5 2 4
    5 1 1
    1 1 3
    3 2 3
    
    :test
    3 1 3
    5 1 3
    6 1 4
    4 1 3
    
    # End of OCCAM data file

# Caveats/TODO:

The following is a list of limitations and potential improvements.

* currently only produces a "no frequency" variant; the same data can be represented with cases and a count (makes a smaller file)
* doesn't check for non-factor variables and may behave unpredictably with these (e.g. cardinality = number of cases!)
* use existing R tools to bin continuous data or re-bin factors to lower cardinality
* would be nicer if it aligned the header section on commas for readability
* allow specifying exact variable labels as part of the input

Additional Related Projects:
* creating binning code with analysis that can analyze a varaible and propose different options for binning/re-binning
* create tool for "time series masking"

# Recent Updates

## Version: 0.21 (24 October 2018)

* added ability to handle up to 676 variables (26 ^ 2) with names AA, BA, ... ZZ
* added ability to use Z-based variable labels for Dependent Variables (DVs).
    * when number variables <= 26, DVs are Z, Y, X, ...  and IVs are A, B, C, ...
    * when more than 26 variables, DVs are ZZ, ZY, ZX, ..., and IVs are AA, BB, CC, ..., YY, BA, CA, ... 
* fixed bug where if no test data is provided a ":test" is no longer included in the output
