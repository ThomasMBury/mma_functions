# Readme for mma_functions

This repository consists of multiple packages containing functions that can be imported into a Mathematica notebook.\n

To import the functions download the .wl file and use:\
Get["filepath/package.wl"]\
at the start of the notebook.

Information about each function can be obtained with\
?fun\
in the mathematica command line.

## ews_functions.wl

This package includes the functions I wrote to compute EWS [Scheffer et al. 2009](https://www.nature.com/articles/nature08227)
from time-series data. Details on how to implement each function can be obtained using\
?fun\
in the Mathematica command line. The functions include

*TBdetrend* uses a Gaussian filter to smooth data

*TBvariance* computes the variance of residuals over a rolling window.

*TBautocorrelation* computes the autocorrelation of residuals over a rolling window.