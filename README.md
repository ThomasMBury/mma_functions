# Readme for mma_functions

This repository consists of multiple packages containing functions that can be imported into a Mathematica notebook.

To import the functions download the .wl file and use:\
`Get["filepath/package.wl"]`\
at the start of your notebook.

Information on how to implement each function can be obtained with\
`?fun`\
in the mathematica command line.

At some point I plan to transfer them to an open-source language - most likely Python.

## ews_functions.wl

This package includes functions to compute early warning signals (EWS) from time-series data. For 
a review of EWS, check out [Scheffer et al. 2009](https://www.nature.com/articles/nature08227).


`TBewsCompute` computes the standard EWS - variance, coefficient of variation, autocorrelation and skewness, after smoothing the time-series data with a Gaussian kernel.\
Optional arguments include bandwidth, rolling window size and lag time for autocorrelation.

`TBplotEWS` creates a plot of the time-series and the EWS as a grid


