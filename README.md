# Predicting the Success of an Online Concert Series - A Quantile Regression Analysis

This repository contains any R-code and data used for the popularity-analysis of the Tiny Desk Concert series.
The study focuses on the implementation of quantile regression, thus identifying heterogeneous effects that are crucial to the success of a concert.

# Outline of the code-folder
**Code**
**YoutubeAPI** - Retrieval of the data from the NPR music channel, by using the "YouTube Data API v3".  
**artistFolloer** - Retrieval of the followerbase of the performing artist.   
**forwardVarSel** - Application of quantile regression and retrieval of the OoS-R^2 for comparison of model-performances. Here, forward variable selection is used.   
**bhetaTable** - Application of quantile regression and retrieval of the corresponding bheta-coefficients.   
**modleComparison_likesAndComments** - Comparison of model performance when regressing on likeCount or commentCount whether than viewCount.  
**ageAna** - Analysis of a subset of the data to extract the influence of the age variable.  
**plots.R** - Code for every figure visible in the thesis.  
