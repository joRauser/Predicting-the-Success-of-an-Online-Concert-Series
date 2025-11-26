# Predicting the Success of an Online Concert Series - A Quantile Regression Analysis

This repository contains any R-code and data used for the popularity-analysis of the Tiny Desk Concert series.
The study focuses on the implementation of quantile regression, thus identifying heterogeneous effects that are crucial to the success of a concert.

# Outline of the code-folder
**YoutubeAPI.R** - Retrieval of the data from the NPR music channel, by using the "YouTube Data API v3".  
**artistFollower.R** - Retrieval of the followerbase of the performing artist.   
**forwardVarSel.R** - Application of quantile regression and retrieval of the OoS-R^2 for comparison of model-performances. Here, forward variable selection is used.   
**bhetaTable.R** - Application of quantile regression and retrieval of the corresponding bheta-coefficients.   
**modleComparison_likesAndComments.R** - Comparison of model performance when regressing on likeCount or commentCount whether than viewCount.  
**ageAna.R** - Analysis of a subset of the data to extract the influence of the age variable.  
**plots.R** - Code for every figure visible in the thesis.  

# Further information
The mainly used dataset is "vidStatsMC.csv", as it is the cleaned version of "Youtube VideoData.csv", also including an artist Followerbase which is stored in "FollowerDF_Stand..."   
Note, that "YoutubeAPI.R" and "artistFollower.R" are essential code-snippets for building the dataset, while "forwardVarSel.R" and "bhetaTable.R" contain every important function used in analysis.   

To run the code in "YouTubeAPI.R" one most create a Google Developer account and log in with the "yt_oauth" from the R-package "tuber".  

Generally, this code can be used in any other field of interest with only minor adjustments (e.g. changing the channel ID). 
