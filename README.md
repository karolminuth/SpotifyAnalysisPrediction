# SpotifyAnalysisPrediction
# *Analysis music on Spotify and predictions of preferences*

#### Data comes from https://www.kaggle.com/geomack/spotifyclassification

## *Purpose*
##### The purpose of this project is analyzing data, checking influence features of music on target, which is representing that song is set as not pleasant or pleasant for user


## *Requirements*
* R programming language
* RStudio
* Necessary libraries are on the beginning of each script

## *Project structure*
* `Data_analysis.R` -> Whole processing of data and analysis with visualization 
* Classifiers scripts with prediction and visualization: `GBM.R`, `Logistic_Regression.R`, `Random_Forest.R`,
 `SVM_SVC.R`, `Trees.R`, `XGBoost.R`
* `Report_generator.Rmd` -> R Markdown file which contains shortened code of all scripts  
with description, comments and also is generating final report in html file
* `Report.html` -> Final done report of data analysis with visualization and predictions of classifiers
* `Report_generator.html` -> New generated report

## Additional information
Classifiers scripts contains more details like:
* Confusion Matrix
* ROC Values
* Normal plots and ggplots

