# RRcourse-project

Repository for the Reproducible research classes 2022. Improvements regarding reproducibility of bachelor thesis about classification of pulsar stars. 

## Subject of analysis

The original bachelor thesis aimed at comparing various classification models of radio emissions and analyze their performance and accuracy. Pulsar stars, a rare type of neutron stars, produce a pattern of broadband radio emissions, that can be recorded on Earth. Most of the signals detected on Earth, however, are just radio frequency interferences (RFI) and noise, which makes a real pulsar star signal hard to spot. So far every emission needed to be checked manually by an astronomer, but with the progress of machine learning, various models are being used that classify signals automatically. 

In the thesis two models were compared: logistic regression and random forest. The original conclusion was that despite the similarly high accuracy of classification of the pulsar stars, logistic regression may be better suited for this task since it achieved higher accuracy regarding the positive class.

In this project, the analysis was re-created using the reproducible research recommendations. Original codes were re-written in a clear, and most importantly, working manner. Codes were commented for better understanding of is done in the study. Renv package was used to create a reproducible environment regarding libraries used. Furthermore, functions were documented and tested. Additionally, study was reproduced in Python to check if the conclusion will still hold.

Data used in this study was collected from the High Time Resolution Survey [1] and was later shared on the [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/HTRU2).

## How to reproduce the research

Original codes for the pulsar star signal prediction can be found in the folder *src_R*, under the names *logistic_regression_original_script.R* and *RandomForest_original_script.R*. The re-written analysis can be found in the file *revised_codes.R* in the same folder. All the functions needed to run the script (with documentation) are in the *functions* folder. Original data, that was also used for this project, is located in the *data* folder. Codes for analysis in Python can be found in the *src_Python* file. Results of the revised study are in the *results* folder. Files regarding the renv's environment are located in the *renv* folder. Below you can find instructions for reproducing the environment regarding packages used in this project (for consistency of packages versions). 


## Renv instructions:

To reproduce the working environment simply run `renv::restore()` in the directory
with the project. In some cases you may also need to activate the project first. 
If you see the following after calling `renv::restore()`

*This project has not yet been activated.  
Activating this project will ensure the project library is used during restore.  
Please see `?renv::activate` for more details.  

Would you like to activate this project before restore? [Y/n]*

Just accept by typing *Y* and pressing enter. 

Then if you update or install any package that will be needed in the project just run 
`renv::snapshot()` and renv lockfile will be updated. Then just push your changes 
to the remote repository. 
