#!/usr/bin/env python
# coding: utf-8

# In[ ]:


#import sklearn
import pandas as pd
import numpy as np
import scipy as sp


# In[ ]:


import os
os.getcwd()


# In[ ]:


#os.chdir('/Users/ola/Desktop/UW/Semester 4/Reproducible Research')
path="/Users/ola/Desktop/UW/Semester 4/Reproducible Research"
os.chdir(path)


df = pd.read_csv("pulsar_stars.csv",
                 sep    = ",",
                 header = 1)


# In[ ]:


#or this method


# In[ ]:


#read in the file

import csv

with open('/Users/ola/Downloads/pulsar_stars.csv', 'r') as file:
    reader = csv.reader(file, delimiter= ',')
    for row in reader:
        print(row)


# In[ ]:


#or this method 


# In[ ]:


pulsar_stars = pd.read_csv(r'/Users/ola/Downloads/pulsar_stars.csv')
print (df.to_string())


# In[ ]:


#change column names

pulsar_stars = df.rename({'a': 'X', 'b': 'Y'}, axis='columns')

pulsar_stars = df.rename({'Mean of the integrated profile' : 'mean', 
                          'Standard deviation of the integrated profile' : 'stand_deviation',
                         'Excess kurtosis of the integrated profile' : 'kurtosis',
                         'Skewness of the integrated profile' : 'skewness',
                         'Mean of the DM-SNR curve' : 'mean_DM.SNR',
                         'Standard deviation of the DM-SNR curve' : 'stand_deviation_DM.SNR',
                         'Excess kurtosis of the DM-SNR curve' : 'kurtosis_DM.SNR',
                         'Skewness of the DM-SNR curve' : 'skewness_DM.SNR',
                         'target_class' : 'class'}, 
                         axis = 'columns')


# In[ ]:


#first model

from sklearn import linear_model

#independent variables
X = pulsar_stars[["mean", "stand_deviation", "kurtosis", "skewness", "mean_DM.SNR", "stand_deviation_DM.SNR", 
                  "kurtosis_DM.SNR", "skewness_DM.SNR"]]

#dependent variable
y = pulsar_stars[["class"]]

logreg = linear_model.LogisticRegression()
model1 = logreg.fit(X, y)

pulsar_stars.describe()
coef = logreg.coef_
intercept = logreg.intercept_


#vif
cc = np.corrcoef(pulsar_stars, rowvar = False)
VIF = np.linalg.inv(cc)
VIF.diagonal()

pulsar_stars2 = pulsar_stars[["mean", "stand_deviation", "kurtosis", "skewness", 
                              "mean_DM.SNR", "stand_deviation_DM.SNR", "class"]]

#as.factor
pulsar_stars2['class'], _ = pd.factorize(pulsar_stars2['class'], sort = True)

#sample split
from sklearn.cross_validation import train_test_split

train, test = train_test_split(quality, train_size=0.75, random_state=88)

qualityTrain = pd.DataFrame(train, columns = quality.columns)
qualityTest = pd.DataFrame(test, columns = quality.columns)


# In[ ]:


#second model

from sklearn import linear_model

#independent variables
X = qualityTrain[["mean", "stand_deviation", "kurtosis", "skewness", "mean_DM.SNR", "stand_deviation_DM.SNR"]]

#dependent variable
y = qualityTrain[["class"]]

logreg = linear_model.LogisticRegression()
model2 = logreg.fit(X, y)

qualityTrain.describe()
coef = logreg.coef_
intercept = logreg.intercept_


#vif2
cc = np.corrcoef(qualityTrain, rowvar = False)
VIF = np.linalg.inv(cc)
VIF.diagonal()

#correlations
pulsar_stars2.corr()

from biokit.viz import pulsar_stars2
c = corrplot.Corrplot(pulsar_stars2)
c.plot()

#as.factor
qualityTest['class'], _ = pd.factorize(qualityTest['class'], sort = True)



from pandas.stats.api import ols
import matplotlib.pyplot as plt

#predict
linmodel = ols(y, X)
linpred = linmodel.predict(qualityTest)

#cut off
threshold = Find_Optimal_Cutoff(qualityTest['class'], linpred)



def if_this_else_that(x, list_of_checks, yes_label, no_label):
    if x in list_of_checks:
        res = yes_label
    else: 
        res = no_label
    return(res)


glm.pred = if_this_else_that(linpred, linpred > 0.1, 1, 0)
glm.pred, _ = pd.factorize(glm.pred, sort = True)

#ROC Curve
import scikitplot as skplt
import matplotlib.pyplot as plt
skplt.metrics.plot_roc_curve(qualityTest['class'], linpred)
plt.show()

from sklearn.metrics import roc_curve, auc
fpr, tpr,_ = roc_curve(qualityTest['class'], linpred)
roc_auc = auc(fpr, tpr)

