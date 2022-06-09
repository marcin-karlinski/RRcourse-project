# -*- coding: utf-8 -*-
"""Untitled1.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1Lmnxyj5byY0vTHh63xBeHZEt4lQLeY_P
"""

# Import the required libraries

import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, classification_report

# Read the dataset

pulsar_stars = pd.read_csv("pulsar_stars.csv", delimiter=",")
pulsar_stars.head()

# Rename the columns

pulsar_stars.columns = [
  "mean",
  "stand_deviation",
  "kurtosis",
  "skewness",
  "mean_DM.SNR",
  "stand_deviation_DM.SNR",
  "kurtosis_DM.SNR",
  "skewness_DM.SNR",
  "class",
]
pulsar_stars.head()

# Defining training variables, we're taking all the columns
# except the last one that is our target variable that we will try to predict.

X = pulsar_stars.drop(['class'], axis=1)
y = pulsar_stars['class']

# Creating train and test sets

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.25, random_state = 42)

print(f"Train size: {len(X_train)}.\nTest size: {len(X_test)}.")

# Train the logistic regression model

log_reg = LogisticRegression(max_iter=10000)
log_reg.fit(X_train, y_train)

# Predict the target variable

y_pred = log_reg.predict(X_test)

# Print the accuracy score in percentage terms.

print('Model accuracy score with 10 decision-trees : {0:0.4f}'. format(accuracy_score(y_test, y_pred)))

# Get importance
importance = log_reg.coef_[0]

# Summarize feature importance
for i,v in enumerate(importance):
	print(f'Feature: {X_train.columns[i]}, Score: %.4f' % (v))

#Print classification report

print(classification_report(y_test, y_pred))