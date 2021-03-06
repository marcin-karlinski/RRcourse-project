# -*- coding: utf-8 -*-

# -- Sheet --

# Import the required libraries

import pandas as pd
import matplotlib.pyplot as plt
from imblearn.over_sampling import SMOTE
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

#since the distribution of the target variable is highly imbalanced, I will perform SMOTE resampling method
oversample = SMOTE()
X, y = oversample.fit_resample(X, y)

# Creating train and test sets

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.25, random_state = 42)

print(f"Train size: {len(X_train)}.\nTest size: {len(X_test)}.")

# Train the logistic regression model

log_reg = LogisticRegression(max_iter=10000)
log_reg.fit(X_train, y_train)

# Predict the target variable

y_pred = log_reg.predict(X_test)

# Print the accuracy score in percentage terms.

print('Model accuracy score : {0:0.4f}'. format(accuracy_score(y_test, y_pred)))

# Get importance
importance = log_reg.coef_[0]

# Summarize feature importance
for i,v in enumerate(importance):
	print(f'Feature: {X_train.columns[i]}, Score: %.4f' % (v))

#Print classification report

print(classification_report(y_test, y_pred))

#exporting results to a csv file
report = classification_report(y_test, y_pred, output_dict=True)
df = pd.DataFrame(report).transpose()
print(df)
df.to_csv('glm_conf_matrix_python.csv')

