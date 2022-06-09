# -*- coding: utf-8 -*-

# -- Sheet --

# Import required libraries

import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score, classification_report

# Read the csv

pulsar_stars = pd.read_csv("pulsar_stars.csv", delimiter=",")
pulsar_stars.head()

# Print the columns

pulsar_stars.columns

# Trim the column names (remove leading/trailing whitespaces).

trimmed_columns = [x.strip() for x in pulsar_stars.columns]
pulsar_stars.columns = trimmed_columns
pulsar_stars.columns

# Rename columns

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

# Initializing Random Forest Regressor and training the model.

rf = RandomForestClassifier(n_estimators = 1000, random_state = 42)
rf.fit(X_train, y_train)

# Predict the test set

y_predictions = rf.predict(X_test)

# Print the accurasy score in percentage terms.

print('Model accuracy score with 10 decision-trees : {0:0.4f}'. format(accuracy_score(y_test, y_predictions)))

# Print the most important features

feature_scores = pd.Series(rf.feature_importances_, index=X_train.columns).sort_values(ascending=False)
feature_scores

#Print classification report

print(classification_report(y_test, y_predictions))

#exporting results to a csv file
report = classification_report(y_test, y_predictions, output_dict=True)
df = pd.DataFrame(report).transpose()
print(df)
df.to_csv('rf_conf_matrix_python.csv')

