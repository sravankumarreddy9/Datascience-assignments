# -*- coding: utf-8 -*-
"""
Created on Sat Jul 27 15:44:27 2019

@author: Sravan
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import statsmodels.formula.api as smf
calories_data = pd.read_csv("E:\\EXCEL R DATA SCIENCE\\assignments\\completed\\Simple linear regression\\calories_consumed.csv")
#checking for correlation coefficient
np.corrcoef(calories_data['caloriesconsumed'], calories_data['weightgained'])
#cor for x and y is 0.94 which means correlation for this dataset is good.
#checking for outliers present in the dataset
plt.boxplot(calories_data['caloriesconsumed'])
#There are no outliers present in the caloriesconsumed
plt.boxplot(calories_data['weightgained'])
#There are no outliers present in the weightgained
#Checking with histogram
plt.hist(calories_data['caloriesconsumed'], color=['orange'],bins=5)
plt.xlabel('caloriesconsumed')
plt.hist(calories_data['weightgained'], color=['green'], bins=5)
plt.ylabel('weightgained')
#From the histograms it looks like the data is right skewed

plt.scatter(calories_data['weightgained'],calories_data['caloriesconsumed'])
plt.xlabel('caloriesconsumed')
plt.ylabel('weightgained')

model1 = smf.ols("calories_data['weightgained'] ~ calories_data['caloriesconsumed']", data=calories_data).fit()
model1.params
model1.summary()
#calculating for the residuals 
pred1 = model1.predict(calories_data['caloriesconsumed'])
error = calories_data['weightgained'] - pred1
np.mean(error) 
#sum of errors for this model is zero.
np.sqrt(sum(error**2)/12)
j#RMSE = 111
plt.scatter(pred1,calories_data['weightgained'])
#checking for other models for better r2, rmse values
model2=smf.ols("calories_data['weightgained'] ~ calories_data['caloriesconsumed'] +(calories_data['caloriesconsumed']*calories_data['caloriesconsumed'])",data=calories_data).fit()

model2.summary()

model2.resid
pred2 = model2.predict(calories_data['caloriesconsumed'])
np.mean(model2.resid)
np.sqrt(sum(model2.resid**2)/12) 
plt.scatter(pred2,calories_data['weightgained'])
