# -*- coding: utf-8 -*-
"""
Created on Tue Jul  2 13:08:36 2019

@author: Sravan
"""

import pandas as pd
import numpy as np
import scipy 
from scipy import stats
import statsmodels.formula.api as smf
import matplotlib.pyplot as plt

calories = pd.read_csv("C:\\Users\\Sravan\\Desktop\\EXCEL R DATA SCIENCE\\assignments\\Simple linear regression\\calories_consumed.csv")
calories.columns

plt.hist(calories.caloriesconsumed)
plt.boxplot(calories.caloriesconsumed)
plt.boxplot(calories.caloriesconsumed, 0, "rs", 0)
 
plt.hist(calories.weightgained)
plt.boxplot(calories.weightgained)
plt.boxplot(calories.weightgained, 0, "rs", 0)
np.mean(calories.caloriesconsumed)
np.mean(calories.weightgained)
np.mean(calories)
np.std(calories)

plt.plot(calories.caloriesconsumed, calories.weightgained, "bo");plt.xlabel("caloriesconsumed");plt.ylabel("weightgained")
calories.weightgained.corr(calories.caloriesconsumed)
np.corrcoef(calories.weightgained,calories.caloriesconsumed)

model=smf.ols("weightgained~caloriesconsumed", data=calories).fit()
model.params
model.summary()
model.conf_int(0.05)

pred = model.predict(calories.iloc[:,1])

plt.scatter(x=calories['caloriesconsumed'],y=calories['weightgained'],color='red');plt.plot(calories['caloriesconsumed'],pred,color='black');plt.xlabel('caloriesconsumed');plt.ylabel('weightgained')

pred.corr(calories.weightgained)

#Transforming variables for accuracy

model2 = smf.ols('weightgained~np.log(caloriesconsumed)', data = calories).fit()
model2.params
model2.summary()
