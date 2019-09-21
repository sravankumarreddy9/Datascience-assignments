# -*- coding: utf-8 -*-
"""
Created on Mon Jul  1 00:21:13 2019

@author: Sravan
"""

import pandas as pd
import numpy as np
import scipy 
from scipy import stats
import statsmodels.api as sm

buyer_ratio = pd.read_csv("C:\\Users\\Sravan\\Desktop\\EXCEL R DATA SCIENCE\\assignments\\hypothesis testing\\BuyerRatio.csv")
buyer={'males':[50, 142, 131, 70],
       'females':[435, 1523, 1356, 750]}
df_buyer= pd.DataFrame(buyer)
buyer=stats.shapiro(df_buyer.males)
buyer_pvalue=buyer[1]
print("p value is: "+str(buyer_pvalue))
#p =0.34 > 0.05 follows normal distribution
buyer_f=stats.shapiro(df_buyer.females)
buyerf_pvalue=buyer_f[1]
print("p value is: "+str(buyerf_pvalue))
#p = 0.54 > 0.05 follows normal distribution
count=pd.crosstab(df_buyer["males"],df_buyer["females"])
count
#Y is discrete and x is discrete with more than two variables Hence we take chisquare Test
Chisquares_results=scipy.stats.chi2_contingency(count)
Chi_pvalue=Chisquares_results[1]
print("the p value: "+str(Chi_pvalue))
#p value = 0.21 > 0.05 hence we accept the null hypothesis.
#H0: All proportions are equal. Male and Female buyers are same across regions.
#Ha = All proportions are not equal. Male and Female buyers are not same across regions.