# -*- coding: utf-8 -*-
"""
Created on Sun Jun 30 16:03:53 2019

@author: Sravan
"""

import pandas as pd
import numpy as np
import scipy
from scipy import stats
import statsmodels.api as sm

cutlets=pd.read_csv("C:\\Users\\Sravan\\Desktop\\EXCEL R DATA SCIENCE\\assignments\\hypothesis testing\\Cutlets.csv")
# Normality test
Cutlets=stats.shapiro(cutlets.cutlet_a)
Cutlets_pvalue=Cutlets[1]
print("p value is: "+str(Cutlets_pvalue))
#p=0.31 > 0.05 follows normal distribution
SdCutlets=stats.shapiro(cutlets.cutlet_b)
SdCutlets_pvalue=SdCutlets[1]
print("p value is: "+str(SdCutlets_pvalue))
#p=0.52 > 0.05 follows normal distribution
#variance test
scipy.stats.levene(cutlets.cutlet_a, cutlets.cutlet_b)
#p=0.41 > 0.05 all variances are equal.
# y is continous and x is discrete with two variables hence the test is two sample t test
scipy.stats.ttest_ind(cutlets.cutlet_a, cutlets.cutlet_b)
scipy.stats.ttest_ind(cutlets.cutlet_a, cutlets.cutlet_b, equal_var = True)
#p=0.47 > 0.05 hence we accept the null hypothesis 
#H0: Average of diameters of cutlets unit a is equal to average of diameters of cutlets unit b.
#Ha: Average of diameters of cutlets unit a is not equal to the unit b.