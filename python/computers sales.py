# -*- coding: utf-8 -*-
"""
Created on Fri Aug  2 21:17:43 2019

@author: Sravan
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
computers_data = pd.read_csv("C:\\Users\\Sravan\\Desktop\\EXCEL R DATA SCIENCE\\assignments\\decision tree\\Company_Data.csv")
computers_data.head()
computers_data.columns
computers_data.dtypes
computers_data['Sales'].unique()

computers_data_str_columns = ['ShelveLoc', 'Urban', 'US']
from sklearn import preprocessing
for i in computers_data_str_columns:
        num = preprocessing.LabelEncoder()
        computers_data[i] = num.fit_transform(computers_data[i])
from sklearn.model_selection import train_test_split
train,test = train_test_split(computers_data,test_size = 0.2)
from sklearn.tree import  DecisionTreeClassifier
