# -*- coding: utf-8 -*-
"""
Created on Sun Aug  4 02:30:30 2019

@author: Sravan
"""

import pandas as pd 
import numpy as np
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
from sklearn.preprocessing import scale 
wine_data = pd.read_csv("C:\\Users\\Sravan\\Desktop\\EXCEL R DATA SCIENCE\\assignments\\PCA\\wine.csv")
wine_data.describe()

# Considering only numerical data 
wine_data = wine.ix[:,1:]
wine_data.head(4)

# Normalizing the numerical data 
normalized = scale(wine_data)

pca = PCA(n_components = 13)
pca_values = pca.fit_transform(normalized)
# The amount of variance that each PCA explains is 
var1 = pca.explained_variance_ratio_
var1
pca.components_[0]

# Cumulative variance 

var2 = np.cumsum(np.round(var1,decimals = 4)*100)
var2
'''
array([ 36.2 ,  55.41,  66.53,  73.6 ,  80.16,  85.1 ,  89.34,  92.02,
        94.24,  96.17,  97.91,  99.21, 100.01])
'''
#observation if we take first 10 columns 96 % of data is covered

# Variance plot for PCA components obtained 
plt.plot(var2,color="red")

# plot between PCA1 and PCA2 
x = pca_values[:,0]
y = pca_values[:,1]
z = pca_values[:,2]
plt.scatter(x,y,color=["blue"])

################### Clustering  ##########################
#kmeans with original data set
from sklearn.cluster import KMeans
Sum_of_squared_distances = [] 

K = range(1,15)
for i in K:
    kmeans = KMeans(n_clusters = i)
    kmeans.fit(wine.iloc[:,1:])
    kmeans.labels_
    Sum_of_squared_distances.append(kmeans.inertia_)

#elbow curve
#As k increases, the sum of squared distance tends to zero from K = 3 sharp bend
plt.plot(K, Sum_of_squared_distances, 'bx-')
plt.xlabel('k')
plt.ylabel('Sum_of_squared_distances')
#hierarchical clustering with original data set
from scipy.cluster.hierarchy import linkage 

import scipy.cluster.hierarchy as sch # for creating dendrogram 

#p = np.array(df_norm) # converting into numpy array format 
z = linkage(wine_data.iloc[:,1:], method="complete",metric="euclidean")

plt.figure(figsize=(25, 5));plt.title('Hierarchical Clustering Dendrogram');plt.xlabel('Index');plt.ylabel('Distance')
sch.dendrogram(
    z,
    leaf_rotation=0.,  # rotates the x axis labels
    leaf_font_size=8.,  # font size for the x axis labels
)
# Now applying AgglomerativeClustering choosing 3 as clusters from the dendrogram
from	sklearn.cluster	import	AgglomerativeClustering 
h_complete	=	AgglomerativeClustering(n_clusters=3,	linkage='complete',affinity = "euclidean").fit(wine_data.iloc[:,1:]) 

cluster_labels=pd.Series(h_complete.labels_)

wine_data['clust']=cluster_labels # creating a  new column and assigning it to new column 
wine_data['clust'].value_counts()
'''
1    83
2    52
0    43
'''


#kmeans with first 3 principal component scores 
from sklearn.cluster import KMeans
values = pd.DataFrame(pca_values[:,0:4])

Sum_of_squared_distances = [] 

K = range(1,15)
for i in K:
    kmeans = KMeans(n_clusters = i)
    kmeans.fit(values)
    kmeans.labels_
    Sum_of_squared_distances.append(kmeans.inertia_)

#elbow curve
#As k increases, the sum of squared distance tends to zero from K = 3 sharp bend
plt.plot(K, Sum_of_squared_distances, 'bx-')
plt.xlabel('k')
plt.ylabel('Sum_of_squared_distances')

#hierarchical clustering with PCA
from scipy.cluster.hierarchy import linkage 

import scipy.cluster.hierarchy as sch # for creating dendrogram 

#p = np.array(df_norm) # converting into numpy array format 
z = linkage(new_df, method="complete",metric="euclidean")

plt.figure(figsize=(25, 5));plt.title('Hierarchical Clustering Dendrogram');plt.xlabel('Index');plt.ylabel('Distance')
sch.dendrogram(
    z,
    leaf_rotation=0.,  # rotates the x axis labels
    leaf_font_size=8.,  # font size for the x axis labels
)
plt.show()

# Now applying AgglomerativeClustering choosing 4 as clusters from the dendrogram
from	sklearn.cluster	import	AgglomerativeClustering 
h_complete	=	AgglomerativeClustering(n_clusters=4,	linkage='complete',affinity = "euclidean").fit(new_df) 

cluster_labels=pd.Series(h_complete.labels_)

new_df['clust']=cluster_labels # creating a  new column and assigning it to new column 
new_df['clust'].value_counts()

'''
Final observation :
1) With full data without PCA
K Means :- 3 Clusters (best fit)
Hireracy :- 3 Clusters (best fit)

2) With PCA takin 3 inital PCA values
K Means :- 3 Clusters (best fit)
Hireracy :- 4 Clusters (best fit)
'''