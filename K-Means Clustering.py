##########################################################################

# Copyright (c) 2017 Nandini Khanwalkar 
# nandini2@pdx.edu

##########################################################################

import os
import random
import math
import numpy as np
import sklearn
from sklearn.metrics import *

train_data = np.loadtxt('optdigits.train', delimiter=',')
X_train, y_train = train_data[:, np.arange(64)], train_data[:, 64]
test_data = np.loadtxt('optdigits.test', delimiter=',')
X_test, y_test = test_data[:, np.arange(64)], test_data[:, 64]

def find_nearest_centre(centers, dataset, k):
	idx_cluster = [[] for i in range(k)]												# Make k lists corresponding to each cluster-center
	for i in range(0, dataset.shape[0]):												# For each vector in dataset
		C_i = np.argmin(np.sqrt(np.sum(np.power(centers - dataset[i, :], 2), axis=1)))	# Compute closest cluster center
		idx_cluster[C_i].append(i)														# Add vector index to that cluster-center's list
	return idx_cluster																	# Return cluster indices

def find_K_means(k):
	centroids = np.random.choice(np.arange(17), k*64, replace=1).reshape(k, 64)	# Randomly initialize k cluster centers
	while True:
		idx_cluster = find_nearest_centre(centroids, X_train, k)				# Seperate data into clusters by finding nearest center
		k_means = []
		for i in range(k):														# Do for all clusters:
			if (len(idx_cluster[i]) != 0):										# If the cluster is not empty
				k_means.append(np.mean(X_train[idx_cluster[i], :], axis=0))		# Move the center to the mean of all vectors in the cluster
			else:																# Otherwise
				k_means.append(centroids[i, :])									# Keep center at its place
		if (np.sum(abs(centroids - k_means)) == 0):								# If there is no change in any of the centers
			break																# Stop moving the centers and exit loop
		centroids = np.asarray(k_means)											# Otherwise repeat with new set of centers

#	Compute Errors :
	MSE = []
	for i in range(k):
		MSE.append(np.nan_to_num(np.divide(np.sum(np.power(X_train[idx_cluster[i], :] - k_means[i], 2)), len(idx_cluster[i]))))
	Avg_MSE = np.divide(np.sum(MSE), np.count_nonzero(idx_cluster))
	Sq_Sep = 0
	for i in range(k):
		for j in range(i+1, k):
			Sq_Sep += np.sum(np.power(k_means[i] - k_means[j], 2))
	MSS = (2*Sq_Sep)/(k*(k-1))
	return np.asarray(k_means)[np.nonzero(idx_cluster)[0], :], MSE, Avg_MSE, MSS, np.asarray(idx_cluster)[np.nonzero(idx_cluster)[0]]

def get_best_clustering(k):
	Avg_MSE = float('Infinity')
	for i in range(5):				# Cluster the entire data 5 times
		model = find_K_means(k)				
		if model[2]<Avg_MSE:		# If the current Avg_MSE is less than previous then swap the clustering results		
			best_model = model 		# Replace best_model with new best_model 
			Avg_MSE = model[2] 		# Replace lest Avg_MSE with ne least Avg_MSE
	return best_model				# Return the clustering results which had least Avg_MSE

def assign_cluster_class(idx_cluster):
	cluster_class = []
	for i in range(len(idx_cluster)):						# Do for each cluster:
		count = np.zeros(10)
		for j in range(len(y_train[idx_cluster[i]])):		# Among all the vectors in that cluster
			count[int(y_train[idx_cluster[i]][j])] += 1		# Count the occurence of each class
		cluster_class.append(np.argmax(count))				# Find the most frequent class and assign it to that cluster
	return cluster_class

def visualize_cluster_centers(Clustering, cluster_class, directory):
	rootdir = os.getcwd()							# Get current working directory
	if not os.path.exists(directory):				
		os.makedirs(directory)						# Create a directory corresponding to value of k in current directory
	outdir = os.path.join(rootdir, directory)
	for i in range(len(Clustering)):				# Create a .pgm file for each cluster
		fout = open(os.path.join(outdir, '_'.join(['Cluster', str(i), 'Class', str(cluster_class[i]), '.pgm'])), 'w+')
		fout.write('P2\n8 8\n16\n')					# Write header in .pgm file
		for j in range(64):
			fout.write(str(math.floor(Clustering[i,j])) + ' ')		# Write image data in .pgm file

def K_means_Clustering(k):
	Clustering, MSE, Avg_MSE, MSS, idx_cluster = get_best_clustering(k)		# Get best clustering for given k
	print('\n Results for (K =', k,') :\n\n\tNo. of clusters = ', len(Clustering), '\n\tAverage Mean Square Error = ', Avg_MSE, '\n\tMean Square Seperation = ', MSS, '\n')
	cluster_class = assign_cluster_class(idx_cluster)			# Assign classes to each cluster center
	print('\tAssigned classes to clusters : ', cluster_class)
	test_set_clusters = find_nearest_centre(Clustering, X_test, len(cluster_class))		# Cluster test data by finding nearest center for each vector
	pred = np.zeros(y_test.shape[0])
	for i in range(len(test_set_clusters)):				# For each cluster
		pred[test_set_clusters[i]] = cluster_class[i]	# For vectors in a cluster, predicted class is the class assigned to the cluster
	print('\tClustering Accuracy = ', accuracy_score(y_test, pred), '\n\n Confusion Matrix :\n')
	print(confusion_matrix(y_test, pred), '\n')
	visualize_cluster_centers(Clustering, cluster_class, '-'.join(['K',str(k)]))

K_means_Clustering(k=10)	# Preform K-means clustering for k = 10
print('==================================================')
K_means_Clustering(k=30)	# Preform K-means clustering for k = 30
k =1
while(k):
	k = input("\tEnter number of initial random seeds\n\tOR\n\tEnter 0 to exit...")
	K_means_Clustering(k)
