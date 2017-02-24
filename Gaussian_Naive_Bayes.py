##########################################################################

# Copyright (c) 2017 Nandini Khanwalkar 
# nandini2@pdx.edu

##########################################################################

import os
import math
import numpy as np
import sklearn
from sklearn.utils import shuffle
from sklearn.metrics import *
from sklearn.linear_model import *
from sklearn.model_selection import train_test_split

# Data Processing :

data_file = shuffle(np.loadtxt('spambase.data.csv', delimiter=','))														# Load data and shuffle 
X_train, X_test, y_train, y_test = train_test_split(data_file[:, np.arange(57)], data_file[:, 57], test_size=0.50)	# Split data in train & test sets
indices = [np.nonzero(y_train==0)[0], np.nonzero(y_train)[0]]								# Group the indices of samples by class
mean = np.transpose([np.mean(X_train[indices[0], :], axis=0), np.mean(X_train[indices[1], :], axis=0)])			# Compute mean for each feature w.r.t. class
std = np.transpose([np.std(X_train[indices[0], :], axis=0), np.std(X_train[indices[1], :], axis=0)])			# Compute S.D. for each feature w.r.t. class

###########################################################################

# Gaussian Naive Bayes :

logP_class = np.log([1-np.mean(y_train), np.mean(y_train)])	# Compute prior probabilities for both classes and take their log.

zero_std = np.nonzero(std==0)[0]				# Find the feature indices which have zero standard deviation
if (np.any(zero_std)):						# If any such indices exist
	np.place(std, std==0, 0.0001)				# Replace the zero standard deviation value with a small number OR
#	mean = np.delete(mean, zero_std, axis=0)		# {	Remove those features' value of mean,
#	std = np.delete(std, zero_std, axis=0)			# 	and standard deviation
#	X_train = np.delete(X_train, zero_std, axis=1)		# 	Remove those features from the training samples
#	X_test = np.delete(X_test, zero_std, axis=1)		# 	and testing samples	}

pred = []
for i in range(0, X_test.shape[0]):
	# Compute independent probabilities of features w.r.t. class
	P_X = np.divide(np.exp(-1*(np.divide(np.power(np.subtract(X_test[i, :].reshape(X_test.shape[1], 1), mean), 2), 2*np.power(std, 2)))), math.sqrt(2*np.pi)*std)
	# Compute class prediction for the test sample
	Class_X = np.argmax(logP_class+np.sum(np.nan_to_num(np.log(P_X)), axis=0))	
	pred.append(Class_X)

print('\nGaussian Naive Bayes :\n\tAccuracy\t= '+str(accuracy_score(y_test, pred))+'\n\tPrecision\t= '+str(precision_score(y_test, pred)) +'\n\tRecall\t\t= '+str(recall_score(y_test, pred))+'\n\nConfusion Matrix :')
print(confusion_matrix(y_test, pred))

###########################################################################
