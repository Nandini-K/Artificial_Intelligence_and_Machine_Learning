##########################################################################

# Copyright (c) 2017 Nandini Khanwalkar 
# nandini2@pdx.edu

##########################################################################

import os
import random
import numpy as np
import sklearn
from sklearn.metrics import *

in_size = 785
out_size = 10
train_set = 60000
test_set = 10000
max_epochs = 70
l_rates = [0.1, 0.01, 0.001]

##########################################################################

def fwd_prop(data):		
	ao = np.dot(np.reshape(data, (1, in_size)), wi2o)									# Compute output array
	return ao

def evaluate(ao, y_t):
	ym = np.insert(np.zeros((1, out_size-1)), np.argmax(ao), 1)							# Compute array representation of predicted output
	t_k = np.insert(np.zeros((1, out_size-1)), y_t, 1)									# Compute array for target value 
	return t_k-ym

def back_prop(error, data, wi2o):
	delta = np.dot(np.reshape(data, (in_size, 1)), np.reshape(error, (1, out_size)))
	wi2o += (eta * delta)																# Compute new set of weights
	return wi2o
	
##########################################################################

def train_PN(wi2o):
	for i in range(0, train_set):
		ao = fwd_prop(train_data[i, :])							# Feed-forward an image sample to get output array
		error = evaluate(ao, int(train_labels[i]))				# Evaluate to find array representation of predicted output				
		wi2o = back_prop(error, train_data[i, :], wi2o)			# Back propagate error through the network to get adjusted weights
	return wi2o

def test_PN(dataset, data_labels, set_size):
	pred = []
	for i in range(0, set_size):
		ao = fwd_prop(dataset[i, :])							# Feed-forward an image sample to get output array
		pred.append(np.argmax(ao))								# Append the predicted output to pred list 
	return accuracy_score(data_labels, pred), pred

##########################################################################

def load_data(file_name):
	data_file = np.loadtxt(file_name, delimiter=',')
	dataset = np.insert(data_file[:, np.arange(1, in_size)]/255, 0, 1, axis=1)
	data_labels = data_file[:, 0]
	return dataset, data_labels	

####################################################################################################

# Load Training and Test Sets :
print("\nLoading Training Set")
train_data, train_labels = load_data('mnist_train.csv')
print("\nLoading Test Set\n")
test_data, test_labels = load_data('mnist_test.csv')

for eta in l_rates:
	
# Randomize Weights :
	#wi2o = (np.random.rand(in_size, out_size) - 0.5)*(0.5)
	wi2o = (np.random.rand(in_size, out_size) - 0.5)*(0.1)									# Generate weight matrix with random weights 
	#wi2o = (np.random.rand(in_size, out_size) - 0.5)*(1.5)									
	#wi2o = (np.random.rand(in_size, out_size) - 0.5)*(2)								

# Run Epochs :
	prev_accu = 1
	epoch = 0
	while (1):
		curr_accu, pred = test_PN(train_data, train_labels, train_set)						# Test network on training set and get training accuracy
		print("Epoch " + str(epoch) + " :\tTraining Set Accuracy = " + str(curr_accu))
		if abs(prev_accu - curr_accu)<0.0001 or epoch==max_epochs:
			break																			# If network is converged, stop training
		test_accu, pred = test_PN(test_data, test_labels, test_set)							# Test network on test set and get accuracy on test set
		print("\t\tTest Set Accuracy = " + str(test_accu))
		prev_accu = curr_accu
		epoch+=1
		wi2o = train_PN(wi2o)																# Train the network
		
# Test Network again :
	test_accu, pred = test_PN(test_data, test_labels, test_set)								# Test network on test set and get test accuracy
	print("\t\tTest Set Accuracy = " + str(test_accu) + "\n\nLearning Rate = " + str(eta) + "\n\nConfusion Matrix :\n")
	print(confusion_matrix(test_labels, pred))
	print("\n")