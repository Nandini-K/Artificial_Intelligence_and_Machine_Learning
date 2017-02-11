##########################################################################

# Copyright (c) 2017 Nandini Khanwalkar 
# nandini2@pdx.edu

##########################################################################

import os
import random
import numpy as np
import sklearn
from sklearn.metrics import *
from sklearn.model_selection import train_test_split

in_size = 785
out_size = 10
train_set = 60000
test_set = 10000
n = [20, 50, 100]
alphas = [0, 0.25, 0.5]
eta = 0.1

##########################################################################

def sigmoid(z):
	return 1/(1 + np.exp(-z))

def dsigmoid(z):
	return z*(1-z)

def fwd_prop(data, wi2h, wh2o):
	ai = np.reshape(data, (1, in_size))
	ah = sigmoid(np.dot(ai, wi2h))
	ah[0][0] = 1
	ao = sigmoid(np.dot(ah, wh2o))
	return ai, ah, ao

def back_prop(error, ai, ah, ao, wh2o, wi2h, d_wh2o_prev, d_wi2h_prev, mv):
	delta_k = dsigmoid(ao)*error															
	delta_j = dsigmoid(ah)*np.dot(delta_k, np.transpose(wh2o))								
	d_wh2o_curr = (eta*np.dot(np.transpose(ah), delta_k)) + (mv*d_wh2o_prev)
	d_wi2h_curr = (eta*np.dot(np.transpose(ai), delta_j)) + (mv*d_wi2h_prev)
	wh2o += d_wh2o_curr
	wi2h += d_wi2h_curr
	return wh2o, wi2h, d_wh2o_curr, d_wi2h_curr

##########################################################################

def train_NN(wh2o, wi2h, d_wh2o_prev, d_wi2h_prev, mv):
	for i in range(0, train_set):
		ai, ah, ao = fwd_prop(train_data[i, :], wi2h, wh2o)																# Feed-forward an image sample to get output array
		t_k = np.insert((np.zeros((1, out_size-1)) + 0.0001), int(train_labels[i]), 0.9999)								# Compute array for target value 
		wh2o, wi2h, d_wh2o_prev, d_wi2h_prev = back_prop(t_k-ao, ai, ah, ao, wh2o, wi2h, d_wh2o_prev, d_wi2h_prev, mv) 	# Backpropagate the error to obtain updated weights
	return wi2h, wh2o

def test_NN(dataset, data_labels, set_size, wi2h, wh2o):
	pred = []
	for i in range(0, set_size):
		ai, ah, ao = fwd_prop(dataset[i, :], wi2h, wh2o)															# Feed-forward an image sample to get output array
		pred.append(np.argmax(ao))																					# Append the predicted output to pred list 
	return accuracy_score(data_labels, pred), pred

def Neural_Network(h_size, mv):
	
	# Randomize Weights :
	wi2h = (np.random.rand(in_size, h_size) - 0.5)*0.1
	wh2o = (np.random.rand(h_size, out_size) - 0.5)*0.1

	# Initialize delta_w_(t-1) arrays to 0 arrays :
	d_wh2o_prev = np.zeros(wh2o.shape)
	d_wi2h_prev = np.zeros(wi2h.shape)
	
	# Run Epochs :	
	for epoch in range(0, 50):
		train_accu, pred = test_NN(train_data, train_labels, train_set, wi2h, wh2o)									# Test network on training set and get accuracy and prediction
		test_accu, pred = test_NN(test_data, test_labels, test_set, wi2h, wh2o)										# Test network on test set and get accuracy and prediction
		print("Epoch " + str(epoch) + " :\tTraining Set Accuracy = " + str(train_accu) + "\n\t\tTest Set Accuracy = " + str(test_accu))
		wi2h, wh2o = train_NN(wh2o, wi2h, d_wh2o_prev, d_wi2h_prev, mv)												# Train network to compute new weights
	epoch += 1
	train_accu, pred = test_NN(train_data, train_labels, train_set, wi2h, wh2o)										# Test network on training set and get accuracy	and prediction
	test_accu, pred = test_NN(test_data, test_labels, test_set, wi2h, wh2o)											# Test network on test set and get accuracy and prediction
	print("Epoch " + str(epoch) + " :\tTraining Set Accuracy = " + str(train_accu) + "\n\t\tTest Set Accuracy = " + str(test_accu) + "\n\nHidden Layer Size = " + str(h_size) + "\tMomentum = " + str(mv) + "\tTraining Samples = " + str(train_set) + "\n\nConfusion Matrix :\n")
	print(confusion_matrix(test_labels, pred))
	print("\n")
	return

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

# Experiment 1 :
for h_size in n:
	Neural_Network(h_size, 0.9)																		# Varying the number of hidden units

# Experiment 2 :
for mv in alphas:
	Neural_Network(100, mv)																			# Varying the momentum

# Experiment 3 :
for i in range(0, 2):
	train_data, X, train_labels, Y = train_test_split(train_data, train_labels, test_size=0.50)		# Splitting data to experiment on half and quarter of the original dataset
	train_set = int(train_set/2)
	Neural_Network(100, 0.9)
