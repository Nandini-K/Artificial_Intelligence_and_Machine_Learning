##########################################################################

# Copyright (c) 2017 Nandini Khanwalkar 
# nandini2@pdx.edu

##########################################################################

import os
import random
import numpy as np
import sklearn
from sklearn.utils import shuffle
from sklearn.svm import *
from sklearn.metrics import *
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import *
import matplotlib.pyplot as plt

# Data Processing :

data_file = shuffle(np.loadtxt('spambase.data.csv', delimiter=','))
X_train, X_test, y_train, y_test = train_test_split(data_file[:, np.arange(57)], data_file[:, 57], test_size=0.50)
scaler = StandardScaler().fit(X_train)
X_train = scaler.transform(X_train)
X_test = scaler.transform(X_test)
model = SVC(kernel='linear').fit(X_train, y_train)

###########################################################################

# Experiment 1 :

y_pred = model.predict(X_test)
print('\nExperiment 1 :\n\tAccuracy\t= '+str(accuracy_score(y_test, y_pred))+'\n\tPrecision\t= '+str(precision_score(y_test, y_pred)) +'\n\tRecall\t\t= '+str(recall_score(y_test, y_pred)))
y_score = model.decision_function(X_test)
fpr, tpr, thresholds = roc_curve(y_test, y_score)
auc = roc_auc_score(y_test, y_score)
plt.figure(figsize=(8, 7.5), dpi=100)
plt.plot(fpr, tpr, color='blue', label='ROC Curve\n(area under curve = %f)' %auc,  lw=2)
plt.plot([0, 1], [0, 1], color='red', lw=2, linestyle='--')
plt.xlabel('\nFalse Positive Rate\n', size=18)
plt.ylabel('\nTrue Positive Rate\n', size=18)
plt.title('Spambase Database Classified With Linear SVM\n', size=20)
plt.legend(loc='lower right')
plt.show()

###########################################################################

# Experiment 2 :

print('\nExperiment 2 :')
w = np.copy(model.coef_)
i = np.argmax(w)
X_train_m = X_train[:, i].reshape(X_train.shape[0], 1)
X_test_m = X_test[:, i].reshape(X_test.shape[0], 1)
w[0][i] = float('-Infinity')
for m in range(2, 58):
	i = np.argmax(w)
	w[0][i] = float('-Infinity')
	X_train_m = np.insert(X_train_m, 0, X_train[:, i], axis=1)
	X_test_m = np.insert(X_test_m, 0, X_test[:, i], axis=1)
	print('\tm = ' + str(m) + ',\t\taccuracy = ' + str(accuracy_score(y_test, model.fit(X_train_m, y_train).predict(X_test_m))))

###########################################################################

# Experiment 3 :

print('\nExperiment 3 :')
for m in range(2,58):
	random_indices = np.random.choice(np.arange(57), m, replace=0)
	X_train_m = X_train[:, random_indices]
	X_test_m = X_test[:, random_indices]
	print('\tm = ' + str(m) + ',\t\taccuracy = ' + str(accuracy_score(y_test, model.fit(X_train_m, y_train).predict(X_test_m))))

###########################################################################