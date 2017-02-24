# Artificial Intelligence and Machine Learning

This repository contains code for implementing various AI and ML algorithms along with the required datasets. A brief description is given below:-

## Gaussian_Naive_Bayes.py :
Implementation of Bayesian Learning for classification of spam and non-spam emails using UCI's Spambase database (see the spambase.data.csv). To execute the code save the .csv file and the .py file in your current directory and run it with python. Make sure your python has Numpy and Sklearn packages installed.

## SVM_Feature_Selection_SkLearn.py :
A short program to study the effects of random and weighted feature selection on the accuracy of SVM. Training and testing performed on UCI's Spambase database (see the spambase.data.csv) using Scikit Learn Library functions. To execute the code save the .csv file and the .py file in your current directory and run it with python. Make sure your python has Numpy and Sklearn packages installed.

## Neural_Network.py : 
Implementation of Neural Network from the scratch for handwritten digit recognition. The database used is mnist database which is provided in .csv format in mnist_train_&_test.zip. The program experiments with varying number of hidden units, varying momentum values, and variations in the size of training set to observe changes in the network performance. To execute the code save both the .csv files and the .py file in your current directory and run it with python. Make sure your python has Numpy and Sklearn packages installed.

## Perceptron_Learning.py :
Implementation of Perceptron Learning Algorithm from the scratch for handwritten digit recognition. The database used is mnist database which is provided in .csv format in mnist_train_&_test.zip. The program experiments with different sets of initial random weights and varying learning rates to observe the changes in the network performance. To execute the code save both the .csv files and the .py file in your current directory and run it with python. Make sure your python has Numpy and Sklearn packages installed.

## Genetic_Algorithm_TSP.lisp :
Implementation of Genetic Algorithm to find a near-optimum solution for the Travelling Salesman Problem. A sample map (Genetic_Algorithm_TSP_map) is provided which can be used with the code. To execute the code save the .lisp file and the map in your current directory and run it with clisp. Enter the name of the input file when prompted by the program. If you use an input file other than the one given here, please make sure it has the following format:
n               (number of cities) \n
1 x1  y1    (city number and coordinates) \n
2 x2  y2 \n
.........
n xn  yn \n
source               (start city number) 

## Tower_of_Hanoi_Iterative.lisp / Tower_of_Hanoi_Recursive.lisp :
Solves the Tower of Hanoi puzzle in iterative and recursive manner respectively using State-Space Search. Execute the code with clisp, enter the desired height of the tower when prompted by the program.
