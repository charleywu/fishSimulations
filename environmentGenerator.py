# -*- coding: utf-8 -*-
"""
Dynamic environment generator
Charley Wu, July 2018
"""

#Creates a fitness landscape using a Gaussian process prior
import numpy as np
import gpflow
import json
from matplotlib import pyplot as plt
from sklearn import preprocessing
import tensorflow as tf
from memory_profiler import profile
plt.ioff()
from numpy.random import standard_normal
from scipy.linalg import cholesky


#environment shape
xmin=0
xmax=25
tmin=0
tmax = 25


#Create experiment data
for rbf_lambda in (2,4,8): #lengthscale of RBF kernel
	for w_sigma in (0.01, 0.05): #variance of white noise kernel
		for exp_factor in (1,2): #exponentiation of rewards
			#define kernels
			k1 = gpflow.kernels.RBF(input_dim=3) 
			k1.lengthscales = rbf_lambda
			k2 = gpflow.kernels.White(input_dim=3, variance=w_sigma)
			k = k1 + k2
			for i in range(10): #repetitions
				#Create grid using xmin and xmax
				xx, yy, zz = np.mgrid[xmin:xmax+1, xmin:xmax+1, tmin:tmax+1]
				X = np.vstack((xx.flatten(), yy.flatten(), zz.flatten())).T 
				cov = k.compute_K_symm(X) + np.eye(X.shape[0]) * 1e-06 #compute covariance matrix of x X with a small amount of added noise (nugget) to avoid matrix inversion issues
				#sample from multivariate normal
				l = cholesky(cov, check_finite=False, overwrite_a=True) #faster than  SVD
				s = np.zeros(X.shape[0]) + l.dot(standard_normal(X.shape[0]))
				#s = np.random.multivariate_normal(np.zeros(X.shape[0]), cov, 3) #Slower method that uses SVD
				#set min-max range for scaler
				min_max_scaler = preprocessing.MinMaxScaler(feature_range=(0,1))
				s = min_max_scaler.fit_transform(s.reshape(-1,1)).flatten()
				#exponentiate 
				s = pow(s,exp_factor)
				#convert into JSON object
				jsonData = {}
				counter = 0
				for x1 in range(xmin,xmax+1):
					for x2 in range(xmin,xmax+1):
						for x3 in range(tmin,tmax+1):
							jsonData[counter] = {'x1':x1, 'x2':x2,'x3':x3, 'y':s[counter]}
							counter+=1
				#save payout matrix
				filename = 'environments/lambda_%i_sigma_%.2f_exp_%i.%i.json' % (rbf_lambda, w_sigma, exp_factor, i)
				with open(filename, 'w') as fp:
					json.dump(jsonData, fp)

