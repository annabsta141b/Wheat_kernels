# Wheat Kernels Modeling

The data for this report comes from https://www.kaggle.com/dongeorge/seed-from-uci

Investigated the significance of geometrical properties of kernels in determining the type of wheat kernels.
Initially used a baseline odds multinomial regression model but found a lack of fit. Corrected by fitting a conditional logistic regression model. Found that given the area of the kernel is larger than 14.3, kernels that have irregular shapes are more likely to be Rosa kernels.
