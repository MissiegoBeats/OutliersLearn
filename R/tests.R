#TODO delete this file when everything works
#Nota: para cuando quiera cambiar la documentación de las funciones roxygen2::roxygenize()

#This is the mahalanobis example execution added to the annex:
inputData = t(matrix(c(3,2,3.5,12,4.7,4.1,5.2,4.9,7.1,6.1,6.2,5.2,14,5.3),2,7,dimnames=list(c("r","d"))));
inputData = as.matrix(inputData);

inputData = c(1,2,3,4,5,6,1);
mean = mean_outliersLearn(inputData);
sd = sd_outliersLearn(inputData, mean);
print(sd);
#lof(inputData, 3, 0.4, TRUE);
#knn(inputData, 3, 2, TRUE);
