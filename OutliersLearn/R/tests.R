#TODO delete this file when everything works
#Nota: para cuando quiera cambiar la documentación de las funciones roxygen2::roxygenize()

#print(inputData)
#dist(inputData)
#plot(inputData);
#inputData <- transform_to_vector(inputData)
#print(inputData)
#knn(inputData,3.5,3,TRUE);
#pca_method(inputData, TRUE);
inputData = t(matrix(c(3,2,3.5,12,4.7,4.1,5.2,4.9,7.1,6.1,6.2,5.2,14,5.3),2,7,dimnames=list(c("r","d"))));
inputData = data.frame(inputData);
z_score_method(inputData,2,FALSE);
