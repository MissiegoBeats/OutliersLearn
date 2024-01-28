#TODO delete this file when everything works
muestra = t(matrix(c(3,2,3.5,12,4.7,4.1,5.2,4.9,7.1,6.1,6.2,5.2,14,5.3),2,7,dimnames=list(c("r","d"))))
muestra = data.frame(muestra)
print(muestra)
muestra <- transform_to_vector(muestra)
print(muestra)
#boxandwhiskers(muestra,2,TRUE)
boxandwhiskers(muestra,2,FALSE)
