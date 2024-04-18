reconstruct_prcomp <- function(pca, comp = NULL){

  # Esta función reconstruye las mismas observaciones con las que se ha creado el
  # PCA empleando únicamente determinadas componentes principales.

  # Parameters
  # ----------
  # pca: "prcomp"
  #   objeto prcomp con los resultados del PCA.
  #
  # comp: "numeric"
  #   componentes principales empleadas en la reconstrucción.
  #
  # Return
  # ----------
  # "matrix" con la reconstrucción de cada observación. Las dimensiones de la
  # matriz son las mismas que las de la matriz o dataframe con el que se estrenó
  # el objeto pca.

  # Se comprueba que el objeto PCA es de clase "prcomp"
  if (class(pca) != "prcomp") {
    stop("El objeto PCA debe de haber sido creado con la función prcomp()")
  }

  # Si no se especifica comp, se emplean todas las componentes.
  if (is.null(comp)) {
    comp <- seq_along(pca$sdev)
  }

  # Reconstrucción
  recon <- as.matrix(pca$x[, comp]) %*% t(as.matrix(pca$rotation[, comp]))

  # Si se ha aplicado centrado o escalado se revierte la trasformación.
  if (pca$scale[1] != FALSE) {
    recon <- scale(recon , center = FALSE, scale = 1/pca$scale)
  }
  if (pca$center[1] != FALSE) {
    recon <- scale(recon , center = -1*pca$center, scale = FALSE)
  }

  return(recon)
}
