pca_method <- function(data, tutorialMode){
  library(dplyr)
  if(tutorialMode){
    #PCA calculation on the dataset
    pca <- prcomp(
      x = data %>% select(-y),
      center = TRUE,
      scale. = TRUE
    )
    get_eig(X = pca)
    fviz_eig(
      X         = pca,
      choice    = "variance",
      addlabels = TRUE,
      ncp       = 21,
      ylim      = c(0, 100),
      main      = "Porcentaje de varianza explicada por componente",
      xlab      = "Componente",
      ylab      = "Porcentaje de varianza explicada"
    )
    ggplot(data = get_eig(X = pca),
           aes(x = as.factor(1:nrow(get_eig(X = pca))),
               y = cumulative.variance.percent,
               group = 1)
    ) +
      geom_col(fill = "steelblue", color = "steelblue") +
      geom_line(color = "black", linetype = "solid") +
      geom_point(shape = 19, color = "black") +
      geom_text(aes(label = paste0(round(cumulative.variance.percent, 1), "%")),
                size = 3, vjust = -0.5, hjust = 0.7) +
      geom_hline(yintercept = 90, color = "firebrick", linetype = "dashed") +
      labs(title = "Porcentaje de varianza acumulada PCA",
           x = "componente",
           y = "Porcentaje de varianza acumulada") +
      theme_bw()
    # Contribución a la primera componente.
    p1 <- fviz_contrib(X = pca, choice = "var", axes = 1, top = 10)
    # Contribución a la segunda componente.
    p2 <- fviz_contrib(X = pca, choice = "var", axes = 2, top = 10)
    # Contribución conjunta a las 3 primeras componentes.
    p3 <- fviz_contrib(X = pca, choice = "var", axes = 1:3, top = 10)
    ggpubr::ggarrange(p1, p2, p3, nrow = 1)

    # Se aplica la reconstrucción
    reconstruccion <- reconstruct_prcomp(pca = pca, comp = NULL)

    # Se comparan las 2 primeras variables, originales y reconstruidas, de la primera
    # observación del set de data.
    print(as.numeric(reconstruccion[1, 1:2]), digits = 22)

    print(as.numeric(df_cardio[1, 1:2]), digits = 22)

    # Reconstrucción empleando las 11 primeras componentes (90% de la varianza explicada)
    reconstruccion <- reconstruct_prcomp(pca = pca, comp = 1:11)

    # Cálculo del error cuadrático medio de recostrucción
    error_reconstruccion <- reconstruccion - select(data, -y)
    error_reconstruccion <- error_reconstruccion^2
    error_reconstruccion <- apply(X = error_reconstruccion, MARGIN = 1, FUN = mean)

    # Distribución del error de reconstrucción
    tibble(error_reconstruccion) %>%
      ggplot(aes(x = error_reconstruccion)) +
      geom_density(fill = "steelblue") +
      labs(title = "Distribución de los errores de reconstrucción (PCA)",
           x = "Error de reconstrucción") +
      theme_bw()

    # Se añade el error de reconstrucción al dataframe original.
    data$error_reconstruccion <- error_reconstruccion

    ggplot(data = data,
           aes(x = y, y = log2(error_reconstruccion))) +
      geom_jitter(aes(color = y), width = 0.03, alpha = 0.3) +
      geom_violin(alpha = 0) +
      geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0) +
      stat_summary(fun = "mean", colour = "orangered2", size = 3, geom = "point") +
      labs(title = "Error de reconstruccion PCA",
           x = "clasificación (0 = normal, 1 = anomalía)",
           y = "log2(error reconstrucción)") +
      theme_bw() +
      theme(legend.position = "none")

    resultados <- data %>%
      select(y, error_reconstruccion) %>%
      arrange(desc(error_reconstruccion)) %>%
      mutate(clasificacion = if_else(row_number() <= 176, "1", "0"))

    mat_confusion <- MLmetrics::ConfusionMatrix(
      y_pred = resultados$clasificacion,
      y_true = resultados$y
    )
    mat_confusion

    # Reconstrucción empleando todas excepto las 4 primeras componentes
    reconstruccion <- reconstruct_prcomp(pca = pca, comp = 5:21)

    # Cálculo del error cuadrático medio de reconstrucción
    error_reconstruccion <- reconstruccion - select(data, -y)
    error_reconstruccion <- error_reconstruccion^2
    error_reconstruccion <- apply(X = error_reconstruccion, MARGIN = 1, FUN = mean)

    # Se añade el error de reconstrucción al dataframe original.
    data$error_reconstruccion <- error_reconstruccion

    ggplot(data = data,
           aes(x = y, y = log2(error_reconstruccion))) +
      geom_jitter(aes(color = y), width = 0.03, alpha = 0.3) +
      geom_violin(alpha = 0) +
      geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0) +
      stat_summary(fun = "mean", colour = "orangered2", size = 3, geom = "point") +
      labs(title = "Error de reconstruccion PCA",
           x = "clasificación (0 = normal, 1 = anomalía)",
           y = "log2(error reconstrucción)") +
      theme_bw() +
      theme(legend.position = "none")

    resultados <- data %>%
      select(y, error_reconstruccion) %>%
      arrange(desc(error_reconstruccion)) %>%
      mutate(clasificacion = if_else(row_number() <= 176, "1", "0"))

    mat_confusion <- MLmetrics::ConfusionMatrix(
      y_pred = resultados$clasificacion,
      y_true = resultados$y
    )
    mat_confusion
  }else{
    #PCA calculation on the dataset
    pca <- prcomp(
      x = data %>% select(-y),
      center = TRUE,
      scale. = TRUE
    )
    get_eig(X = pca)
    fviz_eig(
      X         = pca,
      choice    = "variance",
      addlabels = TRUE,
      ncp       = 21,
      ylim      = c(0, 100),
      main      = "Porcentaje de varianza explicada por componente",
      xlab      = "Componente",
      ylab      = "Porcentaje de varianza explicada"
    )
    ggplot(data = get_eig(X = pca),
           aes(x = as.factor(1:nrow(get_eig(X = pca))),
               y = cumulative.variance.percent,
               group = 1)
    ) +
      geom_col(fill = "steelblue", color = "steelblue") +
      geom_line(color = "black", linetype = "solid") +
      geom_point(shape = 19, color = "black") +
      geom_text(aes(label = paste0(round(cumulative.variance.percent, 1), "%")),
                size = 3, vjust = -0.5, hjust = 0.7) +
      geom_hline(yintercept = 90, color = "firebrick", linetype = "dashed") +
      labs(title = "Porcentaje de varianza acumulada PCA",
           x = "componente",
           y = "Porcentaje de varianza acumulada") +
      theme_bw()
    # Contribución a la primera componente.
    p1 <- fviz_contrib(X = pca, choice = "var", axes = 1, top = 10)
    # Contribución a la segunda componente.
    p2 <- fviz_contrib(X = pca, choice = "var", axes = 2, top = 10)
    # Contribución conjunta a las 3 primeras componentes.
    p3 <- fviz_contrib(X = pca, choice = "var", axes = 1:3, top = 10)
    ggpubr::ggarrange(p1, p2, p3, nrow = 1)

    # Se aplica la reconstrucción
    reconstruccion <- reconstruct_prcomp(pca = pca, comp = NULL)

    # Se comparan las 2 primeras variables, originales y reconstruidas, de la primera
    # observación del set de data.
    print(as.numeric(reconstruccion[1, 1:2]), digits = 22)

    print(as.numeric(df_cardio[1, 1:2]), digits = 22)

    # Reconstrucción empleando las 11 primeras componentes (90% de la varianza explicada)
    reconstruccion <- reconstruct_prcomp(pca = pca, comp = 1:11)

    # Cálculo del error cuadrático medio de recostrucción
    error_reconstruccion <- reconstruccion - select(data, -y)
    error_reconstruccion <- error_reconstruccion^2
    error_reconstruccion <- apply(X = error_reconstruccion, MARGIN = 1, FUN = mean)

    # Distribución del error de reconstrucción
    tibble(error_reconstruccion) %>%
      ggplot(aes(x = error_reconstruccion)) +
      geom_density(fill = "steelblue") +
      labs(title = "Distribución de los errores de reconstrucción (PCA)",
           x = "Error de reconstrucción") +
      theme_bw()

    # Se añade el error de reconstrucción al dataframe original.
    data$error_reconstruccion <- error_reconstruccion

    ggplot(data = data,
           aes(x = y, y = log2(error_reconstruccion))) +
      geom_jitter(aes(color = y), width = 0.03, alpha = 0.3) +
      geom_violin(alpha = 0) +
      geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0) +
      stat_summary(fun = "mean", colour = "orangered2", size = 3, geom = "point") +
      labs(title = "Error de reconstruccion PCA",
           x = "clasificación (0 = normal, 1 = anomalía)",
           y = "log2(error reconstrucción)") +
      theme_bw() +
      theme(legend.position = "none")

    resultados <- data %>%
      select(y, error_reconstruccion) %>%
      arrange(desc(error_reconstruccion)) %>%
      mutate(clasificacion = if_else(row_number() <= 176, "1", "0"))

    mat_confusion <- MLmetrics::ConfusionMatrix(
      y_pred = resultados$clasificacion,
      y_true = resultados$y
    )
    mat_confusion

    # Reconstrucción empleando todas excepto las 4 primeras componentes
    reconstruccion <- reconstruct_prcomp(pca = pca, comp = 5:21)

    # Cálculo del error cuadrático medio de reconstrucción
    error_reconstruccion <- reconstruccion - select(data, -y)
    error_reconstruccion <- error_reconstruccion^2
    error_reconstruccion <- apply(X = error_reconstruccion, MARGIN = 1, FUN = mean)

    # Se añade el error de reconstrucción al dataframe original.
    data$error_reconstruccion <- error_reconstruccion

    ggplot(data = data,
           aes(x = y, y = log2(error_reconstruccion))) +
      geom_jitter(aes(color = y), width = 0.03, alpha = 0.3) +
      geom_violin(alpha = 0) +
      geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0) +
      stat_summary(fun = "mean", colour = "orangered2", size = 3, geom = "point") +
      labs(title = "Error de reconstruccion PCA",
           x = "clasificación (0 = normal, 1 = anomalía)",
           y = "log2(error reconstrucción)") +
      theme_bw() +
      theme(legend.position = "none")

    resultados <- data %>%
      select(y, error_reconstruccion) %>%
      arrange(desc(error_reconstruccion)) %>%
      mutate(clasificacion = if_else(row_number() <= 176, "1", "0"))

    mat_confusion <- MLmetrics::ConfusionMatrix(
      y_pred = resultados$clasificacion,
      y_true = resultados$y
    )
    mat_confusion
  }
}
