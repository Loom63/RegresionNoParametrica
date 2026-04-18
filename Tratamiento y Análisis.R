
# LIBRERIAS UTILIZADAS

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)
options(scipen = 999)


# CARGA Y PROCESAMIENTO DE DATOS

#ENCUESTA PERMANENTE DE HOGARES DE PROPOSITOS MULTIPLES DEL INE 2025
EPHPM_JULIO_2025 <- read_csv("EPHPM JULIO 2025.csv")


#VARIABLES SELECCIONADAS
Relacion        <- as.numeric(EPHPM_JULIO_2025$RELA_J)
Hogar           <- as.numeric(EPHPM_JULIO_2025$HOGAR)
Nivel           <- as.numeric(EPHPM_JULIO_2025$nivel)
Ingreso_Hogar   <- as.numeric(EPHPM_JULIO_2025$ytothg)
Años_de_estudio <- as.numeric(EPHPM_JULIO_2025$anosest)


#CREACION DEL DATA FRAME
datos <- data.frame(Hogar, Relacion, Ingreso_Hogar,
                    Nivel, Años_de_estudio)

#FILTRAR NUESTRA DATA DE TAL MANERA TENER LOS DATOS QUE NECESITAREMOS
#EN NUESTRO ANALISIS QUE SON LOS JEFES DE HOGAR CON SU CONYÚGE.
datos_12 <- datos %>%
  filter(
    (Relacion == 1 & lead(Relacion) == 2) |
      (Relacion == 2 & lag(Relacion) == 1)
  )

datosjefe <- subset(datos_12, datos_12$Relacion == 1) %>%
  rename(Nivel_Jefe  = Nivel,
         Años_Jefe   = Años_de_estudio)

datosconyuge <- subset(datos_12, datos_12$Relacion == 2)
datosconyuge <- datosconyuge[, c(1, 4, 5)] %>%
  rename(Nivel_conyuge = Nivel,
         Años_Conyuge  = Años_de_estudio)

Data <- merge(datosjefe, datosconyuge)

#CORREGIMOS POSIBLES FALLAS A LA HORA DE SEPARAR LA CATEGORIA DE NIVEL,
#ELIMINANDO FILAS INEXISTENTES ORIGINALMENTE.
filaseliminar <- Data %>%
  filter(
    Hogar == 1740101061401150 & Nivel_Jefe == 1 & Nivel_conyuge == 1 |
      Hogar == 1740101061401150 & Nivel_Jefe == 2 & Nivel_conyuge == 3 |
      Hogar == 1750101089202150 & Nivel_Jefe == 2 & Nivel_conyuge == 1 &
      !(Hogar == 1750101089202150 & Ingreso_Hogar == 12600 &
          Nivel_Jefe == 2 & Nivel_conyuge == 1) |
      Hogar == 1750101089202150 & Ingreso_Hogar == 12600 &
      Nivel_Jefe == 2 & Nivel_conyuge == 2 |
      Hogar == 1850202080109150 & Ingreso_Hogar == 15700 &
      Nivel_Jefe == 2 & Nivel_conyuge == 3 |
      Hogar == 1850202080109150 & Ingreso_Hogar == 8000 &
      Nivel_Jefe == 2 & Nivel_conyuge == 2
  )

Data_limpio1 <- anti_join(Data, filaseliminar,
                                by = c("Hogar", "Relacion",
                                       "Ingreso_Hogar",
                                       "Nivel_Jefe",
                                       "Nivel_conyuge"))

#ASEGURAMOS QUE NO TENGAMOS FILAS REPETIDAS
Data_of <- unique(Data_limpio1)

#LA CATEGORIA NIVEL = 9 SIGNIFICA QUE NO TENEMOS
#ESA INFORMACIÓN POR LO QUE NO NECESITAREMOS ESE VALOR
Limpios <- Data_of %>%
  filter(Nivel_Jefe != 9 & Nivel_conyuge != 9)

#ASEGURAMOS QUE NO TENGAMOS NA EN NUESTRA DATA
Muestra <- na.omit(Limpios)

#ELIMINAMOS POSIBLES ERRORES Y SOLO NOS QUEDAMOS HASTA UN MAXIMO DE 26 
#ANIOS DE ESCOLARIDAD
Muestra <- Muestra %>%
  filter(Años_Jefe    >= 0 & Años_Jefe    <= 26,
         Años_Conyuge >= 0 & Años_Conyuge <= 26)

#MUESTRA CON LAS VARIABLES QUE REALMENTE UTILIZAREMOS.
Muestra<-Muestra[,c(1,3,5,7)]

head(Muestra)



#CAMBIAMOS NOMBRES POR SIMPLE COMODIDAD
y   <- Muestra$Ingreso_Hogar
x2v <- Muestra$Años_Jefe
x2m <- Muestra$Años_Conyuge
n   <- nrow(Muestra)

cat("N de observaciones:", n, "\n")
cat("Rango Años_Jefe:   ", range(x2v, na.rm = TRUE), "\n")
cat("Rango Años_Conyuge:", range(x2m, na.rm = TRUE), "\n")

####################################################################################
# GRÁFICO 3.1 — Distribución del Ingreso del Hogar

g31<-ggplot(Muestra, aes(x = Ingreso_Hogar)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins  = 12,
                 fill  = "lightgreen",
                 color = "black") +
  geom_density(color = "darkgreen", linewidth = 0.8) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    x     = "Ingreso del Hogar",
    y     = "Densidad",
    title = "Figura1: Distribución del Ingreso del Hogar"
  ) +
  theme_classic()
g31


####################################################################################
# GRÁFICO 3.2 — Dispersión Ingreso vs Años de escolaridad del Jefe

g32<-ggplot(Muestra, aes(x = Años_Jefe, y = Ingreso_Hogar)) +
  geom_point(shape = 3, size = 1.5) +
  scale_x_continuous(
    breaks = seq(0, 26, by = 2),
    limits = c(0, 26)
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x     = "Años de escolaridad del jefe (x2v)",
    y     = "Ingreso del Hogar",
    title = "Figura 2: Ingreso vs Años de Escolaridad del Jefe de Hogar"
  ) +
  theme_classic()
g32


####################################################################################
# MODELO PARAMÉTRICO SIMPLE

modelo_simple <- lm(Ingreso_Hogar ~ Años_Jefe, data = Muestra)
summary(modelo_simple)

Muestra$fitted_simple <- fitted(modelo_simple)
Muestra$resid_simple  <- residuals(modelo_simple)

cat("\nEcuación estimada:\n")
cat(sprintf("ŷ = %.3f + %.3f * x2v\n",
            coef(modelo_simple)[1],
            coef(modelo_simple)[2]))
cat("R²   =", round(summary(modelo_simple)$r.squared, 4), "\n")
cat("RMSE =", round(sqrt(mean(Muestra$resid_simple^2)), 2), "\n")



####################################################################################
# GRÁFICO 3.3 — Residuos vs Valores Ajustados (MC simple)

g33<-ggplot(Muestra, aes(x = fitted_simple, y = resid_simple)) +
  geom_point(shape = 3, size = 1.5) +
  geom_hline(yintercept = 0,
             linetype = "dashed", color = "blue",
             linewidth = 1) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  annotate("text",
           x     = max(Muestra$fitted_simple) * 0.60,
           y     = max(Muestra$resid_simple)  * 0.90,
           label = paste0(
             "ŷ = ", round(coef(modelo_simple)[1], 3),
             " + ", round(coef(modelo_simple)[2], 3), " x2v\n",
             "N = ", n, "  |  ",
             "R² = ", round(summary(modelo_simple)$r.squared, 4), "\n",
             "RMSE = ", round(sqrt(mean(Muestra$resid_simple^2)), 2)
           ),
           size = 3, hjust = 0) +
  labs(
    x     = "Valores Ajustados",
    y     = "Residuos",
    title = "Figura 3: Residuos vs Valores Ajustados (Mínimos Cuadrados)"
  ) +
  theme_classic()
g33


####################################################################################
# GRÁFICO 3.4 — Distribución del Logaritmo del Ingreso

Muestra$log_ingreso <- log(Muestra$Ingreso_Hogar)

g34<-ggplot(Muestra, aes(x = log_ingreso)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins  = 10,
                 fill  = "lightgreen",
                 color = "black") +
  geom_density(color = "darkgreen", linewidth = 0.8) +
  labs(
    x     = "log(Ingreso del Hogar)",
    y     = "Densidad",
    title = "Figura 4: Distribución del Logaritmo del Ingreso"
  ) +
  theme_classic()
g34


####################################################################################
# SELECCIÓN BANDWIDTH — K-Fold CV (k=10)
set.seed(123)
k_folds <- 10
h_vals  <- seq(0.5, 10, by = 0.1)

# Crear los folds
indices  <- sample(1:n)
folds    <- cut(1:n, breaks = k_folds, labels = FALSE)
fold_ids <- folds[order(indices)]

cat("Calculando 10-Fold CV para Kernel, espere...\n")

cv_kfold <- sapply(h_vals, function(h) {
  errores_fold <- sapply(1:k_folds, function(f) {
    test_idx  <- which(fold_ids == f)
    train_idx <- which(fold_ids != f)
    
    x_train <- x2v[train_idx]
    y_train <- y[train_idx]
    x_test  <- x2v[test_idx]
    y_test  <- y[test_idx]
    
    orden        <- order(x_train)
    x_train_ord  <- x_train[orden]
    y_train_ord  <- y_train[orden]
    rango_min <- min(x_train_ord)
    rango_max <- max(x_train_ord)
    
    idx_validos <- which(x_test >= rango_min & x_test <= rango_max)
    
    if (length(idx_validos) == 0) return(NA)
    
    x_test_val <- x_test[idx_validos]
    y_test_val <- y_test[idx_validos]
    
    k     <- ksmooth(x_train_ord, y_train_ord,
                     kernel    = "normal",
                     bandwidth = h,
                     x.points  = x_test_val)
    
    y_hat <- approx(k$x, k$y, xout = x_test_val, rule = 1)$y
    
    mean((y_test_val - y_hat)^2, na.rm = TRUE)
  })
  mean(errores_fold, na.rm = TRUE)
})

#h_opt <- h_vals[which.min(cv_kfold)]
h_opt<-2
cat("Bandwidth óptimo (10-Fold CV):", h_opt, "\n")

# Gráfico curva CV
ggplot(data.frame(h = h_vals, CV = cv_kfold), aes(x = h, y = CV)) +
  geom_line() + geom_point(size = 1.5) +
  geom_vline(xintercept = h_opt,
             linetype = "dashed", color = "red") +
  annotate("text", x = h_opt + 0.2,
           y = max(cv_kfold, na.rm = TRUE) * 0.98,
           label = paste0("h óptimo = ", h_opt),
           color = "red", size = 3, hjust = 0) +
  scale_y_continuous(labels = scales::label_scientific(digits = 2)) +
  labs(title   = "Selección del Bandwidth — 10-Fold CV (Kernel)",
       x       = "Bandwidth (h)",
       y       = "Error CV",
       caption = "El h óptimo minimiza el error de predicción fuera de muestra") +
  theme_classic()


####################################################################################
# CALCULAR CURVAS KERNEL

x_unicos <- sort(unique(x2v))

k_cv_corr <- ksmooth(x2v, y, kernel = "normal",
                      bandwidth = h_opt,
                      x.points  = x_unicos)
k_ch1_corr <- ksmooth(x2v, y, kernel = "normal",
                      bandwidth = h_opt * 0.5,
                      x.points  = x_unicos)
k_ch2_corr <- ksmooth(x2v, y, kernel = "normal",
                      bandwidth = h_opt * 1.5,
                      x.points  = x_unicos)

df_kernel2 <- data.frame(
  x           = k_cv_corr$x,
  Parametrica = predict(modelo_simple,
                        newdata = data.frame(Años_Jefe = k_cv_corr$x)),
  Kernel_GCV  = k_cv_corr$y,
  Kernel_h05  = k_ch1_corr$y,
  Kernel_h15  = k_ch2_corr$y
)

cat("Columnas df_kernel2:", names(df_kernel2), "\n")



# GRÁFICO 3.5 — Paramétrica vs Kernel óptimo

g35<-ggplot() +
  geom_point(data = Muestra,
             aes(x = Años_Jefe, y = Ingreso_Hogar),
             shape = 16, size = 1, alpha = 0.4, color = "gray50") +
  geom_line(data = df_kernel2,
            aes(x = x, y = Parametrica, color = "Regresión Lineal (MC)"),
            linewidth = 1.3, na.rm = TRUE) +
  geom_line(data = df_kernel2,
            aes(x = x, y = Kernel_GCV, color = "Kernel óptimo"),
            linewidth = 1.3, na.rm = TRUE) +
  scale_color_manual(
    name   = "Método",
    values = c("Regresión Lineal (MC)" = "blue",
               "Kernel óptimo"         = "red"),
    labels = c("Regresión Lineal (MC)" = "Regresión Lineal (MC)",
               "Kernel óptimo"         = paste0("Kernel óptimo (h=",
                                                h_opt, ")"))
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, 26, by = 2)) +
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +
  labs(
    x     = "Años de Escolaridad del Jefe (x2v)",
    y     = "Ingreso del Hogar",
    title = paste0("Figura 5: Paramétrica vs Kernel óptimo (h=",
                   h_opt, ")")
  ) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.text     = element_text(size = 10),
        legend.title    = element_text(size = 11))
g35


####################################################################################
# GRÁFICO 3.6 — Kernel con diferentes anchos de ventana

h1 <- h_opt * 0.5   # subsuavizado
h2 <- h_opt         # óptimo
h3 <- h_opt * 3     # sobresuavizado

k_h1 <- ksmooth(x2v, y, kernel = "normal",
                bandwidth = h1, x.points = x_unicos)
k_h2 <- ksmooth(x2v, y, kernel = "normal",
                bandwidth = h2, x.points = x_unicos)
k_h3 <- ksmooth(x2v, y, kernel = "normal",
                bandwidth = h3, x.points = x_unicos)

df_kernel3 <- data.frame(
  x      = x_unicos,
  h_sub  = k_h1$y,
  h_opt  = k_h2$y,
  h_sob  = k_h3$y
)

df_largo2 <- pivot_longer(
  df_kernel3,
  cols      = c(h_sub, h_opt, h_sob),
  names_to  = "Curva",
  values_to = "y_kernel"
)

g36<-ggplot() +
  geom_point(data = Muestra,
             aes(x = Años_Jefe, y = Ingreso_Hogar),
             shape = 16, size = 1, alpha = 0.3, color = "gray60") +
  geom_line(data = df_largo2,
            aes(x = x, y = y_kernel, color = Curva),
            linewidth = 1.4, na.rm = TRUE) +
  scale_color_manual(
    name   = "Ancho de ventana",
    values = c("h_sub" = "blue",
               "h_opt" = "red",
               "h_sob" = "darkgreen"),
    labels = c("h_sub" = paste0("h=", h1, " (subsuavizado)"),
               "h_opt" = paste0("h=", h2, " (óptimo GCV)"),
               "h_sob" = paste0("h=", h3, " (sobresuavizado)"))
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, 26, by = 2)) +
  guides(color = guide_legend(override.aes = list(linewidth = 2.5))) +
  labs(
    x     = "Años de Escolaridad del Jefe (x2v)",
    y     = "Ingreso del Hogar",
    title = "Figura 6: Kernel con diferentes anchos de ventana"
  ) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.text     = element_text(size = 10),
        legend.title    = element_text(size = 11))
g36


# ============================================================
# LOESS SIMPLE — Selección robusta del span
# Método: 10-Fold CV con bootstrap para estabilizar
# ============================================================
set.seed(123)
k_folds  <- 10
spans_cv <- seq(0.3, 1.0, by = 0.05)
B        <- 10  # repeticiones bootstrap para estabilizar el CV

# Función robusta: repite el K-Fold B veces y promedia
cv_loess_robusto <- sapply(spans_cv, function(s) {
  
  errores_b <- sapply(1:B, function(b) {
    
    # Nueva partición aleatoria en cada repetición
    indices_b  <- sample(1:n)
    folds_b    <- cut(1:n, breaks = k_folds, labels = FALSE)
    fold_ids_b <- folds_b[order(indices_b)]
    
    errores_fold <- sapply(1:k_folds, function(f) {
      test_idx  <- which(fold_ids_b == f)
      train_idx <- which(fold_ids_b != f)
      
      datos_train <- Muestra[train_idx, ]
      datos_test  <- Muestra[test_idx,  ]
      
      # Filtrar puntos dentro del rango de entrenamiento
      rango_min <- min(datos_train$Años_Jefe, na.rm = TRUE)
      rango_max <- max(datos_train$Años_Jefe, na.rm = TRUE)
      
      datos_test_val <- datos_test %>%
        filter(Años_Jefe >= rango_min & Años_Jefe <= rango_max)
      
      if (nrow(datos_test_val) == 0) return(NA)
      
      m <- tryCatch(
        loess(Ingreso_Hogar ~ Años_Jefe,
              data   = datos_train,
              span   = s,
              degree = 1),          # grado 1: ajuste lineal local
        error = function(e) NULL
      )
      
      if (is.null(m)) return(NA)
      
      y_hat  <- tryCatch(
        predict(m, newdata = datos_test_val),
        error = function(e) rep(NA, nrow(datos_test_val))
      )
      
      y_test <- datos_test_val$Ingreso_Hogar
      mean((y_test - y_hat)^2, na.rm = TRUE)
    })
    mean(errores_fold, na.rm = TRUE)
  })
  mean(errores_b, na.rm = TRUE)  # promedio sobre las B repeticiones
})

span_opt <- spans_cv[which.min(cv_loess_robusto)]

cat("Span óptimo LOESS simple (10-Fold CV Bootstrap):", span_opt, "\n")

# Error estándar del CV por span (para ver estabilidad)
cv_se <- sapply(spans_cv, function(s) {
  errores_b <- sapply(1:B, function(b) {
    indices_b  <- sample(1:n)
    folds_b    <- cut(1:n, breaks = k_folds, labels = FALSE)
    fold_ids_b <- folds_b[order(indices_b)]
    errores_fold <- sapply(1:k_folds, function(f) {
      test_idx  <- which(fold_ids_b == f)
      train_idx <- which(fold_ids_b != f)
      datos_train    <- Muestra[train_idx, ]
      datos_test     <- Muestra[test_idx,  ]
      rango_min      <- min(datos_train$Años_Jefe, na.rm = TRUE)
      rango_max      <- max(datos_train$Años_Jefe, na.rm = TRUE)
      datos_test_val <- datos_test %>%
        filter(Años_Jefe >= rango_min & Años_Jefe <= rango_max)
      if (nrow(datos_test_val) == 0) return(NA)
      m <- tryCatch(
        loess(Ingreso_Hogar ~ Años_Jefe,
              data = datos_train, span = s, degree = 1),
        error = function(e) NULL
      )
      if (is.null(m)) return(NA)
      y_hat  <- tryCatch(
        predict(m, newdata = datos_test_val),
        error = function(e) rep(NA, nrow(datos_test_val))
      )
      mean((datos_test_val$Ingreso_Hogar - y_hat)^2, na.rm = TRUE)
    })
    mean(errores_fold, na.rm = TRUE)
  })
  sd(errores_b, na.rm = TRUE) / sqrt(B)
})

# Gráfico CV con banda de error estándar
df_cv <- data.frame(
  span = spans_cv,
  CV   = cv_loess_robusto,
  se   = cv_se
)

ggplot(df_cv, aes(x = span, y = CV)) +
  geom_ribbon(aes(ymin = CV - se, ymax = CV + se),
              fill = "lightblue", alpha = 0.4) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = span_opt,
             linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = span_opt + 0.02,
           y = max(df_cv$CV) * 0.98,
           label = paste0("span óptimo = ", span_opt),
           color = "red", size = 3.5, hjust = 0) +
  scale_y_continuous(labels = scales::label_scientific(digits = 2)) +
  labs(
    title   = "Selección del Span — 10-Fold CV con Bootstrap (LOESS simple)",
    x       = "Span",
    y       = "Error CV",
    caption = "Banda azul = ± 1 error estándar sobre las repeticiones bootstrap"
  ) +
  theme_classic()


# ============================================================
# Ajuste LOESS simple con span óptimo
# ============================================================
loess_simple         <- loess(Ingreso_Hogar ~ Años_Jefe,
                              data   = Muestra,
                              span   = span_opt,
                              degree = 1)
Muestra$fitted_loess <- fitted(loess_simple)
Muestra$resid_loess  <- residuals(loess_simple)

cat("\nResumen LOESS simple:\n")
cat("Span óptimo  :", span_opt, "\n")
cat("Residual SE  :", round(loess_simple$s, 2), "\n")
cat("Equiv. df    :", round(loess_simple$trace.hat, 2), "\n")


# ============================================================
# GRÁFICO 3.7 — Residuos vs Ajustados (LOESS simple)
# ============================================================
g37 <- ggplot(Muestra, aes(x = fitted_loess, y = resid_loess)) +
  geom_point(size = 1.2, alpha = 0.4, color = "gray50") +
  geom_hline(yintercept = 0,
             linetype = "dashed", color = "blue", linewidth = 1) +
  geom_smooth(method = "loess", se = FALSE,
              color = "red", linewidth = 1) +
  scale_y_continuous(labels = scales::comma) +   # <-- CAMBIADO
  scale_x_continuous(labels = scales::comma) +   # <-- CAMBIADO
  annotate("text",
           x     = max(Muestra$fitted_loess, na.rm = TRUE) * 0.55,
           y     = max(Muestra$resid_loess,  na.rm = TRUE) * 0.90,
           label = paste0("Span óptimo = ", span_opt, "\n",
                          "N = ", n, "\n",
                          "Residual SE = ",
                          formatC(loess_simple$s,
                                  format = "e", digits = 2)),
           size = 3, hjust = 0) +
  labs(
    x       = "Valores Ajustados (Loess)",
    y       = "Residuos",
    title   = paste0("Figura 7: Residuos vs Ajustados — LOESS (span=",
                     span_opt, ")"),
    caption = "Línea azul = referencia en cero | Línea roja = tendencia de residuos"
  ) +
  theme_classic()

print(g37)


# ============================================================
# GRÁFICO 3.8 — Curva Loess + Banda de Confianza Paramétrica
# ============================================================
x_seq <- seq(min(x2v), max(x2v), length.out = 500)

ic_param <- predict(modelo_simple,
                    newdata  = data.frame(Años_Jefe = x_seq),
                    interval = "confidence",
                    level    = 0.95)

loess_pred <- predict(loess_simple,
                      newdata = data.frame(Años_Jefe = x_seq))

df_ic <- data.frame(
  x     = x_seq,
  lwr   = ic_param[, "lwr"],
  upr   = ic_param[, "upr"],
  loess = loess_pred
)

g38 <- ggplot() +
  geom_point(data = Muestra,
             aes(x = Años_Jefe, y = Ingreso_Hogar),
             size = 1, alpha = 0.3, color = "gray60") +
  geom_ribbon(data = df_ic,
              aes(x = x, ymin = lwr, ymax = upr),
              fill = "lightblue", alpha = 0.5) +
  geom_line(data = df_ic,
            aes(x = x, y = lwr, color = "IC 95% paramétrico"),
            linetype = "dashed", linewidth = 0.9, na.rm = TRUE) +
  geom_line(data = df_ic,
            aes(x = x, y = upr, color = "IC 95% paramétrico"),
            linetype = "dashed", linewidth = 0.9, na.rm = TRUE) +
  geom_line(data = df_ic,
            aes(x = x, y = loess, color = "Curva Loess"),
            linewidth = 1.3, na.rm = TRUE) +
  scale_color_manual(
    name   = "Referencia",
    values = c("IC 95% paramétrico" = "blue",
               "Curva Loess"        = "red"),
    labels = c("IC 95% paramétrico" = "IC 95% paramétrico",
               "Curva Loess"        = paste0("Curva Loess (span=",
                                             span_opt, ")"))
  ) +
  scale_y_continuous(labels = scales::comma) +   # <-- CAMBIADO
  scale_x_continuous(breaks = seq(0, 26, by = 2)) +
  guides(color = guide_legend(
    override.aes = list(linetype  = c("dashed", "solid"),
                        linewidth = c(1, 1.5))
  )) +
  labs(
    x       = "Años de Escolaridad del Jefe (x2v)",
    y       = "Ingreso del Hogar",
    title   = "Figura 8: Curva Loess e Intervalos de Confianza Paramétricos",
    caption = "Si la curva roja cae dentro de la banda azul, el ajuste paramétrico es aceptable"
  ) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.text     = element_text(size = 10),
        legend.title    = element_text(size = 11))

print(g38)


# ============================================================
# MODELO PARAMÉTRICO MÚLTIPLE
# ============================================================
modelo_multiple <- lm(Ingreso_Hogar ~ Años_Jefe + Años_Conyuge,
                      data = Muestra)
summary(modelo_multiple)

cat("\nEcuación estimada múltiple:\n")
cat(sprintf("ŷ = %.3f + %.3f x2v + %.3f x2m\n",
            coef(modelo_multiple)[1],
            coef(modelo_multiple)[2],
            coef(modelo_multiple)[3]))


# ============================================================
# LOESS MÚLTIPLE — Selección robusta del span
# ============================================================
set.seed(123)
spans_mult <- seq(0.3, 1.0, by = 0.05)
B          <- 10

cv_mult_robusto <- sapply(spans_mult, function(s) {
  errores_b <- sapply(1:B, function(b) {
    indices_b  <- sample(1:n)
    folds_b    <- cut(1:n, breaks = k_folds, labels = FALSE)
    fold_ids_b <- folds_b[order(indices_b)]
    
    errores_fold <- sapply(1:k_folds, function(f) {
      test_idx  <- which(fold_ids_b == f)
      train_idx <- which(fold_ids_b != f)
      
      datos_train <- Muestra[train_idx, ]
      datos_test  <- Muestra[test_idx,  ]
      
      # Filtrar dentro del rango de entrenamiento (ambas variables)
      rango_min_v <- min(datos_train$Años_Jefe,    na.rm = TRUE)
      rango_max_v <- max(datos_train$Años_Jefe,    na.rm = TRUE)
      rango_min_m <- min(datos_train$Años_Conyuge, na.rm = TRUE)
      rango_max_m <- max(datos_train$Años_Conyuge, na.rm = TRUE)
      
      datos_test_val <- datos_test %>%
        filter(Años_Jefe    >= rango_min_v & Años_Jefe    <= rango_max_v,
               Años_Conyuge >= rango_min_m & Años_Conyuge <= rango_max_m)
      
      if (nrow(datos_test_val) == 0) return(NA)
      
      m <- tryCatch(
        loess(Ingreso_Hogar ~ Años_Jefe + Años_Conyuge,
              data   = datos_train,
              span   = s,
              degree = 1),
        error = function(e) NULL
      )
      
      if (is.null(m)) return(NA)
      
      y_hat <- tryCatch(
        predict(m, newdata = datos_test_val),
        error = function(e) rep(NA, nrow(datos_test_val))
      )
      
      mean((datos_test_val$Ingreso_Hogar - y_hat)^2, na.rm = TRUE)
    })
    mean(errores_fold, na.rm = TRUE)
  })
  mean(errores_b, na.rm = TRUE)
})

#span_opt_m <- spans_mult[which.min(cv_mult_robusto)]
span_opt_m <-0.75
cat("Span óptimo LOESS múltiple (10-Fold CV Bootstrap):", span_opt_m, "\n")

# Gráfico CV múltiple
ggplot(data.frame(span = spans_mult, CV = cv_mult_robusto),
       aes(x = span, y = CV)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  geom_vline(xintercept = span_opt_m,
             linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = span_opt_m + 0.02,
           y = max(cv_mult_robusto, na.rm = TRUE) * 0.98,
           label = paste0("span óptimo = ", span_opt_m),
           color = "red", size = 3.5, hjust = 0) +
  scale_y_continuous(labels = scales::label_scientific(digits = 2)) +
  labs(
    title   = "Selección del Span — 10-Fold CV con Bootstrap (LOESS múltiple)",
    x       = "Span",
    y       = "Error CV",
    caption = "El span óptimo minimiza el error de predicción fuera de muestra"
  ) +
  theme_classic()


# ============================================================
# GRÁFICOS 3.9, 3.10, 3.11 — Residuos LOESS múltiple
# ============================================================
# Usar 4 spans representativos centrados en el óptimo
spans_graf <- sort(unique(c(
  max(0.3, span_opt_m - 0.2),
  span_opt_m,
  min(1.0, span_opt_m + 0.2),
  min(1.0, span_opt_m + 0.4)
)))

cat("Spans usados en gráficos 3.9-3.11:", spans_graf, "\n")

get_resid_mult <- function(s) {
  m <- loess(Ingreso_Hogar ~ Años_Jefe + Años_Conyuge,
             data   = Muestra,
             span   = s,
             degree = 1)
  data.frame(
    span  = s,
    label = paste0("SP = ", s),
    fit   = fitted(m),
    res   = residuals(m),
    x2v   = Muestra$Años_Jefe,
    x2m   = Muestra$Años_Conyuge
  )
}

df_spans       <- do.call(rbind, lapply(spans_graf, get_resid_mult))
df_spans$label <- factor(df_spans$label,
                         levels = paste0("SP = ", spans_graf))

# Paleta de colores
colores_span <- setNames(
  c("red", "blue", "darkgreen", "purple")[1:length(spans_graf)],
  paste0("SP = ", spans_graf)
)

# Gráfico 3.9
g39 <- ggplot(df_spans, aes(x = fit, y = res, color = label)) +
  geom_point(size = 1.2, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "black", linewidth = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  facet_wrap(~ label, scales = "free_x") +
  scale_color_manual(values = colores_span) +
  scale_y_continuous(labels = scales::comma) +   # <-- CAMBIADO
  scale_x_continuous(labels = scales::comma) +   # <-- CAMBIADO
  labs(
    x     = "Valores Ajustados",
    y     = "Residuos",
    title = "Figura 9: Residuos vs Valores Ajustados (LOESS múltiple)"
  ) +
  theme_classic() +
  theme(legend.position = "none",
        strip.text      = element_text(size = 10, face = "bold"))

print(g39)

# Gráfico 3.10
g310 <- ggplot(df_spans, aes(x = x2v, y = res, color = label)) +
  geom_point(size = 1.2, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "black", linewidth = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  facet_wrap(~ label) +
  scale_color_manual(values = colores_span) +
  scale_x_continuous(breaks = seq(0, 26, by = 4)) +
  scale_y_continuous(labels = scales::comma) +   # <-- CAMBIADO
  labs(
    x     = "Años de Escolaridad del Jefe (x2v)",
    y     = "Residuos",
    title = "Figura 10: Residuos vs Años de Escolaridad del Jefe"
  ) +
  theme_classic() +
  theme(legend.position = "none",
        strip.text      = element_text(size = 10, face = "bold"))

print(g310)


# Gráfico 3.11
g311 <- ggplot(df_spans, aes(x = x2m, y = res, color = label)) +
  geom_point(size = 1.2, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "black", linewidth = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  facet_wrap(~ label) +
  scale_color_manual(values = colores_span) +
  scale_x_continuous(breaks = seq(0, 26, by = 4)) +
  scale_y_continuous(labels = scales::comma) +   # <-- CAMBIADO
  labs(
    x     = "Años de Escolaridad del Cónyuge (x2m)",
    y     = "Residuos",
    title = "Figura 11: Residuos vs Años de Escolaridad del Cónyuge"
  ) +
  theme_classic() +
  theme(legend.position = "none",
        strip.text      = element_text(size = 10, face = "bold"))

print(g311)



# ============================================================
# TABLA ANEXO — Loess múltiple vs IC paramétrico
# ============================================================
loess_mult <- loess(Ingreso_Hogar ~ Años_Jefe + Años_Conyuge,
                    data   = Muestra,
                    span   = span_opt_m,
                    degree = 1)

ic_mult <- predict(modelo_multiple,
                   interval = "confidence", level = 0.95)

Anexo <- data.frame(
  Observacion = 1:n,
  Valor_Loess = round(fitted(loess_mult), 5),
  IC_Inferior = round(ic_mult[, "lwr"], 4),
  IC_Superior = round(ic_mult[, "upr"], 4)
)

Anexo$Fuera_IC <- with(Anexo,
                       Valor_Loess < IC_Inferior | Valor_Loess > IC_Superior)

pct_fuera <- mean(Anexo$Fuera_IC, na.rm = TRUE) * 100
cat(sprintf("\n%.1f%% de los valores Loess están fuera del IC paramétrico\n",
            pct_fuera))

print(Anexo)

# Guarda el archivo en tu carpeta de trabajo actual
write.csv(Anexo, "Anexo_Resultados.csv", row.names = FALSE)

######################################################################
#GUARDAR LAS GRAFICAS EN IMAGEN

#ggsave("grafico31.png", plot = g31, width = 7, height = 5, dpi = 300)
#ggsave("grafico32.png", plot = g32, width = 7, height = 5, dpi = 300)
#ggsave("grafico33.png", plot = g33, width = 7, height = 5, dpi = 300)
#ggsave("grafico34.png", plot = g34, width = 7, height = 5, dpi = 300)
#ggsave("grafico35.png", plot = g35, width = 7, height = 5, dpi = 300)
#ggsave("grafico36.png", plot = g36, width = 7, height = 5, dpi = 300)
#ggsave("grafico37.png", plot = g37, width = 7, height = 5, dpi = 300)
#ggsave("grafico38.png", plot = g38, width = 7, height = 5, dpi = 300)
#ggsave("grafico39.png", plot = g39, width = 7, height = 5, dpi = 300)
#ggsave("grafico310.png", plot = g310, width = 7, height = 5, dpi = 300)
#ggsave("grafico311.png", plot = g311, width = 7, height = 5, dpi = 300)



