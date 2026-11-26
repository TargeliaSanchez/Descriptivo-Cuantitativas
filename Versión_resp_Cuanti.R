variable_ind <- df_procesada$`11. Ocupación actual`;var_respuesta<- df_procesada$`3. Edad`


test_normalidad <- function(variable_ind, grupo = NULL) {
  if (!is.numeric(variable_ind)) {
    stop("La variable independiente debe ser numérica.")
  }
  
  # Función auxiliar interna para evaluar un solo vector numérico
  evaluar_vector <- function(x) {
    x_limpio <- na.omit(x)
    n <- length(x_limpio)
    
    if (length(unique(x_limpio)) <= 1) {
      return(-0.03) # Variable constante o sin variabilidad
    }
    if (n <= 2) {
      return(-0.3)  # Tamaño de muestra insuficiente
    }
    if (n >= 30) {
      return(nortest::ad.test(x_limpio)$p.value)
    } else {
      return(shapiro.test(x_limpio)$p.value)
    }
  }
  
  # Caso 1: Sin variable de agrupación (análisis univariado)
  if (is.null(grupo)) {
    return(evaluar_vector(variable_ind))
  }
  
  # Caso 2: Con variable de agrupación (bivariado)
  # Aseguramos que se alineen los NAs de ambas variables
  datos_validos <- !is.na(variable_ind) & !is.na(grupo)
  var_filtrada <- variable_ind[datos_validos]
  grupo_filtrado <- droplevels(as.factor(grupo[datos_validos]))
  
  # Aplica la evaluación a cada categoría y devuelve un vector nombrado
  p_valores <- tapply(var_filtrada, grupo_filtrado, evaluar_vector)
  return(as.numeric(p_valores)) # O deja p_valores sin as.numeric() si quieres conservar los nombres de cada grupo
}







Resumen_res_cuanti <- function(variable_ind, var_respuesta){
  var_respuesta<- as.numeric(var_respuesta)
  variable_ind <- droplevels(as.factor(variable_ind))  # asegurar factor y limpiar niveles vac?os
  p_vals   <- test_normalidad(var_respuesta, variable_ind)   # Evaluar normalidad
  
  
  # 1. Validar si algún grupo tuvo error (-0.3 o -0.03)
  if (any(p_vals < 0.05)) {
    # Manejar caso de muestra muy pequeña o constante (ej. reportar Mediana/RIC o NA)
    es_normal <- FALSE 
  } else {
    # 2. Si TODOS los grupos tienen p >= 0.05, es normal. Si AL MENOS UNO es < 0.05, gana no normal.
    es_normal <- all(p_vals >= 0.05) 
  }
  
  
  
  
  # Funcion auxiliar para calcular las estad?sticas descriptivas por grupo
  # Entradas: var (independiente), grupo(var_respuesta cualitativa), p (normalidad)
  calcular_estadisticas <- function(var, grupo, p_norm) {
    if (!es_normal) {
      Med <- tapply(var, grupo, median,   na.rm = TRUE)
      Q1_Sd <- tapply(var, grupo, quantile, 0.25, na.rm = TRUE)
      tipo_res <- "Mediana [RIC]"
    } else {
      Med <- tapply(var, grupo, mean,     na.rm = TRUE)
      Q1_Sd <- tapply(var, grupo, sd, na.rm = TRUE)
      tipo_res <- rep("Media [sd]")
    }
    #Q1_Sd <- tapply(var, grupo, quantile, 0.25, na.rm = TRUE)
    Q3    <- tapply(var, grupo, quantile, 0.75, na.rm = TRUE)
    n     <- tapply(1 - is.na(var), grupo, sum)
    return(list(Med = round(Med, 2),
                Q1_Sd = round(Q1_Sd, 2),
                Q3 = round(Q3, 2),
                n = n))
  }
  
  # --- Estadisticos por grupo (para mostrar en tabla) ---
  estadisticas <- calcular_estadisticas(var_respuesta, variable_ind, es_normal)
  
  # --- Seleccion de niveles v?lidos (n >= 2) para la prueba ---
  n_por_grupo <- estadisticas$n
  validar_niveles  <- names(n_por_grupo[n_por_grupo >= 2])# para determinar la prueba
  
  if (length(validar_niveles) >= 2) {
    idx   <- !is.na(var_respuesta) & !is.na(variable_ind) & (variable_ind %in% validar_niveles)
    var_cat <- var_respuesta[idx]
    grupo_cat <- droplevels(as.factor(variable_ind[idx]))
    
    # Elegir prueba acorde a normalidad usando el subconjunto
    if (!es_normal) {
      `Valor P` <- if (nlevels(grupo_cat) == 2) {
        suppressWarnings(wilcox.test(var_cat ~ grupo_cat, exact = FALSE)$p.value)
      } else {
        kruskal.test(var_cat ~ grupo_cat)$p.value
      }
    } else {
      `Valor P` <- if (nlevels(grupo_cat) == 2) {
        t.test(var_cat ~ grupo_cat)$p.value
      } else {
        anova(aov(var_cat ~ grupo_cat))$`Pr(>F)`[1]
      }
    }
  } else {
    `Valor P` <- NA_real_
  }
  
  
  # --- Total univariado de la variable (como ya hac?as) ---
  Total <- resumen_univariado(variable_ind)
  
  # --- Armar tabla bivariada como en tu versi?n ---
  if (!es_normal) {
    bivariado <- cbind(
      N   = estadisticas$n,
      Res = paste0(estadisticas$Med, " [", estadisticas$Q1_Sd, " - ", estadisticas$Q3, "]")
    )
  } else {
    bivariado <- cbind(
      N   = estadisticas$n,
      Res = paste0(estadisticas$Med, " [", estadisticas$Q1_Sd, "]")
    )
  }
  

  categorias <- names(estadisticas$n)
  resultado_final <- rbind("",cbind(
    categoría = rownames(bivariado),
    bivariado,
    `Valor P` = rep(round(`Valor P`, 2), length(categorias))
  ))
  colnames(resultado_final) <- c("categoría","N","Resumen" ,"Valor P")
  return(resultado_final)
  print(p_va)
}




#-------------------------------------------------------------------------------
#
#-------------------------------------------------------------------------------

Resumen <- function(BD, var_respuesta, s=1) {
  
  # Guardamos el nombre real de lo que entra ANTES de convertir a data.frame
  nombre_entrada <- deparse(substitute(BD))
  nombre_limpio <- gsub(".*\\$|.*\\[\\[\"|\"\\]\\]", "", nombre_entrada)
  
  BD <- as.data.frame(BD)
  # Si el nombre de la columna es gen?rico como "BD" o "V1", le ponemos el limpio
  if(names(BD)[1] %in% c("BD", "V1", "variable_actual","nm")) {
    names(BD)[1] <- nombre_limpio
  }
  
  
  #BD <- as.data.frame(BD)
  n <- ncol(BD)
  nom_Variable <- c()
  fram <- data.frame()
  #varT <- "Media (Sd)/Mediana(RIC)"
  variable_ind <- c()
  
  if (s == 1) {
    for (i in 1:n) {
      Res <- resumen_univariado(BD[, i])
      Res_Medida <- as.data.frame(Res$Medida) 
      Res_Medida[1,1]<- names(BD)[i]
      #colnames(Res_Medida)<-"V2"
      # Asegurar que el data.frame vac?o 'fram' tenga nombres de columna antes del primer rbind
      if(i == 1) {
        fram <- setNames(data.frame(matrix(ncol = ncol(Res_Medida), nrow = 0)), names(Res_Medida))
      }
      
      fram <- rbind(fram, Res_Medida) # <-- Ahora hace rbind con un data.frame de 1 fila
      
      #nom_Variable <- c(nom_Variable, names(BD)[i]) # Solo un nombre de variable por resumen univariado
      #variable_ind <- c("N", varT)
    }
  }
  if (s == 2) {
    for (i in 1:n) {
      Res <- resumen_bivariado(BD[, i], var_respuesta)
      Res[1,1]<- names(BD)[i]
      fram <- rbind(fram, Res)
      # nom_Variable <- c(nom_Variable, rep(names(BD)[i], 2))
      
    }
  }
  if (s == 3) {
    for (i in 1:n) {
      Res <- Resumen_res_cuanti(BD[, i], var_respuesta)
      Res[1,1]<- names(BD)[i]
      fram <- rbind(fram, Res)
      # nom_Variable <- c(nom_Variable, rep(names(BD)[i], 2))
      
    }
  }
  return(cbind(fram#, nom_Variable
  ))
}

A <- data.frame()

for (i in 1:length(Covariables)){
 A<-test_normalidad(df_procesada$EQ_VAS_Score, df_procesada[,Covariables[i]])
 print(Covariables[i]);print(A)
}



















