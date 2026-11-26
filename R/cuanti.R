###################################################
##-----------------------------------------------##
##      DESCRIPTIVO VARIABLES CUANTITATIVAS      ##----
##-----------------------------------------------##
###################################################


# FUNCION PARA EVALUAR NORMALIDAD
# Entrada: variable cuantitativa
# Valida el suspuesto de normaidad de la variable correspondiente al n
# Salida: valor p prueba de normalidad
test_normalidad <- function(variable_ind) {
  if (is.numeric(variable_ind)) {
    n <- sum(!is.na(variable_ind))
    if (length(unique(na.omit(variable_ind))) > 1) {
      if (n >= 30) {
        library(nortest)
        return(ad.test(variable_ind)$p.value)
      } else if (n <= 2) {
        return(-0.3)
      } else {
        return(shapiro.test(variable_ind)$p.value)
      }
    } else {
      return(-0.03)
    }
  }
}



# ==============================================================================
# FUNcion: resumen_univariado
# Descripcion: Calcula medidas de tendencia central y dispersi?n seg?n normalidad.
# Entradas: 
#   - x: Vector num?rico (variable cuantitativa).
# Salidas: Lista con $Medida (df resumen) y $Pval (valor p normalidad).
# ==============================================================================

resumen_univariado <- function(variable_ind) {
  
  nombre_var_ind <- obtener_nombre(variable_ind)
  variable_ind<-as.numeric(variable_ind)
  if (tipoV(data.frame(variable_ind))$Tipo == "numeric") {
    p_valor_normalidad <- test_normalidad(variable_ind) # prueba de normalidad
    n1 <- sum(!is.na(variable_ind))
    n <- paste0("n = ",sum(!is.na(variable_ind)))
    NAs <- length(variable_ind) - n1
    
    
    if (p_valor_normalidad <= 0.05) {  # No normal
      Med <- median(variable_ind, na.rm = TRUE)
      Q1_Sd <- quantile(na.omit(variable_ind), 0.25)
      Q3 <- quantile(na.omit(variable_ind), 0.75)
      Res <- paste0(round(Med, 2), " [", round(Q1_Sd, 2), " - ", round(Q3, 2), "]")
      tipo_res <- "Mediana [RIC]"
    } else {  # Normal
      Med <- mean(variable_ind, na.rm = TRUE)
      Q1_Sd <- sd(variable_ind, na.rm = TRUE)
      Res <- paste0(round(Med, 2), " [", round(Q1_Sd, 2), "]")
      tipo_res <- rep("Media [sd]")
    }
    
    Medida = t(rbind(cbind(nombre_var_ind,tipo_res),cbind(n = n, Res = Res)))
    colnames(Medida)<- c("Variable","Total")
    Total <- list(
      Medida = Medida,
      Pval = round(p_valor_normalidad, 2)
    )
    return(Total)
  }
}


# ==============================================================================
# FUNcion: resumen_bivariado
# Descripcion: Realiza comparaciones entre dos variables (ej. t-test, ANOVA o Wilcoxon).
# Entradas: variable_ind (independiente), var_respuesta
# Salidas: df resumen bivar9iado y completo con valor p de asociacion
# ==============================================================================

resumen_bivariado <- function(variable_ind, var_respuesta) {
  variable_ind <- as.numeric(variable_ind)
  var_respuesta     <- droplevels(as.factor(var_respuesta))  # asegurar factor y limpiar niveles vac?os
  p_valor_normalidad    <- round(test_normalidad(variable_ind), 2)   # Evaluar normalidad
  
  # Funcion auxiliar para calcular las estad?sticas descriptivas por grupo
  # Entradas: var (independiente), grupo(var_respuesta cualitativa), p (normalidad)
  calcular_estadisticas <- function(var, grupo, p_norm) {
    if (p_norm <= 0.05) {
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
  estadisticas <- calcular_estadisticas(variable_ind, var_respuesta, p_valor_normalidad)
  
  # --- Seleccion de niveles v?lidos (n >= 2) para la prueba ---
  n_por_grupo <- estadisticas$n
  validar_niveles  <- names(n_por_grupo[n_por_grupo >= 2])# para determinar la prueba
  
  if (length(validar_niveles) >= 2) {
    idx   <- !is.na(variable_ind) & !is.na(var_respuesta) & (var_respuesta %in% validar_niveles)
    var_cat <- variable_ind[idx]
    grupo_cat <- droplevels(as.factor(var_respuesta[idx]))
    
    # Elegir prueba acorde a normalidad usando el subconjunto
    if (p_valor_normalidad <= 0.05) {
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
  if (p_valor_normalidad <= 0.05) {
    bivariado <- rbind(
      n   = paste0("n = ", estadisticas$n),
      Res = paste0(estadisticas$Med, " [", estadisticas$Q1_Sd, " - ", estadisticas$Q3, "]")
    )
  } else {
    bivariado <- rbind(
      n   = paste0("n = ", estadisticas$n),
      Res = paste0(estadisticas$Med, " [", estadisticas$Q1_Sd, "]")
    )
  }
  
  categorias <- names(estadisticas$n)
  resultado_final <- cbind(
    Total = Total$Medida[,1],
    bivariado,
    Total  = Total$Medida[,2],
    `Valor P` = rbind(round(`Valor P`, 2), "")
  )
  colnames(resultado_final) <- c("Variable",categorias,"Total", "Valor P")
  return(resultado_final)
}

#===============================================================================
# Resumen_res_cuanti
# Esta función es similar a res_cuanti, pero con la particularidad que se utiliza cuando 
# la respuesta es cuantitativa por lo cual hará las descriptivas de las variables independientes cualitatvas
# Respecto a una variable cuantitativa. --- pendiente hacerlo para cuando amabas sean cuantitativas y arrojar el algoritmo final así.
# Entradas:
# variable_ind, var_respuesta
#===============================================================================


Resumen_res_cuanti <- function(variable_ind, var_respuesta){
  var_respuesta<- as.numeric(var_respuesta)
  variable_ind <- droplevels(as.factor(variable_ind))  # asegurar factor y limpiar niveles vac?os
  p_valor_normalidad    <- round(test_normalidad(var_respuesta), 2)   # Evaluar normalidad
  
  
  # Funcion auxiliar para calcular las estad?sticas descriptivas por grupo
  # Entradas: var (independiente), grupo(var_respuesta cualitativa), p (normalidad)
  calcular_estadisticas <- function(var, grupo, p_norm) {
    if (p_norm <= 0.05) {
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
  estadisticas <- calcular_estadisticas(var_respuesta, variable_ind, p_valor_normalidad)
  
  # --- Seleccion de niveles v?lidos (n >= 2) para la prueba ---
  n_por_grupo <- estadisticas$n
  validar_niveles  <- names(n_por_grupo[n_por_grupo >= 2])# para determinar la prueba
  
  if (length(validar_niveles) >= 2) {
    idx   <- !is.na(var_respuesta) & !is.na(variable_ind) & (variable_ind %in% validar_niveles)
    var_cat <- var_respuesta[idx]
    grupo_cat <- droplevels(as.factor(variable_ind[idx]))
    
    # Elegir prueba acorde a normalidad usando el subconjunto
    if (p_valor_normalidad <= 0.05) {
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
  if (p_valor_normalidad <= 0.05) {
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
  resultado_final <- rbind(seq(1:4),cbind(
    categoría = rownames(bivariado),
    bivariado,
    `Valor P` = rbind(round(`Valor P`, 2), "")
  ))
  colnames(resultado_final) <- c("categoría","N","Resumen" ,"Valor P")
  return(resultado_final)
}

#===============================================================================
# Resumen
# Funcion que realiza el an?lisis descriptivo cuantitativo de var_ind
# Entradas:
# BD, var_respuesta, 
# s = 1 (por defecto): Univariado (Descriptivo b?sico)
# s = 2: Bivariado cuando la variable de respuesta es cualitativa (Relacion entre variables)
# s = 3: Bivariado cuando la variable de respuesta es cuantitativa (Solo asociación entre cuali y cuantis)
#===============================================================================

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
  return(cbind(fram)) #, nom_Variable
}


#===============================================================================
# Funcion para realizar correlacion en formato largo
# Entrasdas: BD1, BD2 (opcional), metodo (por defecto spearman)
# Realiza correlacion de dos bases de datos, si se ingresa una se 
# realiza la correlacion sobre sus varables asociadas
# Salidas: df con las correlaciones y las variables correspondientes
#===============================================================================

CorrelacionVertical <- function(BD1, BD2 = NULL, metodo = "spearman") {
  # Asegurar que m?todo sea v?lido
  metodo <- match.arg(metodo, c("spearman", "pearson"))
  
  # Si BD2 no se proporciona, se correlacionan columnas de BD1 entre s?
  if (is.null(BD2)) {
    BD2 <- BD1
  }
  
  # Asegurarse que sean data.frames
  BD1 <- as.data.frame(BD1)
  BD2 <- as.data.frame(BD2)
  
  n1 <- ncol(BD1)
  n2 <- ncol(BD2)
  
  resultados <- data.frame()
  
  for (i in 1:n1) {
    var1 <- as.numeric(BD1[[i]])
    nombre1 <- names(BD1)[i]
    
    for (j in 1:n2) {
      var_respuesta <- as.numeric(BD2[[j]])
      nombre2 <- names(BD2)[j]
      
      # Excluir NA
      casos_completos <- complete.cases(var1, var_respuesta)
      if (sum(casos_completos) > 2) {
        cor_valor <- round(cor(var1[casos_completos], var_respuesta[casos_completos], 
                               method = metodo), 2)
        p_valor <- round(cor.test(var1[casos_completos], var_respuesta[casos_completos], 
                                  method = metodo)$p.value, 2)
      } else {
        cor_valor <- NA
        p_valor <- NA
      }
      
      fila <- data.frame(
        Variable1 = nombre1,
        Variable2 = nombre2,
        Correlacion = cor_valor,
        Pvalor = p_valor,
        Metodo = metodo
      )
      
      resultados <- rbind(resultados, fila)
    }
  }
  
  return(list(resultados, unique(resultados$Variable1)))
}


#===============================================================================
# Funcion que realiza el an?lissi descriptivo con mediana y valores min y max
# Entrada: variable_ind
#===============================================================================

DesMin<-function(variable_ind){ # FUN. DESCRIPTIVO GENERAL
  if(tipoV(variable_ind)$Tipo=="numeric"){
    Mediana<-median(variable_ind,na.rm = T)
    Min<-min(variable_ind,na.rm = T)
    Max<-max(variable_ind,na.rm = T)
    #p_valor_normalidad<-p_valor_normalidad
    n<-(sum(1-is.na(variable_ind)))
    NAs<-length(variable_ind)-n
    Total<-round(cbind(Mediana,Min,Max,n),2)
    Total
  }
}
