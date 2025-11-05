#---------------------------------------------------------------#
#              Establecer el tipo de variables                  #
#---------------------------------------------------------------#

tipoV <- function(BD) {
  Exploratorio <- as.data.frame(BD)
  tipo <- sapply(Exploratorio, class)

  # Validación de posibles fechas (tipo texto pero convertibles)
  es_fecha_convertible <- sapply(Exploratorio, function(x) {
    if (inherits(x, c("Date", "POSIXct", "POSIXt"))) return(TRUE)
    if (!is.character(x)) return(FALSE)
    suppressWarnings({
      posibles_fechas <- as.Date(x, format = "%Y-%m-%d")
      propor_na <- mean(is.na(posibles_fechas))
      propor_na < 0.99
    })
  })

  # Validación de numéricos o convertibles
  es_num_convertible <- sapply(Exploratorio, function(x) {
    if (inherits(x, c("Date", "POSIXct", "POSIXt"))) return(FALSE)
    suppressWarnings({
      x_num <- as.numeric(as.character(x))
      propor_na <- mean(is.na(x_num))
      propor_na < 0.99
    })
  })

  # Nombres de variables por tipo
  nomFecha <- names(Exploratorio)[es_fecha_convertible]
  nomCuan <- setdiff(names(Exploratorio)[es_num_convertible | (tipo == "numeric")], nomFecha)
  nomCual <- setdiff(names(Exploratorio), c(nomCuan, nomFecha))
  

  factores <- names(Exploratorio)[tipo == "factor"]
  lista <- names(Exploratorio)[tipo == "list"]
  #nomCual <- setdiff(c(names(Exploratorio)[tipo == "character"], factores), nomFecha)

  Resultado <- list(
    Tipo = tipo,
    Cuantis = nomCuan,
    Cualis = nomCual,
    Fechas = nomFecha,
    Factores = factores,
    Listas = lista
  )
  return(Resultado)
}




###################################################
##-----------------------------------------------##
##      DESCRIPTIVO VARIABLES CUANTITATIVAS      ##----
##-----------------------------------------------##
###################################################


#FUNCIÃ“N PARA EVALUAR NORMALIDAD
PP_val <- function(Variable) {
  if (is.numeric(Variable)) {
    n <- sum(!is.na(Variable))
    if (length(unique(na.omit(Variable))) > 1) {
      if (n >= 30) {
        library(nortest)
        return(ad.test(Variable)$p.value)
      } else if (n <= 2) {
        return(-0.3)
      } else {
        return(shapiro.test(Variable)$p.value)
      }
    } else {
      return(-0.03)
    }
  }
}

DesG <- function(Variable) {
  Variable<-as.numeric(Variable)
  if (tipoV(data.frame(Variable))$Tipo == "numeric") {
    p_val <- PP_val(Variable)
    n <- sum(!is.na(Variable))
    NAs <- length(Variable) - n
    
    if (p_val <= 0.05) {  # No normal
      Med <- median(Variable, na.rm = TRUE)
      Q1_Sd <- quantile(na.omit(Variable), 0.25)
      Q3 <- quantile(na.omit(Variable), 0.75)
      Res <- paste0(round(Med, 2), " [", round(Q1_Sd, 2), " - ", round(Q3, 2), "]")
    } else {  # Normal
      Med <- mean(Variable, na.rm = TRUE)
      Sd <- sd(Variable, na.rm = TRUE)
      Res <- paste0(round(Med, 2), " [", round(Sd, 2), "]")
    }
    
    Total <- list(
      Medida = t(cbind(n = n, Res = Res)),
      Pval = round(p_val, 2)
    )
    return(Total)
  }
}

#####################################
####### descriptivo bivariado 

## ANÁLISIS DESCRIPTIVO BIVARIADO (con subset para n>=2)
## ANÁLISIS DESCRIPTIVO BIVARIADO (con subset para n>=2)
Des_Cuanti <- function(Variable, var2) {
  Variable <- as.numeric(Variable)
  var2     <- droplevels(as.factor(var2))  # asegurar factor y limpiar niveles vacíos
  p_val    <- round(PP_val(Variable), 2)   # Evaluar normalidad

  # Función auxiliar para calcular las estadísticas descriptivas por grupo
  calc_stats <- function(var, grp, p) {
    if (p <= 0.05) {
      Med <- tapply(var, grp, median,   na.rm = TRUE)
    } else {
      Med <- tapply(var, grp, mean,     na.rm = TRUE)
    }
    Q1_Sd <- tapply(var, grp, quantile, 0.25, na.rm = TRUE)
    Sd <- sd(Variable, na.rm = TRUE)
    Q3    <- tapply(var, grp, quantile, 0.75, na.rm = TRUE)
    n     <- tapply(1 - is.na(var), grp, sum)
    return(list(Med = round(Med, 2),
                Q1_Sd = round(Q1_Sd, 2),
                Q3 = round(Q3, 2),
                n = n, Sd=round(Sd,2)))
  }

  # --- Estadísticos por grupo (para mostrar en tabla) ---
  stats <- calc_stats(Variable, var2, p_val)

  # --- Selección de niveles válidos (n >= 2) para la prueba ---
  n_per_group <- stats$n
  valid_lvls  <- names(n_per_group[n_per_group >= 2])

  if (length(valid_lvls) >= 2) {
    idx   <- !is.na(Variable) & !is.na(var2) & (var2 %in% valid_lvls)
    v_sub <- Variable[idx]
    g_sub <- droplevels(as.factor(var2[idx]))

    # Elegir prueba por normalidad usando el subconjunto
    if (p_val <= 0.05) {
      `Valor P` <- if (nlevels(g_sub) == 2) {
        suppressWarnings(wilcox.test(v_sub ~ g_sub, exact = FALSE)$p.value)
      } else {
        kruskal.test(v_sub ~ g_sub)$p.value
      }
    } else {
      `Valor P` <- if (nlevels(g_sub) == 2) {
        t.test(v_sub ~ g_sub)$p.value
      } else {
        anova(aov(v_sub ~ g_sub))$`Pr(>F)`[1]
      }
    }
  } else {
    `Valor P` <- NA_real_
  }

  # --- Total univariado de la variable (como ya hacías) ---
  Total <- DesG(Variable)

  # --- Armar tabla bivariada como en tu versión ---
  if (p_val <= 0.05) {
    Bivariate <- rbind(
      n   = paste0("n = ", stats$n),
      Res = paste0(stats$Med, " [", stats$Q1_Sd, " - ", stats$Q3, "]")
    )
  } else {
    Bivariate <- rbind(
      n   = paste0("n = ", stats$n),
      Res = paste0(stats$Med, " [", stats$Sd, "]")
    )
  }

  categorias <- names(stats$n)
  resultado_final <- cbind(
    Bivariate,
    Total   = Total$Medida,
    `Valor P` = round(rbind(`Valor P`, `Valor P`), 2)
  )
  colnames(resultado_final) <- c(categorias, "Total", "Valor P")
  return(resultado_final)
}


##ANÃLISIS DESCRIPTIVO BIVARIADO

Des_Cuanti_Viejp <- function(Variable, var2) {
  Variable<-as.numeric(Variable)
  p_val <- round(PP_val(Variable),2) # Evaluar normalidad
  
  # Función auxiliar para calcular las estadísticas descriptivas
  calc_stats <- function(var, grp,p) {
    if(p<=0.5){
      Med <- tapply(var, grp, median, na.rm = TRUE)
    }
    else{
      Med <- tapply(var, grp, mean, na.rm = TRUE)
    }
    Q1_Sd <- tapply(var, grp, quantile, 0.25, na.rm = TRUE)
    Q3 <- tapply(var, grp, quantile, 0.75, na.rm = TRUE)
    Sd <- sd(Variable, na.rm = TRUE)
    n <- tapply(1 - is.na(var), grp, sum) # Contar valores no NA
    NAs <- length(var) - n
    return(list(Med = round(Med,2), Q1_Sd = round(Q1_Sd,2), Q3 = round(Q3,2), 
                n = n, Sd = round(Sd,2)))
  }
  
  # Evaluar normalidad y decidir prueba
  if (p_val <= 0.05) {
    # No normal: usar medianas y pruebas no paramétricas
    stats <- calc_stats(Variable, var2,p_val)
    if (sum(stats$n <= 1) >= 1) { # sum(stats$n <= 1) >= 1
      `Valor P` <- NA # Si alguna categoría tiene <= 1 datos, no se puede hacer prueba
    } else if (sum(stats$n == 0) == 0) {
      categorias <- unique(na.omit(var2))
      `Valor P` <- if (length(categorias) == 2) {
        wilcox.test(Variable ~ var2, exact = FALSE)$p.value
      } else {
        kruskal.test(Variable ~ var2)$p.value
      }
    }
    Total <- DesG(Variable)
    Bivariate <- rbind(n = paste0("n = ",stats$n), Res= paste0(stats$Med," [", stats$Q1_Sd, " - ", Q3=stats$Q3,"]"))
    categorias <- names(stats$n)
    # Combina las matrices
    resultado_final <- cbind(Bivariate, Total = Total$Medida, `Valor P` = round(rbind(`Valor P`, `Valor P`), 2))
    colnames(resultado_final) <- c(categorias, "Total", "Valor P") # NUEVO
    return(resultado_final)
    #return(cbind(Bivariate, Total=Total$Medida,Total2=round(rbind(`Valor P`,`Valor P`),2) ))
    #return(Bivariate)
  } else {
    # Normal: usar medias y pruebas paramétricas
    stats <- calc_stats(Variable, var2,p_val)
    if (sum(stats$n <= 1) >= 1) { ######## el valor anterior (sum(stats$n <= 1) >= 1)
      `Valor P` <- NA
    } else if (sum(stats$n == 0) == 0) {
      categorias <- unique(na.omit(var2))
      `Valor P` <- if (length(categorias) == 2) {
        t.test(Variable ~ var2)$p.value
      } else {
        anova(aov(Variable ~ var2))$`Pr(>F)`[1]
      }
    }
    Total <- DesG(Variable)
    Bivariate <- rbind( n = paste0("n = ",stats$n), Res=paste0(stats$Med," [",stats$Q1_Sd,"]"))
    categorias <- names(stats$n) # NUEVO
    # Combina las matrices
    resultado_final <- cbind(Bivariate, Total = Total$Medida, `Valor P` = round(rbind(`Valor P`, `Valor P`), 2))
    colnames(resultado_final) <- c(categorias, "Total", "Valor P") # NUEVO
    return(resultado_final)
    #return(cbind(Bivariate, Total=Total$Medida,Total2=round(rbind(`Valor P`,`Valor P`),2)))
    #return(Bivariate)
  }
}


CorrelacionVertical <- function(BD1, BD2 = NULL, metodo = "spearman") {
  # Asegurar que método sea válido
  metodo <- match.arg(metodo, c("spearman", "pearson"))
  
  # Si BD2 no se proporciona, se correlacionan columnas de BD1 entre sí
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
      var2 <- as.numeric(BD2[[j]])
      nombre2 <- names(BD2)[j]
      
      # Excluir NA
      casos_completos <- complete.cases(var1, var2)
      if (sum(casos_completos) > 2) {
        cor_valor <- round(cor(var1[casos_completos], var2[casos_completos], method = metodo), 2)
        p_valor <- round(cor.test(var1[casos_completos], var2[casos_completos], method = metodo)$p.value, 2)
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







DesMin<-function(Variable){ # FUN. DESCRIPTIVO GENERAL
  if(tipoV(Variable)$Tipo=="numeric"){
    Med<-median(Variable,na.rm = T)
    Min<-min(Variable,na.rm = T)
    Max<-max(Variable,na.rm = T)
    #p_val<-p_val
    n<-(sum(1-is.na(Variable)))
    NAs<-length(Variable)-n
    Total<-round(cbind(Med,Min,Max,n),2)
    Total
  }
}

Resumen <- function(BD, respuesta, s) {
  BD <- as.data.frame(BD)
  n <- ncol(BD)
  nom_Variable <- c()
  fram <- data.frame()
  varT <- "Media (Sd)/Mediana(RIC)"
  Variable <- c()
  
  if (s == 1) {
    # ... (El código para s=1 se mantiene igual)
    for (i in 1:n) {
      Res <- Des_Cuanti(BD[, i], respuesta)
      fram <- rbind(fram, Res)
      nom_Variable <- c(nom_Variable, rep(names(BD)[i], 2))
      Variable <- c(Variable,
                    names(BD)[i],varT)
    }
  } 
  else { # Caso s=2 (Univariado)
    for (i in 1:n) {
      Res <- DesG(BD[, i])
      
      # *** MODIFICACIÓN AQUÍ: Extrae el componente 'Medida' y lo convierte a data.frame
      # para asegurar la consistencia de las columnas.
      Res_Medida <- as.data.frame(Res$Medida) 
      
      # Asegurar que el data.frame vacío 'fram' tenga nombres de columna antes del primer rbind
      if(i == 1) {
          fram <- setNames(data.frame(matrix(ncol = ncol(Res_Medida), nrow = 0)), names(Res_Medida))
      }
      
      fram <- rbind(fram, Res_Medida) # <-- Ahora hace rbind con un data.frame de 1 fila
      
      nom_Variable <- c(nom_Variable, names(BD)[i]) # Solo un nombre de variable por resumen univariado
      Variable <- c(Variable, varT)
    }
  }
  return(cbind(V1 = Variable, fram, nom_Variable))
}


#FunciÃ³n para hallar la frecuencia esperada deuna tabla de contingencias

###################################################
##-----------------------------------------------##
##       DESCRIPTIVO VARIABLES CUALITATIVAS      ##
##-----------------------------------------------##
###################################################


freq_esperada<-function(Table){
  margin.table(Table,1)->f
  margin.table(Table,2)->c
  cbind(f*c[1]/margin.table(Table), f*c[2]/margin.table(Table))
}#FunciÃ³n para hallar la frecuencia esperada deuna tabla de contingencias


### Función para la normalidad y seleccion del estadistico de centralidad
### Media o mediana

Validacionn <- function(Table) {  ## función para validar el tipo de prueba
  # Manejo de errores para la función
  tryCatch({
    # Obtención de frecuencias esperadas
    T_Es <- freq_esperada(Table)
    
    # Manejo de valores NA en T_Es
    T_Es <- na.omit(T_Es)
    
    # Verificación de la proporción de frecuencias esperadas <= 5
    if (prod(dim(Table)) > 0) {
      prop_T_Es <- sum(T_Es <= 5, na.rm = TRUE) / prod(dim(Table))
    } else {
      prop_T_Es <- 0
    }
    
    # Validación del caso con más del 25% de frecuencias esperadas <= 5
    if (prop_T_Es >= 0.25) {
      if (prod(dim(Table)) > 6) {
        # Más de 6 celdas: prueba de Fisher simulada
        prueba <- tryCatch({
          fisher.test(Table, simulate.p.value = TRUE)$p.value
        }, error = function(e) {
          NA  # Retorna NA en caso de error
        })
        p <- paste0(round(prueba,2), "**")
      } else if (prod(dim(Table)) >= 4 & prod(dim(Table)) <= 6) {
        # Entre 4 y 6 celdas: prueba de Fisher exacta
        prueba <- fisher.test(Table)$p.value
        p <- paste0(round(prueba,2), "**")
      } else {
        # Menos de 4 celdas: no es posible realizar la prueba
        prueba <- NA
        p <- paste0(round(prueba,2), "_")
      }
    } else {
      # Menos del 25% de celdas con frecuencias esperadas <= 5
      if (prod(dim(Table)) > 4) {
        # Más de 4 celdas: Chi-square con simulación
        prueba <- chisq.test(Table, simulate.p.value = TRUE)$p.value
        p <- paste0(round(prueba,2), "*")
      } else {
        # 4 o menos celdas: Chi-square estándar
        prueba <- chisq.test(Table)$p.value
        p <- paste0(round(prueba,2), "*")
      }
    }
    
    return(p)
    
  }, error = function(e) {
    # Retorna NA si hay algún error
    return("Error")
  })
}





CualiG <- function(Variable, var2, s) {
  
  if (s == 1) {
    if (is.factor(var2)) {
      T_1 <- table(Variable, droplevels(var2))
    } else {
      T_1 <- table(Variable, var2)
    }
    
    p_1 <- round(prop.table(T_1, 2), 4)#proporción bivariada
    N <- margin.table(T_1, 1)## Marginal fila tabla bivariada
    N2 <- table(var2)## Marginal columna tabla bivariada
    Tabla_1 <- T_1 ## Tabla bivariada concatenda con el N
    
    #Prop_T1 <- rbind(rep("n = ", length(unique(na.omit(var2)))), p_1)
    #### Generación de la nueva versión de las tablas 
    tabla_part_1 <- matrix(
      paste0(T_1, " (", round(100 * p_1, 2), ")"),  # Si quieres porcentajes redondeados
      nrow = nrow(T_1),
      dimnames = dimnames(T_1)
    )
    Margin_fil <- table(Variable)
    Margin_coll <- margin.table(T_1,1)
    Prop_Total <- c(round(sum(T_1)/length(Variable) * 100, 2),round(prop.table(Margin_fil) * 100, 2))
    #Totaln <- c(tot=sum(Margin_coll),N)
    Totaln <- c(sum(Margin_coll),N)
    
    
    Total<-paste0(Totaln," (",Prop_Total,")")
    
    #Ptotal <- round(Totaln / length(Variable) * 100, 2)
    #Prop_T1 <- c(Ptotal, Prop_Total)
    variables<-paste0("n = ",margin.table(T_1,2))#paste0(Totaln," [",Prop_T1,"]")
    Valor_P <- Validacionn(T_1)
    #Prueba <- Validacionn(T_1)[2]
    `Valor P`<-Valor_P
    Tabla_bivariada<-cbind(rbind(variables=variables,tabla_part_1 ),Total,`Valor P`)
    
    TotalG <- c(Totaln, Total)
    
    Prop_TotalG <- c("n = ", Prop_Total)

    
    #row.names(Tabla_bivariada)[1]<-c("Variable",names(Variable)) ############toby aqui es el ajuste
    
    return(cbind(Variable = rownames(Tabla_bivariada),Tabla_bivariada))
  } else if (s == 2) {
    Total <- table(Variable)
    Totaln <- sum(Total)
    TotalG <- c(Variable = Totaln, Total)
    Prop_Total <- round(prop.table(Total) * 100, 2)
    Ptotal <- round(Totaln / length(Variable) * 100, 1)
    Prop_T1 <- c(Ptotal, Prop_Total)
    Cual <-  cbind(TotalG, Prop_T1)
    return(cbind(rownames(Cual),paste0(TotalG,"(", Prop_T1,")")))##en el paste va cual
  } else {
    T_1 <- table(Variable, var2)
    Orr <- RR(T_1)
    return(Orr)
  }
}




tablas <- function(BD, respuesta, s) {
  BD <- as.data.frame(BD)
  f <- data.frame()
  nom_Variable <- c()
  n <- dim(BD)[2]
  nombres<-names(BD)
  
  for (i in 1:n) {
    variable_actual <- BD[[i]]
    if (all(is.na(variable_actual))) next
    
    resultado <- tryCatch({
      CualiG(variable_actual, respuesta, s)
    }, error = function(e) {
      warning(paste("Error en la variable", names(BD)[i], ":", e$message))
      return(NULL)
    })
    
    if (!is.null(resultado)) {
      resultado[1,1]<-nombres[i]
      f <- rbind(f,resultado)
      if (is.factor(variable_actual)) {
        nom_Variable <- c(nom_Variable, c("N", rep(names(BD)[i], length(levels(variable_actual)))))
      } else {
        nom_Variable <- c(nom_Variable, c("N", rep(names(BD)[i], length(unique(na.omit(variable_actual))))))
      }
    }
  }
  
  if(s==1){
    Resultado <- cbind(f,nom_Variable)
  }else{
    Resultado <- cbind(f,nom_Variable)
  }
  
  return(Resultado)
}


##############-------------------------------------------
CV_por_grupo <- function(BD, grupo) {
  BD <- as.data.frame(BD)
  grupo <- as.factor(grupo)
  
  resultado <- data.frame(Variable = character())
  
  for (var in names(BD)) {
    variable <- BD[[var]]
    
    if (is.numeric(variable)) {
      # Media y SD por grupo
      Medias <- tapply(variable, grupo, mean, na.rm = TRUE)
      SDs <- tapply(variable, grupo, sd, na.rm = TRUE)
      CVs <- round((SDs / Medias) * 100, 0)
      
      # Total
      media_total <- mean(variable, na.rm = TRUE)
      sd_total <- sd(variable, na.rm = TRUE)
      cv_total <- round((sd_total / media_total) * 100, 2)
      
      # Armar fila
      fila <- as.data.frame(t(CVs))
      fila$Total <- cv_total
      fila$Variable <- var
      
      resultado <- rbind(resultado, fila)
    }
  }
  
  # Reordenar columnas: Variable + grupos + Total
  resultado <- resultado[, c("Variable", setdiff(names(resultado), "Variable"))]
  return(resultado)
}
#----------------------------------------------------------------------------------
#                       ANÁLISIS COMPLETO
#----------------------------------------------------------------------------------

AnalisisCompleto2 <- function(BD, respuesta = NULL, s = 1) {
  BD <- as.data.frame(BD)
  tipos <- tipoV(BD)  # usa tus reglas para Cuantis y Cualis

  # Preparar respuesta (por nombre o vector); en bivariado, factor
  if (s == 1) {
    if (is.null(respuesta)) stop("Para s = 1 (bivariado) debes pasar 'respuesta'.")
    if (is.character(respuesta) && length(respuesta) == 1 && respuesta %in% names(BD)) {
      respuesta_vec <- BD[[respuesta]]
    } else {
      respuesta_vec <- respuesta
    }
    if (!is.factor(respuesta_vec)) respuesta_vec <- factor(respuesta_vec)
  } else {
    respuesta_vec <- NULL
  }

  piezas <- list()

  ## --- Cuantitativas ---
  if (length(tipos$Cuantis) > 0) {
    for (nm in tipos$Cuantis) {
      Tab <- tryCatch(
        Resumen(BD[, nm, drop = FALSE], respuesta_vec, s),
        error = function(e) NULL
      )
      if (!is.null(Tab) && nrow(Tab) > 0) {
        # armoniza nombres
        colnames(Tab)[colnames(Tab) == "nom_Variable"] <- "Variable"
        colnames(Tab)[colnames(Tab) == "V1"] <- "Estadístico"
        Tab$Tipo <- "Cuantitativa"
        piezas[[length(piezas) + 1]] <- Tab
      }
    }
  }

  ## --- Cualitativas ---
  if (length(tipos$Cualis) > 0) {
    for (nm in tipos$Cualis) {
      Tab <- tryCatch(
        tablas(BD[, nm, drop = FALSE], respuesta_vec, s),
        error = function(e) NULL
      )
      if (!is.null(Tab) && nrow(Tab) > 0) {
        # posibles nombres que salen de 'tablas'
        nms <- names(Tab)
        nms[nms == "nom_Variable"] <- "Variable"
        nms[nms == "rownames(Cual)"] <- "Estadístico"
        nms[nms == "rownames(Tabla_bivariada)"] <- "Estadístico"
        names(Tab) <- nms
        if (!"Variable" %in% names(Tab))     Tab$Variable <- nm
        if (!"Estadístico" %in% names(Tab))  Tab$Estadístico <- NA
        Tab$Tipo <- "Cualitativa"
        piezas[[length(piezas) + 1]] <- Tab
      }
    }
  }

  if (length(piezas) == 0) return(data.frame())

  # Unir con columnas diferentes (union de nombres + relleno con NA)
  all_cols <- Reduce(union, lapply(piezas, names))
  piezas <- lapply(piezas, function(df) {
    faltan <- setdiff(all_cols, names(df))
    for (cc in faltan) df[[cc]] <- NA
    df[, all_cols, drop = FALSE]
  })
  R <- do.call(rbind, piezas)
  rownames(R) <- NULL

  # Orden final: Variable | Estadístico | ... | Tipo
  primero <- c("Variable", "Estadístico")
  otros   <- setdiff(names(R), c(primero, "Tipo"))
  R <- R[, c(primero, otros, "Tipo"), drop = FALSE]
  R
}


AnalisisCompleto <- function(BD, respuesta = NULL, s = 1) {
  BD <- as.data.frame(BD)
  tipos <- tipoV(BD)  # tus reglas

  # preparar respuesta (nombre o vector); en bivariado => factor
  if (s == 1) {
    if (is.null(respuesta)) stop("Para s = 1 (bivariado) debes pasar 'respuesta'.")
    if (is.character(respuesta) && length(respuesta) == 1 && respuesta %in% names(BD)) {
      respuesta_vec <- BD[[respuesta]]
    } else {
      respuesta_vec <- respuesta
    }
    if (!is.factor(respuesta_vec)) respuesta_vec <- factor(respuesta_vec)
  } else {
    respuesta_vec <- NULL
  }

  piezas <- list()

  # >>> recorre en el ORDEN ORIGINAL <<<
  for (nm in names(BD)) {
    tipo_nm <- if (nm %in% tipos$Cuantis) "Cuantitativa" else if (nm %in% tipos$Cualis) "Cualitativa" else NA
    if (is.na(tipo_nm)) next

    Tab <- tryCatch({
      if (tipo_nm == "Cuantitativa") {
        Resumen(BD[, nm, drop = FALSE], respuesta_vec, s)
      } else {
        tablas(BD[, nm, drop = FALSE],  respuesta_vec, s)
      }
    }, error = function(e) NULL)

    if (is.null(Tab) || nrow(Tab) == 0) next

    # armonizar nombres de salida
    nms <- names(Tab)
    nms[nms == "nom_Variable"] <- "Variable"
    nms[nms == "V1"] <- "Estadístico"
    nms[nms == "rownames(Cual)"] <- "Estadístico"
    nms[nms == "rownames(Tabla_bivariada)"] <- "Estadístico"
    names(Tab) <- nms

    if (!"Variable" %in% names(Tab))    Tab$Variable <- nm
    if (!"Estadístico" %in% names(Tab)) Tab$Estadístico <- NA

    Tab$Tipo <- tipo_nm
    piezas[[length(piezas) + 1]] <- Tab
  }

  if (length(piezas) == 0) return(data.frame())

  # unir con columnas dispares
  all_cols <- Reduce(union, lapply(piezas, names))
  piezas <- lapply(piezas, function(df) {
    faltan <- setdiff(all_cols, names(df))
    for (cc in faltan) df[[cc]] <- NA
    df[, all_cols, drop = FALSE]
  })
  R <- do.call(rbind, piezas)
  rownames(R) <- NULL

  # orden final: Variable | Estadístico | ... | Tipo
  primero <- c("Variable", "Estadístico")
  otros   <- setdiff(names(R), c(primero, "Tipo"))
  R <- R[, c(primero, otros, "Tipo"), drop = FALSE]

  # (seguro extra) reordenar filas por el orden de la BD si hiciera falta
  if ("Variable" %in% names(R)) {
    R <- R[order(match(R$Variable, names(BD))), , drop = FALSE]
  }

  R
}

