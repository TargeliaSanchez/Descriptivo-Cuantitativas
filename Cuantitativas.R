#---------------------------------------------------------------#
#                      FUNCIONES AUXILIARES                     #
#---------------------------------------------------------------#

# clasificacion <- tipoV(mi_base)
# analisis_cualitativo_cuantis <- lapply(mi_base[, clasificacion$Cuantis], resumen_univariado)
#===============================================================================
# Determinar tipo de variable
# Entrada: Base de datos
# Salidas: Lista de 6 vectores
# tipo: vector con ls tipos de variables en la BD y encabezado el nombre de cada variable
# Cuantis, CualisCuantis, Fechas, Factores, Listas: vectores con con nombres de variables correspondiente en cada caso
#===============================================================================

tipoV <- function(BD) {
  
  # ---- Normalizacion ----
  if (is.vector(BD) || is.factor(BD)) {
    Exploratorio <- data.frame(V1 = BD)
  } else if (is.list(BD) && !is.data.frame(BD)) {
    Exploratorio <- as.data.frame(BD, stringsAsFactors = FALSE)
  } else {
    Exploratorio <- as.data.frame(BD, stringsAsFactors = FALSE)
  }
  
  tipo <- sapply(Exploratorio, class)
  es_lista <- sapply(Exploratorio, is.list)
  
  Exploratorio_atom <- Exploratorio[!es_lista]
  
  es_fecha_convertible <- sapply(Exploratorio_atom, function(x) {
    if (inherits(x, c("Date", "POSIXct", "POSIXt"))) return(TRUE)
    if (!is.character(x)) return(FALSE)
    
    suppressWarnings({
      posibles_fechas <- as.Date(x, format = "%Y-%m-%d")
      mean(is.na(posibles_fechas)) < 0.99
    })
  })
  
  es_num_convertible <- sapply(Exploratorio_atom, function(x) {
    if (inherits(x, c("Date", "POSIXct", "POSIXt"))) return(FALSE)
    
    suppressWarnings({
      x_num <- as.numeric(as.character(x))
      mean(is.na(x_num)) < 0.99
    })
  })
  
  nombres <- names(Exploratorio_atom)
  
  nomFecha <- nombres[es_fecha_convertible]
  nomCuan  <- setdiff(nombres[es_num_convertible | tipo[nombres] == "numeric"], nomFecha)
  nomCual  <- setdiff(nombres, c(nomCuan, nomFecha))
  
  factores <- names(Exploratorio)[tipo == "factor"]
  listas   <- names(Exploratorio)[es_lista]
  
  # ---- SALIDA ----
  Resultado <- list(
    Tipo     = tipo,
    Cuantis  = nomCuan,
    Cualis   = nomCual,
    Fechas   = nomFecha,
    Factores = factores,
    Listas   = listas
  )
  
  return(Resultado)
}

#===============================================================================
# nombre de variable
obtener_nombre <- function(variable) {
  # 1. Capturamos la expresi?n
  expr <- substitute(variable)
  
  # 2. Si es una expresi?n compleja (como base$EDAD o base[["EDAD"]])
  if (is.call(expr)) {
    # El nombre de la columna suele ser el ?ltimo elemento de la llamada
    nombre <- as.character(expr[[length(expr)]])
    return(nombre)
  }
  
  # 3. Si por alguna raz?n fallara lo anterior, usamos deparse como plan B
  return(deparse(expr))
}



#===============================================================================

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

resumen_univariado(base_int358$EDAD)


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
  
  # --- Estad?sticos por grupo (para mostrar en tabla) ---
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

resumen_bivariado(base_int358$EDAD,base_int358$SEXO)

#===============================================================================
# Resumen
# Funcion que realiza el an?lisis descriptivo cuantitativo de var_ind
# Entradas:
# BD, var_respuesta, 
# s = 1 (por defecto): Univariado (Descriptivo b?sico)
# s = 2: Bivariado (Relacion entre variables)
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
  return(cbind(fram#, nom_Variable
               ))
}

#Resumen(base_int358,base_int358$SEXO,2)
Resumen(base_int358$`SESIONES REALIZADAS`,base_int358$SEXO,2)
Resumen(base_int358$`SESIONES REALIZADAS`,base_int358$SEXO,1)

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

###################################################
##-----------------------------------------------##
##       DESCRIPTIVO VARIABLES CUALITATIVAS      ##
##-----------------------------------------------##
###################################################

#===============================================================================
# freq_esperada 
# Entrada: tabla
# retorna la tabla con los valores esperados
# Salida: tabla valor esperado
#===============================================================================

freq_esperada<-function(Table){
  margin.table(Table,1)->f
  margin.table(Table,2)->c
  cbind(f*c[1]/margin.table(Table), f*c[2]/margin.table(Table))
}

tabla <- table(base_int358$SEXO,base_int358$SEXO)
freq_esperada(tabla)

#===============================================================================
# prueaba_asociacion 
# Entrada: tabla
# Salida: valor p (? Fisher * Chi cuadrado)
#===============================================================================

prueba_asociacion <- function(Table) {  ## funcion para validar el tipo de prueba
  # Manejo de errores para la funcion
  tryCatch({
    # Obtencion de frecuencias esperadas
    T_Es <- freq_esperada(Table)
    
    # Manejo de valores NA en T_Es
    T_Es <- na.omit(T_Es)
    
    # Verificacion de la proporcion de frecuencias esperadas <= 5
    if (prod(dim(Table)) > 0) {
      prop_T_Es <- sum(T_Es <= 5, na.rm = TRUE) / prod(dim(Table))
    } else {
      prop_T_Es <- 0
    }
    
    # Validacion del caso con m?s del 25% de frecuencias esperadas <= 5
    if (prop_T_Es >= 0.25) {
      if (prod(dim(Table)) > 6) {
        # M?s de 6 celdas: prueba de Fisher simulada
        prueba <- tryCatch({
          fisher.test(Table, simulate.p.value = TRUE)$p.value
        }, error = function(e) {
          NA  # Retorna NA en caso de error
        })
        p <- paste0(round(prueba,2), "\u2020")
      } else if (prod(dim(Table)) >= 4 & prod(dim(Table)) <= 6) {
        # Entre 4 y 6 celdas: prueba de Fisher exacta
        prueba <- fisher.test(Table)$p.value
        p <- paste0(round(prueba,2), "\u2020")
      } else {
        # Menos de 4 celdas: no es posible realizar la prueba
        prueba <- NA
        p <- paste0(round(prueba,2), "_")
      }
    } else {
      # Menos del 25% de celdas con frecuencias esperadas <= 5
      if (prod(dim(Table)) > 4) {
        # M?s de 4 celdas: Chi-square con simulacion
        prueba <- chisq.test(Table, simulate.p.value = TRUE)$p.value
        p <- paste0(round(prueba,2), "*")
      } else {
        # 4 o menos celdas: Chi-square est?ndar
        prueba <- chisq.test(Table)$p.value
        p <- paste0(round(prueba,2), "*")
      }
    }
    
    return(p)
    
  }, error = function(e) {
    # Retorna NA si hay alg?n error
    return("Error")
  })
}

prueba_asociacion(tabla)

#===============================================================================
# CualiG (esqueleto)
# Entradas: variable_ind, var_respuesta, s
# s = 1: tabla univariada
# s = 2: tabla bivariada
# Salidas: data frame de tabla de frecuencias estructurado
# Realiza an?lisis cualitativo para una sola variable independiente
#===============================================================================

CualiG <- function(variable_ind, var_respuesta, s=1) {
  
  if (s == 1) {
    Total <- table(variable_ind)
    Totaln <- sum(Total)
    Prop_Total <- round(prop.table(Total) * 100, 2)
    Ptotal <- round(Totaln / length(variable_ind) * 100, 1)
    
    TotalG <- c(paste0("n = ",Totaln," (", Ptotal,")" ), 
                paste0(Total," (",Prop_Total,")"))
    Variable <-c("Total", rownames(Total))
    Prop_T1 <- c(Ptotal, Prop_Total)
    Cual <- cbind(Total = TotalG, Prop_T1)
    return(cbind(Variable,Total = TotalG))##en el paste va cual
    #return(Cual)
  }
  else if (s == 2) {
    if (is.factor(var_respuesta)) {
      T_1 <- table(variable_ind, droplevels(var_respuesta))
    } else {
      T_1 <- table(variable_ind, var_respuesta)
    }
    
    p_1 <- round(prop.table(T_1, 2), 4)#proporcion bivariada
    N <- margin.table(T_1, 1)## Marginal fila tabla bivariada
    N2 <- table(var_respuesta)## Marginal columna tabla bivariada
    Tabla_1 <- T_1 ## Tabla bivariada concatenda con el N
    
    #Prop_T1 <- rbind(rep("n = ", length(unique(na.omit(var_respuesta)))), p_1)
    #### Generacion de la nueva versi?n de las analisis_cualitativo 
    tabla_part_1 <- matrix(
      paste0(T_1, " (", round(100 * p_1, 2), ")"),  # Si quieres porcentajes redondeados
      nrow = nrow(T_1),
      dimnames = dimnames(T_1)
    )
    Margin_fil <- table(variable_ind)
    Margin_coll <- margin.table(T_1,1)
    Prop_Total <- c(round(sum(T_1)/length(variable_ind) * 100, 2),round(prop.table(Margin_fil) * 100, 2))
    #Totaln <- c(tot=sum(Margin_coll),N)
    Totaln <- c(sum(Margin_coll),N)
    
    
    Total<-paste0(Totaln," (",Prop_Total,")")
    
    variables<-paste0("n = ",margin.table(T_1,2))#paste0(Totaln," [",Prop_T1,"]")
    Valor_P <- prueba_asociacion(T_1)
    #Prueba <- prueba_asociacion(T_1)[2]
    `Valor P`<-Valor_P
    Tabla_bivariada<-cbind(rbind(variables=variables,tabla_part_1 ),Total,`Valor P`)
    
    TotalG <- c(Totaln, Total)
    
    Prop_TotalG <- c("n = ", Prop_Total)
  
    return(cbind(Variable = rownames(Tabla_bivariada),Tabla_bivariada))
  } else {
    T_1 <- table(Variable, var_respuesta)
    Orr <- RR(T_1)
    return(Orr)
  }
}

CualiG(base_int358$SEXO, base_int358$SEXO,2)
tablas(base_int358$SEXO, base_int358$SEXO,1)

#===============================================================================
# analisis_cualitativo
# Entradas: variable_ind, var_respuesta, s
# s = 1: tabla univariada
# s = 2: tabla bivariada
# Salidas: data frame de tabla de frecuencias estructurado
# Realiza an?lisis cualitativo para las variables en una base de datos
#===============================================================================

analisis_cualitativo <- function(BD, var_respuesta, s=1) {
  BD <- as.data.frame(BD)
  f <- data.frame()
  nom_Variable <- c()
  n <- dim(BD)[2]
  nombres<-names(BD)
  
  for (i in 1:n) {
    variable_actual <- BD[[i]]
    if (all(is.na(variable_actual))) next
    if (is.numeric(as.numeric(variable_actual))& length(unique(na.omit(variable_actual)))>15) next
    
    resultado <- tryCatch({
      CualiG(variable_actual, var_respuesta, s)
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

analisis_cualitativo(base_int358[,2:6], base_int358$SEXO,2)
names(analisis_cualitativo(base_int358, base_int358$SEXO,2))

#===============================================================================
# AnalisisCompleto2
# Entradas: variable_ind, var_respuesta, s
# s = 1: tabla univariada
# s = 2: tabla bivariada
# Salidas: data frame de tabla de frecuencias estructurado
# Realiza an?lisis cualitativo para las variables en una base de datos
#===============================================================================

AnalisisCompleto2 <- function(BD, var_respuesta = NULL, s = 1) {
  BD <- as.data.frame(BD)
  tipos <- tipoV(BD)  # usa tus reglas para Cuantis y Cualis
  
  # Preparar var_respuesta (por nombre o vector); en bivariado, factor
  if (s == 2) {
    if (is.null(var_respuesta)) stop("Para s = 2 (bivariado) debes pasar 'var_respuesta'.")
    if (is.character(var_respuesta) && length(var_respuesta) == 1 && var_respuesta %in% names(BD)) {
      respuesta_vec <- BD[[var_respuesta]]
    } else {
      respuesta_vec <- var_respuesta
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
        #Tab[1, 1] <- nm 
        Tab$Tipo <- "Cuantitativa"
        piezas[[length(piezas) + 1]] <- Tab
      }
    }
  }
  
  ## --- Cualitativas ---
  if (length(tipos$Cualis) > 0) {
    for (nm in tipos$Cualis) {
      Tab <- tryCatch(
        suppressWarnings(analisis_cualitativo(BD[, nm, drop = FALSE], respuesta_vec, s)),
        error = function(e) NULL
      )
      if (!is.null(Tab) && nrow(Tab) > 0) {
        # posibles nombres que salen de 'analisis_cualitativo'
        nms <- names(Tab)
        nms[nms == "nom_Variable"] <- "Variable"
        #nms[nms == "rownames(Cual)"] <- "Estad?stico"
        #nms[nms == "rownames(Tabla_bivariada)"] <- "Estad?stico"
        names(Tab) <- nms
        if (!"variable_ind" %in% names(Tab))     Tab$variable_ind <- nm
        if (!"Estad?stico" %in% names(Tab))  Tab$Estad?stico <- NA
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
  
  # Orden final: variable_ind | Estad?stico | ... | Tipo
  primero <- c("variable_ind", "Estad?stico")
  otros   <- setdiff(names(R), c(primero, "Tipo"))
  R <- R[, c(primero, otros, "Tipo"), drop = FALSE]
  R[,-2]
}

#===============================================================================
# AnalisisCompleto
# Entradas: variable_ind, var_respuesta, s
# s = 1: tabla univariada
# s = 2: tabla bivariada
# Salidas: data frame de tabla de frecuencias estructurado
# Realiza an?lisis cualitativo para las variables en una base de datos
#===============================================================================

AnalisisCompleto <- function(BD, var_respuesta = NULL, s = 1) {
  BD <- as.data.frame(BD)
  tipos <- tipoV(BD)  # tus reglas
  
  # preparar var_respuesta (nombre o vector); en bivariado => factor
  if (s == 2) {
    if (is.null(var_respuesta)) stop("Para s = 1 (bivariado) debes pasar 'var_respuesta'.")
    if (is.character(var_respuesta) && length(var_respuesta) == 1 && var_respuesta %in% names(BD)) {
      respuesta_vec <- BD[[var_respuesta]]
    } else {
      respuesta_vec <- var_respuesta
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
        analisis_cualitativo(BD[, nm, drop = FALSE],  respuesta_vec, s)
      }
    }, error = function(e) NULL)
    
    if (is.null(Tab) || nrow(Tab) == 0) next
    
    # armonizar nombres de salida
    nms <- names(Tab)
    #nms[nms == "nom_Variable"] <- "variable_ind"
    #nms[nms == "V1"] <- "Estad?stico"
    #nms[nms == "rownames(Cual)"] <- "Estad?stico"
    #nms[nms == "rownames(Tabla_bivariada)"] <- "Estad?stico"
    #names(Tab) <- nms
    
    if (!"variable_ind" %in% names(Tab))    Tab$variable_ind <- nm
    #if (!"Estad?stico" %in% names(Tab)) Tab$Estad?stico <- NA
    
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
  
  # orden final: variable_ind | Estad?stico | ... | Tipo
  primero <- c("Variable","variable_ind")
  otros   <- setdiff(names(R), c(primero, "Tipo"))
  R <- R[, c(primero, otros, "Tipo"), drop = FALSE]
  
  # (seguro extra) reordenar filas por el orden de la BD si hiciera falta
  if ("variable_ind" %in% names(R)) {
    R <- R[order(match(R$variable_ind, names(BD))), , drop = FALSE]
  }
  
  R
}








Analisis_especial <- function(
    vars_vec,
    char,
    BD,
    col_respuesta,
    col_grupo,
    col_variable,
    col_tiempo
) {
  
  R_list <- list()
  k <- 1
  
  for (i in seq_along(vars_vec)) {
    for (j in seq_along(char)) {
      
      tmp <- BD %>%
        filter(
          .data[[col_grupo]]    == char[j],
          .data[[col_variable]] == vars_vec[i]
        )
      
      a <- AnalisisCompleto(
        tmp[[col_respuesta]],
        tmp[[col_tiempo]],
        1
      )
      
      if (!is.null(a) && nrow(a) > 0) {
        a <- a %>%
          mutate(
            variable_ind   = vars_vec[i],
            Grupo_char = char[j]
          )
        
        R_list[[k]] <- a
        k <- k + 1
      }
    }
  }
  
  bind_rows(R_list)
}


limpiar_fecha <- function(x) {
  x <- as.character(x)
  # 1. Eliminar texto (todo lo que no sea n?mero o / o espacio)
  x <- str_replace_all(x, "[^0-9/ ]", " ")
  # 2. Extraer la PRIMERA fecha dd/mm/yyyy
  fecha_dmy <- str_extract(x, "\\b\\d{1,2}/\\d{1,2}/\\d{4}\\b")
  # 3. Extraer solo a?o (si no hubo fecha completa)
  anio <- str_extract(x, "\\b(19|20)\\d{2}\\b")
  # 4. Detectar n?meros tipo Excel
  excel_num <- suppressWarnings(as.numeric(x))
  # 5. Construir fecha final
  fecha_final <- case_when(
    !is.na(fecha_dmy) ~ dmy(fecha_dmy),
    is.na(fecha_dmy) & !is.na(anio) ~ ymd(paste0(anio, "-01-01")),
    is.na(fecha_dmy) & !is.na(excel_num) ~ as.Date(excel_num, origin = "1899-12-30"),
    TRUE ~ NA_Date_
  )  
  return(fecha_final)
}



