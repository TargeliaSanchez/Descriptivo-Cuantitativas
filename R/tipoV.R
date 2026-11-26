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
  nomLogit <- setdiff(nombres, c(nomCuan, nomFecha, nomCual, listas, factores))
  
  # ---- SALIDA ----
  Resultado <- list(
    Tipo     = tipo,
    Cuantis  = nomCuan,
    Cualis   = nomCual,
    Fechas   = nomFecha,
    Factores = factores,
    Listas   = listas,
    Logico   = nomLogit
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



