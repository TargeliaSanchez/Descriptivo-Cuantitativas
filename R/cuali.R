
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


