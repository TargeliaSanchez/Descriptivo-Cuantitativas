tipoV <- function(BD) {
  Exploratorio <- as.data.frame(BD)
  tipo <- sapply(Exploratorio, class)
  nomCual <- names(Exploratorio)[tipo == "character"]
  nomCuan <- names(Exploratorio)[tipo == "numeric"]
  Otros <- names(Exploratorio)[tipo %in% c("POSIXct", "logical")]
  factores <- names(Exploratorio)[tipo == "factor"]
  lista <- names(Exploratorio)[tipo == "list"]
  Resultado <- list(
    Tipo = tipo,
    Cualis = nomCual,
    Cuantis = nomCuan,
    factores = factores,
    lista = lista
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
      Q1_Sd <- sd(Variable, na.rm = TRUE)
      Res <- paste0(round(Med, 2), " [", round(Q1_Sd, 2), "]")
    }
    
    Total <- list(
      Medida = t(cbind(n = n, Res = Res)),
      Pval = round(p_val, 2)
    )
    return(Total)
  }
}

##ANÃLISIS DESCRIPTIVO BIVARIADO

Des_Cuanti <- function(Variable, var2) {
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
    n <- tapply(1 - is.na(var), grp, sum) # Contar valores no NA
    NAs <- length(var) - n
    return(list(Med = round(Med,2), Q1_Sd = round(Q1_Sd,2), Q3 = round(Q3,2), 
                n = n))
  }
  
  # Evaluar normalidad y decidir prueba
  if (p_val <= 0.05) {
    # No normal: usar medianas y pruebas no paramétricas
    stats <- calc_stats(Variable, var2,p_val)
    if (sum(stats$n <= 1) >= 1) {
      P_val <- NA # Si alguna categoría tiene <= 1 datos, no se puede hacer prueba
    } else if (sum(stats$n == 0) == 0) {
      categorias <- unique(na.omit(var2))
      P_val <- if (length(categorias) == 2) {
        wilcox.test(Variable ~ var2, exact = FALSE)$p.value
      } else {
        kruskal.test(Variable ~ var2)$p.value
      }
    }
    Total <- DesG(Variable)
    Bivariate <- rbind(n = stats$n, Res= paste0(stats$Med," [", stats$Q1_Sd, " - ", Q3=stats$Q3,"]"))
    return(cbind(Bivariate, Total=Total$Medida,Total2=round(rbind(P_val,P_val),2) ))
    #return(Bivariate)
  } else {
    # Normal: usar medias y pruebas paramétricas
    stats <- calc_stats(Variable, var2,p_val)
    if (sum(stats$n <= 1) >= 1) {
      P_val <- NA
    } else if (sum(stats$n == 0) == 0) {
      categorias <- unique(na.omit(var2))
      P_val <- if (length(categorias) == 2) {
        t.test(Variable ~ var2)$p.value
      } else {
        anova(aov(Variable ~ var2))$`Pr(>F)`[1]
      }
    }
    Total <- DesG(Variable)
    Bivariate <- rbind( n = stats$n, Res=paste0(stats$Med," [",stats$Q1_Sd,"]"))
    return(cbind(Bivariate, Total=Total$Medida,Total2=round(rbind(P_val,P_val),2)))
    #return(Bivariate)
  }
}

Resumen <- function(BD, respuesta, s) {
  BD <- as.data.frame(BD)
  n <- ncol(BD)
  nom_V <- c()
  fram <- data.frame()
  varT <- "Media (Sd)/Mediana(RIC)"
  Variable <- c()  # INICIALIZAR AQUÍ
  
  if (s == 1) {
    for (i in 1:n) {
      Res <- Des_Cuanti(BD[, i], respuesta)
      fram <- rbind(fram, Res)
      nom_V <- c(nom_V, rep(names(BD)[i], 2))
      Variable <- c(Variable,
                    names(BD)[i],varT)
    }
  } else {
    for (i in 1:n) {
      Res <- DesG(BD[, i])
      fram <- rbind(fram, Res)
      nom_V <- c(nom_V, rep(names(BD)[i], 1))
      Variable <- c(Variable,varT)
    }
  }
  return(cbind(Variable,fram, nom_V))
  return(fram)
}
