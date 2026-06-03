# Descriptivo-Cuantitativas
# 📊 AutomataEDA: Motor de Análisis Exploratorio y Estadístico Automatizado en R

Este repositorio contiene un conjunto de funciones auxiliares en **R** diseñadas para automatizar por completo el **Análisis Exploratorio de Datos (EDA)** tanto univariado como bivariado. El script detecta de forma inteligente el tipo de variables de tu base de datos, evalúa supuestos estadísticos (como la normalidad) y genera tablas formateadas listas para reportes clínicos, académicos o científicos.

## ✨ Características Principales

* **Identificación Inteligente de Variables (`tipoV`):** Clasifica automáticamente las columnas de un DataFrame en cuantitativas, cualitativas, fechas, factores, listas o lógicos.
* **Decisión Estadística Automatizada (`test_normalidad`):** Aplica la prueba de *Shapiro-Wilk* (para $n < 30$) o *Anderson-Darling* (para $n \geq 30$) para decidir si una variable es paramétrica o no.
* **Resúmenes Univariados Adaptativos (`resumen_univariado`):** * Si la variable es **normal**, reporta `Media [Desviación Estándar]`.
    * Si **no es normal**, reporta `Mediana [Rango Intercuartílico]`.
* **Análisis Bivariado Automático (`resumen_bivariado`):** Compara variables cuantitativas vs. una variable respuesta cualitativa. Elige la prueba correcta automáticamente: **t-test / ANOVA** (si hay normalidad) o **Wilcoxon / Kruskal-Wallis** (si no la hay).
* **Análisis Cualitativo Robusto (`analisis_cualitativo`):** Genera tablas de contingencia con frecuencias y porcentajes, calculando valores p mediante **Chi-cuadrado** o **Prueba Exacta de Fisher** según el cumplimiento de las frecuencias esperadas.

---

## 🛠️ Requisitos e Instalación

El script utiliza funciones de paquetes estándar de R y algunas librerías específicas para pruebas avanzadas y manipulación de texto. Asegúrate de tenerlos instalados:

```R
install.packages(c("nortest", "tidyverse", "lubridate"))
