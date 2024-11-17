# Limpiar el entorno y cargar librerías
rm(list = ls())
gc()
options(scipen = 999)

# Instalar y cargar librerías (descomentar si es necesario)
# install.packages(c("data.table", "kableExtra"))

library(data.table)
require("kableExtra")
library(tidyr)
library(tidyverse)
library(ggplot2)
if(!require(igraph)) install.packages("igraph")
library(igraph)

# Definir los datos
lista_w <- list(
  
  data_largo_red = c(4.779, 5.069, 5.969, 5.249, 4.449, 6.439, 5.899, 4.349, 4.409, 5.469, 4.479, 4.959, 7.109, 5.679, 4.819, 5.399, 4.829, 5.269, 4.769, 4.789),
  
  data_largo_sin_red = c(5.549, 7.939, 3.949, 5.129, 6.009, 6.949, 4.779, 4.359, 5.639, 6.029, 6.259, 5.749, 5.779, 3.469, 5.929, 4.769, 6.039, 6.369, 6.949, 6.369),
  
  data_corto_red = c(5.559, 5.779, 5.409, 6.109, 6.259, 4.929, 5.739, 6.109, 5.099, 5.519, 5.379, 6.109, 5.369, 5.429, 4.949, 5.469, 4.529, 5.399, 5.319, 5.439),
  
  data_corto_SUMA_PCA = c(4.439, 5.959, 6.449, 5.779, 6.139, 4.689, 5.799, 5.729, 5.399, 5.019, 2.879, 3.429, 5.609, 6.419, 5.189, 4.859, 4.529, 5.469, 6.139, 6.509),
  
  data_corto_sin_red = c(5.869, 5.559, 5.669, 6.529, 3.559, 6.219, 5.129, 6.529, 6.709, 5.519, 6.549, 4.239, 6.699, 5.649, 6.829, 6.129, 5.679, 4.999, 6.469, 5369),

  data_largo_SUMA_PCA = c(5.899, 6.139, 6.309, 5.299, 7.729, 5.359, 4.709, 4.909, 6.649, 4.719, 5.409, 5.539, 6.289, 5.339, 6.299, 6.309, 5.799, 6.959, 6.889, 6.139)
  
  )

# Calcular promedios y desviaciones estándar
resultados_stats <- data.table(
  Grupo = names(lista_w),
  Promedio = sapply(lista_w, mean),
  Desviacion_Estandar = sapply(lista_w, sd)
)

# Ordenar de mayor a menor por promedio
resultados_stats <- resultados_stats[order(-Promedio)]

# Mostrar tabla de promedios y desviaciones estándar
print(resultados_stats %>%
        kable("html", caption = "Promedios y Desviaciones Estándar de Cada Grupo") %>%
        kable_styling(full_width = FALSE))

# Comparaciones de Wilcoxon
resultados_wilcoxon <- data.frame(Comparacion = character(), Valor_p = numeric(), stringsAsFactors = FALSE)

# Comparar todos los grupos entre sí
for (i in 1:length(lista_w)) {
  for (j in 1:length(lista_w)) {
    if (i < j) {  # Solo comparaciones únicas
      resultado <- wilcox.test(lista_w[[i]], lista_w[[j]], paired = FALSE, exact = FALSE)
      resultados_wilcoxon <- rbind(resultados_wilcoxon, 
                                   data.frame(Comparacion = paste(names(lista_w)[i], "vs", names(lista_w)[j]),
                                              Valor_p = resultado$p.value))
    }
  }
}

# Filtrar resultados significativos (p < 0.05) y ordenarlos de menor a mayor
resultados_wilcoxon <- resultados_wilcoxon[order(resultados_wilcoxon$Valor_p), ] 

# Mostrar tabla de resultados de Wilcoxon
print(resultados_wilcoxon %>%
        kable("html", caption = "Resultados de la Prueba de Wilcoxon") %>%
        kable_styling(full_width = FALSE))


# Inicializar un dataframe vacío para almacenar los resultados
resultados_combinaciones <- data.frame(Combinacion = character(), Valor_p = numeric(), stringsAsFactors = FALSE)

# Obtener todas las combinaciones posibles de los grupos
combinaciones <- combn(names(lista_w), 2)

# Comparar cada combinación y calcular el valor p
for (i in 1:ncol(combinaciones)) {
  grupo1 <- combinaciones[1, i]
  grupo2 <- combinaciones[2, i]
  
  # Realizar el test de Wilcoxon
  resultado <- wilcox.test(lista_w[[grupo1]], lista_w[[grupo2]], paired = FALSE, exact = FALSE)
  
  # Añadir el resultado al dataframe
  resultados_combinaciones <- rbind(resultados_combinaciones, 
                                    data.frame(Combinacion = paste(grupo1, "vs", grupo2),
                                               Valor_p = resultado$p.value))
}
# Ordenar por valor de p en forma ascendente
resultados_combinaciones <- resultados_combinaciones[order(resultados_combinaciones$Valor_p), ]

# Mostrar la tabla de combinaciones y valores p
print(resultados_combinaciones %>%
        kable("html", caption = "Combinaciones de Grupos y Valores de p") %>%
        kable_styling(full_width = FALSE))


# Dividimos las comparaciones en nodos de origen y destino
comparaciones <- resultados_wilcoxon

# Separar las comparaciones en columnas from y to
comparaciones <- comparaciones %>%
  separate(Comparacion, into = c("from", "to"), sep = " vs ")

comparaciones <- comparaciones %>%
  mutate(temp = from, from = to, to = temp) %>%
  select(-temp)

# Crear un conjunto de vértices único desde las columnas from y to
vertices <- unique(c(comparaciones$from, comparaciones$to)) %>%
  data.frame(name = .)


# Crear el grafo asegurándonos de que todas las columnas sean compatibles
g <- graph_from_data_frame(d = comparaciones, directed = TRUE, vertices = vertices)


# Agregar el valor p como peso de las aristas
E(g)$weight <- comparaciones$Valor_p

# Filtrar las aristas con valor p mayor a 0.05 para eliminarlas del grafo
g <- delete_edges(g, E(g)[E(g)$weight >= 0.05])

# Definir el color y estilo de las aristas que quedan
E(g)$color <- "red"  # Solo se mostrarán las rojas
E(g)$arrow.size <- 0.5
E(g)$label <- round(E(g)$weight, 3)  # Mostrar valor p en las aristas

# Dibujar el grafo
plot(g, 
     edge.label = E(g)$label,  # Mostrar valores p en las aristas
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     vertex.size = 30,
     main = "Red de Comparaciones - Test de Wilcoxon")






