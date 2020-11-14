# limpiar 
rm(list=ls())


library(tidyverse)
library(treemap)
library(ggplot2)

#directorio de base de datos
dir1 <- "C:\\Users\\Dis22442\\Documents\\DREVF"
#leer base de datos
data <- read_csv(paste(dir1, "rnped_limpia.csv", sep="/"))

# PREGUNTA 2: Entre hombres y mujeres, ¿cuál fue el grupo más vulnerable en el 2011?
## Preparación:
# 1) Filtrar a únicamente los registros del año 2011
# 2) Agrupar por la columna sexo
# 3) Agrega una nueva columna calculando el promedio ponderado de las tasas por cada grupo.
# Pro Tip: weighted.mean() calcula el promedio ponderado con las tasas y ponderando la población

data <- read.csv(paste(dir1, "rnped_limpia.csv", sep="/"))

tempo <- data %>%
  filter(year==2011) %>%
  group_by(sexo) %>%
  summarise(tdes = weighted.mean(tdes, pob))

## Gráfica: completa el código faltante
# Genera una barra para cada sexo, graficando su tasa.
# Agrega el título: Tasa de desaparecidos 2011
# El subtítulo: por sexo
# Añade las tasas en la cima de cada barra redondeando a 2 decimales y ajusta el texto verticalmente
# Agrega un texto descriptivo para el eje X y para el eje Y.
gr <- ggplot(tempo, aes(x=sexo, y=tdes)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=round(tdes, 2), vjust=-1)) + #2 es el no.dedecimales, vjust(negativo)=aparece arriba
  labs(title="Tasa de desaparecidos /n 2011", subtitle = "por sexo", x="sexo", y="tdes")

ggsave(paste(dir1, "20.png", sep="/"), plot=gr, width=12, height=12)

# PREGUNTA 3: Imaginen que queremos ver el porcentaje de desaparecidos que hay por sexo y edad.
# Por ejemplo: ¿Qué porcentaje de mujeres desaparecidas tienen entre 0 y 11 años?
## Preparación:
# 1) Agrupar por la columna sexo y rango de edad. Recuerda que el orden en el que pongas las columnas dentro
# de group_by importa.
# 2) Colapsa los datos para obtener el total de cada grupo y reemplaza los valores de la columa total
# 3) Agrega una nueva columna "totales" que sea igual a la sumatoria de los valores de la columna total
# por cada grupo.
# 4) Ayudándote de las columnas total y totales, calcula el porcentaje que representa cada rango de edad
# del total de personas desaparecidas por cada grupo.

tempo <- data %>%
  group_by(sexo, rango_edad) %>%
  summarise(total = sum(total)) %>%
  mutate(totales = sum(total),
         porcentaje = round((total/totales)*100, 2))

## Gráfica
# 1) Grafica los valores de la columna sexo en el eje X y los de porcentaje en eje Y
# 2) Utiliza los valores de la columa "rango_edad" para colorear cada porcentaje
# 3) Establece un valor para el ángulo


gr <- ggplot(tempo, aes(x=sexo, y=porcentaje, fill=rango_edad)) +
  geom_bar(stat="identity") + # Especificando y expl�citamente
  geom_text(aes(label=paste0(porcentaje,"%")), position=position_stack(vjust=0.5), angle=90) +
  scale_fill_manual(values=c("#FFFFCC","#C7E9B4","#7FCDBB","#41B6C4","#2C7FB8","#253494")) +
  labs(title="Porcentaje de desaparecidos entre 2010 y 2015 \npor sexo y edad",
       x="sexo", y="porcentaje", fill="rango_edad") +
  coord_flip() +
  theme_bw()

ggsave(paste(dir1, "21.png", sep="//"), plot=gr, width=12, height=12)

# PREGUNTA 4.1: Para la pregunta 4, existe otra alternativa
## Recordemos que nuestros datos son los siguientes:
tempo <- data %>%
  group_by(sexo, ent, nom_ent) %>%
  summarise(tdes = weighted.mean(tdes, pob)) %>%
  mutate(tdes = round(tdes, 2))

# Encontrar la función faltante que permita visualizar las tasas para cada sexo por separado, pero sin perder
# la división existente entre estados.
# Pro Tip: Consulta la sección "faceting" del cheat sheet.

gr <- ggplot(tempo, aes(x=sexo, y=tdes)) +
  geom_bar(stat="identity", fill="#41B6C4") +
  geom_text(aes(label=tdes), vjust=-0.3, size=3) +
  facet_grid(sexo~ent) +
  labs(title="Tasa de desaparecidos por sexo y estado \n2010 - 2015",
       x="Sexo", y="Tasa de desaparecidos") +
  theme_bw() +
  theme(axis.text.x = element_blank()) # Elige un ángulo que permita visualizar correctamente el texto

ggsave(paste(dir1, "22.png", sep="/"), plot=gr, width=20, height=12)

# Ejercicios Klustera:
#libreria
library(lubridate)


# base de datos
dir2 <- "C:\\Users\\Dis22442\\Documents\\DREVF\\klustera\\klustera"
data <- read.csv(paste(dir2, "e.csv", sep="/"))

# ¿Que día de la semana contamos con mayor número de visitantes?
# Filtrar la variable visitor
# Agrupar por día de semana
# Colapsar contando número de registros por día de semana
data$visitor
tempo <- data %>%
  filter(visitor == "true") %>%
  group_by(day_of_week_tz) %>%
  count()

gr <- ggplot(data, aes(x=day_of_week_tz, y= TRUE)) + geom_bar(stat = "identity")

ggsave(paste(dir2, "23.png", sep="/"), plot=gr, width=12, height = 12)
paste(dir2, "23.png", sep="/")

# ¿Meses con mayor número de visitantes?
# Filtrar: Registros de forma mensual
# Agrupar: visitantes y no visitantes
# Colapsar: Contar número de registros por grupo

data$visitor
tempo <- data %>%
  filter(visitor == TRUE) %>%
  group_by(month_tz) %>%
  count(TRUE)

gr <- ggplot(data, aes(x=month_tz, y= visitor)) + geom_bar(stat = "identity")

ggsave(paste(dir2, "24.png", sep="/"), plot=gr, width=12, height = 12)
paste(dir2, "24.png", sep="/")

# ¿Cuál oficina recibe más visitantes?
# Filtrar: obtener los registros solo de visitantes (visitor).
# Agrupar: por día de la semana.
# Colapsar: contar los registros por oficina.

data$visitor
tempo <- data %>%
  filter(visitor == "true") %>%
  group_by(branch_office) %>%
  count()

gr <- ggplot(data, aes(x=branch_office, y= visitor)) + geom_bar(stat = "identity")

ggsave(paste(dir2, "25.png", sep="/"), plot=gr, width=12, height = 12)
paste(dir2, "25.png", sep="/")

# ¿Cuál oficina recibe más visitantes?
# Filtrar: obtener los registros solo de visitantes (visitor).
# Agrupar: por día de la semana.
# Colapsar: contar los registros por oficina.
