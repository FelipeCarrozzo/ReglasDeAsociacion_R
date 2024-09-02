rm(list = ls())
#cargo librerias
library(dplyr)
library(ggplot2)
library(arules)
library(arulesSequences)
library(skimr)
library(DataExplorer)
library(corrplot)
library(lubridate)
library(sf)
library(maps)
library(paletteer)
library(stringr)

#cargo los datos csv
data <- read.csv("e-shop clothing 2008.csv", header = TRUE, sep = ";")

#---- 
#a) Explore los datos y presente sus características principales
unique(data$country)
str(data)

skim(data)
# plot_missing(data) # grafico para ver datos faltantes

# #convierto variables de int a factor
# data$month <- as.factor(data$month)
# data$session.ID <- as.factor(data$session.ID)
# data$order <- as.factor(data$order)

#creo la variable 'date' con la fecha de cada sesión
data$date <- paste(data$day, data$month, sep = "/")

#month:
#es una variable numérica cuando deberia ser categórica, además relacionamos los 
#valores numéricos con su equivalente en mes: april (4) august(8)
meses <- c("April", "May", "June", "July", "August")
names(meses) <- 4:8
# Reemplazo los números por los nombres de los meses en 'month'
data$month <- meses[as.factor(data$month)]
summary(data$month)

#day:
class(data$day)
#cambiamos el tipo a categorico
data$day <- as.factor(data$day)
class(data$day)

#order:
#cambio el tipo a categorico
data$order <- as.integer(data$order)
class(data$order)

#country:
#cambiamos a tipo factor y segun el diccionario brindado por la descripcion del data set

paises <- c("Australia", "Austria", "Belgium", "British Virgin Islands", "Cayman Islands",
            "Christmas Island", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia",
            "unidentified", "Faroe Islands", "Finland", "France", "Germany", "Greece", "Hungary",
            "Iceland", "India", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Mexico",
            "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russia", "San Marino",
            "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Arab Emirates",
            "United Kingdom", "USA", "biz (*.biz)" , "com (*.com)" , "int (*.int)", "net (*.net)" , "org (*.org)")
names(paises) <- 1:47

# Reemplazo los IDs por los nombres de los países en 'country'
data$country <- paises[as.factor(data$country)]
unique(data$country)

#elimino valores que no son nombres de países
data <- data %>% filter(country != "com (*.com)")
data <- data %>% filter(country != "net (*.net)")
data <- data %>% filter(country != "biz (*.biz)")
data <- data %>% filter(country != "org (*.org)")
data <- data %>% filter(country != "int (*.int)")
unique(data$country)

#session.ID:
data$session.ID <- as.factor(data$session.ID)
class(data$session.ID)

#  page.1..main.category.:
unique(data$page.1..main.category.)
categorias <- c("trousers", "skirts", "blouses", "sale")
names(categorias) <- 1:4
data$page.1..main.category. <- categorias[as.factor(data$page.1..main.category.)]

# page.2..clothing.model.:
class(data$page.2..clothing.model.)
data$page.2..clothing.model. <- as.factor(data$page.2..clothing.model.)

#colour:

colores <- c("beige", "black", "blue", "brown", "burgundy", "gray", "green",
             "navy blue", "of many colors", "olive", "pink", "red", "violet", 
             "white")
names(colores) <- 1:14

data$colour <- colores[as.factor(data$colour)]

#location:
class(data$location)
unique(data$location)
ubicacion <- c("top left", "top in the middle", "top right", 
               "bottom left", "bottom in the middle", "bottom right")
names(ubicacion) <- 1:6
data$location <- ubicacion[as.factor(data$location)]


# model.photography:
unique(data$model.photography)
modelo <- c("en face", "profile")
names(modelo) <- 1:2
data$model.photography <- modelo[as.factor(data$model.photography)]

#price:
class(data$price)

#price.2:
unique(data$price.2)
precio.media <- c("higher than the average", "cheaper than the average")
names(precio.media) <- 1:2
data$price.2 <- precio.media [as.factor(data$price.2)]

#convierto a factor
class(data$page)
data$page <- as.factor(data$page)

#renombro variables confusas con rename() de dplyr
data <- data %>%
  rename(main_category = page.1..main.category.,
         clothing_model = page.2..clothing.model.,
         model_photography = model.photography)

class(data$clothing_model)

#----
# b)
# Utilizando las gráficas que considere adecuadas, muestre: clicks por sesión, 
# sesiones por país, productos vistos por sesión y por categoría de producto

# ---clicks por sesión
#dataframe con la cantidad de clicks por sesión
clicks_sesion <- data %>%
  group_by(session.ID) %>%
  summarize(clicks = n()) %>%
  arrange(desc(clicks))
#ordenar de mayor a menor
clicks_sesion <- clicks_sesion[order(clicks_sesion$clicks, decreasing = T),]
#guardo los que tengan 'clicks' mayor a 100
clicks_sesion100 <- clicks_sesion[clicks_sesion$clicks > 100, ]

#grafico el resultado
ggplot(clicks_sesion100, aes(x = session.ID, y = clicks, group = 1)) +
  geom_line(color = "#458B00", size = 1) +
  geom_point(color = "#CD661D") +
  geom_text(aes(label = clicks), vjust = -0.2, size = 3) +
  ggtitle('Clicks por sesión') +
  labs(x = "Número de sesión", y = "Cantidad de clicks") +
  theme_minimal()

# ---sesiones por país
#dataframe con la cantidad de sesiones por país
sesiones_pais <- data %>%
  group_by(country) %>%
  summarize(sesiones = n()) %>%
  arrange(desc(sesiones))
#elimino de 'country' los valores Poland para ver otros valores,
#ya que Poland es un outlier en la distribución
sesiones_pais <- sesiones_pais[sesiones_pais$country != "Poland", ]

#ordeno de mayor a menor
sesiones_pais <- sesiones_pais[order(sesiones_pais$sesiones, decreasing = T),]
#guardo los que tengan 'sesiones' mayor a 150
sesiones100 <- sesiones_pais[sesiones_pais$sesiones > 150, ]

#grafico el resultado
p <- ggplot(sesiones100, aes(x = country, y = sesiones, group = 1)) +
  geom_line(color = "#458B00", size = 1) +
  geom_point(color = "#CD661D") +
  geom_text(aes(label = country), vjust = -0.2, size = 4) +
  ggtitle('Sesiones por país') +
  labs(x = "País", y = "Sesiones") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # Ajusta las etiquetas del eje x
  theme(axis.text.x = element_blank()) # Oculta las etiquetas duplicadas
  theme_minimal()

#guardo la imagen con cierto tamaño
ggsave("gg_sesionesPais.png", plot = p, width = 10, height = 5)
  
# ---productos vistos por sesión 
#dataframe con la cantidad de productos vistos por sesión
productos_vistos <- data %>%
  group_by(session.ID) %>%
  summarize(productos_vistos = n_distinct(clothing_model))

#ordenar de mayor a menor
productos_vistos <- productos_vistos[order(productos_vistos$productos_vistos, decreasing = T),]
productosVistos70 <- productos_vistos[productos_vistos$productos_vistos > 70, ]

ggplot(productosVistos70, aes(x = session.ID, y = productos_vistos, group = 1)) +
  geom_line(color = "#458B00", size = 1) +
  geom_point(color = "#CD661D") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + # Ajusta las etiquetas del eje x a vertical
  geom_text(aes(label = productos_vistos), vjust = -0.2, size = 3) +
  labs(title = "Productos vistos por sesión", 
       x = "Número de sesión", y = "Total de productos") +
  theme_minimal()

#por categoría de producto
#dataframe con la cantidad de productos vistos por categoría
productos_categoria <- data %>%
  group_by(main_category) %>%
  summarize(productos_vistos = n_distinct(clothing_model))

#hago un ggplot histograma
ggplot(data = productos_categoria) +
  geom_bar(data = productos_categoria, aes(x = main_category, y = productos_vistos, fill = main_category), 
           stat = "identity", width = 0.3) +
  scale_fill_manual(values = c("#A1D991", "#51B16B", "#137F3B", "#00501D")) +  # Asigna los colores específicos
  labs(title = "Productos vistos según categoría de producto", 
       x = "Categoría de producto", y = "Total de productos  visitados") +
  guides(fill = guide_legend(title = NULL)) +  # Quita el título de la leyenda
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#----
# c)
# ¿Cómo ha sido la evolución de los clicks de navegación a lo largo de los 
# meses estudiados?

# Cuento los clics por mes. No tengo en cuenta el año ya que es único.
monthly_clicks <- data %>%
  group_by(month) %>%
  summarize(clicks = n()) %>%
  arrange(month)

# Gráfico de la evolución de los clics por mes
ggplot(monthly_clicks, aes(x = month, y = clicks, group = 1)) +
  geom_line(color = "#8FBC8F", size = 1.5) +
  geom_point(color = "#9932CC", size = 2.5) +
  geom_text(aes(label = clicks), vjust = -0.2, size = 3) +
  labs(title = "Evolución de los clics de navegación por mes",
       x = "Fecha", y = "Número de clics") +
  #ordenar por meses
  scale_x_discrete(limits = c("April", "May", "June", "July", "August")) +
  theme_minimal()

april <- subset(data, data$month=="April")
may <- subset(data, data$month=="May")
june <- subset(data, data$month=="June")
july <- subset(data, data$month=="July")
august <- subset(data, data$month=="August")


#----
# d) Encuentre el número de transacciones e ítems (pensando cada sesión
#    como una transacción)

# transacciones: df con el ID de sesión/transacción y cantidad de clicks
transacciones <- data %>%
  group_by(session.ID) %>%
  summarise(cant_transacciones = n_distinct(order))

#cantidad de transacciones
nrow(transacciones) #23096 transacciones

# items:
items <- unique(data$clothing_model)
length(items) #217 items

#----
# e) Encuentre un conjunto de itemsets frecuentes para un soporte mínimo
# de 2% y con una longitud mínima de 2 ítems

#agrupo por session.ID y ordeno por data
dataItemFrec <- data %>% 
  group_by(session.ID) %>% 
  arrange(data) %>%
  # Eliminamos los eventos donde aparecen productos repetidos
  distinct(session.ID, clothing_model, .keep_all = TRUE) %>%
  summarise(Items = list(clothing_model)) %>%
  # Creamos un ItemID dentro de cada sessionID
  mutate(ItemID = row_number()) %>% 
  select(session.ID, ItemID, Items) %>% 
  ungroup ()

#creo un objeto transactions
transacciones <- as(dataItemFrec$Items, "transactions")

itemsFrec <- apriori(transacciones, 
                  parameter = list(support = 0.02,
                                   minlen = 2,
                                   target = "frequent itemsets"),
                  control = list(verbose = F))
itemsFrec
#cantidad de itemsets frecuentes:  66 itemsets
inspect(sort(itemsFrec, decreasing = T, by = "support")[1:10])

#paso resultados a un dataframe
itemsets_frecuentes <- as(sort(itemsFrec, decreasing = T, by = "support")[1:10],
                          "data.frame")

# f) Encuentre las reglas de asociación para los datos de navegación
# correspondientes a Polonia, en la categoría “blusas”. Para un soporte
# mínimo de 2% y una confianza de 20%. Muestre las 10 reglas de mayor
# soporte.

# 'data' es el df original del csv
data_poland_blusas <- data %>%
  filter(country == "Poland", main_category == "blouses")

#selecciono del df 'data_poland_blusas' las columnas clothing_model y session.ID
data_poland_blusas <- data_poland_blusas %>%
  select(session.ID, clothing_model) 

# creo el objeto transacciones con la funcion transactions
transaccionespb <- transactions(data_poland_blusas, format = "long")
inspect(transaccionespb[1:10])

# genero las reglas de asociación
reglas_poland <- apriori(transaccionespb, 
                         parameter = list(supp = 0.02, 
                                          conf = 0.2,
                                          target = "rules"))
reglas_poland # Me da 16 reglas

# ordeno por mayor soporte y muestro las diez primeras
reglas_significativas_pb_ordenadas <- sort(reglas_poland, by = "support", decreasing = TRUE)

inspect(head(reglas_significativas_pb_ordenadas,10))

# g) Encuentre las reglas para la República Checa, en la misma categoría del
# ítem anterior, pero para un soporte mínimo de 4% y una confianza de
# 25%. Muestre las 10 reglas de mayor soporte.

# filtro datos para República Checa y categoría "blusas" con filter
data_checa_blusas <- data %>%
  filter(country == "Czech Republic", main_category == "blouses")

# convierto datos filtrados a transacciones
data_checa_blusas <- data_checa_blusas %>%
  select(session.ID, clothing_model)

# creo el objeto transacciones
transaccioneschecab <- transactions(data_checa_blusas, format = 'long')

# genero las reglas de asociación
reglas_checa <- apriori(transaccioneschecab, 
                               parameter = list(supp = 0.04, 
                                                conf = 0.25,
                                                target = "rules"))

# filtro las reglas para eliminar las que no tienen antecedentes
resultado_checa_blusas <- subset(reglas_checa, size(lhs(reglas_checa)) > 0)

# ordeno por mayor soporte y muestro las diez primeras
resultado_checa_ord <- sort(resultado_checa_blusas, by = "support", decreasing = TRUE)
inspect(head(resultado_checa_ord,10))

#----
# h) Encuentre las secuencias más frecuentes que tienen más de un
# elemento (ítem) y un soporte mayor a 3%.
data2 <- data %>%
  group_by(session.ID) %>%
    arrange(data$order) %>%
    #eliminamos registros donde se repiten los productos
    #creamos un itemID dentro de cada sessionID
    mutate(itemID = row_number()) %>%
    #seleccionamos las columnas y desagrupamos
    select(session.ID, date, itemID, clothing_model) %>%
    ungroup() %>%
    #convierto a factor el ID session y el clothing_model
    mutate(across(.cols = c('session.ID', 'clothing_model'), .f = as.factor))

#con arrange ordeno por session.ID
data2 <- data2 %>% arrange(session.ID)
  
#genero un objeto 'transactions' 
#renombro 'clothing_model' a 'items' para que lo entienda el algoritmo cSPADE
transaccionesSeq <- as(data2 %>% transmute(items = clothing_model), "transactions")

#renombro las columnas
transactionInfo(transaccionesSeq)$sequenceID <- data2$session.ID
transactionInfo(transaccionesSeq)$eventID <- data2$itemID

#acomodo los nombres de los items
itemLabels(transaccionesSeq) <- str_replace_all(itemLabels(transaccionesSeq),"items=", "")

#veo las transacciones
inspect(head(transaccionesSeq, 10))

#creo un objeto 'cspade' con soporte 3%
secuencias <- cspade(transaccionesSeq,
                     parameter = list(support = 0.03),
                     control = list(verbose = FALSE))

#ordenar por soporte
secuencias <- sort(secuencias, by = "support", decreasing = T)
inspect(secuencias[1:10])

#paso a dataframe
secuencias.df <- secuencias
secuencias.df <- as(secuencias.df, "data.frame")

# Agregamos el tamaño de cada secuecnia
secuencias.df$cant_elementos <- (str_count(secuencias.df$sequence, ",") + 1)
secuencias_filtradas <- subset(secuencias.df, secuencias.df$cant_elementos == 2) 
secuencias_filtradas

#--------------------------------------FIN-----------------------------------



