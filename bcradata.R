##################################################################

rm(list=ls())

library(dplyr)
library("installr")
library("rio")
library(tidyverse)
library(rvest)
library(openxlsx)
library(scales) 
library(data.table)

#directorio <- "/Users/IDECOR/Documents/Code/BCRA/"
directorio <- "/Users/stefa/Documents/Code/BCRA/"
setwd(directorio); getwd()


########### Tipo de cambio: Dólar blue y dólar oficial ###########

tc <- read.csv("https://api.bluelytics.com.ar/v2/evolution.csv"); names(tc)

tc.blue <- subset(tc, tc$type == "Blue")
tc.of <- subset(tc, tc$type == "Oficial")

tc.blue <- rename(tc.blue, Fecha = day, Comprador = value_buy, Vendedor = value_sell); tc.blue 
tc.of <- rename(tc.of, Fecha = day, Comprador = value_buy, Vendedor = value_sell); tc.of 

summary(tc.blue)
summary(tc.of)

write.xlsx(tc.blue, "dolarblue.xlsx")
write.xlsx(tc.of, "dolaroficial.xlsx")

# # 

head(tc.blue); head(tc.of)

graph <- left_join(tc.blue, tc.of, by = "Fecha"); graph

graph <- graph %>%
  select(Fecha, Vendedor.x, Vendedor.y) %>%
  rename(Blue = Vendedor.x, Oficial = Vendedor.y) %>%
  mutate(Fecha = as.POSIXct(Fecha, format = "%Y-%m-%d"))

graph <- graph %>% 
  filter(year(Fecha) >= 2019)

graph$Brecha <- graph$Blue / graph$Oficial
head(graph)
graph$Brecha <- percent(graph$Brecha, accuracy = 0.1)
head(graph)

ggplot(data = graph, aes(x = Fecha)) + 
  geom_line(aes(y = Blue), color = "blue") + 
  geom_line(aes(y = Oficial), color = "red") +
  ggtitle("Dólar Blue vs Dólar Oficial") + 
  ylab("Valor dólar") +
  scale_x_datetime(labels = scales::date_format(format = "%Y-%m-%d", tz = "UTC", locale = NULL))

#ggplot(data = graph, aes(x = Fecha)) + 
#  geom_bar(aes(fill = Brecha), width = 30, color = "lightblue") + 
#  ggtitle("Brecha cambiaria (en %)") + 
#  ylab("Brecha") +
#  scale_x_datetime(labels = scales::date_format(format = "%Y-%m-%d", tz = "UTC", locale = NULL))

rm(list=ls())


# Tipo de Cambio Minorista de Referencia de la Ciudad de Buenos Aires

source("automat_fun.R")
concotiza(directorio)
load("tc.Rda"); tc
head(tc)


############################## LELIQs ############################

url <- "https://www.bcra.gob.ar/PublicacionesEstadisticas/Historial-Leliq.asp"
file <- "/Users/IDECOR/Documents/Code/BCRA/leliqdata.html"
download.file(url, file)

leliq <- import("leliqdata.html")
convert("leliqdata.html", "leliqdata.csv")

leliq <- read.csv("/Users/IDECOR/Documents/Code/BCRA/leliqdata.csv", header = T, sep = ",", dec = ",")
leliq <- as.data.frame(leliq)
head(leliq)

leliq <- rename(leliq, Fecha_Subasta = V1, 
                Valor_nominal_pesos = V2, 
                Vto = V3, 
                Plazo_dias = V4, 
                Tasa_min = V5, 
                Tasa_max = V6, 
                Tasa_prom = V7); names(leliq)

options(scipen = 999) # Remueve notación científica
summary(leliq)

leliq$Valor_nominal_pesos <- as.numeric(gsub(",", ".", gsub("\\.", "", leliq$Valor_nominal_pesos))); class(leliq$Valor_nominal_pesos)

summary(leliq$Valor_nominal_pesos)
summary(leliq)
head(leliq)

write.xlsx(leliq, "leliqdata.xlsx")

#graph = leliq  %>% 
#  group_by(Vto)  %>% 
#  summarise(LELIQs_pesos = sum(Valor_nominal_pesos))
#graph

#ggplot(data = graph, 
#       aes(x = Vto, y = LELIQs_pesos)) + geom_line() + ggtitle("LELIQs (en $ trillones)")
  #scale_y_continuous(labels = scales::label_number_si()) 

rm(list=ls())


######################### AGREGADOS MONETARIOS #########################

url <- "https://www.bcra.gob.ar/Pdfs/PublicacionesEstadisticas/series.xlsm"
file <- "/Users/IDECOR/Documents/Code/BCRA/series.xlsm"
download.file(url, file)

series <- fread('https://www.bcra.gob.ar/Pdfs/PublicacionesEstadisticas/series.xlsm')

series <- readxl::read_excel("/Users/IDECOR/Documents/Code/BCRA/series.xlsm")





