###ELaboración propia con colaboración del Dr. Niels Martínez Guevara y Lic. Omar Alexandro Texon Olguín###

install.packages("NLP") 
install.packages("tm") # Para stopwords en español
install.packages("tidyverse")
install.packages("tidytext")
install.packages("data.table")
install.packages("readxl")
install.packages("knitr")
install.packages("pdftools")
install.packages("scales")
install.packages("gridExtra") 
install.packages("stringi") # Para quitar tildes
install.packages("RColorBrewer") # Para nube de palabras
install.packages("MASS")
install.packages("wordcloud") # Para nube de palabras

options(scipen = 999999)

library(NLP) 
library(tm) # Para stopwords en español
library(tidyverse)
library(tidytext)
library(data.table)
library(readxl)
library(knitr)
library(pdftools)
library(scales)
library(gridExtra) 
library(stringi) # Para quitar tildes
library(RColorBrewer) # Para nube de palabras
library(MASS)
library(wordcloud) # Para nube de palabras

#setwd("C:/Users/alex-/Desktop/Tesis/Articulos de investigación/Memorias de congreso_2023/ESPAÑOL")
#setwd("C:/Users/alex-/Desktop/Tesis/Articulos de investigación/Memorias de congreso_2023/ESPAÑOL/PDF")
setwd("D:/Saraí Elizabeth Pérez González_Tesis/Articulos de investigación/Memorias de congreso_2023/INGLES/PDF")


# Cargamos datos

df1<-pdf_text("Always.pdf")
df2<-pdf_text("ForescastingC.pdf")
df3<-pdf_text("ModelsL.pdf")
df4<-pdf_text("Problems.pdf")
df5<-pdf_text("StatistiscsinA.pdf")


mode(df1)

# Convertimos el texto a formato estructurado
df1<- tibble(text = df1)
df2<- tibble(text = df2)
df3<- tibble(text = df3)
df4<- tibble(text = df4)
df5<- tibble(text = df5)


View(df1)


Limpiar_1 <- function(texto){
  # El orden de la limpieza no es aleatorio
  
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(texto,"http\\S*", "")
  
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  
  # Eliminación de tildes 
  nuevo_texto <- stri_trans_general(nuevo_texto,"Latin-ASCII")
  
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", "")
  
  return(nuevo_texto)
}




Limpiar_2en<-function(palabras){
  
  # Para las palabras en Ingles
  stop_words_Ing <- tibble(line_Number = 1:1298, 
                               word =c(stopwords("en", source = "stopwords-iso"))) # Incluimos algunas letras que se repiten 
  
  palabras<-anti_join(palabras, stop_words_Ing)
  
  # Para eliminar problemas con función Limpiar_1. 
  # No elimina palabras con una sola vocal. 
  # Lo hago tras eliminar las stopwords
  
  palabras<-palabras %>%
    filter(str_detect(word,"[a-z]")) %>% # Selecciono solo palabras
    filter(!str_detect(word,"^[aeiou]")) %>% # Elimino cadenas con una sola vocal
    filter(!str_detect(word,"^[^aeiou]+$")) # Elimino cadenas con una sola consonante
  
  return(palabras)
}

df1

df1<-unnest_tokens(df1,word, text) # 
df2<-unnest_tokens(df2,word, text) # 
df3<-unnest_tokens(df3,word, text) # 
df4<-unnest_tokens(df4,word, text) #
df5<-unnest_tokens(df5,word, text) # 


df1$word<-Limpiar_1(texto=df1$word)
df2$word<-Limpiar_1(texto=df2$word)
df3$word<-Limpiar_1(texto=df3$word)
df4$word<-Limpiar_1(texto=df4$word)
df5$word<-Limpiar_1(texto=df5$word)


df1<-Limpiar_2en(palabras=df1)
df2<-Limpiar_2en(palabras=df2)
df3<-Limpiar_2en(palabras=df3)
df4<-Limpiar_2en(palabras=df4)
df5<-Limpiar_2en(palabras=df5)


Frecuencia<-bind_rows(
  mutate(df1,line= row_number(), `Plan`="Always"),
  mutate(df2,line= row_number(), `Plan`="ForescastingC"),
  mutate(df3,line= row_number(), `Plan`="ModelsL"),
  mutate(df4,line= row_number(), `Plan`="Problems"),
  mutate(df5,line= row_number(), `Plan`="StatistiscsinA")) %>% 
  count(Plan, word) %>%
  group_by(Plan) %>%
  mutate(proportion = n / sum(n)) %>% 
  #dplyr::select(-n) %>% # Para eliminar el problema con el select de la libería Mass
  spread(Plan, proportion) %>% 
  gather(Plan, proportion, `Always`:`StatistiscsinA`)

Frecuencia1=na.omit(Frecuencia)

kable(Frecuencia1)

names(Frecuencia1)


x11()
Frecuencia1 %>% 
  group_by(Plan) %>% 
  summarise(conteo = n()) %>% 
  ggplot(aes(y=reorder(Plan,conteo),x=conteo, color=Plan, fill=Plan))+ 
  labs(x="Plan", y =" Número de Palabras")+
  geom_col() + 
  theme_bw()+
  geom_text(aes(label=conteo), size=5, color="black", vjust = -0.2)+
  labs(title = "Conteo de Palabras",
       subtitle = "Memorias de congreso en inglés",
       caption = "Fuente: Elaboración propia")+
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        plot.caption = element_text(hjust = -0.03))





################################################
################################################
################################################
################################################
#################LZ#################
################################################
################################################
################################################
################################################
################################################
################################################




# Contar las frecuencias de las palabras
word_freq <- table(words)

# Crear un data frame con las frecuencias
word_freq_df <- data.frame(Word = Frecuencia1$word, Frequency = Frecuencia1$n,grupo=Frecuencia1$Plan)

#word_freq_df = data.frame(Word = names(word_freq), Frequency = as.numeric(word_freq))


# Ordenar el data frame por frecuencia en orden descendente
word_freq_df <- word_freq_df[order(-word_freq_df$Frequency), ]

# Crear un rango de valores para el eje X
rank <- seq_along(word_freq_df$Word)


# Graficar la Ley de Zipf con un gráfico de línea
x11()
ggplot(word_freq_df, aes(x = rank, y = Frequency, color = grupo)) +
  geom_line() +
  scale_x_continuous(trans = "log", breaks = 10^seq(0, log10(nrow(word_freq_df)), by = 1)) +
  scale_y_log10() +
  labs(x = "Rank (log)", y = "Frecuencia (log)") +
  ggtitle("Ley de Zipf con Conteo de Palabras") +
  theme_minimal()


word_freq_df1=word_freq_df
colnames(word_freq_df1)=c("Word","Frequency","MCIG")

x11()
ggplot(word_freq_df1, aes(x = rank, y = Frequency, color = MCIG)) +
  geom_line() +
  scale_x_continuous(trans = "log", breaks = 10^seq(0, log10(nrow(word_freq_df1)), by = 1)) +
  scale_y_log10() +
  labs(x = "Rango (log)",
       y = "Frecuencia (log)") +
  theme_minimal()+
  labs(title = "Ley de Zipf con Conteo de Palabras",
       subtitle = "Memorias de congreso en inglés",
       caption = "Fuente: Elaboración propia")+
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        plot.caption = element_text(hjust = -0.03))


