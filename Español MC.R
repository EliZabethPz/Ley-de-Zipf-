###ELaboración propia con colaboración del Dr. Niels Martínez Guevara y Lic. Omar Alexandro Texon Olguín###

#install.packages("NLP") 
#install.packages("tm") # Para stopwords en español
#install.packages("tidyverse")
#install.packages("tidytext")
#install.packages("data.table")
#install.packages("readxl")
#install.packages("knitr")
#install.packages("pdftools")
#install.packages("scales")
#install.packages("gridExtra") 
#install.packages("stringi") # Para quitar tildes
#install.packages("RColorBrewer") # Para nube de palabras
#install.packages("MASS")
#install.packages("wordcloud") # Para nube de palabras

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
setwd("D:/Saraí Elizabeth Pérez González_Tesis/Articulos de investigación/Memorias de congreso_2023/ESPAÑOL/PDF")
setwd("D:/Saraí Elizabeth Pérez González_Tesis/Articulos de investigación/Revistas/ESPAÑOL/PDF")
# Cargamos datos

df1<-pdf_text("Alofonía.pdf")
df2<-pdf_text("Bajamotivación.pdf")
df3<-pdf_text("Biotecnología.pdf")
df4<-pdf_text("Defensanacional.pdf")
df5<-pdf_text("Ecuatorio.pdf")
df6<-pdf_text("Mixteco.pdf")
df7<-pdf_text("Mobiliário.pdf")
df8<-pdf_text("Morfológica.pdf")
df9<-pdf_text("MVirtual.pdf")
df10<-pdf_text("PazyDemo.pdf")
df11<-pdf_text("Pedagogíco.pdf")
df12<-pdf_text("Quinua.pdf")
df13<-pdf_text("Totonaco.pdf")
df14<-pdf_text("TranscripciónT.pdf")
df15<-pdf_text("TutoríaEN.pdf")

mode(df1)

# Convertimos el texto a formato estructurado
df1<- tibble(text = df1)
df2<- tibble(text = df2)
df3<- tibble(text = df3)
df4<- tibble(text = df4)
df5<- tibble(text = df5)
df6<- tibble(text = df6)
df7<- tibble(text = df7)
df8<- tibble(text = df8)
df9<- tibble(text = df9)
df10<- tibble(text = df10)
df11<- tibble(text = df11)
df12<- tibble(text = df12)
df13<- tibble(text = df13)
df14<- tibble(text = df14)
df15<- tibble(text = df15)



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

Limpiar_2<-function(palabras){
  
  # Para las palabras en Español
  stop_words_spanish <- tibble(line_Number = 1:311, 
                               word =c(stopwords("spanish"),"ello","va","s")) # Incluimos algunas letras que se repiten 
  
  palabras<-anti_join(palabras, stop_words_spanish)
  
  # Para eliminar problemas con función Limpiar_1. 
  # No elimina palabras con una sola vocal. 
  # Lo hago tras eliminar las stopwords
  
  palabras<-palabras %>%
    filter(str_detect(word,"[a-z]")) %>% # Selecciono solo palabras
    filter(!str_detect(word,"^[aeiou]")) %>% # Elimino cadenas con una sola vocal
    filter(!str_detect(word,"^[^aeiou]+$")) # Elimino cadenas con una sola consonante
  
  return(palabras)
}




df1<-unnest_tokens(df1,word, text) # admin.pdf
df2<-unnest_tokens(df2,word, text) # Baja motivación
df3<-unnest_tokens(df3,word, text) # Biotecnología y Bioingeniería
df4<-unnest_tokens(df4,word, text) #Caballero_CILLA_VII
df5<-unnest_tokens(df5,word, text) # Congreso Cientifico de la quinua
df6<-unnest_tokens(df6,word, text) # Escobar_CILLA_VIII 2023
df7<-unnest_tokens(df7,word, text) # HistoriaMobiliario 
df8<-unnest_tokens(df8,word, text) # Levy_y_Hernández-Green_CILLA_VIII
df9<-unnest_tokens(df9,word, text) # Memorias al congreso
df10<-unnest_tokens(df10,word, text) # Memoriasdecongresosversusartculoscientficos
df11<-unnest_tokens(df11,word, text) # MemoriasVirtual
df12<-unnest_tokens(df12,word, text) # paz democracia y desarrollo
df13<-unnest_tokens(df13,word, text) # Peters-CILLA_VIII
df14<-unnest_tokens(df14,word, text) # Sullivant_CILLA_VIII
df15<-unnest_tokens(df15,word, text) # Tutoría en la formación inicial


df1$word<-Limpiar_1(texto=df1$word)
df2$word<-Limpiar_1(texto=df2$word)
df3$word<-Limpiar_1(texto=df3$word)
df4$word<-Limpiar_1(texto=df4$word)
df5$word<-Limpiar_1(texto=df5$word)
df6$word<-Limpiar_1(texto=df6$word)
df7$word<-Limpiar_1(texto=df7$word)
df8$word<-Limpiar_1(texto=df8$word)
df9$word<-Limpiar_1(texto=df9$word)
df10$word<-Limpiar_1(texto=df10$word)
df11$word<-Limpiar_1(texto=df11$word)
df12$word<-Limpiar_1(texto=df12$word)
df13$word<-Limpiar_1(texto=df13$word)
df14$word<-Limpiar_1(texto=df14$word)
df15$word<-Limpiar_1(texto=df15$word)

df1<-Limpiar_2(palabras=df1)
df2<-Limpiar_2(palabras=df2)
df3<-Limpiar_2(palabras=df3)
df4<-Limpiar_2(palabras=df4)
df5<-Limpiar_2(palabras=df5)
df6<-Limpiar_2(palabras=df6)
df7<-Limpiar_2(palabras=df7)
df8<-Limpiar_2(palabras=df8)
df9<-Limpiar_2(palabras=df9)
df10<-Limpiar_2(palabras=df10)
df11<-Limpiar_2(palabras=df11)
df12<-Limpiar_2(palabras=df12)
df13<-Limpiar_2(palabras=df13)
df14<-Limpiar_2(palabras=df14)
df15<-Limpiar_2(palabras=df15)



Frecuencia<-bind_rows(
    mutate(df1,line= row_number(), `Plan`="Alofonía"),
    mutate(df2,line= row_number(), `Plan`="Bajamotivación"),
    mutate(df3,line= row_number(), `Plan`="Biotecnología"),
    mutate(df4,line= row_number(), `Plan`="Defensanacional"),
    mutate(df5,line= row_number(), `Plan`="Ecuatorio"),
    mutate(df6,line= row_number(), `Plan`="Mixteco"),
    mutate(df7,line= row_number(), `Plan`="Mobiliário"),
    mutate(df8,line= row_number(), `Plan`="Morfológica"),
    mutate(df9,line= row_number(), `Plan`="MVirtual"),
    mutate(df10,line= row_number(), `Plan`="PazyDemo"),
    mutate(df11,line= row_number(), `Plan`="Pedagogíco"),
    mutate(df12,line= row_number(), `Plan`="Quinua"),
    mutate(df13,line= row_number(), `Plan`="Totonaco"),
    mutate(df14,line= row_number(), `Plan`="TranscripciónT"),
    mutate(df15,line= row_number(), `Plan`="TutoríaEN")) %>% 
  count(Plan, word) %>%
  group_by(Plan) %>%
  mutate(proportion = n / sum(n)) %>% 
  #dplyr::select(-n) %>% # Para eliminar el problema con el select de la libería Mass
  spread(Plan, proportion) %>% 
  gather(Plan, proportion, `Alofonía`:`TutoríaEN`)

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
       subtitle = "Memorias de congreso en Español",
       caption = "Fuente: Elaboración propia")+
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        plot.caption = element_text(hjust = -0.03))


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
colnames(word_freq_df1)=c("Word","Frequency","MCE")

x11()
ggplot(word_freq_df1, aes(x = rank, y = Frequency, color = MCE)) +
  geom_line() +
  scale_x_continuous(trans = "log", breaks = 10^seq(0, log10(nrow(word_freq_df1)), by = 1)) +
  scale_y_log10() +
  labs(x = "Rango (log)",
       y = "Frecuencia (log)") +
  theme_minimal()+
  labs(title = "Ley de Zipf con Conteo de Palabras",
       subtitle = "Memorias de congreso en español",
       caption = "Fuente: Elaboración propia")+
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        plot.caption = element_text(hjust = -0.03))
