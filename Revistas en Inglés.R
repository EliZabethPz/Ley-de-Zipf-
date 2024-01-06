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
install.packages("stopwords") # Para nube de palabras

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
library(stopwords)

#setwd("C:/Users/alex-/Desktop/Tesis/Articulos de investigación/Memorias de congreso_2023/ESPAÑOL")
#setwd("C:/Users/alex-/Desktop/Tesis/Articulos de investigación/Memorias de congreso_2023/ESPAÑOL/PDF")
setwd("D:/Saraí Elizabeth Pérez González_Tesis/Articulos de investigación/Revistas/INGLES")


# Cargamos datos
#df1<-pdf_text("admin.pdf")
#df2<-pdf_text("Baja motivación.pdf")
#df3<-pdf_text("Biotecnología y Bioingeniería.pdf")
#df4<-pdf_text("Caballero_CILLA_VIII.pdf")

df1<-pdf_text("Education.pdf")
df2<-pdf_text("Factors.pdf")
df3<-pdf_text("Gameplay.pdf")
df4<-pdf_text("Infres Oaxaca.pdf")
df5<-pdf_text("Interpreting.pdf")
df6<-pdf_text("Literature review.pdf")
df7<-pdf_text("Networking.pdf")
df8<-pdf_text("Neural machine.pdf")
df9<-pdf_text("Podcasts.pdf")
df10<-pdf_text("Robots.pdf")
df11<-pdf_text("Smartwatches.pdf")
df12<-pdf_text("Systematic review.pdf")
df13<-pdf_text("Technology.pdf")
df14<-pdf_text("Virtual reality.pdf")
df15<-pdf_text("Website.pdf")

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

View(df4)


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
  
  # Para las palabras en Español
  stop_words_ENG <- tibble(line_Number = 1:1298, 
                               word =c(stopwords("en", source = "stopwords-iso"))) # Incluimos algunas letras que se repiten 
  
  palabras<-anti_join(palabras, stop_words_ENG)
  
  # Para eliminar problemas con función Limpiar_1. 
  # No elimina palabras con una sola vocal. 
  # Lo hago tras eliminar las stopwords
  
  palabras<-palabras %>%
    filter(str_detect(word,"[a-z]")) %>% # Selecciono solo palabras
    filter(!str_detect(word,"^[aeiou]")) %>% # Elimino cadenas con una sola vocal
    filter(!str_detect(word,"^[^aeiou]+$")) # Elimino cadenas con una sola consonante
  
  return(palabras)
}



df1<-unnest_tokens(df1,word, text) # 
df2<-unnest_tokens(df2,word, text) # 
df3<-unnest_tokens(df3,word, text) # 
df4<-unnest_tokens(df4,word, text) #
df5<-unnest_tokens(df5,word, text) # 
df6<-unnest_tokens(df6,word, text) # 
df7<-unnest_tokens(df7,word, text) # 
df8<-unnest_tokens(df8,word, text) # 
df9<-unnest_tokens(df9,word, text) #
df10<-unnest_tokens(df10,word, text) # 
df11<-unnest_tokens(df11,word, text) # 
df12<-unnest_tokens(df12,word, text) # 
df13<-unnest_tokens(df13,word, text) # 
df14<-unnest_tokens(df14,word, text) #
df15<-unnest_tokens(df15,word, text) # 


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

df1<-Limpiar_2en(palabras=df1)
df2<-Limpiar_2en(palabras=df2)
df3<-Limpiar_2en(palabras=df3)
df4<-Limpiar_2en(palabras=df4)
df5<-Limpiar_2en(palabras=df5)
df6<-Limpiar_2en(palabras=df6)
df7<-Limpiar_2en(palabras=df7)
df8<-Limpiar_2en(palabras=df8)
df9<-Limpiar_2en(palabras=df9)
df10<-Limpiar_2en(palabras=df10)
df11<-Limpiar_2en(palabras=df11)
df12<-Limpiar_2en(palabras=df12)
df13<-Limpiar_2en(palabras=df13)
df14<-Limpiar_2en(palabras=df14)
df15<-Limpiar_2en(palabras=df15)



Frecuencia<-bind_rows(
  mutate(df1,line= row_number(), `Plan`="Education"),
  mutate(df2,line= row_number(), `Plan`="Factors"),
  mutate(df3,line= row_number(), `Plan`="Gameplay"),
  mutate(df4,line= row_number(), `Plan`="Infres Oaxaca"),
  mutate(df5,line= row_number(), `Plan`="InterpretingR"),
  mutate(df6,line= row_number(), `Plan`="Literature review"),
  mutate(df7,line= row_number(), `Plan`="Networking"),
  mutate(df8,line= row_number(), `Plan`="Neural machine"),
  mutate(df9,line= row_number(), `Plan`="Podcast"),
  mutate(df10,line= row_number(), `Plan`="Robots"),
  mutate(df11,line= row_number(), `Plan`="Smartwatches"),
  mutate(df12,line= row_number(), `Plan`="Systematic review"),
  mutate(df13,line= row_number(), `Plan`="Technology"),
  mutate(df14,line= row_number(), `Plan`="Virtual reality"),
  mutate(df15,line= row_number(), `Plan`="Website")) %>% 
  count(Plan, word) %>%
  group_by(Plan) %>%
  mutate(proportion = n / sum(n)) %>% 
  #dplyr::select(-n) %>% # Para eliminar el problema con el select de la libería Mass
  spread(Plan, proportion) %>% 
  gather(Plan, proportion, `Education`:`Website`)

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
       subtitle = "Articulos de Revistas en ingles",
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
colnames(word_freq_df1)=c("Word","Frequency","ARIEN")

x11()
ggplot(word_freq_df1, aes(x = rank, y = Frequency, color = ARIEN)) +
  geom_line() +
  scale_x_continuous(trans = "log", breaks = 10^seq(0, log10(nrow(word_freq_df1)), by = 1)) +
  scale_y_log10() +
  labs(x = "Rango (log)",
       y = "Frecuencia (log)") +
  theme_minimal()+
  labs(title = "Ley de Zipf con Conteo de Palabras",
       subtitle = "Articulos de revistas en inglés",
       caption = "Fuente: Elaboración propia")+
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        plot.caption = element_text(hjust = -0.03))