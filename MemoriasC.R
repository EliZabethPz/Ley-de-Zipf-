###ELaboración propia con colaboración del Dr. Niels Martínez Guevara y Lic. Omar Alexandro Texon Olguín###

### ARTÍCULOS DE MEMORIAS DE CONGRESOS ###

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


setwd("D:/Saraí Elizabeth Pérez González_Tesis/Articulos de investigación/Memorias de congreso_2023/TODOS")

####CARGAR DATOS###

df1=pdf_text("Alofonía.pdf")
df2=pdf_text("Bajamotivación.pdf")
df3=pdf_text("Biotecnología.pdf")
df4=pdf_text("Defensanacional.pdf")
df5=pdf_text("Ecuatorio.pdf")
df6=pdf_text("Mixteco.pdf")
df7=pdf_text("Mobiliário.pdf")#
df8=pdf_text("Morfológica.pdf")
df9=pdf_text("MVirtual.pdf")#
df10=pdf_text("PazyDemo.pdf")
df11=pdf_text("Pedagogíco.pdf")
df12=pdf_text("Quinua.pdf")
df13=pdf_text("Totonaco.pdf")
df14=pdf_text("TranscripciónT.pdf")
df15=pdf_text("TutoríaEN.pdf")
df16=pdf_text("Always.pdf")
df17=pdf_text("ForescastingC.pdf")
df18=pdf_text("ModelsL.pdf")
df19=pdf_text("Problems.pdf")
df20=pdf_text("StatistiscsinA.pdf")
df21=pdf_text("contemporaneo.pdf")
df22=pdf_text("Genealógica.pdf")
df23=pdf_text("UmaMetodologia.pdf")#
df24=pdf_text("Pedagógica.pdf")
df25=pdf_text("Pragmática.pdf")


df1=tibble(text=df1)
df2=tibble(text=df2)
df3=tibble(text=df3)
df4=tibble(text=df4)
df5=tibble(text=df5)
df6=tibble(text=df6)
df7=tibble(text=df7)
df8=tibble(text=df8)
df9=tibble(text=df9)
df10=tibble(text=df10)
df11=tibble(text=df11)
df12=tibble(text=df12)
df13=tibble(text=df13)
df14=tibble(text=df14)
df15=tibble(text=df15)
df16=tibble(text=df16)
df17=tibble(text=df17)
df18=tibble(text=df18)
df19=tibble(text=df19)
df20=tibble(text=df20)
df21=tibble(text=df21)
df22=tibble(text=df22)
df23=tibble(text=df23)
df24=tibble(text=df24)
df25=tibble(text=df25)




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



Limpiar_2en<-function(palabras){
  
  # Para las palabras en Español
  stop_words_spanish <- tibble(line_Number = 1:1298, 
                               word =c(stopwords("en", source = "stopwords-iso"))) # Incluimos algunas letras que se repiten 
  
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



Limpiar_2po<-function(palabras){
  
  # Para las palabras en Español
  stop_words_spanish <- tibble(line_Number = 1:209, 
                               word =c(stopwords("portuguese"),"que","como","são","até","ser","é")) # Incluimos algunas letras que se repiten 
  
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
df16<-unnest_tokens(df16,word, text) # 
df17<-unnest_tokens(df17,word, text) # 
df18<-unnest_tokens(df18,word, text) # 
df19<-unnest_tokens(df19,word, text) # 
df20<-unnest_tokens(df20,word, text) # 
df21<-unnest_tokens(df21,word, text) # 
df22<-unnest_tokens(df22,word, text) # 
df23<-unnest_tokens(df23,word, text) # 
df24<-unnest_tokens(df24,word, text) # 
df25<-unnest_tokens(df25,word, text) # 


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
df16$word<-Limpiar_1(texto=df16$word)
df17$word<-Limpiar_1(texto=df17$word)
df18$word<-Limpiar_1(texto=df18$word)
df19$word<-Limpiar_1(texto=df19$word)
df20$word<-Limpiar_1(texto=df20$word)
df21$word<-Limpiar_1(texto=df21$word)
df22$word<-Limpiar_1(texto=df22$word)
df23$word<-Limpiar_1(texto=df23$word)
df24$word<-Limpiar_1(texto=df24$word)
df25$word<-Limpiar_1(texto=df25$word)


df1<-Limpiar_2(palabras=df1)
df2<-Limpiar_2(palabras=df2)
df3<-Limpiar_2(palabras=df3)
df4<-Limpiar_2(palabras=df4)
df5<-Limpiar_2(palabras=df5)
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
df16<-Limpiar_2en(palabras=df16)
df17<-Limpiar_2en(palabras=df17)
df18<-Limpiar_2en(palabras=df18)
df19<-Limpiar_2en(palabras=df19)
df20<-Limpiar_2en(palabras=df20)
df21<-Limpiar_2po(palabras=df21)
df22<-Limpiar_2po(palabras=df22)
df23<-Limpiar_2po(palabras=df23)
df24<-Limpiar_2po(palabras=df24)
df25<-Limpiar_2po(palabras=df25)




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
  mutate(df15,line= row_number(), `Plan`="TutoríaEN"),
  mutate(df16,line= row_number(), `Plan`="Always"),
  mutate(df17,line= row_number(), `Plan`="ForescastingC"),
  mutate(df18,line= row_number(), `Plan`="ModelsL"),
  mutate(df19,line= row_number(), `Plan`="Problems"),
  mutate(df21,line= row_number(), `Plan`="Trabalho"),
  mutate(df22,line= row_number(), `Plan`="Genealógica"),
  mutate(df23,line= row_number(), `Plan`="Humanismo"),
  mutate(df24,line= row_number(), `Plan`="Pedagógica"),
  mutate(df25,line= row_number(), `Plan`="Pragmática"),
  mutate(df20,line= row_number(), `Plan`="StatistiscsinA")) %>% 
  count(Plan, word) %>%
  group_by(Plan) %>%
  mutate(proportion = n / sum(n)) %>% 
  #dplyr::select(-n) %>% # Para eliminar el problema con el select de la libería Mass
  spread(Plan, proportion) %>% 
  gather(Plan, proportion, `Alofonía`:`StatistiscsinA`)

Frecuencia1=na.omit(Frecuencia)
Frecuencia1

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
       subtitle = "Memorias de congreso",
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
colnames(word_freq_df1)=c("Word","Frequency","ARI")

x11()
ggplot(word_freq_df1, aes(x = rank, y = Frequency, color = ARI)) +
  geom_line() +
  scale_x_continuous(trans = "log", breaks = 10^seq(0, log10(nrow(word_freq_df1)), by = 1)) +
  scale_y_log10() +
  labs(x = "Rango (log)",
       y = "Frecuencia (log)") +
  theme_minimal()+
  labs(title = "Ley de Zipf con Conteo de Palabras",
       subtitle = "Artículos de revistas en español, ingles y portuguese",
       caption = "Fuente: Elaboración propia")+
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        plot.caption = element_text(hjust = -0.03))
