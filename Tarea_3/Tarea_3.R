
if (!require('devtools')) {
  install.packages('devtools')
}
if (!require('tidytext')) {
  install.packages('tidytext')
}
if (!require('dplyr')) {
  install.packages('dplyr')
}
if (!require('curl')) {
  install.packages('curl')
}
if (!require('readr')) {
  install.packages('readr')
}
if (!require('stringr')) {
  install.packages('stringr')
}
if (!require('gutenbergr')) {
  install_version("gutenbergr", version = "0.1.5", repos = "http://cran.us.r-project.org/"); 
  
}
library('plyr')
library('sjmisc')
library('devtools')
library('tidytext')
library('dplyr')
library('curl')
library('readr')
library('stringr')
#install_version("gutenbergr", version = "0.1.5", repos = "http://cran.us.r-project.org/"); 
library('gutenbergr')


Codigo        <- 58791  
Libro         <- gutenberg_download(c(Codigo))

#1) Remover Caracteres especiales 
Libro         <- as.data.frame(gsub("[[:punct:]]", "", as.matrix(Libro))) 

#2) Variables de Letras y palabras
letras        <- Libro %>% unnest_tokens(chars, text, "characters")
palabras      <- Libro %>% unnest_tokens(words, text, "words")
lineas        <- Libro %>% unnest_tokens(lines, text, "lines")

#3) Frecuencias de Palabras y letras, ordenados por numero de frecuencia

FrecuenciaPalabras <- as.data.frame( sort(table( palabras$words),decreasing = TRUE))
FrecuenciaLetras   <- as.data.frame( sort(table( letras$chars)  ,decreasing = TRUE))

#4) Remover numeros de las frecuencias

FrecuenciaPalabras <- FrecuenciaPalabras[-grep('^\\d+$', FrecuenciaPalabras$Var1),]
FrecuenciaLetras   <- FrecuenciaLetras  [-grep('^\\d+$', FrecuenciaLetras$Var1),]



#4.1) Remover articulos y conjunciones de la frecuencia de las palabras

Articulos          <- elements <- read.csv(file.path( "Filter.csv"))
FrecuenciaPalabras <- FrecuenciaPalabras[!(FrecuenciaPalabras$Var1 %in% Articulos$Articles),]
FrecuenciaPalabras <- FrecuenciaPalabras[!(FrecuenciaPalabras$Var1 %in% Articulos$Conjunctions),]    
FrecuenciaPalabras <- FrecuenciaPalabras[!(FrecuenciaPalabras$Var1 %in% Articulos$Pronouns),]    



NumeroLetras <- numeric()
for(i in 1:nrow(palabras))
{
  row          <- palabras[i,]$words
  Numero       <- nchar(row, type = "chars")
  NumeroLetras <- append(NumeroLetras,Numero)
}

# 1) Distribución Geometrica
# Obtengo la probabilidad de que aparezca una palabra con menos de 3 letras
# y saco su distribucion

 a            <- length(NumeroLetras[NumeroLetras < 3])
 n            <- length(NumeroLetras)
 Probabilidad <- (a/n)

 resultados = numeric()
 
 while (length(resultados) < 10000)
 {
   counter    = 0
   while(TRUE)
   {
     counter = counter + 1
     if(runif(1) < Probabilidad)
     {
       break;
     }
   }
   resultados = c(resultados,counter)
   
 }

png(filename = "Geometrica.png",
    width = 600, height = 600)

hist(resultados,main=" ", ylab="Frecuencias", xlab="Resultados")
# 2) Distribucion Hyper Geometrica
# Obtengo la probabilidad de que aparezcan 5 letras mayores que 2
# en una muestra de 100 palabras


TotalPalabras <- length(palabras$words)
NoCompatibles <- length(NumeroLetras[NumeroLetras < 2])
Compatibles   <- TotalPalabras - NoCompatibles
muestra       <- 100
requeridas    <- 5
replicas      <- 1000
contador      <- 0
tabla         <- c(rep(FALSE,NoCompatibles), rep(TRUE,Compatibles))

ProbabilidadValida = numeric();
indice = 0;
while(indice < 500)
{
  for(replica in 1:replicas)
  {
    validas        <- sample(tabla,muestra,replace = FALSE)
    contador       <- contador + (muestra - sum(validas) > requeridas)
    
  }
  p = (contador/replicas)
  ProbabilidadValida <- append(ProbabilidadValida,p)
  indice = indice + 1; 
}
png(filename = "HyperGeometrica.png",
    width = 600, height = 600)

hist(ProbabilidadValida,main=" ", ylab="Frecuencias", xlab="Resultados")


#3) Distribucion regular cual es la probabilidad
# de sacar letras mayores que 7 en 200 repeticiones

a            <- length(NumeroLetras[NumeroLetras > 7])
n            <- length(NumeroLetras)
Probabilidad <- (a/n)
print(Probabilidad)
repeticiones_regular = 200
n = 20
resultadosRegular = numeric()
while(length(resultadosRegular) < repeticiones_regular)
{
  exitos = 0;
  
  for(intento in 1:n)
  {
    if(runif(1) > Probabilidad)
    {
      exitos = exitos + 1;
    }
  };
  resultadosRegular = c(resultadosRegular,exitos)
  
}
png(filename = "Regular.png",
    width = 600, height = 600)

hist(resultadosRegular,main=" ", ylab="Frecuencias", xlab="Resultados")

#4 distribucion binomial negativa
# repetir hasta junta 15 casos de exito donde se encuentra
# una palabra de mas de 6 letras de largo

a            <- length(NumeroLetras[NumeroLetras < 6])
n            <- length(NumeroLetras)
Probabilidad <- (a/n)
resultadosNegativa = numeric()
k          = 15
while (length(resultadosNegativa) < n)
{
  exitos     = 0
  counter    = 0
  while(TRUE)
  {
    counter = counter + 1
    if(runif(1) < Probabilidad)
    {
      exitos = exitos + 1
      if(exitos == k)
      {
        break;
      }
    }
  }
  resultadosNegativa = c(resultadosNegativa,counter)
  
}
png(filename = "Negativo.png",
    width = 600, height = 600)

hist(resultadosNegativa,main=" ", ylab="Frecuencias", xlab="Resultados")

while (!is.null(dev.list()))  dev.off()





