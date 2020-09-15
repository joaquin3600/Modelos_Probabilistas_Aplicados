


#install.packages('devtools')  ; 
#install.packages('tidytext')  ; 
#install.packages('dplyr')     ; 
#install.packages('curl')      ; 
#install.packages('stringr')   ; 

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
    
    #Articulos     <- read_csv("Articles.csv")
        Articulos     <- elements <- read.csv(file.path( "Filter.csv"))
    #3) Frecuencias de Palabras y letras, ordenados por numero de frecuencia
    
        FrecuenciaPalabras <- as.data.frame( sort(table( palabras$words),decreasing = TRUE))
        FrecuenciaLetras   <- as.data.frame( sort(table( letras$chars)  ,decreasing = TRUE))
    
    #4) Remover numeros de las frecuencias
        FrecuenciaPalabras <- FrecuenciaPalabras[-grep('^\\d+$', FrecuenciaPalabras$Var1),]
        FrecuenciaLetras   <- FrecuenciaLetras  [-grep('^\\d+$', FrecuenciaLetras$Var1),]
    #4.1) Remover articulos y conjunciones de la frecuencia de las palabras
       
        FrecuenciaPalabras = FrecuenciaPalabras[!(FrecuenciaPalabras$Var1 %in% Articulos$Articles),]
        FrecuenciaPalabras = FrecuenciaPalabras[!(FrecuenciaPalabras$Var1 %in% Articulos$Conjunctions),]    
        FrecuenciaPalabras = FrecuenciaPalabras[!(FrecuenciaPalabras$Var1 %in% Articulos$Pronouns),]    
       
    #5) Obtener Primer y Tercer Quantil para limites de analisis
        
        png(filename = "boxplot.png",
            width = 400, height = 400)
        boxplot(FrecuenciaLetras$Freq,
                xlab       = "Letras",
                col        = "orange");
        title(ylab = "Letras", line = 10);
        
        
        PrimerQuantilLetras        <- quantile(FrecuenciaLetras$Freq, 0.25)
        TercerQuantilLetras        <- quantile(FrecuenciaLetras$Freq, 0.75)
        FrecuenciaLetras           <- FrecuenciaLetras[(FrecuenciaLetras$Freq >= PrimerQuantilLetras),]
        FrecuenciaLetras           <- FrecuenciaLetras[(FrecuenciaLetras$Freq <= TercerQuantilLetras),]
        
        
        FrecuenciaPalabras         <- FrecuenciaPalabras[(FrecuenciaPalabras$Freq > 100),]
        PrimerQuantilPalabras      <- quantile(FrecuenciaPalabras$Freq, 0.25)
        TercerQuantilPalabras      <- quantile(FrecuenciaPalabras$Freq, 0.75)
        FrecuenciaPalabras         <- FrecuenciaPalabras[(FrecuenciaPalabras$Freq > PrimerQuantilPalabras),]
        FrecuenciaPalabras         <- FrecuenciaPalabras[(FrecuenciaPalabras$Freq < TercerQuantilPalabras),]  
        
        png(filename = "Letras.png" ,
            width = 400, height = 400)
        par(las=1)
        barplot(FrecuenciaLetras$Freq,names.arg   = FrecuenciaLetras$Var1 ,xlab="Letras" )
        
        
        png(filename = "Palabras.png",
            width = 500, height = 500)
        par(las=1) # make label text perpendicular to axis
       
        barplot(FrecuenciaPalabras$Freq,names.arg = FrecuenciaPalabras$Var1,xlab="Frecuencia", horiz=TRUE)
        
        dev.off()
    
    
    
    
    
    
