
library('plyr')
library('sjmisc')
library('devtools')
library('tidytext')
library('dplyr')
library('curl')
library('readr')
library('stringr')
library(tidyverse)
library(scales)
library(reshape2)
# 1) Metodos ---------------------------------

RegresionLineal = function(X, Y) 
{
  
  MediaX      <- mean(X)
  MediaY      <- mean(Y)
  DesviacionX <- sd(X)
  DesviacionY <- sd(Y)
  
  r           <- cor(X,Y)
  m           <- r * DesviacionY / DesviacionX
  b           <- MediaY - m * MediaX
  Linea       <- m * X + b
  return (Linea);
}
ValorSiguiente = function(X, Y) 
{
  
  MediaX      <- mean(X)
  MediaY      <- mean(Y)
  DesviacionX <- sd(X)
  DesviacionY <- sd(Y)
  
  r           <- cor(X,Y)
  m           <- r * DesviacionY / DesviacionX
  b           <- MediaY - m * MediaX
  number      <- max(X) + 1
  Yp          <- m * number + b
  return (Yp);
}
ValorSSiguiente = function(X, Y) 
{
  
  MediaX      <- mean(X)
  MediaY      <- mean(Y)
  DesviacionX <- sd(X)
  DesviacionY <- sd(Y)
  
  r           <- cor(X,Y)
  m           <- r * DesviacionY / DesviacionX
  b           <- MediaY - m * MediaX
  number      <- max(X) + 2
  Yp          <- m * number + b
  return (Yp);
}

while (!is.null(dev.list()))  dev.off()
# Tabulados ---------------------------------

    MortalidadMexico  <- read.csv(file.path("Tabulados/MEXICO.csv")       , row.names=NULL,sep=";",fileEncoding="UTF-8-BOM")
    MortalidadBrasil  <- read.csv(file.path("Tabulados/BRASIL.csv")       , row.names=NULL,sep=";",fileEncoding="UTF-8-BOM")
    MortalidadUSA     <- read.csv(file.path("Tabulados/ESTADOSUNIDOS.csv"), row.names=NULL,sep=";",fileEncoding="UTF-8-BOM")
    MortalidadCanada  <- read.csv(file.path("Tabulados/CANADA.csv")       , row.names=NULL,sep=";",fileEncoding="UTF-8-BOM")

# 1)Mexico ---------------------------------

    YmaxMexico = trunc(max(MortalidadMexico$Total) + 10)
    YminMexico = min(MortalidadMexico$Total)
    YminMexico = YminMexico - (YminMexico/10)
    png(filename  = "DefuncionesAnualesMexico.png",
        width  = 800             ,
        height = 500)
    par=par(ps=18)


    ggplot(
      MortalidadMexico, 
      aes(x         = Ejercicio, 
          y         = Total,
          position  = 'dodge')) + 
      geom_bar(
        stat  = "identity",
        fill  = "#a5d5e6")     +
      scale_y_continuous(
        name    ="Defunciones", 
        labels  = comma,
        limits  =c(YminMexico,YmaxMexico),
        oob     = rescale_none) +
      
      scale_x_continuous("Año", 
                         breaks=c(2010:2019)) + theme(text = element_text(size=20)) +
      geom_text(
        aes(x         = Ejercicio, 
            y         = Total, 
            label     = comma(Total)),
        position  = position_dodge(width = 1),
        vjust     = -0.5, size = 5
      ) 

# 2)Brasil ---------------------------------

    YmaxBrasil = trunc(max(MortalidadBrasil$Total) + 10)
    YminBrasil = min(MortalidadBrasil$Total)
    YminBrasil = YminBrasil - (YminBrasil/10)
    png(filename  = "DefuncionesAnualesBrasil.png",
        width  = 800             ,
        height = 500)
    par=par(ps=18)


    ggplot(
      MortalidadBrasil, 
      aes(x         = Ejercicio, 
          y         = Total,
          position  = 'dodge')) + 
      geom_bar(
        stat  = "identity",
        fill  = "#f2962c")     +
      scale_y_continuous(
        name    ="Defunciones", 
        labels  = comma,
        limits  =c(YminBrasil,YmaxBrasil),
        oob     = rescale_none) +
      
      scale_x_continuous("Año", 
                         breaks=c(2010:2019)) + theme(text = element_text(size=20)) +
      geom_text(
        aes(x         = Ejercicio, 
            y         = Total, 
            label     = comma(Total)),
        position  = position_dodge(width = 1),
        vjust     = -0.5, size = 5
      ) 

# 3)USA ---------------------------------

    YmaxUSA = trunc(max(MortalidadUSA$Total) + 10)
    YminUSA = min(MortalidadUSA$Total)
    YminUSA = YminUSA - (YminUSA/10)
    png(filename  = "DefuncionesAnualesUSA.png",
        width  = 800             ,
        height = 500)
    par=par(ps=18)


    ggplot(
      MortalidadUSA, 
      aes(x         = Ejercicio, 
          y         = Total,
          position  = 'dodge')) + 
      geom_bar(
        stat  = "identity",
        fill  = "#008f0e")     +
      scale_y_continuous(
        name    ="Defunciones", 
        labels  = comma,
        limits  =c(YminUSA,YmaxUSA),
        oob     = rescale_none) +
      
      scale_x_continuous("Año", 
                         breaks=c(2010:2019)) + theme(text = element_text(size=20)) +
      geom_text(
        aes(x         = Ejercicio, 
            y         = Total, 
            label     = comma(Total)),
        position  = position_dodge(width = 1),
        vjust     = -0.5, size = 5
      ) 

# 4)Canada ---------------------------------
    YmaxCanada = trunc(max(MortalidadCanada$Total) + 10)
    YminCanada = min(MortalidadCanada$Total)
    YminCanada = YminCanada - (YminCanada/10)

    png(filename  = "DefuncionesAnualesCanada.png",
        width  = 800             ,
        height = 500)
    par=par(ps=18)

    ggplot(
      MortalidadCanada, 
      aes(x         = Ejercicio, 
          y         = Total,
          position  = 'dodge')) + 
      geom_bar(
        stat  = "identity",
        fill  = "#4a14de")     +
      scale_y_continuous(
        name    ="Defunciones", 
        labels  = comma,
        limits  =c(YminCanada,YmaxCanada),
        oob     = rescale_none) +
      
      scale_x_continuous("Año", 
                         breaks=c(2010:2019)) + theme(text = element_text(size=20)) +
      geom_text(
        aes(x         = Ejercicio, 
            y         = Total, 
            label     = comma(Total)),
        position  = position_dodge(width = 1),
        vjust     = -0.5, size = 5
      )

# 5) Mortalidad hombres y mujeres ---------------------------------

    ylimHombres = trunc(max(MortalidadMexico$Hombres) + 10)
    YminMujeres = min(MortalidadMexico$Mujeres)
    YminMujeres = YminMujeres - (YminMexico/10)
    YminMujeres  

    png(filename  = "DefuncionesAnualesPorSexo.png",
        width  = 800             ,
        height = 500)
    par=par(ps=18)

    dfm <- melt(MortalidadMexico[,c('Ejercicio','Hombres','Mujeres')],id.vars = 1,varnames=c('AgeGroup','AgeGroup', 'Geo'))

    ggplot(
      dfm,
      aes(x = Ejercicio,y = value)) + 
      geom_bar(
        aes(fill = variable),
        stat     = "identity",
        position = "dodge")           +
      scale_x_continuous("Año", 
                         breaks=c(2010:2019)) +
      scale_y_continuous(
        name="Defunciones", 
        labels = comma,
        limits=c(YminMujeres,ylimHombres),
        oob = rescale_none)    + theme(text = element_text(size=20)) +
      
      geom_text(aes(label=comma(value)), position=position_dodge(width=0.9),
                vjust = -0.5, size = 2)

# 6) Defunciones Anuales Regresion Mexico ---------------------------------

    png(filename  = "DefuncionesAnualesRegresionMexico.png",
        width  = 800             ,
        height = 500)
    par=par(ps=50)

    ggplot(data = MortalidadMexico, aes(x = Ejercicio, y = Total)) +
      geom_point() +
      scale_y_continuous(
        name="Defunciones", 
        labels = comma,
        limits=c(YminMexico,YmaxMexico),
        oob = rescale_none)  +
      
      scale_x_continuous("Año", 
                         breaks=c(2010:2019)) +
      geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
      theme_bw() + labs(x = "", y = "") + theme(text = element_text(size=30))

 # 7) Defunciones Anuales Regresion Canada ---------------------------------

    png(filename  = "DefuncionesAnualesRegresionCanada.png",
        width  = 800             ,
        height = 500)
    par=par(ps=50)

    ggplot(data = MortalidadCanada, aes(x = Ejercicio, y = Total)) +
      geom_point() +
      scale_y_continuous(
        name="Defunciones", 
        labels = comma,
        limits=c(YminCanada,YmaxCanada),
        oob = rescale_none)  +
      
      scale_x_continuous("Año", 
                         breaks=c(2010:2019)) +
      geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
      theme_bw() + labs(x = "", y = "") + theme(text = element_text(size=30))

# 8) Defunciones Anuales Regresion USA ---------------------------------

    png(filename  = "DefuncionesAnualesRegresionUSA.png",
        width  = 800             ,
        height = 500)
    par=par(ps=50)

    ggplot(data = MortalidadUSA, aes(x = Ejercicio, y = Total)) +
      geom_point() +
      scale_y_continuous(
        name="Defunciones", 
        labels = comma,
        limits=c(YminUSA,YmaxUSA),
        oob = rescale_none)  +
      
      scale_x_continuous("Año", 
                         breaks=c(2010:2019)) +
      geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
      theme_bw() + labs(x = "", y = "") + theme(text = element_text(size=30))

# 9) Defunciones Anuales Regresion Brasil ---------------------------------

    png(filename  = "DefuncionesAnualesRegresionBrasil.png",
        width  = 800             ,
        height = 500)
    par=par(ps=50)

    ggplot(data = MortalidadBrasil, aes(x = Ejercicio, y = Total)) +
      geom_point() +
      scale_y_continuous(
        name="Defunciones", 
        labels = comma,
        limits=c(YminBrasil,YmaxBrasil),
        oob = rescale_none)  +
      
      scale_x_continuous("Año", 
                         breaks=c(2010:2019)) +
      geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
      theme_bw() + labs(x = "", y = "") + theme(text = element_text(size=30))

# 10) regresion lineal 2020 Mexico ---------------------------------

    NumerodeEjerciciosMexico            <- length(MortalidadMexico$Ejercicio)
    VectorNuevoMexico                   <- RegresionLineal(c(1:NumerodeEjerciciosMexico), MortalidadMexico$Total)
    VectorSiguienteMexico               <- ValorSiguiente(c(1:10), MortalidadMexico$Total)

    MortalidadRegresionMexico           <- data.frame("Ejercicio" = c(2010:2020), "Total" = c(VectorNuevoMexico,VectorSiguienteMexico))

    ylimiteRMexico  <- trunc(max(MortalidadRegresionMexico$Total) + 10)
    YminiminoMexico <- min(MortalidadRegresionMexico$Total)
    YminiminoMexico <- YminiminoMexico - (YminMexico/10)

    png(filename  = "DefuncionesAnualesRegresionFinalMexico.png",
        width  = 800             ,
        height = 500)
    par=par(ps=18)


    ggplot(
      MortalidadRegresionMexico,
      aes(x=Ejercicio, y=Total,position = 'dodge')) +
      geom_bar(
        stat = "identity",
        fill="#a5d5e6")     +
      scale_y_continuous(
        name="Defunciones",
        labels = comma,
        limits=c(YminiminoMexico,ylimiteRMexico),
        oob = rescale_none) +
      
      scale_x_continuous("Año",
                         breaks=c(2010:2020)) +
      
      geom_text(
        aes(x = Ejercicio, y = Total, label = comma(Total)),
        position = position_dodge(width = 1),
        vjust = -0.5, size = 5
      ) + geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
      theme_bw() + labs(x = "", y = "") + 
      theme(text = element_text(size=20)) +
      geom_line(data=MortalidadRegresionMexico,aes(y=Total,x= Ejercicio,color="Regresión lineal"),size=1 ) + 
      scale_color_discrete(name = "", labels = c("Regresión lineal"))   

# 11) regresion lineal 2020 Canada ---------------------------------

      NumerodeEjerciciosCanada            <- length(MortalidadCanada$Ejercicio)
      VectorNuevoCanada                   <- RegresionLineal(c(1:NumerodeEjerciciosCanada), MortalidadCanada$Total)
      VectorSiguienteCanada               <- ValorSiguiente(c(1:9), MortalidadCanada$Total)
      VectorSiguienteSCanada              <- ValorSSiguiente(c(1:9), MortalidadCanada$Total)

      MortalidadRegresionCanada           <- data.frame("Ejercicio" = c(2010:2020), "Total" = c(VectorNuevoCanada,VectorSiguienteCanada,VectorSiguienteSCanada))

      ylimiteRCanada  <- trunc(max(MortalidadRegresionCanada$Total) + 10)
      YminiminoCanada <- min(MortalidadRegresionCanada$Total)
      YminiminoCanada <- YminiminoCanada - (YminCanada/10)

      png(filename  = "DefuncionesAnualesRegresionFinalCanada.png",
          width  = 800             ,
          height = 500)
      par=par(ps=18)


      ggplot(
        MortalidadRegresionCanada,
        aes(x=Ejercicio, y=Total,position = 'dodge')) +
        geom_bar(
          stat = "identity",
          fill="#4a14de")     +
        scale_y_continuous(
          name="Defunciones",
          labels = comma,
          limits=c(YminiminoCanada,ylimiteRCanada),
          oob = rescale_none) +
        
        scale_x_continuous("Año",
                           breaks=c(2010:2020)) +
        
        geom_text(
          aes(x = Ejercicio, y = Total, label = comma(Total)),
          position = position_dodge(width = 1),
          vjust = -0.5, size = 5
        ) + geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
        theme_bw() + labs(x = "", y = "") + 
        theme(text = element_text(size=20)) +
        geom_line(data=MortalidadRegresionCanada,aes(y=Total,x= Ejercicio,color="Regresión lineal"),size=1 ) + 
        scale_color_discrete(name = "", labels = c("Regresión lineal"))     

# 12) regresion lineal 2020 USA ------------------------------------
    NumerodeEjerciciosUSA               <- length(MortalidadUSA$Ejercicio)
    VectorNuevoUSA                      <- RegresionLineal(c(1:NumerodeEjerciciosUSA), MortalidadUSA$Total)
    VectorSiguienteUSA                  <- ValorSiguiente(c(1:9), MortalidadUSA$Total)
    VectorSiguienteSUSA                 <- ValorSSiguiente(c(1:9), MortalidadUSA$Total)

    MortalidadRegresionUSA              <- data.frame("Ejercicio" = c(2010:2020), "Total" = c(VectorNuevoUSA,VectorSiguienteUSA,VectorSiguienteSUSA))

    ylimiteRUSA  <- trunc(max(MortalidadRegresionUSA$Total) + 10)
    YminiminoUSA <- min(MortalidadRegresionUSA$Total)
    YminiminoUSA <- YminiminoUSA - (YminUSA/10)

    png(filename  = "DefuncionesAnualesRegresionFinalUSA.png",
        width  = 800             ,
        height = 500)
    par=par(ps=18)


    ggplot(
      MortalidadRegresionUSA,
      aes(x=Ejercicio, y=Total,position = 'dodge')) +
      geom_bar(
        stat = "identity",
        fill="#008f0e")     +
      scale_y_continuous(
        name="Defunciones",
        labels = comma,
        limits=c(YminiminoUSA,ylimiteRUSA),
        oob = rescale_none) +
      
      scale_x_continuous("Año",
                         breaks=c(2010:2020)) +
      
      geom_text(
        aes(x = Ejercicio, y = Total, label = comma(Total)),
        position = position_dodge(width = 1),
        vjust = -0.5, size = 5
      ) + geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
      theme_bw() + labs(x = "", y = "") + 
      theme(text = element_text(size=20)) +
      geom_line(data=MortalidadRegresionUSA,aes(y=Total,x= Ejercicio,color="Regresión lineal"),size=1 ) + 
      scale_color_discrete(name = "", labels = c("Regresión lineal"))

# 13) regresion lineal 2020 Brasil ---------------------------------

    NumerodeEjerciciosBrasil                <- length(MortalidadBrasil$Ejercicio)
    VectorNuevoBrasil                       <- RegresionLineal(c(1:NumerodeEjerciciosBrasil), MortalidadBrasil$Total)
    VectorSiguienteBrasil                   <- ValorSiguiente(c(1:9), MortalidadBrasil$Total)
    VectorSiguienteSBrasil                  <- ValorSSiguiente(c(1:9), MortalidadBrasil$Total)


    MortalidadRegresionBrasil               <- data.frame("Ejercicio" = c(2010:2020), "Total" = c(VectorNuevoBrasil,VectorSiguienteBrasil,VectorSiguienteSBrasil))

    ylimiteBrasil   <- trunc(max(MortalidadRegresionBrasil$Total) + 10)
    YminiminoBrasil <- min(MortalidadRegresionBrasil$Total)
    YminiminoBrasil <- YminiminoBrasil - (YminBrasil/10)

    png(filename  = "DefuncionesAnualesRegresionFinalBrasil.png",
        width  = 800             ,
        height = 500)
    par=par(ps=18)


    ggplot(
      MortalidadRegresionBrasil,
      aes(x=Ejercicio, y=Total,position = 'dodge')) +
      geom_bar(
        stat = "identity",
        fill="#f2962c")     +
      scale_y_continuous(
        name="Defunciones",
        labels = comma,
        limits=c(YminiminoBrasil,ylimiteBrasil),
        oob = rescale_none) +
      
      scale_x_continuous("Año",
                         breaks=c(2010:2020)) +
      
      geom_text(
        aes(x = Ejercicio, y = Total, label = comma(Total)),
        position = position_dodge(width = 1),
        vjust = -0.5, size = 5
      ) + geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
      theme_bw() + labs(x = "", y = "") + 
      theme(text = element_text(size=20)) +
      geom_line(data=MortalidadRegresionBrasil,aes(y=Total,x= Ejercicio,color="Regresión lineal"),size=1 ) + 
      scale_color_discrete(name = "", labels = c("Regresión lineal"))


# 14) Error estandar USA ---------------------------------
    NumeroElementosUSA     <- length(MortalidadUSA$Total)
    DiferenciaCuadradosUSA <- sum((MortalidadUSA$Total - VectorNuevoUSA)**2)
    ErrorEstandarUSA       <- sqrt(DiferenciaCuadradosUSA/NumeroElementosUSA)

    print(ErrorEstandarUSA)
    MaximoUSA       <- max(MortalidadRegresionUSA$Total)
    print(MaximoUSA)
    
    PrimerRangoUSA  <- MaximoUSA - ErrorEstandarUSA
    SegundoRangoUSA <- MaximoUSA + ErrorEstandarUSA
    
    sprintf("[%f , %f]", PrimerRangoUSA, SegundoRangoUSA)

# 15) Error estandar Brasil---------------------------------
    NumeroElementosBrasil     <- length(MortalidadBrasil$Total)
    DiferenciaCuadradosBrasil <- sum((MortalidadBrasil$Total - VectorNuevoBrasil)**2)
    ErrorEstandarBrasil       <- sqrt(DiferenciaCuadradosBrasil/NumeroElementosBrasil)

    print(ErrorEstandarBrasil)
    MaximoBrasil       <- max(MortalidadRegresionBrasil$Total)
    print(MaximoBrasil)
    
    PrimerRangoBrasil  <- MaximoBrasil - ErrorEstandarBrasil
    SegundoRangoBrasil <- MaximoBrasil + ErrorEstandarBrasil
    
    sprintf("[%f , %f]", PrimerRangoBrasil, SegundoRangoBrasil)

# 16) Error estandar Mexico---------------------------------
    NumeroElementosMexico     <- length(MortalidadMexico$Total)
    DiferenciaCuadradosMexico <- sum((MortalidadMexico$Total - VectorNuevoMexico)**2)
    ErrorEstandarMexico       <- sqrt(DiferenciaCuadradosMexico/NumeroElementosMexico)

    print(ErrorEstandarMexico)
    MaximoMexico       <- max(MortalidadRegresionMexico$Total)
    print(MaximoMexico)
    PrimerRangoMexico  <- MaximoMexico - ErrorEstandarMexico
    SegundoRangoMexico <- MaximoMexico + ErrorEstandarMexico
    
    sprintf("[%f , %f]", PrimerRangoMexico, SegundoRangoMexico)

# 17) Error estandar Canada ---------------------------------
    NumeroElementosCanada     <- length(MortalidadCanada$Total)
    DiferenciaCuadradosCanada <- sum((MortalidadCanada$Total - VectorNuevoCanada)**2)
    ErrorEstandarCanada       <- sqrt(DiferenciaCuadradosCanada/NumeroElementosCanada)

    print(ErrorEstandarCanada)
    MaximoCanada       <- max(MortalidadRegresionCanada$Total)
    print(MaximoCanada)
    PrimerRangoCanada  <- MaximoCanada - ErrorEstandarCanada
    SegundoRangoCanada <- MaximoCanada + ErrorEstandarCanada
    
    sprintf("[%f , %f]", PrimerRangoCanada, SegundoRangoCanada)

    while (!is.null(dev.list()))  dev.off()
