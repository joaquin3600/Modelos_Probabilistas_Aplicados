
#
Replicas  <- 10
Baraja    <- c(2,3,4,5,6,7,8,9,10)

  Coleccion <- c()
  Media     <- c()
  for(j in 1:Replicas)
  {
    Ganancias <- 0;
    for(i in 1:50)
    {
      carta  <- sample(Baraja, 1)
      if((carta %% 2) == 0)
      {
        Ganancias = - 1;
      }else {
        Ganancias =   1;
      }
      ArregloGanancias = c(ArregloGanancias,Ganancias)
    }
    Media = c(Media,mean(ArregloGanancias))
  }
  png(filename  = "hist10.png",
      width  = 800             ,
      height = 500)
  opar=par(ps=18)

  hist(Media,
       main="",
       xlab="Media",
       ylab="Frecuencia",
       border="black",
       col="purple")

  png(filename  = "boxplot10.png",
      width  = 800             ,
      height = 500)

  opar=par(ps=18)

  boxplot(Media,
          xlab        = "10 replicas",
          ylab        = "Media",
          col         = "purple",
          border      =  "black"
          )



LLaves         <- c(1:6)
contadores     <- c()
Medias         <- c()
for(j in 1:10)
{
  for(i in 1:100)
  {
    LLaveCorrecta  <- sample(LLaves, 1)
    contador       <- 0;
    for (LLave in LLaves)
    {
      if(LLave == LLaveCorrecta)
      {
        break;
      }else{
        contador = contador + 1;
      }
    }
    contadores = c(contadores,contador)
  }
  Medias  <- c(Medias,mean(contadores))
}

  png(filename  = "boxplotllave.png",
      width  = 800             ,
      height = 500)
  opar=par(ps=18)

  boxplot(Media,
          xlab        = "10 replicas",
          ylab        = "Media",
          col         = "purple",
          border      =  "black"
          )
  png(filename  = "histllave.png",
      width  = 800             ,
      height = 500)
  opar=par(ps=18)
    hist(Media,
         main="",
         xlab="Media",
         ylab="Frecuencia",
         border="black",
         col="purple")


Numeros         <- c(-1,0,1)
ArrayMedias     <- c()
ArrayVarianza   <- c()
ArrayDestandar  <- c()
for(j in 1:1000)
{
  DistribucionNumeros <- c()
  for(i in 1:100)
  {
    Numero    <- sample(Numeros, 1)
    DistribucionNumeros <- c(DistribucionNumeros,Numero)
  }
  ArrayMedias     = c(ArrayMedias,mean(DistribucionNumeros))
  ArrayVarianza   = c(ArrayVarianza,var(DistribucionNumeros))
  ArrayDestandar  = c(ArrayDestandar,sd(DistribucionNumeros))
}


  png(filename  = "histmedia.png",
      width  = 800             ,
      height = 500)
  opar=par(ps=18)
    hist(ArrayMedias,
         main="",
         xlab="Media",
         ylab="Frecuencia",
         border="black",
         col="purple")
    
      png(filename  = "histvarianza.png",
          width  = 800             ,
          height = 500)
  opar=par(ps=18)
    hist(ArrayVarianza,
         main="",
         xlab="Varianza",
         ylab="Frecuencia",
         border="black",
         col="green")
    
      png(filename  = "histDE.png",
          width  = 800             ,
          height = 500)
  opar=par(ps=18)
    hist(ArrayDestandar,
         main="",
         xlab="Desviación estandar",
         ylab="Frecuencia",
         border="black",
         col="gold")
    
   while (!is.null(dev.list()))  dev.off()
    