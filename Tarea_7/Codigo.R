
#1) Medoto de correlacion de Pearson COMPLEJA #################

  CorrelacionSimple = function(VectorX, VectorY) 
  {
    x        <- VectorX - mean(VectorX)
    y        <- VectorY - mean(VectorY)
    
    raux     <- sum(x * y) / sqrt(sum(x**2) * sum(y**2))
    return (raux);
  }
#2)Medoto de correlacion de Pearson SIMPLE #################
  CorrelacionCompleja = function(VectorX, VectorY) 
  {
    n                  <- length(VectorX)
    SumatoriaX         <- sum(VectorX)
    SumatoriaY         <- sum(VectorY)
    
    numerador          <- sum(VectorX * VectorY) - (SumatoriaX * SumatoriaY) / n
    denominadorX       <- sum(VectorX **2) - (SumatoriaX**2) / n
    denominadorY       <- sum(VectorY **2) - (SumatoriaY**2) / n
    denominador        <- sqrt(denominadorX * denominadorY)
    correlacion        <- numerador / denominador
    return (correlacion);
  }
  #A)Funcion Manipulacion de X * 2  #################
  ManipulacionX_1 = function(VectorX) 
  {
    return (VectorX * 2);
  }

  

#3) Programa  #################
      #   Datos <- data.frame(
      #     VectorX =  c(1.60, 1.63, 1.68 , 1.67 , 1.53,1.58,1.57,1.58,1.54,1.60,1.56,1.53),
      #     VectorY =  c(56  , 59  , 63   , 62   , 50  ,54  ,53  ,58  ,48  ,55  ,54  ,51)
      #   )
      #   
      # correlacionS      <- CorrelacionSimple(Datos$VectorX, Datos$VectorY)
      # correlacionC      <- CorrelacionCompleja(Datos$VectorX, Datos$VectorY)
      # print(correlacionC)
      # print(correlacionS)
      # 
      # print(cor(Datos$VectorX, Datos$VectorY))

#4) Correlacion Test  #################
      # png(filename  = "CorrelacionTest.png",
      #     width  = 800             ,
      #     height = 500)
      # opar=par(ps=18)
      # plot(Datos$VectorX, Datos$VectorY, yaxt="none",
      #      type="p",
      #      ylab="Estatura (m.)", 
      #      xlab="Peso (kg.)", 
      #      pch=16,
      #      cex=2,
      #      col = rep(1:3, rep(50, 3)),
      #      panel.first = grid())
      # 
      # axis(2,c(40,45,50,55,60,65),las=2)

# regression lineal
      # abline(lm( Datos$VectorY~Datos$VectorX ), col="red")

# regression local
      # lines( lowess( Datos$VectorX,Datos$VectorY ), col = "blue", type = "b", lty = 2, pch = 18) 
      # 
      # 
      # legend(min(Datos$VectorX), max(Datos$VectorY), 
      #        legend = c("regresion lineal", "regresion local"),
      #        col    = c("red", "blue"), 
      #        lty    = 1:2, 
      #        cex    = 0.8)

#5) Correlacion negativa  #################

      # Datos_2 <- data.frame(
      #   VectorX =  c(1, 2, 3 ,4, 5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
      #   VectorY =  c(16, 16, 14 ,14, 12,12,11,11,10,10,9,9,9,8,8,7,7,6,6,6)
      # )
      # 
      # correlacionS      <- CorrelacionSimple(Datos_2$VectorX, Datos_2$VectorY)
      # correlacionC      <- CorrelacionCompleja(Datos_2$VectorX, Datos_2$VectorY)
      # print(correlacionC)
      # print(correlacionS)
      # 
      # png(filename  = "CorrelacionHoras.png",
      #     width  = 800             ,
      #     height = 500)
      # 
      # opar=par(ps=18)
      # plot(Datos_2$VectorX, Datos_2$VectorY,
      #      type="p",
      #      ylab="Horas", 
      #      xlab="Edad", 
      #      pch=16,
      #      cex=2,
      #      col = rep(1:3, rep(50, 3)),
      #      panel.first = grid())
      # 
      # # regression lineal
      # abline(lm( Datos_2$VectorY~Datos_2$VectorX ), col="red")
      # 
      # legend(min(Datos_2$VectorX) * 13, max(Datos_2$VectorY), 
      #        legend = c("regresion lineal", "regresion local"),
      #        col    = c("red", "blue"), 
      #        lty    = 1:2, 
      #        cex    = 0.8)
      # 

#6) Correlacion Ejemplos  #################
      
        #       Datos_3 <- data.frame(
        #         VectorX =  c(1, 2, 3 ,4, 5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
        #       )
        # 
        #       contador <- 0
        #       VectorY  <- numeric()
        #       for(i in 1: 20)
        #       {
        # 
        #         contador <- contador + 1
        #         print(contador)
        #         VectorY  <- c(VectorY,contador)
        # 
        #       }
        # 
        #       Datos_3[,'VectorY'] <- VectorY
        # 
        #       correlacionS      <- CorrelacionSimple(Datos_3$VectorX, Datos_3$VectorY)
        #       correlacionC      <- CorrelacionCompleja(Datos_3$VectorX, Datos_3$VectorY)
        #       print(correlacionC)
        #       print(correlacionS)
        #   
        #     # png(filename  = "CorrelacionPositiva.png",
        #     #     width  = 800             ,
        #     #     height = 500)
        # 
        #       opar=par(ps=18)
        #       plot(Datos_3$VectorX, Datos_3$VectorY,
        #            type="p",
        #            ylab="Eje Y",
        #            xlab="Eje X",
        #            pch=16,
        #            cex=2,
        #            col = rep(1:3, rep(50, 3)),
        #            panel.first = grid())
        # 
        # # regression lineal
        #       abline(lm( Datos_3$VectorY~Datos_2$VectorX ), col="red")
        # 
        #       legend(min(Datos_3$VectorX) , max(Datos_3$VectorY),
        #              legend = c("regresion lineal"),
        #              col    = c("red"),
        #              lty    = 1:2,
        #              cex    = 0.8)
        # 
        # # Vector Y_2
        #       contador <- 0
        #       VectorYY  <- numeric()
        #       for(i in 1: 20)
        #       {
        # 
        #         contador <- contador - 1
        #         print(contador)
        #         VectorYY  <- c(VectorYY,contador)
        # 
        #       }
        # 
        #       Datos_3[,'VectorYY'] <- VectorYY
        # 
        #       correlacionS      <- CorrelacionSimple(Datos_3$VectorX, Datos_3$VectorYY)
        #       correlacionC      <- CorrelacionCompleja(Datos_3$VectorX, Datos_3$VectorYY)
        #       print(correlacionC)
        #       print(correlacionS)
        # 
        #       # png(filename  = "CorrelacionNegativa.png",
        #       #     width  = 800             ,
        #       #     height = 500)
        # 
        #       opar=par(ps=18)
        #       plot(Datos_3$VectorX, Datos_3$VectorYY,
        #            type="p",
        #            ylab="Eje Y",
        #            xlab="Eje X",
        #            pch=16,
        #            cex=2,
        #            col = rep(1:3, rep(50, 3)),
        #            panel.first = grid())
        # 
        #       # regression lineal
        #       abline(lm( Datos_3$VectorYY~Datos_2$VectorX ), col="red")
        # 
        #       legend(min(Datos_3$VectorX) * 13, max(Datos_3$VectorYY),
        #              legend = c("regresion lineal"),
        #              col    = c("red"),
        #              lty    = 1:2,
        #              cex    = 0.8)
        # 
        # # Vector Y_3
        # contador  <- 0
        # VectorYYY  <- numeric()
        # for(i in 1: 20)
        # {
        # 
        #   contador <- runif(1)
        #   print(contador)
        #   VectorYYY  <- c(VectorYYY,contador)
        # 
        # }
        # 
        # Datos_3[,'VectorYYY'] <- VectorYYY
        # 
        # correlacionS      <- CorrelacionSimple(Datos_3$VectorX, Datos_3$VectorYYY)
        # correlacionC      <- CorrelacionCompleja(Datos_3$VectorX, Datos_3$VectorYYY)
        # print(correlacionC)
        # print(correlacionS)
        # # 
        # # png(filename  = "SinCorrelacion.png",
        # #     width  = 800             ,
        # #     height = 500)
        # 
        #       opar=par(ps=18)
        #       plot(Datos_3$VectorX, Datos_3$VectorYYY,
        #            type="p",
        #            ylab="Eje Y",
        #            xlab="Eje X",
        #            pch=16,
        #            cex=2,
        #            col = rep(1:3, rep(50, 3)),
        #            panel.first = grid())


#8) Correlacion Ejemplos  #################
      

      # replicas <- 25
      # contador <- 0
      # VectorX  <- numeric()
      # for(i in 1:replicas)
      # {
      #   contador= contador + 1
      #   VectorX = c(VectorX,contador)
      # }
      # 
      # 
      # VectorY  <- c(rexp(25, rate=1/3) )
      # 
      # Datos_4 <- data.frame(
      #   VectorX =  VectorX,
      #   VectorY =  VectorX**2
      # )
      
      # png(filename  = "CorrelacionPositiva.png",
      #     width  = 500             ,
      #     height = 800)
      # 
      # opar=par(ps=18)
      # plot(Datos_4$VectorX, Datos_4$VectorY,
      #      type="p",
      #      ylab="Eje Y",
      #      xlab="Eje X",
      #      pch=16,
      #      cex=2,
      #      col = rep(1:3, rep(50, 3)),
      #      panel.first = grid())

      # regression lineal
      # abline(lm( Datos_4$VectorY~Datos_4$VectorX ), col="red")
      # 
      # legend(min(Datos_4$VectorX) , max(Datos_4$VectorY),
      #        legend = c("regresion lineal"),
      #        col    = c("red"),
      #        lty    = 1:2,
      #        cex    = 0.8)

 #9) Correlacion Ejemplos Lineal #################
      
      
      
      x <- c(1:20)
      y <- 3*x
      h <- rnorm(20,20,5)
    
      Datos_5 <- data.frame(
        VectorX =  x,
        VectorY =  (y**2) * h
      )
      
      for(i in seq(-2,2,1))
      {
        if(i > 0)
        {
          z <- x^i
        }
        else if(i < 0)
        {
          z <- -1*x^i
        }
        else
        {
          z <- log(abs(x))
        }
        plot(z,y)
        print(i)
        print(cor(y,z))
      }
      
      # png(filename  = "CorrelacionPositiva.png",
      #     width  = 500             ,
      #     height = 800)
      # 
      # opar=par(ps=18)
      # plot(Datos_5$VectorX, Datos_5$VectorY,
      #      type="p",
      #      ylab="Eje Y",
      #      xlab="Eje X",
      #      pch=16,
      #      cex=2,
      #      col = rep(1:3, rep(50, 3)),
      #      panel.first = grid())
      
    
      
# 7) DEV OFF #######################################

# while (!is.null(dev.list()))  dev.off()

