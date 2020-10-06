

# cargar ggplot2
# install.packages("ggplot2")
  library(ggplot2)
  
      #Medoto de Gauss con ambas variables
          gaussian = function(mu, sigma) 
          {
            u      <- runif(2);
            z0     <- sqrt(-2 * log(u[1])) * cos(2 * pi * u[2]);
            z1     <- sqrt(-2 * log(u[1])) * sin(2 * pi * u[2]);
            datos  <- c(z0, z1);
            return (sigma * datos + mu);
           }
      #Medoto de Gauss con solo la variable ZO
          gaussianSingle_1 = function(mu, sigma) 
          {
            u      <- runif(2);
            z0     <- sqrt(-2 * log(u[1])) * cos(2 * pi * u[2]);
            datos  <- c(z0);
            return (sigma * datos + mu);
          }
      #Medoto de Gauss con solo la variable Z1
          gaussianSingle_2 = function(mu, sigma) 
          {
            u      <- runif(2);
            z1     <- sqrt(-2 * log(u[1])) * sin(2 * pi * u[2]);
            datos  <- c(z1);
            return (sigma * datos + mu);
          }
      #Medoto de Gauss con solo la variable ZO
          gaussianSingle_1_UnaVariable = function(mu, sigma) 
          {
            u      <- runif(1);
            z0     <- sqrt(-2 * log(u[1])) * cos(2 * pi * u[1]);
            datos  <- c(z0);
            return (sigma * datos + mu);
          }
      #Medoto de Gauss con solo la variable Z1
          gaussianSingle_2_UnaVariable = function(mu, sigma) 
          {
            u      <- runif(1);
            z1     <- sqrt(-2 * log(u[1])) * sin(2 * pi * u[1]);
            datos  <- c(z1);
            return (sigma * datos + mu);
          }
          
          
          #Simular encuesta de 50 alumnos de una escuela con un promedio de 18 anios    
            Promedio       <- 18
            Edad           <- rnorm(150   ,Promedio,1)
            Edad_2         <- rnorm(500000,Promedio,1)
            Numeros        <- rnorm(50000 ,0       ,5)
            
            Data_Alumnos   <- data.frame(Edad)
            Data_Alumnos_2 <- data.frame(Edad_2)
            Data_Numeros   <- data.frame(Numeros)
            
          # 1 )  Histograma de frecuencia de alumnos con un promedio de 18 anios
            
                #png(filename  = "Frecuencia.png",
                #       width  = 600             , 
                #       height = 400)
                
                #opar=par(ps=18) 
                #      H    = hist(Data_Alumnos$Edad,
                #      main = "",
                #      xlab = "Edad", 
                #      ylab = "Frecuencia",
                #      col  = "darkgoldenrod4")
            
          # 2 ) Histograma de la densidad de alumnos con un promedio de 18 anios
            
                #tmp           <- density(Edad)
                
                #png(filename  = "Densidad.png",
                #       width  = 600             , 
                #       height = 400)
                
                #opar=par(ps=18) 
                
                #hist(Data_Alumnos$Edad,
                #     breaks = 10,
                #     prob   = TRUE,
                #     ylim   = c(0, max(tmp$y)) ,
                #     col    = "darkgoldenrod4",
                #     border = "gray10",
                #     xlab   = 'Edad',
                #     ylab   = 'Densidad',
                #     main   = '',
                #     freq   = FALSE)
                
                #lines(tmp, 
                #      col   = "blue", 
                #      lty   = 3     , 
                #      lwd   = 2)
                
                
                #curve(dnorm(x, 
                #            mean = mean(Data_Alumnos$Edad), 
                #            sd   = sd(Data_Alumnos$Edad)) , 
                #            add  = TRUE                   , 
                #            col  = "red") 
            
          # 3 )  hacer un histograma en ggplot2 de una poblacion de 500000
            
                #png(filename  = "Ideal.png",
                #       width  = 600        , 
                #       height = 400)
                #opar=par(ps=18)
                #ggplot(data       = Data_Alumnos_2  ,
                #       mapping    = aes(x = Edad_2))   +
                #       geom_histogram(binwidth = 0.01) +
                #       theme(text = element_text(size= 18) ) +
                #       labs(title = '',
                #            x     = 'Edad',
                #            y     = 'Frecuencia'
                # )
            
          # 4 )  hacer un histograma con una media de alto valor haciendo una situacion heterogenea
            
                #png(filename  = "Heterogenea.png",
                #       width  = 600        , 
                #       height = 400)
                #opar=par(ps=10)  # Make text 18 point
                #ggplot(data = Data_Numeros,
                #       mapping = aes(x = Numeros))     +
                #       geom_histogram(binwidth = 0.01) +
                #       theme(text = element_text(size= 18) ) +
                #       labs(title = '',
                #            x     = 'Valor',
                #            y     = 'Frecuencia'
                #  )
          # 5 ) obtener una distribucion normal del metodo Box-muller de 2 variables  
            

                # Edades <- numeric()
                # for(i in 1:(50000/2))
                # {
                #   variables      <- gaussian(18, 1)
                #   Edades         <- c(Edades,variables)
                # }
                # 
                # Alumnos_1   <- data.frame(Edades)
                # tmp         <- density(Alumnos_1$Edades)
                # 
                # png(filename  = "BoxMuller.png",
                #        width  = 600        ,
                #        height = 800)
                # 
                # opar=par(ps=18)
                # hist(Alumnos_1$Edades          ,
                #      prob   = TRUE,
                #      ylim   = c(0, max(tmp$y)) ,
                #      breaks = 10,
                #      col    = "darkgoldenrod4",
                #      border = "gray10",
                #      xlab   = 'Datos Z1 y Z0',
                #      ylab   = 'Densidad',
                #      main   = '',
                #      freq   = FALSE)
                # 
                # lines(tmp,
                #       col   = "blue",
                #       lty   = 3     ,
                #       lwd   = 2)
                # 
                # curve(dnorm(x,
                #             mean = mean(Alumnos_1$Edades),
                #             sd   = sd(Alumnos_1$Edades)) ,
                #       add =TRUE,
                #       col ="red")
                
          # 6 ) obtener una distribucion normal del metodo Box-muller de 1 variable Z0 
                
                  # 
                  # Edades <- numeric()
                  # for(i in 1:(50000/2))
                  # {
                  #   variables      <- gaussianSingle_1(18, 1)
                  #   Edades         <- c(Edades,variables)
                  # }
                  # 
                  # Alumnos_2   <- data.frame(Edades)
                  # tmp         <- density(Alumnos_2$Edades)
                  # 
                  # png(filename  = "BoxMuller_z0.png",
                  #     width  = 600                  ,
                  #     height = 800)
                  # 
                  # opar=par(ps=18)
                  # hist(Alumnos_2$Edades          ,
                  #      prob   = TRUE,
                  #      ylim   = c(0, max(tmp$y)) ,
                  #      breaks = 10,
                  #      col    = "darkgoldenrod4",
                  #      border = "gray10",
                  #      xlab   = 'Datos Z0',
                  #      ylab   = 'Densidad',
                  #      main   = '',
                  #      freq   = FALSE)
                  # 
                  # lines(tmp,
                  #       col   = "blue",
                  #       lty   = 3     ,
                  #       lwd   = 2)
                  # 
                  # curve(dnorm(x,
                  #             mean = mean(Alumnos_2$Edades),
                  #             sd   = sd(Alumnos_2$Edades)) ,
                  #       add =TRUE,
                  #       col ="red")
                  # 
                  # 
                  # 
                  # test <- shapiro.test(runif(100)) # Creamos una variable con distribucion uniforme (no normal) con 100 valores
                  # print(test)

                 
                 
          # 7 ) obtener una distribucion normal del metodo Box-muller de 1 variable Z1


         #          Edades <- numeric()
         #          for(i in 1:(50000/2))
         #          {
         #            variables      <- gaussianSingle_2(18, 1)
         #            Edades         <- c(Edades,variables)
         #          }
         # 
         #          Alumnos_3   <- data.frame(Edades)
         #          tmp         <- density(Alumnos_3$Edades)
         # 
         #          # png(filename  = "BoxMuller_z1.png",
         #          #     width  = 600        ,
         #          #     height = 800)
         # 
         #          opar=par(ps=18)
         #          hist(Alumnos_3$Edades          ,
         #               prob   = TRUE,
         #               ylim   = c(0, max(tmp$y)) ,
         #               breaks = 10,
         #               col    = "darkgoldenrod4",
         #               border = "gray10",
         #               xlab   = 'Datos Z1',
         #               ylab   = 'Densidad',
         #               main   = '',
         #               freq   = FALSE)
         # 
         #          lines(tmp,
         #                col   = "blue",
         #                lty   = 3     ,
         #                lwd   = 2)
         # 
         #          curve(dnorm(x,
         #                      mean = mean(Alumnos_3$Edades),
         #                      sd   = sd(Alumnos_3$Edades)) ,
         #                add =TRUE,
         #                col ="red")
         # 
         # 
         # 
         #          test <- shapiro.test(sample(Edades, 5000, replace = TRUE)) # Creamos una variable con distribucion uniforme (no normal) con 100 valores
         #          print(test)
         # 
         # # 8 ) obtener una distribucion normal del metodo Box-muller de 1 variable Z0 con solo una variable uniforme

# 
#                  Edades <- numeric()
#                  for(i in 1:(50000/2))
#                  {
#                    variables      <- gaussianSingle_1_UnaVariable(18, 1)
#                    Edades         <- c(Edades,variables)
#                  }
# 
#                  Alumnos_4   <- data.frame(Edades)
#                  tmp         <- density(Alumnos_4$Edades)
# 
#                 png(filename  = "BoxMuller_z1_variable_1.png",
#                     width  = 600        ,
#                     height = 800)
# 
#                  opar=par(ps=18)
#                  hist(Alumnos_4$Edades          ,
#                       prob   = TRUE,
#                       ylim   = c(0, max(tmp$y)) ,
#                       breaks = 10,
#                       col    = "darkgoldenrod4",
#                       border = "gray10",
#                       xlab   = 'Datos Z0',
#                       ylab   = 'Densidad',
#                       main   = '',
#                       freq   = FALSE)
# 
#                  lines(tmp,
#                        col   = "blue",
#                        lty   = 3     ,
#                        lwd   = 2)
# 
#                  curve(dnorm(x,
#                              mean = mean(Alumnos_4$Edades),
#                              sd   = sd(Alumnos_4$Edades)) ,
#                        add =TRUE,
#                        col ="red")


        # # 9 ) obtener una distribucion normal del metodo Box-muller de 1 variable Z1 con solo una variable uniforme


                 Edades <- numeric()
                 for(i in 1:(50000/2))
                 {
                   variables      <- gaussianSingle_2_UnaVariable(18, 1)
                   Edades         <- c(Edades,variables)
                 }

                 Alumnos_5   <- data.frame(Edades)
                 tmp         <- density(Alumnos_5$Edades)

                 png(filename  = "BoxMuller_z1_variable_2.png",
                     width  = 600        ,
                     height = 800)

                 opar=par(ps=18)
                 hist(Alumnos_4$Edades          ,
                      prob   = TRUE,
                      ylim   = c(0, max(tmp$y)) ,
                      breaks = 10,
                      col    = "darkgoldenrod4",
                      border = "gray10",
                      xlab   = 'Datos Z1',
                      ylab   = 'Densidad',
                      main   = '',
                      freq   = FALSE)

                 lines(tmp,
                       col   = "blue",
                       lty   = 3     ,
                       lwd   = 2)

                 curve(dnorm(x,
                             mean = mean(Alumnos_5$Edades),
                             sd   = sd(Alumnos_5$Edades)) ,
                       add =TRUE,
                       col ="red")

               while (!is.null(dev.list()))  dev.off()