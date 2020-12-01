library(ggplot2)
library(dplyr)

grupo_1 = c()
grupo_2 = c()
for(j in 1:200)
{
  a <- runif(1, min = 0, max = 3)
  n <- 50
  E <- 3
  X <- rnorm(n,E,1)
  # hist(X)
  contador = 0
  for(i in 1:n)
  {
    if(abs(X[i]) >= a)
    {
      contador = contador + 1
    }
  }
  P <- contador/n
  grupo_1 = c(grupo_1,P)
  print(P)

  Cociente <- E/a
  grupo_2 = c(grupo_2,Cociente)
  print(Cociente)
}

n  <- 1000
mu <- 7
de <- 2
x        <- rnorm(n,mu,de)

  sample_1  <- sample(x, 50)
  Promedio <- sum(sample_1)/length(sample_1)
  
  print(Promedio)
  
# promedios <- c()
# for(i in 1:100)
# {
# }


# promedios = sort(promedios, decreasing = FALSE)
# png(filename  = "Lng_1000.png",
#     width  = 800             ,
#     height = 500)
# 
# opar=par(ps=18)
# barplot(promedios,
#         xlab = "Mediciones",
#         ylab = "Promedios",
#         col = "blue")
# abline(h=7, col = "Red")
# legend("bottomright",
#        c("Promedio analítico"),
#        fill = c("red")
# )
# 
# while (!is.null(dev.list()))  dev.off()
