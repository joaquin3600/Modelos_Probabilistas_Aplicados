


X <- runif(100)
Y <- X*2/3

a <- 1
b <- 2
c <- 3
d <- 4

PrimerMiembro  = cov((a * X) + b, (c * Y) + d)

SegundoMiembro = a * c * cov(X,Y)

print(PrimerMiembro)
print(SegundoMiembro)


PrimerMiembro   = var(X + Y)

SegundoMiembro  = var(X) + var(Y) + (2 * cov(X,Y))
print(PrimerMiembro)
print(SegundoMiembro)




#       png(filename  = "histDE.png",
#           width  = 800             ,
#           height = 500)
#   opar=par(ps=18)
#     hist(ArrayDestandar,
#          main="",
#          xlab="Desviación estandar",
#          ylab="Frecuencia",
#          border="black",
#          col="gold")
#     
#    while (!is.null(dev.list()))  dev.off()
#     