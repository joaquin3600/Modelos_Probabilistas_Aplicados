


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



#    while (!is.null(dev.list()))  dev.off()
#     