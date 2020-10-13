

# install.packages("ggplot2")
library(ggplot2)

Visitantes          <- elements <- read.csv(file.path( "Inegi.csv"), fileEncoding="UTF-8-BOM")
VisitantesPorAvion  <- Visitantes$Avion

# 
# png(filename  = "AvionB.png",
#     width  = 800             ,
#     height = 500)
# 
# opar=par(ps=18)
# boxplot(Visitantes$Avion,
#         xlab       = "Ingresos por mes",
#         ylab       = "",
#         col        = c("orange"),
#         border     = "black",
#         horizontal = TRUE,
#         notch      = FALSE,
#         xaxt="n"
# )
# mtext("Aéreo", side=2, line=1)
# axis(side=1, at=axTicks(1),
#      labels=formatC(axTicks(1), format="d", big.mark=','))
# Ingresos por mes

# png(filename  = "MaritimoB.png",
#     width  = 800             ,
#     height = 500)
# opar=par(ps=18)
# boxplot(Visitantes$Maritimo,
#         xlab       = "Ingresos por mes",
#         ylab       = "",
#         col        = c("blue"),
#         border     = "black",
#         horizontal = TRUE,
#         notch      = FALSE,
#         xaxt="n"
# )
# mtext("Maritimo", side=2, line=1)
# axis(side=1, at=axTicks(1),
#      labels=formatC(axTicks(1), format="d", big.mark=','))
# 
# png(filename  = "TerrestreB.png",
#     width  = 800             ,
#     height = 500)
# opar=par(ps=18)
# boxplot(Visitantes$Terrestre,
#         xlab       = "Ingresos por mes",
#         ylab       = "",
#         col        = c("yellow"),
#         border     = "black",
#         horizontal = TRUE,
#         notch      = FALSE,
#         xaxt="n"
# )
# mtext("Terrestre", side=2, line=1)
# axis(side=1, at=axTicks(1),
#      labels=formatC(axTicks(1), format="d", big.mark=','))

mean(VisitantesPorAvion)
# 
# png(filename  = "Avion.png",
#     width  = 800             ,
#     height = 500)

opar=par(ps=18)

hist(VisitantesPorAvion,
            main = "",
            xlab = "Ingresos al mes",
            ylab = "Frecuencia",
            col  = "darkgoldenrod4",xaxt="n")

axis(side=1, at=axTicks(1),
     labels=formatC(axTicks(1), format="d", big.mark=','))
##################################################################3
# png(filename  = "Terrestre.png",
#     width  = 800             ,
#     height = 500)
# 
# opar=par(ps=18)
# 
# hist(Visitantes$Terrestre,
#      main = "",
#      xlab = "Ingresos al mes",
#      ylab = "Frecuencia",
#      col  = "darkgoldenrod4",xaxt="n")
# 
# axis(side=1, at=axTicks(1),
#      labels=formatC(axTicks(1), format="d", big.mark=','))

########################################################################
# png(filename  = "Maritimo.png",
#     width  = 800             ,
#     height = 500)
# 
# opar=par(ps=18)
# 
# hist(Visitantes$Maritimo,
#      main = "",
#      xlab = "Ingresos al mes",
#      ylab = "Frecuencia",
#      col  = "darkgoldenrod4",xaxt="n")
# 
# axis(side=1, at=axTicks(1),
#      labels=formatC(axTicks(1), format="d", big.mark=','))

#t.test(VisitantesPorAvion, mu=1540000) # testing if mean of x could be 640049

VisitantesTerrestres <- Visitantes$Terrestre
VisitantesMaritimos <- Visitantes$Maritimo

wilcox.test(VisitantesTerrestres, mu=640049, conf.int = TRUE)
mean(Visitantes$Terrestre)

mean(VisitantesTerrestres)

mean(VisitantesPorAvion)

wilcox.test(VisitantesTerrestres, VisitantesPorAvion, paired = TRUE)  # g for greater


shapiro.test(VisitantesMaritimos)
# 
# 
 # while (!is.null(dev.list()))  dev.off()


