
 ViaAerea                 <-	c( 1824790, 1862497, 990710 , 31100  , 36210 , 135230)
 ViaTerrestre             <-	c( 229655 , 248994 , 205766 , 55250  , 108790, 150880)

 TuristaPeatones          <-	c( 279483 , 298737 , 266299 , 151304 , 155501, 198265)
 TuristaAutomovil         <-	c( 1461464, 1360608, 1302368, 539498 , 590141, 497010)

 ExcursionistasPeatones   <-	c( 799819 , 787536 , 650756 , 354411 ,360008 , 308434)
 ExcursionistasAutomovil  <-	c( 2741397, 2773523, 2496188, 1097160,1134730, 1136793)
 ExcursionistasCruceros   <-	c( 1084297, 830005 , 666071 , 0      ,0      , 0)

 MediosTransporte         <-    c( "Via aérea"    ,
                                   "Via terrestre",
                                   "Peatones"     ,
                                   "automóviles"  ,
                                   "Peatones"     ,
                                   "automóviles"  ,
                                   "En Cruceros")
 
TipoIngreso               <-    c( "Turistas de internación",
                                   "Turistas fronterizos"   ,
                                   "Excursionistas fronterizos",
                                   "Excursionistas en cruceros")


png(filename = "Turismo.png",
    width = 800, height = 800)

 boxplot(ViaAerea,
         ViaTerrestre,
         TuristaPeatones,
         TuristaAutomovil,
         ExcursionistasPeatones,
         ExcursionistasAutomovil,
         ExcursionistasCruceros,
         main          = "Visitantes internacionales que ingresaron al Pais (2020 Enero - 2020 Junio)",
         names         = MediosTransporte,
         xlab          = "Medio de transporte",
         ylab          = "Numero de visitantes",
         col           = c("#05A1A0","#05A1A0", "#E16c32", "#E16c32","#057ea0","#057ea0","#944c00"),
         border        =  "black",
         horizontal    = FALSE,
         notch         = FALSE)
 
 legend("topleft", 
        legend = TipoIngreso         ,
        col    = c( "#05A1A0" , "#E16c32" , "#057ea0" , "#944c00") , 
        bty    = "n",
        pch    = 20 , 
        pt.cex = 3  , 
        cex    = 1  , 
        horiz  = FALSE, 
        inset  = c(0.03, 0.1))
 

 
 dev.off()
