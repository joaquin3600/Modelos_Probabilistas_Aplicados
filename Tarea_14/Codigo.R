
  n      <- 500
  vector <- rexp(n,2)

  hist(vector,
       main  = "",
       xlab  = "Valor",
       ylab  = "Frecuencia",
       border= "black",
       col   = "purple")

  medias      <- numeric()
  sampleprint <- numeric();
  nmuestral   <- 50
  for(i in 1:n)
  {
    sample_1    <- sample(vector, nmuestral)
    medias      <- c(medias, sum(sample_1)/nmuestral)
    sampleprint <- sample_1
  }
  # png(filename  = "medias_50.png",
  #     width  = 800             ,
  #     height = 500)
  opar=par(ps=18)
  hist(medias,
       main  = "",
       xlab  = "Media",
       ylab  = "Frecuencia",
       border= "black",
       col   = "blue")
  abline(v = mean(medias),
         col = "red",
         lwd = 2)
  legend(x = "topright", # location of legend within plot area
         c("Media"),
         col = c("red"),
         lwd = c(2, 2, 2))

  print(sampleprint)



  while (!is.null(dev.list()))  dev.off()
