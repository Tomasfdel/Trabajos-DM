source("../Ejercicio 1/spirals.R")

puntos <- generateSpiralsData(500)

plot(x = 1,
     type = "n",
     xlim = c(-1.25, 1.25), 
     ylim = c(-1.25, 1.25),
     xlab = "", 
     ylab = "",
     main = "Espirales Anidadas, n = 500")

segments(-1.5, 0, 1.5, 0)
segments(0, -1.5, 0, 1.5)
     
points(x = puntos$X1[puntos$X3 == 1],
       y = puntos$X2[puntos$X3 == 1],
       pch=16,cex=0.4,
       col = "skyblue3")
points(x = puntos$X1[puntos$X3 == 0],
       y = puntos$X2[puntos$X3 == 0],
       pch=16,cex=0.4,
       col = "tomato3")

