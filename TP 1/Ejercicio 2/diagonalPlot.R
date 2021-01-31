source("../Ejercicio 1/diagonal.R")

puntos <- generateDiagonalData(500, 2, 0.25)

plot(x = 1,
     type = "n",
     xlim = c(-4, 4), 
     ylim = c(-4, 4),
     xlab = "", 
     ylab = "",
     main = "Diagonal, n = 500, d = 2, C = 0.25")

segments(-4, 0, 4, 0)
segments(0, -4, 0, 4)
     
points(x = puntos$X1[puntos$X3 == 1],
       y = puntos$X2[puntos$X3 == 1],
       pch=16,cex=0.4,
       col = "skyblue3")
points(x = puntos$X1[puntos$X3 == 0],
       y = puntos$X2[puntos$X3 == 0],
       pch=16,cex=0.4,
       col = "tomato3")

