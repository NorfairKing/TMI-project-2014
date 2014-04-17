nas = 1:1
col = c("red","green","blue")

#png(filename="3D.png", height=450, width=800, bg="white")

library(akima)
library(lattice)
library(rgl)

for (i in nas) {
    resFile = paste(c("results/results_", toString(i), ".csv"), collapse="")
    res = read.csv(resFile, header=FALSE)
    circles = res$V2
    scales = res$V3
    times = res$V4
    persp(times)a
}
#dev.off()
