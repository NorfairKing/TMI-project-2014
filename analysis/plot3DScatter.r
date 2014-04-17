#!/usr/bin/Rscript

nas = 1:3
col = c("red","green","blue")

png(filename="3DScatter.png", height=450, width=800, bg="white")

# 3D Scatterplot
library(scatterplot3d)
attach(mtcars)

for (i in nas) {
    resFile = paste(c("scaleResults/results_", toString(i), ".csv"), collapse="")
    res = read.csv(resFile, header=FALSE)
    circles = res$V2
    scales = res$V3
    times = res$V4
    if(i==1){
        o = scatterplot3d(scales,circles, times,
            color=col[i])
    }else{
        o$points3d(scales,circles,times, col=col[i])
    }
}
dev.off()
