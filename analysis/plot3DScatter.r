#!/usr/bin/Rscript

nas = 1:3
col = c("red","green","blue")

png(filename="3DScatter.png", height=450, width=800, bg="white")

# 3D Scatterplot
library(scatterplot3d)
attach(mtcars)

resFile = "results/experiment_3D.csv"
res = read.csv(resFile, header=FALSE)

for (i in nas){

    
    circles = res$V2[res$V1 == i]
    scales = as.numeric(as.character(res$V3[res$V1 == i]))
    times = res$V4[res$V1 == i]

    if(i==1){
        o = scatterplot3d(
            scales, 
            circles,
            times,
            xlab = "Circles",
            ylab = "time (us)",
            zlab = "scale",
            color=col[i])
    }else{
        o$points3d(scales,circles,times, col=col[i])
    }

}

title(main="3D", col.main="black", font.main=4)


dev.off()
