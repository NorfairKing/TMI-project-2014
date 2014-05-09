#!/usr/bin/Rscript

nas = 1:3
col = c("red","green","blue")

png(filename="../verslag/illustraties/3DScatter.png", height=1080, width=1920, bg="white")

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
            xlab = "Scales",
            ylab = "Circles",
            zlab = "Times (us)",
            color=col[i])
    }else{
        o$points3d(scales,circles,times, col=col[i])
    }

}

title(main="3D", col.main="black", font.main=4)


dev.off()
