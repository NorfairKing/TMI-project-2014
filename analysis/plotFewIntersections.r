#!/usr/bin/Rscript
nas = 1:3
color = c("red","green","blue")

png(filename="fewIntersections.png", height=1080, width=1920, bg="white")

for (i in nas) {
    resFile = paste("results/experiment_FewIntersections.csv", collapse="")
    res = read.csv(resFile, header=FALSE)
    
    circles = res$V2[res$V1 == i]
    times = res$V4[res$V1 == i]
    
    if(i == 1){
        plot(
              circles
            , times
            , xlab="Circles"
            , ylab="Time (us)"
            , col=color[i]
            )
    }else{
        points(circles, times, col=color[i])
    }
}


title(
      main="Few Intersections"
    , col.main="black"
    , font.main=4
    )

dev.off()
