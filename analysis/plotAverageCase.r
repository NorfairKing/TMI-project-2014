#!/usr/bin/Rscript
nas = 1:3
color = c("red","green","blue")

png(filename="../verslag/illustraties/averageCase.png", height=600, width=800, bg="white")

for (i in nas) {
    resFile = paste("results/experiment_AverageCase.csv", collapse="")
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

legend(
    'topright'
    , c("Algoritme 1","Algoritme 2","Algoritme 3")
    , lty=c(1,1)
    , lwd=c(2.5,2.5)
    , col=color
    )

dev.off()
