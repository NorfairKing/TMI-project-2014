#!/usr/bin/Rscript
nas = 1:3
color = c("red","green","blue")
scale = 0.5
err = 0.1

png(filename="unscaled.png", height=450, width=800, bg="white")

for (i in nas) {
    resFile = paste(c("results/results_", toString(i), ".csv"), collapse="")
    res = read.csv(resFile, header=FALSE)
    res <- subset(res, abs(as.numeric(as.character(V3))-scale) <= err)
    circles = res$V2
    scales = res$V3
    times = res$V4
    if(i == 1){
        plot(circles, times, xlab="", ylab="", col=color[i])
    }else{
        points(circles, times, col=color[i])
    }
}


title(main="Unscaled", xlab="circles (#)", ylab="time (us)", col.main="black", font.main=4)

dev.off()
