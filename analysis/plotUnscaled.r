nas = 1:3
color = c("red","green","blue")
scale = 1

png(filename="unscaled.png", height=450, width=800, bg="white")

for (i in nas) {
    resFile = paste(c("results/results_", toString(i), ".csv"), collapse="")
    res = read.csv(resFile, header=FALSE)
    circles = res$V2[res$V3==scale]
    times = res$V4[res$V3==scale]
    if(i == 1){
        plot(circles, xlab="", ylab="", times, col=color[i])
    }else{
        points(circles, times, col=color[i])
    }
}


title(main="Unscaled", xlab="circles (#)", ylab="time (us)", col.main="black", font.main=4)

dev.off()
