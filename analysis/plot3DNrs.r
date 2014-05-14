#!/usr/bin/Rscript


png(filename="../verslag/illustraties/3DnrIs.png", height=600, width=800, bg="white")

# 3D Scatterplot
library(scatterplot3d)
attach(mtcars)

resFile = "results/experiment_NrIntersections.csv"
res = read.csv(resFile, header=FALSE)

circles = res$V1
scales = as.numeric(as.character(res$V2))
times = res$V3

o = scatterplot3d(
            scales, 
            circles,
            times,
            xlab = "Scales",
            ylab = "Circles",
            zlab = "# Intersections",
            )

title(
    main="3D # Intersections"
    , col.main="black"
    , font.main=4
    )


dev.off()
