setwd("~/Documents/git/DataAnalysis3")
rm(list = ls())
library(plyr)
library(maps)
library(mapdata)
library(maptools)  
library(scales)
library(ggplot2)
library(mapproj)
library(ggmap)
library(ggbiplot)
library(ggfortify)
library(caret)
library(kernlab)
library(RDRToolbox)
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}




set.seed(1)
ssts <- read.table("./SST011970_032003.dat.txt")
locs <- read.table("./SSTlonlat.dat.txt")
landmasks <- read.table("./SSTlandmask.dat.txt")
indexes <- read.table("./Nino34index.dat")
cnames <- expand.grid(month.abb, 1970:2002)
cnames <- mdply(cnames, 'paste', sep = "")
cnames <- c(cnames[, 3], "Jan2003", "Feb2003", "Mar2003")
colnames(ssts) <- cnames
names(locs) <- c("lon", "lat")
names(landmasks) <- "land"
rownames(indexes) <- cnames
keep <- (landmasks == 0)
temps <- data.frame(ave = apply(ssts, 1, mean), 
                    sd = apply(ssts, 1, sd))
p1 <- ggplot(data = locs[keep, ], 
             aes(x = lon, y = lat, color = temps$ave[keep]))
p1 <- p1 + geom_point() + 
        scale_colour_gradient(name = "Mean Temp.", 
                              low = "darkblue", 
                              high = "yellow") +
        ylab("Latitude") +
        xlab("Longitude") + 
        ggtitle("Mean SST from Different Location") +
        theme(plot.title = element_text(lineheight=1, face="bold"))
p2 <- ggplot(data = locs[keep, ], 
             aes(x = lon, y = lat, color = temps$sd[keep]))
p2 <- p2 + 
        geom_point() + 
        scale_colour_gradient(name = "Std. Dev.", 
                              low = "darkblue", 
                              high = "yellow") +
        ylab("Latitude") +
        xlab("Longitude") + 
        ggtitle("Standard Deviation of SST from Different Locations") +
        theme(plot.title = element_text(lineheight=1, face="bold"))
multiplot(p1, p2, cols=1)


pr.input <- t(ssts[keep, ])
pr.out.nonsd <- prcomp(pr.input)
pr.out.sd <- prcomp(pr.input, scale. = TRUE)
pr.out.nonsd.var <- pr.out.nonsd$sdev^2
pr.out.sd.var <- pr.out.sd$sdev^2
loadingmap1 <- ggplot(data = locs[keep, ], 
                      aes(x = lon, y = lat, color = pr.out.nonsd$rotation[, 1]))
loadingmap1 + 
        geom_point() + 
        scale_colour_gradient(name = "Mean Temp.", 
                              low = "darkblue", 
                              high = "yellow") +
        ylab("Latitude") +
        xlab("Longitude") + 
        ggtitle("First Component Loading Maps") +
        theme(plot.title = element_text(lineheight=1, face="bold"))
loadingmap2 <- ggplot(data = locs[keep, ], 
                      aes(x = lon, y = lat, color = pr.out.nonsd$rotation[, 2]))
loadingmap2 + 
        geom_point() + 
        scale_colour_gradient(name = "Mean Temp.", 
                              low = "darkblue", 
                              high = "yellow") +
        ylab("Latitude") +
        xlab("Longitude") + 
        ggtitle("Second Component Loading Maps") +
        theme(plot.title = element_text(lineheight=1, face="bold"))
pc1series <- ggplot(data.frame(pc1 = pr.out.nonsd$x[, 1]), aes(x = 1:399, y = pc1))
pc1series <- pc1series + geom_line() +
        xlab("Month") +
        ylab("PC1 Scores") +
        ggtitle("First Component Time Series") +
        theme(plot.title = element_text(lineheight=1, face="bold"))
pc2series <- ggplot(data.frame(pc2 = pr.out.nonsd$x[, 2]), aes(x = 1:399, y = pc2))
pc2series <- pc2series + geom_line() + 
        xlab("Month") +
        ylab("PC2 Scores") +
        ggtitle("Second Component Time Series") +
        theme(plot.title = element_text(lineheight=1, face="bold"))
multiplot(pc1series, pc2series, cols=2)



set.seed(3)
Z <- pr.out.nonsd$x[, 1:4]
km.out=kmeans(Z , 3, nstart=20)
km.plot <- ggplot(data = data.frame(Z, class = factor(km.out$cluster)), 
                  aes(x = PC1, y = PC2, colour = class))
km.plot <- km.plot + geom_point()
cl.plot <- ggplot(data = data.frame(Z, class = factor(indexes[, 1])), 
                  aes(x = PC1, y = PC2, colour = class))
cl.plot <- cl.plot + geom_point()
multiplot(km.plot, cl.plot, cols=1)
confusionMatrix(km.out$cluster, indexes[, 1]+2)


hc.average <- hclust(dist(Z), method="average")
hc.complete <- hclust(dist(Z), method="complete")
hc.average.cluster <- c(2, 1, 3)[cutree(hc.average, 3)]
hc.complete.cluster <- c(2, 1, 3)[cutree(hc.complete, 3)]
confusionMatrix(hc.average.cluster, indexes[, 1]+2)
confusionMatrix(hc.complete.cluster, indexes[, 1]+2)

sigma.grid <- c(0.01, 0.005, 0.001, 0.00001)
kpca.out <- c()
kpca.p <- list()
for (i in 1:length(sigma.grid)){
        kpca.out <- c(kpca.out, kpca( ~ ., data = data.frame(pr.input), 
                                      kpar = list(sigma = sigma.grid[i])))
        kpca.p[[i]] <- ggplot(data = data.frame(kpca.out[[i]]@rotated[, 1:2], 
                                                class = factor(indexes[, 1])), 
                              aes(x = X1, y = X2, colour = class)) + 
                geom_point() + 
                ggtitle(i)
}
multiplot(plotlist = kpca.p, cols = 2)
ggplot(data = (temp = data.frame(t(ssts[sample(1:nrow(ssts), 2), ]),class = factor(indexes[, 1]))), aes(x = temp[, 1], y = temp[,2], colour = class))+geom_point()+xlab(names(temp)[1])+ylab(names(temp)[2])


four_result <- data.frame(matrix(NA, ncol = 4, nrow = 0))
names(four_result) <- c("time", "kernel_width", "PC", "Scores")
for (i in 1:length(sigma.grid)){
        if (exists("d")){
                rm(d)
        }
        if (exists("temp")){
                rm(temp)
        }
        d = data.frame(time = 1:nrow(kpca.out[[i]]@rotated[, 1:2]),
                       kernel_width = rep(sigma.grid[i], nrow(kpca.out[[i]]@rotated[, 1:2])),
                       kpca.out[[i]]@rotated[, 1:2])
        temp <- reshape(data = d, 
                        direction = "long", 
                        varying = list(names(d)[3:4]), 
                        v.names = "Scores",
                        idvar = c("time", "kernel_width"),
                        timevar = "PC", 
                        times = c("PC1", "PC2"))
        rownames(temp) <- NULL
        four_result <- rbind(four_result, temp)
}

multi.time.plot <- ggplot(data = four_result, 
                          aes(x = time, y = Scores)) +
        geom_line() +
        facet_grid(kernel_width ~ PC)


k = c(5, 10, 20, 40)
isomap.out <- list()
LLE.out <- list()
for (i in 1:length(k)){
        isomap.out[[i]] <- Isomap(data = pr.input, k = k[i])
        LLE.out[[i]] <- Isomap(data = pr.input, k = k[i])
}
isomap.out <- lapply(isomap.out, 
                     data.frame, 
                     class = factor(indexes[, 1]))
LLE.out <- lapply(LLE.out, 
                  data.frame, 
                  class = factor(indexes[, 1]))
isomap.p <- list()
for (i in 1:length(k)){
        isomap.p[[i]] <- ggplot(data = isomap.out[[i]], 
                                aes(x = dim2.1, y = dim2.2, colour = class)) + 
                geom_point()
}
multiplot(plotlist = isomap.p, cols = 2)
LLE.p <- list()
for (i in 1:length(k)){
        LLE.p[[i]] <- ggplot(data = LLE.out[[i]], 
                             aes(x = dim2.1, y = dim2.2, colour = class)) + 
                geom_point()
}
multiplot(plotlist = LLE.p, cols = 2)
iso.lle.time.series <- rbind(cbind(time = 1:nrow(isomap.out[[4]]), 
                                   method = rep("Isomap", 
                                                nrow(isomap.out[[4]])), 
                                   isomap.out[[4]][, -3]), 
                             cbind(time = 1:nrow(LLE.out[[4]]), 
                                   method = rep("LLE", 
                                                nrow(LLE.out[[4]])), 
                                   LLE.out[[4]][, -3])
)
iso.lle.time.series <- reshape(data = iso.lle.time.series, 
                               direction = "long", 
                               varying = list(names(iso.lle.time.series)[3:4]), 
                               v.names = "Scores", 
                               idvar = c("time", "method"),
                               timevar = "Dim", 
                               times = c("First Dim", "Second Dim")
)
rownames(iso.lle.time.series) <- NULL
iso.lle.time.plot <- ggplot(data = iso.lle.time.series, 
                            aes(x = time, y = Scores)) +
        geom_line() + 
        facet_grid(method ~ Dim)

pp.1 <- isomap.p[[4]]
pp.2 <- LLE.p[[4]]
pp.3 <- kpca.p[[3]]
pp.4 <- ggplot(data = data.frame(pr.out.nonsd$x[, 1:2], 
                                 class = factor(indexes[, 1])), 
               aes(x = PC1, y = PC2, colour = class)) + 
        geom_point()
multiplot(pp.1, pp.3, pp.2, pp.4, cols = 2)


























