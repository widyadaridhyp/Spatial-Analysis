p = readOGR(dsn='D:/Widya/Universitas Indonesia/Topik Khusus (Spasial)/Tugas/Tugas 2/SHP_Jabar/SHP_Jabar/kabkot_jawabarat.shp')
p
plot(p)
str(p)

X2019 <- read_excel("D:/Widya/Universitas Indonesia/Topik Khusus (Spasial)/Tugas/Tugas 2/2019.xlsx")
View(X2019)
X2019
p@data$row <- as.numeric(row.names(p@data))
p@data
str(X2019)
head(X2019)

X2019$row<- as.numeric(X2019$row)
str(X2019)

p@data$row<- as.numeric(row.names(p@data))
p@data$ID <- as.numeric(row.names(p@data))
str(p@data)

temp <- merge(p@data,X2019, by="row", all.x=T, sort=F)
temp
p@data <- temp[order(temp$row),]
p@data$Cluster <- as.numeric(p@data$Cluster)
plotvar <- p@data$Cluster
nclr <- 9
plotclr <- brewer.pal(nclr,'Oranges')
plotclr <- plotclr[1:nclr]
class <- classIntervals(plotvar, nclr, style="equal")

colcode <- findColours(class, plotclr, digits=4)
plot(p, density=16, col="grey", axes=T, cex.axis=.75)
plot(p, col=colcode, add=T)
text(coordinates(p), labels=p$nama.p, cex=.5)
title(xlab="x",ylab='y',cex.lab=.75,line=2.25)
title(main="Hotspot of HIV 2019", sub="Mapped with R",font.sub=2)

p@data
