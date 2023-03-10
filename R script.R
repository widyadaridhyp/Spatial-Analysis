install.packages(c('readxl','GWmodel','car','lmtest','maptools','RColorBrewer','nortest','MASS','sp','spgwr'))
#Library yang digunakan
library(readxl)
library(GWmodel)
library(car)
library(lmtest)
library(maptools)
library(RColorBrewer)
library(spgwr)
library(nortest)
library(MASS)
library(sp)

Data <- read_excel("D:/Widya/Universitas Indonesia/Topik Khusus (Spasial)/Tugas/Tugas 3/poverty1.xlsx")
mod<-lm(kemiskinan~ APS+PDRB+kepadatan+rutilahu+sanitasi+pengangguran, data = Data)
summary(mod)
bptest(mod) #uji heteroskedastisitas
vif(mod) #uji multikolinieritas
AIC(mod) 
durbinWatsonTest(mod) #uji normalitas
dwtest(mod)
ad.test(mod$residuals)

y= Data$kemiskinan
x1= Data$APS
x2= Data$PDRB
x3= Data$kepadatan
x4= Data$rutilahu
x5= Data$sanitasi
x6= Data$pengangguran



#matriks jarak
Data$latitude<-as.numeric(Data$latitude)
Data$longitude <- as.numeric(Data$longitude)
dataspdf<- SpatialPointsDataFrame(Data[,3:4],Data)
head(dataspdf)

coords<- Data[,c("latitude","longitude")]
matriksjarak<-gw.dist(dp.locat = coordinates(dataspdf))
matriksjarak

#pseudostepwise
#GWR GAUSSIAN
DeVar<-"y"
InDeVars<-c('x1',"x2","x3",'x4',"x5","x6")
model1<-model.selection.gwr(DeVar,InDeVars,data=dataspdf,
                            kernel="gaussian",adaptive = FALSE, bw=1, approach = "CV",dMat=matriksjarak)
sortedmod<-model.sort.gwr(model1,numVars = length(InDeVars),
                          ruler.vector = model1[[2]][,2])

model.list<- sortedmod[[1]]
model.view.gwr(DeVar,InDeVars,model.list = model.list)
plot(sortedmod[[2]][,2],col='black',pch=20,lty=5,main="Alternative view of GWE model selection procedure",
     ylab='CV',xlab='Model Number',type='b')


#Menentuka bandwidth optimum
#Untuk model GWR dengan fungsi Kernel Gaussian
bw.mod.gwr.gauss<-bw.gwr(y~x1+x2+x3+x4+x5+x6, data=dataspdf, approach="CV", kernel="gaussian", adaptive=F)
bw.mod.gwr.gauss

#Untuk model GWR dengan fungsi Kernel Bisquare
bw.mod.gwr.bisq<-bw.gwr(y~x1+x2+x3+x4+x5+x6, data=dataspdf, approach="CV", kernel="bisquare", adaptive=F)
bw.mod.gwr.bisq

#Melakukan estimasi parameter model GWR
#Untuk model GWR dengan fungsi Kernel Gaussian
gwr.gaus<-gwr.basic(y~x1+x2+x3+x4+x5+x6 , data=dataspdf, bw=bw.mod.gwr.gauss, kernel="gaussian", adaptive=F)
print(gwr.gaus)

#Untuk model GWR dengan fungsi Kernel Bisquare
gwr.bisq<-gwr.basic(y~x1+x2+x3+x4+x5+x6, data=dataspdf, bw=bw.mod.gwr.bisq, kernel="bisquare", adaptive=F)
print(gwr.bisq)
summary(gwr.bisq)

#Melakukan uji signifikansi parameter GWR
gwr.t.adjust(gwr.gaus)



#SEBARAN PEUBAH SIGNIFIKAN
install.packages(c('foreign','lattice','rgdal','classInt','class','e1071','shapefiles','rgeos','ggmap','prettymapr','tmap'))

library(foreign)
library(lattice)
library(rgdal)
library(classInt)
library(class)
library(e1071)
library(shapefiles)
library(rgeos)
library(ggmap)
library(prettymapr)
library(tmap)

peta<-readShapePoly('D:/Widya/Universitas Indonesia/Topik Khusus (Spasial)/Tugas/Tugas 2/SHP_Jabar/SHP_Jabar/kabkot_jawabarat.shp') 
peta
peta@data$row <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27)
#merge data dengan shapefile
temp <- merge(peta@data, Data, by="row", all.xx=T, sort=F)
peta@data <- temp[order(temp$row),]


#membuat plot variabel y
plotvar <- peta@data$kemiskinan
nclr <- 9
plotclr <- brewer.pal(nclr,'Greens')
plotclr <- plotclr[3:nclr]
class <- classIntervals(plotvar, 6, style='equal')
colcode <- findColours(class, plotclr, digits=6)
plot(peta, density=16, col='grey', axes=T, cex.axis=.75)
plot(peta, col=colcode, add=T)
text(coordinates(peta), labels=peta$kabkot, cex=0.7)
title(xlab='longitude',ylab='latitude', cex.lab=.75,line=2.25)
title(main='Persentase Penduduk Miskin Berdasarkan Kabupaten/Kota di Jawa Barat Tahun 2019', sub='Mapped with R',font.sub=2)

#membuat plot variabel X1
plotvar <- peta@data$APS
nclr <- 9
plotclr <- brewer.pal(nclr,'Oranges')
plotclr <- plotclr[3:nclr]
class <- classIntervals(plotvar, 6, style='equal')
colcode <- findColours(class, plotclr, digits=6)
plot(peta, density=16, col='grey', axes=T, cex.axis=.75)
plot(peta, col=colcode, add=T)
text(coordinates(peta), labels=peta$kabkot, cex=0.7)
title(xlab='longitude',ylab='latitude', cex.lab=.75,line=2.25)
title(main='Angka Partisipasi Sekolah Menengah Berdasarkan Kabupaten/Kota di Jawa Barat Tahun 2019', sub='Mapped with R',font.sub=2)

#membuat plot variabel X2
plotvar <- peta@data$PDRB
nclr <- 9
plotclr <- brewer.pal(nclr,'Reds')
plotclr <- plotclr[3:nclr]
class <- classIntervals(plotvar, 6, style='equal')
colcode <- findColours(class, plotclr, digits=6)
plot(peta, density=16, col='grey', axes=T, cex.axis=.75)
plot(peta, col=colcode, add=T)
text(coordinates(peta), labels=peta$kabkot, cex=0.7)
title(xlab='longitude',ylab='latitude', cex.lab=.75,line=2.25)
title(main='Produk Domestik Regional Bruto Atas Dasar Harga Berlaku Berdasarkan Kabupaten/Kota di Jawa Barat Tahun 2019', sub='Mapped with R',font.sub=2)

#membuat plot variabel X3
plotvar <- peta@data$kepadatan
nclr <- 9
plotclr <- brewer.pal(nclr,'Blues')
plotclr <- plotclr[3:nclr]
class <- classIntervals(plotvar, 6, style='equal')
colcode <- findColours(class, plotclr, digits=6)
plot(peta, density=16, col='grey', axes=T, cex.axis=.75)
plot(peta, col=colcode, add=T)
text(coordinates(peta), labels=peta$kabkot, cex=0.7)
title(xlab='longitude',ylab='latitude', cex.lab=.75,line=2.25)
title(main='Kepadatan Penduduk Berdasarkan Kabupaten/Kota di Jawa Barat Tahun 2019', sub='Mapped with R',font.sub=2)

#membuat plot variabel X4
plotvar <- peta@data$rutilahu
nclr <- 9
plotclr <- brewer.pal(nclr,'BuGn')
plotclr <- plotclr[3:nclr]
class <- classIntervals(plotvar, 6, style='equal')
colcode <- findColours(class, plotclr, digits=6)
plot(peta, density=16, col='grey', axes=T, cex.axis=.75)
plot(peta, col=colcode, add=T)
text(coordinates(peta), labels=peta$kabkot, cex=0.7)
title(xlab='longitude',ylab='latitude', cex.lab=.75,line=2.25)
title(main='Jumlah Pemilik Rumah Tidak Layak Huni (RUTILAHU) Yang Memiliki Lahan Sendiri Berdasarkan Kabupaten/Kota di Jawa Barat Tahun 2019', sub='Mapped with R',font.sub=2)

#membuat plot variabel X5
plotvar <- peta@data$sanitasi
nclr <- 9
plotclr <- brewer.pal(nclr,'BuPu')
plotclr <- plotclr[3:nclr]
class <- classIntervals(plotvar, 6, style='equal')
colcode <- findColours(class, plotclr, digits=6)
plot(peta, density=16, col='grey', axes=T, cex.axis=.75)
plot(peta, col=colcode, add=T)
text(coordinates(peta), labels=peta$kabkot, cex=0.7)
title(xlab='longitude',ylab='latitude', cex.lab=.75,line=2.25)
title(main='Jumlah Desa yang Melaksanakan Sanitasi Total Berbasis Masyarakat (STBM) Berdasarkan Kabupaten/Kota di Jawa Barat Tahun 2019', sub='Mapped with R',font.sub=2)

#membuat plot variabel X6
plotvar <- peta@data$pengangguran
nclr <- 9
plotclr <- brewer.pal(nclr,'OrRd')
plotclr <- plotclr[3:nclr]
class <- classIntervals(plotvar, 6, style='equal')
colcode <- findColours(class, plotclr, digits=6)
plot(peta, density=16, col='grey', axes=T, cex.axis=.75)
plot(peta, col=colcode, add=T)
text(coordinates(peta), labels=peta$kabkot, cex=0.7)
title(xlab='longitude',ylab='latitude', cex.lab=.75,line=2.25)
title(main='Persentase Pengangguran Terbuka Berdasarkan Kabupaten/Kota di Jawa Barat Tahun 2019', sub='Mapped with R',font.sub=2)

#MEMBUAT LEGEND PADA PLOT
kab <- list('sp.pointLabel',dataspdf,label=peta@data$kabkot
            , cex=0.5, col='midnightblue')
titik<-list('sp.points', dataspdf, pch=19, cex=.8,
            col='midnightblue')

#plot peta sebaran parameter X1
gwr_X1<- gwr.bisq$SDF$x1 
peta@data$gwr_X1<- gwr_X1 
spplot(peta,zcol="gwr_X1",main="Peta Sebaran Penduga Parameter Beta1 (x1)", 
       scales = list(draw = TRUE), cuts=8,
       sp.layout=list(kab, titik), xlab="Longitude",
       ylab="Latitude",
       col.regions=brewer.pal(n = 9, name = 'Oranges'))

#plot peta sebaran parameter X2
gwr_X2<- gwr.bisq$SDF$x2 
peta@data$gwr_X2<- gwr_X2 
spplot(peta,zcol="gwr_X2",main="Peta Sebaran Penduga Parameter Beta2 (x2)", 
       scales = list(draw = TRUE), cuts=4,
       sp.layout=list(kab, titik), xlab="Longitude",
       ylab="Latitude",
       col.regions=brewer.pal(n = 5, name = 'Reds'))

#plot peta sebaran parameter X3
gwr_X3<- gwr.bisq$SDF$x3 
peta@data$gwr_X3<- gwr_X3 
spplot(peta,zcol="gwr_X3",main="Peta Sebaran Penduga Parameter Beta3 (x3)", 
       scales = list(draw = TRUE), cuts=7,
       sp.layout=list(kab, titik), xlab="Longitude",
       ylab="Latitude",
       col.regions=brewer.pal(n = 8, name = 'Blues'))

#plot peta sebaran parameter X4
gwr_X4<- gwr.bisq$SDF$x4 
peta@data$gwr_X4<- gwr_X4 
spplot(peta,zcol="gwr_X4",main="Peta Sebaran Penduga Parameter Beta4 (x4)", 
       scales = list(draw = TRUE), cuts=7,
       sp.layout=list(kab, titik), xlab="Longitude",
       ylab="Latitude",
       col.regions=brewer.pal(n = 8, name = 'BuGn'))

#plot peta sebaran parameter X5
gwr_X5<- gwr.bisq$SDF$x5 
peta@data$gwr_X5<- gwr_X5 
spplot(peta,zcol="gwr_X5",main="Peta Sebaran Penduga Parameter Beta5 (x5)", 
       scales = list(draw = TRUE), cuts=5,
       sp.layout=list(kab, titik), xlab="Longitude",
       ylab="Latitude",
       col.regions=brewer.pal(n = 6, name = 'BrBG'))

#plot peta sebaran parameter X6
gwr_X6<- gwr.bisq$SDF$x6 
peta@data$gwr_X6<- gwr_X6 
spplot(peta,zcol="gwr_X6",main="Peta Sebaran Penduga Parameter Beta6 (x6)", 
       scales = list(draw = TRUE), cuts=4,
       sp.layout=list(kab, titik), xlab="Longitude",
       ylab="Latitude",
       col.regions=brewer.pal(n = 5, name = 'OrRd'))

#plot peta sebaran studentized residual model linear
mlr_studres <- mod$residuals
peta@data$mlr_studres<- mlr_studres
spplot(peta, zcol='mlr_studres',
       main='Peta Sebaran Studentized Residual Model Regresi Linier',
       scales = list(draw = TRUE), cuts=7,
       sp.layout =list(kab, titik), xlab='Longitude',
       ylab='Latitude',
       col.regions=brewer.pal(n = 8, name = 'PRGn'))

#plot peta sebaran studentized residual model gwr bisquare 
gwr_studres <- gwr.bisq$SDF$Stud_residual
peta@data$gwr_studres<- gwr_studres
spplot(peta, zcol='gwr_studres',
       main='Peta Sebaran Studentized Residual Model GWR Bisquare',
       scales = list(draw = TRUE), cuts=4,
       sp.layout =list(kab, titik), xlab='Longitude',
       ylab='Latitude',
       col.regions=brewer.pal(n = 5, name = 'PiYG'))


#plot peubah signifikan
#Tabel Sig
# Melakukan uji signifikansi parameter GWR
#Tabel Sig
# Melakukan uji signifikansi parameter GWR
tbisq <-gwr.t.adjust(gwr.bisq)[["results"]][["p"]]
View(tbisq)
t_bisq <- data.frame(tbisq)
write_xlsx(t_bisq, "D:/Widya/Universitas Indonesia/Topik Khusus (Spasial)/Tugas/Tugas 3/tbisq.xlsx")

# Membuat tabel kelompok yang signifikan
sigtable <- tbisq[,-1]
sigtable[sigtable >= 0.05] <- 2
sigtable[sigtable < 0.05] <- 1
sigtable[sigtable == 2] <- 0
View(sigtable)
t_sig <- data.frame(sigtable)
write_xlsx(t_sig, "D:/Widya/Universitas Indonesia/Topik Khusus (Spasial)/Tugas/Tugas 3/sigtable.xlsx")
gwrsdf <- data.frame(gwr.bisq$SDF)
write_xlsx(gwrsdf, "D:/Widya/Universitas Indonesia/Topik Khusus (Spasial)/Tugas/Tugas 3/gwrsdf.xlsx")
unique(sigtable)
sigclass <- c()
for (i in 1:length(sigtable[,1])) {
  a <- sigtable[i,]
  if (isTRUE(identical(a, unique(sigtable)[1,]))) {
    sigclass[i] <- 1
  }
  if (isTRUE(identical(a, unique(sigtable)[2,]))) {
    sigclass[i] <- 2
  }
  if (isTRUE(identical(a, unique(sigtable)[3,]))) {
    sigclass[i] <- 3
  }
  if (isTRUE(identical(a, unique(sigtable)[4,]))) {
    sigclass[i] <- 4
  }
}
sigclass

seq <- rep(1, length(sigclass))
tabel.signifikan <- cbind(peta@data$kabkot, sigclass, seq)
colnames(tabel.signifikan) <- c('kabkot','Kelompok Signifikansi','Seq')
tabel.signifikan <- as.data.frame(tabel.signifikan)
View(tabel.signifikan)

df <- read_excel("D:/Widya/Universitas Indonesia/Topik Khusus (Spasial)/Tugas/Tugas 3/poverty1.xlsx")
View(df)

colnames(tabel.signifikan) <- c("kabkot","Sig","Seq")

peta@data <- merge(peta@data, df, by="kabkot", all.br=T, sort=F)
peta@data <- merge(peta@data, tabel.signifikan, by="kabkot", all.br=T, sort=F)
View(peta@data)
#layout peta
koord<-peta@data
koord.spdf<-SpatialPointsDataFrame(koord[7:8],koord)
kab <-list('sp.pointLabel',koord.spdf,label=peta@data$kabkot, cex=0.5, col='midnightblue')
titik<-list('sp.points', koord.spdf, pch=19, cex=.8, 
            col='midnightblue')

str(peta@data)
View(peta@data)
peta@data$Sig <- as.integer(peta@data$Sig)

spplot(peta, "Sig", main="Peta Sebaran Variabel Signifikan", scales = list(draw = TRUE), 
       sp.layout=list(kab, titik), xlab="longitude", ylab="latitude",cuts=6,
       col.regions=brewer.pal(n = 7, name ='Greens'))

install.packages('writexl')
library(writexl)
df <- data.frame(peta@data)
write_xlsx(df, "D:/Widya/Universitas Indonesia/Topik Khusus (Spasial)/Tugas/Tugas 3/poverty2.xlsx")
