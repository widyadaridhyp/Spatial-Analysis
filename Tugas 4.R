install.packages(c('tmap','readxl','rgdal','raster','spdep','lmtest','nortest','DescTools','RcolorBrewer','car','GWmodel','olsrr','shapefiles','spatialreg','spdep','class','classInt'))

library(sp)
library(spData)
library(readxl)
library(GWmodel)      #GWR Jarak Spasiallibrary(readxl)       #membaca file excel
library(rgdal)        #membaca file shp, fs readOGR
library(raster)       #eksplorasi data
library(spdep)        #pembobot spasial (fungsi poly2nb)
library(lmtest)       #regresi klasik
library(nortest)      #uji kenormalan galat
library(DescTools)    #uji kebebasan galat
library(spatialreg)   #model SAR
library(RColorBrewer) #memberi warna peta
library(car)          #uji multikolinieritas
library(olsrr)
library(shapefiles)
library(classInt)
library(class)
library(tmap)

kemiskinan <- read_excel("D:/Widya/Universitas Indonesia/Topik Khusus (Spasial)/Tugas/Tugas 3/poverty1.xlsx")
y <- kemiskinan$kemiskinan
x1 <- kemiskinan$APS
x2 <- kemiskinan$PDRB
x3 <- kemiskinan$kepadatan
x4 <- kemiskinan$rutilahu
x5 <- kemiskinan$sanitasi
x6 <- kemiskinan$pengangguran
View(kemiskinan)
jabar <- readShapePoly('D:/Widya/Universitas Indonesia/Topik Khusus (Spasial)/Tugas/Tugas 2/SHP_Jabar/SHP_Jabar/kabkot_jawabarat.shp')
str(jabar)
### REGRESI KLASIK ###
#Model Utuh
reg.klasik<-lm(y ~ x1+x2+x3+x4+x5+x6)
summary(reg.klasik)
ols_step_backward_p(reg.klasik)

#Model Terbaik
reg.klasik1 <- lm(y~x1+x2+x3+x6)
summary(reg.klasik1)
AIC(reg.klasik1)

### MORAN'S INDEKS ###
w<-poly2nb(jabar)
ww<-nb2listw(w) ##ww adalah bobot
moran(kemiskinan$kemiskinan, ww, n=length(ww$neighbours), S0=Szero(ww))

moran.plot(kemiskinan$kemiskinan, ww, labels=kemiskinan$kabkot)
moran.test(kemiskinan$kemiskinan,ww, alternative = "greater")
colfunc <- colorRampPalette(c("green", "yellow", "red"))
color <- colfunc(16)
jabar$miskin<-kemiskinan$kemiskinan
spplot(jabar, "miskin", col.regions=color, main="Peta Sebaran Jumlah Penduduk Miskin 2019")

### UJI ASUMSI KLASIK DARI MODEL ###
AIC(reg.klasik1)
err.regklasik<-residuals(reg.klasik1)
#Heterogenitas
bptest(reg.klasik1)

#Dependensi
RunsTest(err.regklasik)

#Normalitas
ad.test(err.regklasik)
qqnorm(err.regklasik,datax=T)
qqline(rnorm(length(err.regklasik),mean(err.regklasik),sd(err.regklasik)),datax=T, col="red")

#Autokorelasi
dwtest(reg.klasik1)

### SPATIAL AUTOCORRELATION: UJI MORAN INDEX###
#Moran Test pada error
moran.test(err.regklasik, ww, randomisation=T, alternative="greater")

### UJI EFEK SPASIAL ###
#Uji LM
LM<-lm.LMtests(reg.klasik1, nb2listw(w, style="W"),test=c("LMerr", "LMlag","RLMerr","RLMlag","SARMA"))
summary(LM) #RLMlag yang terpenuhi

### MODEL SAR ###
#Model
sar<-lagsarlm(reg.klasik1,data=kemiskinan,nb2listw(w))
summary(sar)

#Asumsi
err.sar<-residuals(sar)
ad.test(err.sar)
RunsTest(err.sar)

library(spatialreg)
library(spdep)
bptest.Sarlm(sar)


###MODEL SARMA / GSM###
#Model
gsm<-sacsarlm(reg.klasik1,data=kemiskinan,nb2listw(w))
summary(gsm)
summary(gsm)$adj.r.squared

#Asumsi
err.gsm<-residuals(gsm)
ad.test(err.gsm)
#Dependensi
RunsTest(err.gsm)
bptest.Sarlm(gsm)

#Tinggal simpulkan dengan tabel Goodness of Fit

#PETA

jabar$regklas<-reg.klasik1$residuals
spplot(jabar, "regklas", main="Pesebaran Residual Model Regresi Klasik",
       scales = list(draw = TRUE), cuts=8,
       col.regions=brewer.pal(n = 9, name = 'BrBG'))

jabar$regsar<-sar$residuals
spplot(jabar, "regsar", main="Pesebaran Residual Model Regresi Spasial - SAR",
       scales = list(draw = TRUE), cuts=8,
       col.regions=brewer.pal(n = 9, name = 'PiYG'))

jabar$reggsm<- gsm$residuals
spplot(jabar, "reggsm", main="Pesebaran Residual Model Regresi Spasial - SARMA",
       scales = list(draw = TRUE), cuts=8,
       col.regions=brewer.pal(n = 9, name = 'PRGn'))

install.packages('writexl')
library('writexl')
res <- as.data.frame(jabar)
View(res)
write_xlsx(res, "D:/Widya/Universitas Indonesia/Topik Khusus (Spasial)/Tugas/Tugas 4/residual.xlsx")

