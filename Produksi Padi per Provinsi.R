library(readxl)     #Membaca file data excel
library(plm)        #untuk membuat model
library(kableExtra) #untuk tampilan tabel
library(lmtest)     #uji homoskedastisitas
library(dplyr)      #untuk mengganti nama variabel
library(car)        #untuk uji multikolinearitas
library(ggplot2)    #untuk visualisasi data
theme_set(theme_classic())

# Import Data
dataPadi <- read_xlsx("E:/Produksi Padi per Provinsi.xlsx")
str(dataPadi)
summary(dataPadi)
names(dataPadi)
#menggangti nama variabel
names(dataPadi)[names(dataPadi)=="Luas Lahan Panen Padi (ha Ribu)"] = "X1"
names(dataPadi)[names(dataPadi)=="Curah Hujan (mm)"] = "X2"
names(dataPadi)[names(dataPadi)=="Produksi Padi (100kg/ha)"] = "Y"

#Visualisasi Data
#Histogram ~
ggplot(dataPadi, aes(Y, color=factor(Tahun), fill=factor(Tahun)))+geom_histogram(col="black")
ggplot(dataPadi, aes(Y, color=factor(Provinsi), fill=factor(Provinsi)))+geom_histogram(col="black")

ggplot(dataPadi, aes(X1, color=factor(Tahun), fill=factor(Tahun)))+geom_histogram(col="black")
ggplot(dataPadi, aes(X1, color=factor(Provinsi), fill=factor(Provinsi)))+geom_histogram(col="black")

ggplot(dataPadi, aes(X2, color=factor(Tahun), fill=factor(Tahun)))+geom_histogram(col="black")
ggplot(dataPadi, aes(X2, color=factor(Provinsi), fill=factor(Provinsi)))+geom_histogram(col="black")


#Scatterplot ~
ggplot(dataPadi,aes(x=X1,y=Y))+geom_jitter(aes(col=Tahun))+ geom_smooth(aes(col=Tahun),method="lm",se=F)
ggplot(dataPadi,aes(x=X2,y=Y))+geom_jitter(aes(col=Tahun))+ geom_smooth(aes(col=Tahun),method="lm",se=F)


#time series plot ~ 
ggplot(dataPadi, aes(x = Tahun, y = X1)) + geom_line(aes(color = Provinsi), size = 1)
ggplot(dataPadi, aes(x = Tahun, y = X2)) + geom_line(aes(color = Provinsi), size = 1)
ggplot(dataPadi, aes(x = Tahun, y = Y)) + geom_line(aes(color = Provinsi), size = 1)


#barchart ~ 
ggplot(dataPadi, aes(x=Tahun,y=X1, color=factor(Provinsi), fill=Provinsi))+geom_bar(stat="identity",col="white")
ggplot(dataPadi, aes(x=Tahun,y=X2, color=factor(Provinsi), fill=Provinsi))+geom_bar(stat="identity",col="white")
ggplot(dataPadi, aes(x=Tahun,y=Y, color=factor(Provinsi), fill=Provinsi))+geom_bar(stat="identity",col="white")

ggplot(dataPadi, aes(x=Provinsi,y=X1, color=factor(Tahun), fill=Tahun))+geom_bar(stat="identity",col="white")+coord_flip()
ggplot(dataPadi, aes(x=Provinsi,y=X2, color=factor(Tahun), fill=Tahun))+geom_bar(stat="identity",col="white")+coord_flip()
ggplot(dataPadi, aes(x=Provinsi,y=Y, color=factor(Tahun), fill=Tahun))+geom_bar(stat="identity",col="white")+coord_flip()


#Boxplot
ggplot(dataPadi, aes(Tahun, X1, fill = Provinsi))+geom_boxplot(varwidth = T) 
ggplot(dataPadi, aes(Tahun, X2, fill = Provinsi))+geom_boxplot(varwidth = T) 
ggplot(dataPadi, aes(Tahun, Y, fill = Provinsi))+geom_boxplot(varwidth = T) 

ggplot(dataPadi, aes(Provinsi, X1, fill = Provinsi))+geom_boxplot(varwidth = T)
ggplot(dataPadi, aes(Provinsi, X2, fill = Provinsi))+geom_boxplot(varwidth = T) 
ggplot(dataPadi, aes(Provinsi, Y, fill = Provinsi))+geom_boxplot(varwidth = T) 

  
#Uji chow
common=plm(Y~X1+X2,data=dataPadi,model="pooling")
fixed=plm(Y~X1+X2,data=dataPadi,model="within")
pooltest(common,fixed)

#uji hausmaan
fixed=plm(Y~X1+X2,data=dataPadi,model="within",index = c("Provinsi","Tahun"))
random=plm(Y~X1+X2,data=dataPadi,model="random",index = c("Provinsi","Tahun"))
phtest(fixed,random)

#Uji Breusch Pagan
gr=plm(Y~X1+X2,data=dataPadi,model="random")
plmtest(gr, type="bp")
#Efek Dua Arah
plmtest(gr, effect="twoways", type="bp")
#Efek Individu/Cross Section
plmtest(gr, effect="individual", type="bp")
#Efek Waktu/Time
plmtest(gr, effect="time", type="bp")


#pembuatan model
# model 1
model1=plm(Y~X1+X2,data=dataPadi,model="random",effect="individual",index = c("Provinsi","Tahun"))
vif(model1)
summary(model1)
ranef(model1)
residual1=resid(model1)


# Uji Autokorelasi
pbgtest(model1)
# uji homoskedastisitas
bptest(model1)
#uji normalitas
ks.test(residual1,"pnorm")
