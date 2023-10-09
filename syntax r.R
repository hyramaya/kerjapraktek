
data <- read.csv("D://MAGANG//laporan kp//Jumlah Penderita Cacat Berdasarkan Jenisnya Menurut Kecamatan .csv", sep = ";", header = TRUE)
kp <- data[,c(2:8)]
rownames(kp) <- data$Kecamatan[1:17]
View(kp)

library(gridExtra)
library(factoextra) ##clustering algorithms & visualisasi

summary(is.na(kp)) ##missing value
summary(kp) ##deskriptif data

#PENGECEKAN OUTLIER
library(MVN)
laporan <- kp
data.no.out <- mvn(laporan, multivariateOutlierMethod="quan"
                   ,showNewData=TRUE)

#CEK MULTIKOLINAERITAS
library(PerformanceAnalytics)
chart.Correlation(kp)
multikolinearitas <- cor(kp)
multikolinearitas

#MULTIKO VIF
#cek multikolinearitas
library(car) 
attach(kp)
(multiko.kp1=vif(lm(Tuna.Netra~Bisu.Tuli+Cacat.Tubuh+Mental.Retardasi+Penyakit.Kronis+Cacat.Ganda+Mental.eks.psikotik)))
(multiko.kp2=vif(lm(Bisu.Tuli~Tuna.Netra+Cacat.Tubuh+Mental.Retardasi+Penyakit.Kronis+Cacat.Ganda+Mental.eks.psikotik)))
(multiko.kp3=vif(lm(Cacat.Tubuh~Tuna.Netra+Bisu.Tuli+Mental.Retardasi+Penyakit.Kronis+Cacat.Ganda+Mental.eks.psikotik)))
(multiko.kp4=vif(lm(Mental.Retardasi~Tuna.Netra+Bisu.Tuli+Cacat.Tubuh+Penyakit.Kronis+Cacat.Ganda+Mental.eks.psikotik)))
(multiko.kp5=vif(lm(Penyakit.Kronis~Tuna.Netra+Bisu.Tuli+Cacat.Tubuh+Mental.Retardasi+Cacat.Ganda+Mental.eks.psikotik)))
(multiko.kp6=vif(lm(Cacat.Ganda~Tuna.Netra+Bisu.Tuli+Cacat.Tubuh+Mental.Retardasi+Penyakit.Kronis+Mental.eks.psikotik)))
(multiko.kp7=vif(lm(Mental.eks.psikotik~Tuna.Netra+Bisu.Tuli+Cacat.Tubuh+Mental.Retardasi+Penyakit.Kronis+Cacat.Ganda)))

#Uji kecukupan data dan kelayakan variabel
library(psych)
KMO(kp)

#STANDARISASI DATA
data_standarisasi <- scale(kp)
data_standarisasi

#JARAK ANTAR OBJEK
distance<-get_dist(data_standarisasi)
distance
fviz_dist(distance, gradient=list(low="yellow", mid="white",high="green"))

#MENCARI BANYAKNYA CLUSTER (K)
#METODE SILLHOETTE
fviz_nbclust(data_standarisasi, kmeans, method = "silhouette")

fviz_nbclust(data_standarisasi, FUN=hcut, method="silhouette", hc_method="single")
data2.hc = hclust(dist(data_standarisasi), method="single")
plot(data2.hc,hang=0.5)
rect.hclust(data2.hc, k=2, border=2:3)

#CLUSTERING K-MEANS
kluster.kp<-kmeans(data_standarisasi, centers = 3, nstart = 25) 
fviz_cluster(kluster.kp, data=data_standarisasi)

#DATA FRAME CLUSTERING
hasil_cluster <- data.frame(data_standarisasi, kluster.kp$cluster)
hasil_cluster

##MENGIDENTIFIKASI KARAKTERISTIK CLUSTER
kluster1 <- subset(hasil_cluster, kluster.kp.cluster == 1)
kluster2 <- subset(hasil_cluster, kluster.kp.cluster == 2)
kluster3 <- subset(hasil_cluster, kluster.kp.cluster == 3)

kluster.1 <- sapply(kluster1, mean)
kluster.2 <- sapply(kluster2, mean)
kluster.3 <- sapply(kluster3, mean)
mean_total <- rbind(kluster.1, kluster.2, kluster.3)
mean_total

hasil1 <- data.frame(kp,kluster.kp$cluster) 
kluster11 <- subset(hasil1, kluster.kp.cluster==1) 
kluster21 <- subset(hasil1, kluster.kp.cluster==2)
kluster31 <- subset(hasil1, kluster.kp.cluster==3)
kluster_11 <- sapply(kluster11, mean)
kluster_21 <- sapply(kluster21, mean)
kluster_31 <- sapply(kluster31, mean)
mean_total1 <- rbind(kluster_11,kluster_21, kluster_31) 
mean_total1
