library(cluster)
library(MASS)
library(KMEANS.KNN)
library(clusterSim)
library(factoextra)
library(ggplot2)
library(reshape2)
library(stats)
library(dbscan)
library(pacman)
library(magrittr)
library(factoextra)
library(ggplot2)
library(ggpubr)

set.seed(26)

#Dataset Neuroblastoma
filenameNeuro<-read.csv("10_7717_peerj_5665_dataYM2018_neuroblastoma.csv")
dataaNeuro<-na.omit(filenameNeuro)
dataaNeuro_frame<-as.data.frame(dataaNeuro)
knn_disNeuro<-kNNdist(dataaNeuro_frame, k=ncol(dataaNeuro_frame)+1)  
distancesNeuro <- sort(knn_disNeuro)
 # Creare un dataframe per ggplot2
distanceNeuro_df <- data.frame(rank = 1:length(distancesNeuro),distance = distancesNeuro)
ggplot(distanceNeuro_df, aes(x = rank, y = distance)) +
  geom_line() +
  geom_hline(yintercept = 4.9, color="red", linetype=2)+
    labs(title = "Grafico kNN", x = "Punti", y = "Distanza")

dbs1Neuro<-dbscan(dataaNeuro_frame, eps = 4.9, minPts = ncol(dataaNeuro_frame)+1)
dbs2Neuro<-dbscan(dataaNeuro_frame, eps = 4.9, minPts = ncol(dataaNeuro_frame)+1)
clustertot1Neuro_db<-cbind(dbs1Neuro$cluster, dbs2Neuro$cluster)
gap_statInd1Neuro_db<- index.Gap(dataaNeuro_frame, clustertot1Neuro_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
cat("Risultato Gap statistic con dbscan su primo dataset: ",gap_statInd1Neuro_db$gap, "\n", sep = "")

eps1 = ceiling(nrow(dataaNeuro_frame)/2)
dbs3Neuro<-dbscan(dataaNeuro_frame, eps = eps1, minPts = ncol(dataaNeuro_frame)+1)
dbs4Neuro<-dbscan(dataaNeuro_frame, eps=eps1, minPts = ncol(dataaNeuro_frame)+1)
clustertot2Neuro_db<-cbind(dbs3Neuro$cluster, dbs4Neuro$cluster)
gap_statInd2Neuro_db<-index.Gap(dataaNeuro_frame, clustertot2Neuro_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
cat("Risultato Gap statistic con dbscan utilizzando un diverso eps: ", gap_statInd2Neuro_db$gap, "\n", sep = "")

dbs5Neuro<-dbscan(dataaNeuro_frame, eps=eps1, minPts = eps1+1)
dbs6Neuro<- dbscan(dataaNeuro_frame, eps=eps1, minPts = eps1+1)
clustertot3Neuro_db<-cbind(dbs5Neuro$cluster, dbs6Neuro$cluster)
gap_statInd3Neuro_db<-index.Gap(dataaNeuro_frame, clustertot3Neuro_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
cat("Risultato Gap statistic con dbscan utilizzando l'eps precedente ma un diverso numero minimo di punti: ", gap_statInd3Neuro_db$gap, "\n", sep = "")

#Datsets Sepsi
filenameSepsi<-read.csv("journal.pone.0148699_S1_Text_Sepsis_SIRS_EDITED.csv")
dataaSepsi<-na.omit(filenameSepsi)
dataaSepsi_frame<-as.data.frame(dataaSepsi)
knn_disSepsi<-kNNdist(dataaSepsi_frame, k=ncol(dataaSepsi_frame)+1)
distancesSepsi <- sort(knn_disSepsi)
# Creare un dataframe per ggplot2
distanceSepsi_df <- data.frame(rank = 1:length(distancesSepsi),distance = distancesSepsi)
ggplot(distance2_df, aes(x = rank, y = distance)) +
  geom_line() +
  geom_hline(yintercept = 78, color="red", linetype=2)+
  labs(title = "Grafico kNN", x = "Punti", y = "Distanza")

dbs1Sepsi<-dbscan(dataaSepsi_frame, eps = 78, minPts = ncol(dataaSepsi_frame)+1)
dbs2Sepsi<-dbscan(dataaSepsi_frame, eps = 78, minPts = ncol(dataaSepsi_frame)+1)
clustertot1Sepsi_db<-cbind(dbs1Sepsi$cluster, dbs2Sepsi$cluster)
gap_statInd1Sepsi_db<-index.Gap(dataaSepsi_frame, clustertot1Sepsi_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
cat("Risultato Gap statistic con dbscan su secondo dataset: ", gap_statInd1Sepsi_db$gap, "\n", sep = "")

eps2 = ceiling(nrow(dataaSepsi_frame)/2)
dbs3Sepsi<-dbscan(dataaSepsi_frame, eps = eps2, minPts = ncol(dataaSepsi_frame)+1)
dbs4Sepsi<-dbscan(dataaSepsi_frame, eps = eps2, minPts = ncol(dataaSepsi_frame)+1)
clustertot2Sepsi_db<-cbind(dbs3Sepsi$cluster, dbs4Sepsi$cluster)
gap_statInd2Sepsi_db<-index.Gap(dataaSepsi_frame, clustertot2Sepsi_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
cat("Risultato Gap statistic con dbscan utilizzando un diverso eps: ", gap_statInd2Sepsi_db$gap, "\n", sep = "")

dbs5Sepsi<-dbscan(dataaSepsi_frame, eps=eps2, minPts = eps2+1)
dbs6Sepsi<- dbscan(dataaSepsi_frame, eps=eps2, minPts = eps2+1)
clustertot3Sepsi_db<-cbind(dbs5Sepsi$cluster, dbs6Sepsi$cluster)
gap_statInd3Sepsi_db<-index.Gap(dataaSepsi_frame, clustertot3Sepsi_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
cat("Risultato Gap statistic con dbscan utilizzando l'eps precedente ma un diverso numero minimo di punti: ", gap_statInd3Sepsi_db$gap, "\n", sep = "")

#Dataset Depression and Heart Failure
filenameDhf<-read.csv("journal.pone.0158570_S2File_depression_heart_failure.csv")
dataaDhf<-na.omit(filenameDhf)
dataaDhf_frame<-as.data.frame(dataaDhf)
knn_disDhf<-kNNdist(dataaDhf_frame, k=ncol(dataaDhf_frame)+1)  
distancesDhf <- sort(knn_disDhf)  
# Creare un dataframe per ggplot2
distanceDhf_df <- data.frame(rank = 1:length(distancesDhf),distance = distancesDhf)
ggplot(distanceDhf_df, aes(x = rank, y = distance)) +
  geom_line() + 
  geom_hline(yintercept = 220, color="red", linetype=2)+
  geom_hline(yintercept = 60, color="red", linetype=2)+
labs(title = "Grafico kNN", x = "Punti",y = "Distanza")

dbs1Dhf<-dbscan(dataaDhf_frame, eps = 60, minPts = ncol(dataaDhf_frame)+1)
dbs2Dhf<-dbscan(dataaDhf_frame, eps = 60, minPts = ncol(dataaDhf_frame)+1)
clustertot1Dhf_db<-cbind(dbs1Dhf$cluster, dbs2Dhf$cluster)
gap_statInd1Dhf_db<-index.Gap(dataaDhf_frame, clustertot1Dhf_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
cat("Risultato Gap statistic con dbscan su terzo dataset: ", gap_statInd1Dhf_db$gap, "\n", sep = "")

eps3 = ceiling(nrow(dataa3_frame)/2)
dbs3Dhf<-dbscan(dataaDhf_frame, eps = eps3, minPts = ncol(dataaDhf_frame)+1)
dbs4Dhf<-dbscan(dataaDhf_frame, eps=eps3, minPts = ncol(dataaDhf_frame)+1)
clustertot2Dhf_db<-cbind(dbs3Dhf$cluster, dbs4Dhf$cluster)
gap_statInd2Dhf_db<-index.Gap(dataaDhf_frame, clustertot2Dhf_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
cat("Risultato Gap statistic con dbscan utilizzando un diverso eps: ", gap_statInd2Dhf_db$gap, "\n", sep = "")

dbs5Dhf<-dbscan(dataaDhf_frame, eps=70, minPts = ncol(dataaDhf_frame)+1)
dbs6Dhf<- dbscan(dataaDhf_frame, eps=70, minPts = ncol(dataaDhf_frame)+1)
clustertot3Dhf_db<-cbind(dbs5Dhf$cluster, dbs6Dhf$cluster)
gap_statInd3Dhf_db<-index.Gap(dataaDhf_frame, clustertot3Dhf_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
cat("Risultato Gap statistic con dbscan utilizzando l'eps precedente ma un diverso numero minimo di punti: ", gap_statInd3Dhf_db$gap, "\n", sep = "")

#Datasets Cardiac arrest
filenameCa<-read.csv("journal.pone.0175818_S1Dataset_Spain_cardiac_arrest_EDITED..csv")
dataaCa<-na.omit(filenameCa)
dataaCa_frame<-as.data.frame(dataaCa)
knn_disCa<-kNNdist(dataaCa_frame, k=ncol(dataaCa_frame)+1)  
distancesCa <- sort(knn_disCa)
# Creare un dataframe per ggplot2
distanceCa_df <- data.frame(rank = 1:length(distancesCa),distance = distancesCa)
ggplot(distanceCa_df, aes(x = rank, y = distance)) +
  geom_line() + 
  geom_hline(yintercept = 7.5, color="red", linetype=2)+
labs(title = "Grafico kNN", x = "Punti", y = "Distanza")

dbs1Ca<-dbscan(dataaCa_frame, eps=7.5, minPts = ncol(dataaCa_frame)+1)
dbs2Ca<-dbscan(dataaCa_frame, eps = 7.5, minPts = ncol(dataaCa_frame)+1)
clustertot1Ca_db<-cbind(dbs1Ca$cluster, dbs2Ca$cluster)
gap_statInd1Ca_db<-index.Gap(dataaCa_frame, clustertot1Ca_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
cat("Risultato Gap statistic con dbscan su quarto dataset: ", gap_statInd1Ca_db$gap, "\n", sep = "")

eps4 = ceiling(nrow(dataaCa_frame)/2)
dbs3Ca<-dbscan(dataaCa_frame, eps = eps4, minPts = ncol(dataaCa_frame)+1)
dbs4Ca<-dbscan(dataaCa_frame, eps = eps4, minPts = ncol(dataaCa_frame)+1)
clustertot2Ca_db<-cbind(dbs3Ca$cluster, dbs4Ca$cluster)
gap_statInd2Ca_db<-index.Gap(dataaCa_frame, clustertot2Ca_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
cat("Risultato Gap statistic con dbscan utilizzando un diverso eps: ", gap_statInd2Ca_db$gap, "\n", sep = "")

dbs5Ca<-dbscan(dataaCa_frame, eps=eps4, minPts = eps4+1)
dbs6Ca<- dbscan(dataaCa_frame, eps=eps4, minPts = eps4+1)
clustertot3Ca_db<-cbind(dbs5Ca$cluster, dbs4Ca$cluster)
gap_statInd3Ca_db<-index.Gap(dataaCa_frame, clustertot3Ca_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
cat("Risultato Gap statistic con dbscan utilizzando l'eps precedente ma un diverso numero minimo di punti: ", gap_statInd3Ca_db$gap, "\n", sep = "")


#Dataset Diabetes type 1
filenameDia<-read.csv("Takashi2019_diabetes_type1_dataset_preprocessed.csv")
dataaDia<-na.omit(filenameDia)
dataaDia_frame<-as.data.frame(dataaDia)
knn_disDia<-kNNdist(dataaDia_frame, k=ncol(dataaDia_frame)+1)  
distancesDia <- sort(knn_disDia)
# Creare un dataframe per ggplot2
distanceDia_df <- data.frame(rank = 1:length(distancesDia),distance = distancesDia)
ggplot(distanceDia_df, aes(x = rank, y = distance)) +
  geom_line() + 
  geom_hline(yintercept = 26.5, color="red", linetype=2)+
  geom_hline(yintercept = 24, color="red", linetype=2)+
labs(title = "Grafico kNN", x = "Punti",y = "Distanza")

dbs1Dia<-dbscan(dataaDia_frame, eps=24, minPts = ncol(dataaDia_frame)+1)
dbs2Dia<-dbscan(dataaDia_frame, eps = 24, minPts = ncol(dataaDia_frame)+1)
clustertot1Dia_db<-cbind(dbs1Dia$cluster, dbs2Dia$cluster)
gap_statInd1Dia_db<-index.Gap(dataaDia_frame, clustertot1Dia_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
cat("Risultato Gap statistic con dbscan su quinto dataset: ", gap_statInd1Dia_db$gap, "\n", sep = "")

eps5 = ceiling(nrow(dataaDia_frame)/2)
dbs3Dia<-dbscan(dataaDia_frame, eps = eps5, minPts = ncol(dataaDia_frame)+1)
dbs4Dia<-dbscan(dataaDia_frame, eps=eps5, minPts = ncol(dataaDia_frame)+1)
clustertot2Dia_db<-cbind(dbs3Dia$cluster, dbs4Dia$cluster)
gap_statInd2Dia_db<-index.Gap(dataaDia_frame, clustertot2Dia_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
cat("Risultato Gap statistic con dbscan utilizzando un diverso eps: ", gap_statInd2Dia_db$gap, "\n", sep = "")

dbs5Dia<-dbscan(dataaDia_frame, eps=eps5, minPts = eps5+1)
dbs6Dia<- dbscan(dataaDia_frame, eps=eps5, minPts = eps5+1)
clustertot3Dia_db<-cbind(dbs5Dia$cluster, dbs6Dia$cluster)
gap_statInd3Dia_db<-index.Gap(dataaDia_frame, clustertot3Dia_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
cat("Risultato Gap statistic con dbscan utilizzando l'eps precedente ma un diverso numero minimo di punti: ", gap_statInd3Dia_db$gap, "\n", sep = "")