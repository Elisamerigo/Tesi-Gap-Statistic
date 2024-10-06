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


filename1<-read.csv("10_7717_peerj_5665_dataYM2018_neuroblastoma.csv")
dataa1<-na.omit(filename1)
dataa1_frame<-as.data.frame(dataa1)
knn_dis1<-kNNdist(dataa1_frame, k=ncol(dataa1_frame)+1)  
distances1 <- sort(knn_dis1)
 # Creare un dataframe per ggplot2
distance1_df <- data.frame(rank = 1:length(distances1),distance = distances1)
ggplot(distance1_df, aes(x = rank, y = distance)) +
  geom_line() +
  geom_hline(yintercept = 4.9, color="red", linetype=2)+
    labs(title = "Grafico kNN", x = "Punti", y = "Distanza")

filename2<-read.csv("journal.pone.0148699_S1_Text_Sepsis_SIRS_EDITED.csv")
dataa2<-na.omit(filename2)
dataa2_frame<-as.data.frame(dataa2)
knn_dis2<-kNNdist(dataa2_frame, k=ncol(dataa2_frame)+1)
distances2 <- sort(knn_dis2)
# Creare un dataframe per ggplot2
distance2_df <- data.frame(rank = 1:length(distances2),distance = distances2)
ggplot(distance2_df, aes(x = rank, y = distance)) +
  geom_line() +
  geom_hline(yintercept = 78, color="red", linetype=2)+
  labs(title = "Grafico kNN", x = "Punti", y = "Distanza")

filename3<-read.csv("journal.pone.0158570_S2File_depression_heart_failure.csv")
dataa3<-na.omit(filename3)
dataa3_frame<-as.data.frame(dataa3)
knn_dis3<-kNNdist(dataa3_frame, k=ncol(dataa3_frame)+1)  
distances3 <- sort(knn_dis3)  
# Creare un dataframe per ggplot2
distance3_df <- data.frame(rank = 1:length(distances3),distance = distances3)
ggplot(distance3_df, aes(x = rank, y = distance)) +
  geom_line() + 
  geom_hline(yintercept = 220, color="red", linetype=2)
labs(title = "Grafico kNN",
     x = "Punti",
     y = "Distanza")

filename4<-read.csv("journal.pone.0175818_S1Dataset_Spain_cardiac_arrest_EDITED..csv")
dataa4<-na.omit(filename4)
dataa4_frame<-as.data.frame(dataa4)
knn_dis4<-kNNdist(dataa4_frame, k=ncol(dataa4_frame)+1)  
distances4 <- sort(knn_dis4)
# Creare un dataframe per ggplot2
distance4_df <- data.frame(rank = 1:length(distances4),distance = distances4)
ggplot(distance4_df, aes(x = rank, y = distance)) +
  geom_line() + 
  geom_hline(yintercept = 7.5, color="red", linetype=2)
labs(title = "Grafico kNN",
     x = "Punti",
     y = "Distanza")

filename5<-read.csv("Takashi2019_diabetes_type1_dataset_preprocessed.csv")
dataa5<-na.omit(filename5)
dataa5_frame<-as.data.frame(dataa5)
knn_dis5<-kNNdist(dataa5_frame, k=ncol(dataa5_frame)+1)  
distances5 <- sort(knn_dis5)
# Creare un dataframe per ggplot2
distance5_df <- data.frame(rank = 1:length(distances5),distance = distances5)
ggplot(distance5_df, aes(x = rank, y = distance)) +
  geom_line() + 
  geom_hline(yintercept = 26.5, color="red", linetype=2)
labs(title = "Grafico kNN",
     x = "Punti",
     y = "Distanza")