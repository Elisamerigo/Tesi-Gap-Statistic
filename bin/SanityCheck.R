library(cluster)
library(MASS)
library(clusterSim)
library(ggplot2)
library(factoextra)
library(NbClust)


set.seed(27)
elementi_mat<-2
numero_righe<-6
numero_colonne<-6
bootstrap_clus<-10
cluster_max_clus<-2
random_centroids<-25

#matrice caso 1
matrice_caso1<- matrix(data = elementi_mat, nrow = numero_righe, ncol = numero_colonne)
for (i in 1:nrow(matrice_caso1)) {
  for (j in 1:ncol(matrice_caso1)) {
    righe[i]<-i
    colonne[j]<-j
    if(i>(nrow(matrice_caso1)/2))
      matrice_caso1[i,j]<-5
    
  }
}

data_matrice_caso1<-as.data.frame(matrice_caso1)
col<-c(data_matrice_caso1$V1,data_matrice_caso1$V2, data_matrice_caso1$V3, data_matrice_caso1$V4, data_matrice_caso1$V5, data_matrice_caso1$V6)
elementi<-col
datafr_finale<-data.frame(x=elementi, y=elementi)
ggplot(datafr_finale, aes(x, y))+geom_jitter(colour="blue")

#Calcolo gap statistic 
n_cluster<-2

#Con clusGap
gapclus_caso1<- clusGap(matrice_caso1, FUN = kmeans, nstart=random_centroids, K.max = cluster_max_clus, B=bootstrap_clus)
print(gapclus_caso1)

#Faccio la media dei due valori della gap per ottenere come risultato un solo valore
as.data.frame(gapclus_caso1$Tab)
valori_gap1<-(as.data.frame(gapclus_caso1$Tab))$gap
mean(valori_gap1)
cat("valore medio della gap = ", mean(valori_gap1), "\n", sep = "")

#Con Index.gap
kmeans1_caso1<-kmeans(matrice_caso1, centers = n_cluster)
kmeans2_caso1<-kmeans(matrice_caso1, centers = n_cluster)
clustertot_caso1<- cbind(kmeans1_caso1$cluster, kmeans2_caso1$cluster)
gapInd_caso1<-index.Gap(matrice_caso1, clustertot_caso1, reference.distribution = "unif", B=bootstrap_clus, method = "k-means", centrotypes = "centroids")
cat("valore della gap con Index.gap = ", gapInd_caso1$gap, "\n", sep = "")


#matrice caso 2
numero_da_prendere<-1
n_min<-8
n_max<-9
elementi_mat<-0
matrice_caso2<- matrix(data= elementi_mat, nrow = numero_righe, ncol = numero_colonne)
print(matrice_caso2)
for (g in 1:nrow(matrice_caso2)) {
  for (f in 1:ncol(matrice_caso2)) {
    matrice_caso2[g,f]<-runif(numero_da_prendere, n_min, n_max)
    print(matrice_caso2)
  }
}

data_matrice_caso2<- as.data.frame(matrice_caso2)
col2<-c(data_matrice_caso2$V1,data_matrice_caso2$V2, data_matrice_caso2$V3, data_matrice_caso2$V4, data_matrice_caso2$V5, data_matrice_caso2$V6)
elementi2<-col2
datafr2_finale<-data.frame(x,y=elementi2)
ggplot(datafr2_finale, aes(x, y))+geom_jitter(colour="blue")

#Calcolo la gap statistic 
#Con clusGap
gapclus_caso2<-clusGap(matrice_caso2, FUNcluster = kmeans, nstart=random_centroids, K.max=cluster_max_clus, B=bootstrap_clus)
print(gapclus_caso2)

#Faccio la media dei due valori della gap per ottenere come risultato un solo valore
as.data.frame(gapclus_caso2$Tab)
valori_gap2<-(as.data.frame(gapclus_caso2$Tab))$gap
cat("valore medio della gap = ", mean(valori_gap2), "\n", sep = "")

#Con Index.gap
kmeans1_caso2<-kmeans(matrice_caso2, centers = n_cluster)
kmeans2_caso2<-kmeans(matrice_caso2, centers = n_cluster)
clustertot2_caso2<- cbind(kmeans1_caso2$cluster, kmeans2_caso2$cluster)
gapInd_caso2<-index.Gap(matrice_caso2, clustertot2_caso2, reference.distribution = "unif", B=bootstrap_clus, method = "k-means", centrotypes = "centroids")
cat("valore della gap con Index.gap = ", gapInd_caso2$gap, "\n", sep = "")

