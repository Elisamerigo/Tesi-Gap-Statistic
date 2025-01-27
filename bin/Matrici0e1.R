library(cluster)
library(MASS)
library(clusterSim)
library(ggplot2)


set.seed(21)
n_cluster<-2
options(max.print = 10000)
elementi_mat<-0
numero_righe<-10
numero_colonne<-10
bootstrap_clus<-10
cluster_max_clus<-2
random_centroids<-25

#Matrice
matrice <- matrix(data= elementi_mat,nrow = numero_righe, ncol = numero_colonne)
verbose<-FALSE
for (i in 1:nrow(matrice)) {
  for (j in 1:ncol(matrice)) {
    if(i>(nrow(matrice)/2)){
      matrice[i,j]<-1
      
    }
  }
}
if(verbose){
  print(matrice)
}
kmeans1<-kmeans(matrice, centers = n_cluster)
kmeans2<-kmeans(matrice, centers = n_cluster)
clustertot<-cbind(kmeans1$cluster, kmeans2$cluster)
gap_stat1<-index.Gap(matrice,clustertot,reference.distribution = "unif",B=bootstrap_clus, method = "k-means",centrotypes = "centroids")
cat("valore della gap con Index.Gap = ", gap_stat1$gap, "\n", sep = "")
gap_statC<-clusGap(matrice, FUN=kmeans, nstart=random_centroids, K.max=cluster_max_clus, B=bootstrap_clus)

#faccio la media dei valori di clusgap
as.data.frame(gap_statC$Tab)
valori_gap<-(as.data.frame(gap_statC$Tab))$gap
mean(valori_gap)
cat("valore medio della gap = ", mean(valori_gap), "\n", sep = "")

gap_ind<-array()
n_righe<-array()
gap_clus1<-array()
#gap_clus2<-array()
numero_da_prendere<-1
valore_min<-0
valore_max<-1
d<-array(0, dim = c(1,nrow(matrice)))
cont<-0

while(d[nrow(matrice)]==0){
  trovato<-FALSE
  riga_casuale<-sample(nrow(matrice),1)
  if(verbose){
  print(riga_casuale)
  }
  for (k in 1:nrow(matrice)) {
    if(d[k]==riga_casuale){
      trovato<-TRUE
      break
    }
  }
  if(trovato==FALSE){
    cont<-cont+1
    d[cont]<-riga_casuale
}
}
if(verbose){
print(d)
}
for (f in d) {
  for (g in 1:ncol(matrice)) {
      matrice[f,g]<-round(runif(numero_da_prendere, valore_min, valore_max),2)
      if (g==ncol(matrice)){
        if(verbose){
        print(matrice)
        }
        kmeans3<-kmeans(matrice, centers = n_cluster)
        kmeans4<-kmeans(matrice, centers = n_cluster)
        clustertot1<-cbind(kmeans3$cluster, kmeans4$cluster)
        gap_stat2<-index.Gap(matrice,clustertot1,reference.distribution = "unif",B=bootstrap_clus,method = "k-means", centrotypes = "centroids")
        cat("valore della gap con Index.gap = ", gap_stat2$gap, "\n", sep = "")
        gap_ind[f]<-(gap_stat2$gap)
        n_righe[f]<-f
        gap_statC2<- clusGap(matrice, FUN=kmeans, nstart=random_centroids, K.max=cluster_max_clus, B=bootstrap_clus)
        as.data.frame(gap_statC2$Tab)
        valori2_gap<-(as.data.frame(gap_statC2$Tab))$gap
        mean(valori2_gap)
        cat("valore medio della gap = ", mean(valori2_gap), "\n", sep = "")
        gap_clus1[f]<-(mean(valori2_gap))
        #gap_clus2[f]<-(as.data.frame(gap_statC2$Tab))$gap[2]
      }
    }
    
    }

#Creo dataframe per index.Gap
gapI<-c(gap_stat1$gap, gap_ind)
n_righe2<-c(n_righe,nrow(matrice)+1)
my_data<-data.frame(X=n_righe2, Y=gapI)
print(my_data)
#Stampo il grafico con index.gap
ggplot(my_data, aes(x=n_righe2, y=gapI))+geom_point(shape=20, color="blue")+geom_smooth(method = lm, color="red")+labs(title = "Andamento Gap Statistic con index.Gap", x="numero righe manipolate", y="Gap")

#Creo dataframe per index.gap senza primo punto
gapInd_senza_primopunto<- gap_ind
my_data_senza_primopunto<-data.frame(X=n_righe, Y=gapInd_senza_primopunto)
print(my_data_senza_primopunto)
#Stampo grafico index.gap senza primo punto
ggplot(my_data_senza_primopunto, aes(x=n_righe, y=gapInd_senza_primopunto))+geom_point(shape=20, color="blue")+geom_smooth(method = lm, color="red")+labs(title = "Andamento Gap statistic con index.Gap senza primo punto", x="numero righe manipolate", y="Gap")

#Creo dataframe per clusGap
gapC1<-c(mean(valori_gap), gap_clus1)
#gapC2<-c((as.data.frame(gap_statC2$Tab))$gap[2], gap_clus2)
my_data2<-data.frame(X=n_righe2, Y=gapC1)
print(my_data2)
#Stampo il grafico con clusGap
ggplot(my_data2, aes(x=n_righe2, y=gapC1))+geom_point(shape=20, color="blue")+geom_smooth(method = lm, color="red")+labs(title = "Andamento Gap statistic con clusGap", x="numero righe manipolate", y="Gap")
#ggplot(my_data2, aes(x=n_righe2, y=gapC2))+geom_point(shape=20, color="blue")+geom_smooth(method = lm, color="red")+labs(title = "Andamento Gap statistic con clusGap secondo", x="numero righe manipolate", y="Gap")

#Creo dataframe per clusGap senza primo punto
gapC1_senza_primopunto<-gap_clus1
#gapC2_senza_primopunto<-gap_clus2
my_data2_senza_primopunto<-data.frame(X=n_righe, Y=gapC1_senza_primopunto)
print(my_data2_senza_primopunto)
#Stampo grafico con clusgap senza primo punto
ggplot(my_data2_senza_primopunto, aes(x=n_righe, y=gapC1_senza_primopunto))+geom_point(shape=20, color="blue")+geom_smooth(method = lm, color="red")+labs(title = "Andamento Gap statistic con clusGap senza primo punto", x="numero righe manipolate", y="Gap")
#ggplot(my_data2_senza_primopunto, aes(x=n_righe, y=gapC2_senza_primopunto))+geom_point(shape=20, color="blue")+geom_smooth(method = lm, color="red")+labs(title = "Andamento Gap statistic con clusGap secondo senza primo punto", x="numero righe manipolate", y="Gap")

