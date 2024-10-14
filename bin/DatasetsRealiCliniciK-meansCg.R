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
datasets_filenames <- c("10_7717_peerj_5665_dataYM2018_neuroblastoma.csv",
                        "journal.pone.0148699_S1_Text_Sepsis_SIRS_EDITED.csv",
                        "journal.pone.0158570_S2File_depression_heart_failure.csv",
                        "journal.pone.0175818_S1Dataset_Spain_cardiac_arrest_EDITED..csv",
                        "Takashi2019_diabetes_type1_dataset_preprocessed.csv"
)
datasets <- list()
for (filename in datasets_filenames) {
  data <- read.csv(filename)
  data_mod<-na.omit(data)
  datasets <- c(datasets, list(data_mod))


  n_cluster<- 2
  gap_i<-array()
  bootstrap_clus<-50
  random_centroids<-25

  
  

  for (j in datasets) {
    while (n_cluster<5) {
      kmeans1_k2<-kmeans(j, centers = n_cluster)
      kmeans2_k2<-kmeans(j, centers = n_cluster)
      clustertot_k2<-cbind(kmeans1_k2$cluster, kmeans2_k2$cluster)
      gap_statInd<-index.Gap(j, clustertot_k2, reference.distribution = "unif", B=bootstrap_clus, ,method = "k-means", centrotypes = "centroids")
     gap_i[n_cluster]<-gap_statInd$gap
      cat("Risultato Gap statistic con index.Gap utilizzando il k-means con k = ", n_cluster, " vale: ", gap_i[n_cluster], "\n", sep = "")
      gap_statClus<- clusGap(j, FUN=kmeans, nstart=random_centroids, K.max=n_cluster, B=bootstrap_clus)
      as.data.frame(gap_statClus$Tab)
      val_gap<-(as.data.frame(gap_statClus$Tab))$gap
     mean(val_gap)
      cat("Gap statistic con clusGap facendo la media dei valori utilizzando il k-means con k = ", n_cluster, " vale: ", mean(val_gap), "\n", sep = "")
      if(filename =="journal.pone.0158570_S2File_depression_heart_failure.csv" & n_cluster==4){
       
        cat("Risultato Gap statistic con index.Gap utilizzando il k_means con k = 4 e distanza Manhattan non può essere calcolato: la distanza vale 0", "\n", sep = "")
      }else{
      kmeans2_k2<-KMEANS_FUNCTION(j, k=n_cluster, distance_metric = "manhattan")
      kmeans2_k2<-KMEANS_FUNCTION(j, k=n_cluster, distance_metric = "manhattan")
      clustertot2_k2<-cbind(kmeans2_k2$cluster, kmeans2_k2$cluster)
      gap_statInd2<-index.Gap(j, clustertot2_k2, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
      cat("Risultato Gap statistic con index.Gap utilizzando il k_means con k = ", n_cluster, " e metrica Manhattan", " vale: ", gap_statInd2$gap, "\n", sep = "")
      }
     n_cluster=n_cluster+1
    
    
    }
    data_mod_sca<- data_mod %>% scale() %>% as.data.frame()
    array_method<-c("complete", "single", "average", "ward.D")
    array_metric<-c("euclidean", "manhattan")
    
    
    for (i in array_method) {
      for (g in array_metric) {
        fun_hcut<- hcut(data_mod_sca, hc_method = i, hc_metric = g)
        hc1<-fun_hcut
        hc2<-fun_hcut
        clustertot_hc<-cbind(hc1$cluster, hc2$cluster)
        gap_statInd_hc<-index.Gap(data_mod_sca, clustertot_hc, reference.distribution = "unif", centrotypes = "centroids")
        cat("Risultato Gap statistic utilizzando cluster gerarchico con metodo ", i, " e distanza ", g, " : ", gap_statInd_hc$gap, "\n", sep = "")
        
        if(i == "ward.D"){
          if(g =="manhattan")
          break
        }
      }
      
    }
    
  }
}
