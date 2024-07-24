library(cluster)
library(MASS)
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
      cat("Gap statistic con index.Gap utilizzando il k-means con k = ", n_cluster, " vale: ", gap_i[n_cluster], "\n", sep = "")
      gap_statClus<- clusGap(j, FUN=kmeans, nstart=random_centroids, K.max=n_cluster, B=bootstrap_clus)
      as.data.frame(gap_statClus$Tab)
      val_gap<-(as.data.frame(gap_statClus$Tab))$gap
     mean(val_gap)
      cat("Gap statistic con clusGap facendo la media dei valori utilizzando il k-means con k = ", n_cluster, " vale: ", mean(val_gap), "\n", sep = "")
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
        cat("Risultato gap con metodo ", i, " e distanza ", g, " : ", gap_statInd_hc$gap, "\n", sep = "")
        
        if(i == "ward.D"){
          if(g =="manhattan")
          break
        }
      }
      
    }
    
    if(filename =="10_7717_peerj_5665_dataYM2018_neuroblastoma.csv"){
      #kNNdistplot(j, k=ncol(j)+1)
      #abline(h=4.7, col="red", lty=2)
      dbs11<-dbscan(j, eps = 4.7, minPts = 13)
      dbs21<-dbscan(j, eps = 4.7, minPts = 13)
      clustertot11_db<-cbind(dbs11$cluster, dbs21$cluster)
      gap_statInd1_db<- index.Gap(j, clustertot11_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
      cat("Gap statistic con dbscan su primo dataset: ",gap_statInd1_db$gap, "\n", sep = "")
     
    }
    
    if(filename =="journal.pone.0148699_S1_Text_Sepsis_SIRS_EDITED.csv"){
     # kNNdistplot(j, k=ncol(j)+1)
     # abline(h=94, col="red", lty=2)
      cl52_db<-dbscan(j, eps = 94, minPts = ncol(j)+1)
      cl62_db<-dbscan(j, eps = 94, minPts = ncol(j)+1)
      clall2_db<-cbind(cl52_db$cluster, cl62_db$cluster)
      gap_statInd2_db<-index.Gap(j, clall2_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
      cat("Gap statistic con dbscan su secondo dataset: ", gap_statInd2_db$gap, "\n", sep = "")
      
    }
    
    if(filename == "journal.pone.0158570_S2File_depression_heart_failure.csv"){
     # kNNdistplot(j, k=ncol(j)+1)
     # abline(h=119, col="red", lty=2)
      cl53_db<-dbscan(j, eps = 119, minPts = ncol(j)+1)
      cl63_db<-dbscan(j, eps = 119, minPts = ncol(j)+1)
      clall3_db<-cbind(cl53_db$cluster, cl63_db$cluster)
      gap_statInd3_db<-index.Gap(j, clall3_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
      cat("Gap statistic con dbscan su terzo dataset: ", gap_statInd3_db$gap, "\n", sep = "")
     
    }
    
    if(filename =="journal.pone.0175818_S1Dataset_Spain_cardiac_arrest_EDITED..csv" ){
     # kNNdistplot(j, k=ncol(j)+1)
      #abline(h=8.7, col="red", lty=2)
      cl54_db<-dbscan(j, eps=8.7, minPts = ncol(j)+1)
      cl64_db<-dbscan(j, eps = 8.7, minPts = ncol(j)+1)
      clall4_db<-cbind(cl54_db$cluster, cl64_db$cluster)
      gap_statInd4_db<-index.Gap(j, clall4_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
      cat("Gap statistic con dbscan su quarto dataset: ", gap_statInd4_db$gap, "\n", sep = "")
      
    }
    
    if(filename == "Takashi2019_diabetes_type1_dataset_preprocessed.csv"){
    #  kNNdistplot(j, k=ncol(j)+1)
     # abline(h=29, col="red", lty=2)
      cl55_db<-dbscan(j, eps=29, minPts = ncol(j)+1)
      cl65_db<-dbscan(j, eps = 29, minPts = ncol(j)+1)
      clall5_db<-cbind(cl55_db$cluster, cl65_db$cluster)
      gap_statInd5_db<-index.Gap(j, clall5_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
      cat("Gap statistic con dbscan su quinto dataset: ", gap_statInd5_db$gap, "\n", sep = "")
      
    }
   
    eps2 = ceiling(nrow(j)/2)
    dbs12<-dbscan(j, eps = eps2, minPts = ncol(j)+1)
    dbs22<-dbscan(j, eps=eps2, minPts = ncol(j)+1)
    clustertot12_db<-cbind(dbs12$cluster, dbs22$cluster)
    gap_statInd12_db<-index.Gap(j, clustertot12_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
    cat("Gap statistic con dbscan utilizzando un diverso eps: ", gap_statInd12_db$gap, "\n", sep = "")
    break
    
  }
}
