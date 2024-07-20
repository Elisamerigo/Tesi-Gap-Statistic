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

#Importo i datasets
filename<- "10_7717_peerj_5665_dataYM2018_neuroblastoma.csv"
data<- read.csv(filename)
data_mod<-na.omit(data)
filename<-"journal.pone.0148699_S1_Text_Sepsis_SIRS_EDITED.csv"
data<- read.csv(filename)
data_mod<-na.omit(data)
filename<-"journal.pone.0158570_S2File_depression_heart_failure.csv"
data<- read.csv(filename)
data_mod<-na.omit(data)
filename<-"journal.pone.0175818_S1Dataset_Spain_cardiac_arrest_EDITED..csv"
data<- read.csv(filename)
data_mod<-na.omit(data)
filename<-"Takashi2019_diabetes_type1_dataset_preprocessed.csv"
data<- read.csv(filename)
data_mod<-na.omit(data)

#dataset_array<-c(data1_mod, data2_mod, data3_mod, data4_mod, data5_mod)
n_cluster<- 2
gap_i<-array()
bootstrap_clus<-50
random_centroids<-25

#for (j in dataset_array) {
  while (n_cluster<5) {
    kmeans1_k2<-kmeans(data_mod, centers = n_cluster)
    kmeans2_k2<-kmeans(data_mod, centers = n_cluster)
    clustertot_k2<-cbind(kmeans1_k2$cluster, kmeans2_k2$cluster)
    gap_statInd<-index.Gap(data_mod, clustertot_k2, reference.distribution = "unif", B=bootstrap_clus, ,method = "k-means", centrotypes = "centroids")
    gap_i[n_cluster]<-gap_statInd$gap
    cat("Gap statistic con index.Gap utilizzando il k-means con k = ", n_cluster, " vale: ", gap_i[n_cluster], "\n", sep = "")
    gap_statClus<- clusGap(data_mod, FUN=kmeans, nstart=random_centroids, K.max=n_cluster, B=bootstrap_clus)
    #cat("Gap statistic con clusGap utilizzando il k-means con k = ", n_cluster, " vale: ", sep = "")
    #print(gap_statClus)
    as.data.frame(gap_statClus$Tab)
    val_gap<-(as.data.frame(gap_statClus$Tab))$gap
    mean(val_gap)
    cat("Gap statistic con clusGap facendo la media dei valori utilizzando il k-means con k = ", n_cluster, " vale: ", mean(val_gap), "\n", sep = "")
    n_cluster=n_cluster+1
    
    
  }
#}
#Calcolo il k-means con k=2 con index.Gap



#Calcolo il cluster gerarchico con index.Gap
data_mod_sca<- data_mod %>% scale() %>% as.data.frame()
# distanza<-dist(data_mod_sca, method = "euclidean")
# distanza2<-dist(data_mod_sca, method = "manhattan")

array_method<-c("complete", "single", "average", "ward.D")
array_metric<-c("euclidean", "manhattan")


for (i in array_method) {
  for (j in array_metric) {
    fun_hcut<- hcut(data_mod_sca, hc_method = i, hc_metric = j)
    hc1<-fun_hcut
    hc2<-fun_hcut
    clustertot_hc<-cbind(hc1$cluster, hc2$cluster)
    gap_statInd_hc<-index.Gap(data_mod_sca, clustertot_hc, reference.distribution = "unif", centrotypes = "centroids")
    cat("Risultato gap con metodo ", i, " e distanza ", j, " : ", gap_statInd_hc$gap, "\n", sep = "")
  }
  
}
# fun_compl_euc<- hcut(data_mod_sca, hc_method = "complete", hc_metric = "euclidean")
# hc1_compl_euc<-fun_compl_euc
# hc2_compl_euc<-fun_compl_euc
# clustertot_hc_compl_euc<-cbind(hc1_compl_euc$cluster, hc2_compl_euc$cluster)
# gap_statInd_hc_compl_euc<-index.Gap(data_mod_sca, clustertot_hc_compl_euc, reference.distribution = "unif", centrotypes = "centroids")
# cat("Risultato gap con metodo complete e distanza euclidea: ", gap_statInd_hc_compl_euc$gap, "\n", sep = "")
# 
# fun_compl_manha<-hcut(data_mod_sca, hc_method = "complete", hc_metric = "manhattan")
# hc1_compl_manha<-fun_compl_manha
# hc2_compl_manha<-fun_compl_manha
# clustertot_hc_compl_manha<-cbind(hc1_compl_manha$cluster, hc2_compl_manha$cluster)
# gap_statInd_hc_compl_manha<-index.Gap(data_mod_sca, clustertot_hc_compl_manha, reference.distribution = "unif", centrotypes = "centroids")
# cat("Risultato gap con metodo complete e distanza manhattan: ", gap_statInd_hc_compl_manha$gap, "\n", sep = "")
# 
# 
# fun_single_euc<- hcut(data_mod_sca, hc_method = "single", hc_metric = "euclidean")
# hc1_single_euc<-fun_single_euc
# hc2_single_euc<-fun_single_euc
# clustertot_hc_single_euc<-cbind(hc1_single_euc$cluster, hc2_single_euc$cluster)
# gap_statInd_hc_single_euc<-index.Gap(data_mod_sca, clustertot_hc_single_euc, reference.distribution = "unif", centrotypes = "centroids")
# cat("Risultato gap con metodo single e distanza euclidea: ", gap_statInd_hc_single_euc$gap, "\n", sep = "")
# 
# fun_single_manha<-hcut(data_mod_sca, hc_method = "single", hc_metric = "manhattan")
# hc1_single_manha<-fun_single_manha
# hc2_single_manha<-fun_single_manha
# clustertot_hc_single_manha<-cbind(hc1_single_manha$cluster, hc2_single_manha$cluster)
# gap_statInd_hc_single_manha<-index.Gap(data_mod_sca, clustertot_hc_single_manha, reference.distribution = "unif", centrotypes = "centroids")
# cat("Risultato gap con metodo single e distanza manhattan: ", gap_statInd_hc_single_manha$gap, "\n", sep = "")
# 
# fun_average_euc<-hcut(data_mod_sca, hc_method = "average", hc_metric = "euclidean")
# hc1_average_euc<-fun_average_euc
# hc2_average_euc<-fun_average_euc
# clustertot_average_euc<-cbind(hc1_average_euc$cluster, hc2_average_euc$cluster)
# gap_statInd_hc_average_euc<-index.Gap(data_mod_sca, clustertot_average_euc, reference.distribution = "unif", centrotypes = "centroids")
# cat("Risultato gap con metodo average e distanza euclidea: ", gap_statInd_hc_average_euc$gap, "\n", sep = "")
# 
# fun_average_manha<-hcut(data_mod_sca, hc_method = "average", hc_metric = "manhattan")
# hc1_average_manha<-fun_average_manha
# hc2_average_manha<-fun_average_manha
# clustertot_average_manha<-cbind(hc1_average_manha$cluster, hc2_average_manha$cluster)
# gap_statInd_hc_average_manha<-index.Gap(data_mod_sca, clustertot_average_manha, reference.distribution = "unif", centrotypes = "centroids")
# cat("Risultato gap con metodo average e distanza manhatan: ", gap_statInd_hc_average_manha$gap, "\n", sep = "")
# 
# fun_ward_euc<- hcut(data_mod_sca, hc_method = "ward.D", hc_metric = "euclidean")
# hc1_ward_euc<-fun_ward_euc
# hc2_ward_euc<-fun_ward_euc
# clustertot_ward_euc<-cbind(hc1_ward_euc$cluster, hc2_ward_euc$cluster)
# gap_statInd_hc_ward_euc<-index.Gap(data_mod_sca, clustertot_ward_euc, reference.distribution = "unif", centrotypes = "centroids")
# cat("Risultato gap con metodo ward e distanza euclidea: ", gap_statInd_hc_ward_euc$gap, "\n", sep = "")
# 
# fun_ward_manha<-hcut(data_mod_sca, hc_method = "ward.D", hc_metric = "manhattan")
# hc1_ward_manha<-fun_ward_manha
# hc2_ward_manha<-fun_ward_manha
# clustertot_ward_manha<-cbind(hc1_ward_manha$cluster, hc2_ward_manha$cluster)
# gap_statInd_hc_ward_manha<-index.Gap(data_mod_sca, clustertot_ward_manha, reference.distribution = "unif", centrotypes = "centroids")
# cat("Risultato gap con metodo ward e distanza manhattan: ", gap_statInd_hc_ward_manha$gap, "\n", sep = "")



#Calcolo dbscan con index.Gap
if(filename =="10_7717_peerj_5665_dataYM2018_neuroblastoma.csv"){
  data<- read.csv(filename)
  data_mod<-na.omit(data)
  kNNdistplot(data_mod, k=ncol(data_mod)+1)
  abline(h=4.7, col="red", lty=2)
  dbs11<-dbscan(data_mod, eps = 4.7, minPts = 13)
  dbs21<-dbscan(data_mod, eps = 4.7, minPts = 13)
  clustertot11_db<-cbind(dbs11$cluster, dbs21$cluster)
  gap_statInd1_db<- index.Gap(data_mod, clustertot11_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
  cat("Gap statistic con dbscan su primo dataset: ",gap_statInd1_db$gap, "\n", sep = "")
}

if(filename =="journal.pone.0148699_S1_Text_Sepsis_SIRS_EDITED.csv"){
  data<- read.csv(filename)
  data_mod<-na.omit(data)
  kNNdistplot(data_mod, k=ncol(data_mod)+1)
  abline(h=94, col="red", lty=2)
  cl52_db<-dbscan(data_mod, eps = 94, minPts = ncol(data_mod)+1)
  cl62_db<-dbscan(data_mod, eps = 94, minPts = ncol(data_mod)+1)
  clall2_db<-cbind(cl52_db$cluster, cl62_db$cluster)
  gap_statInd2_db<-index.Gap(data_mod, clall2_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
  cat("Gap statistic con dbscan su secondo dataset: ", gap_statInd2_db$gap, "\n", sep = "")
}

if(filename == "journal.pone.0158570_S2File_depression_heart_failure.csv"){
  data<- read.csv(filename)
  data_mod<-na.omit(data)
  kNNdistplot(data_mod, k=ncol(data_mod)+1)
  abline(h=119, col="red", lty=2)
  cl53_db<-dbscan(data_mod, eps = 119, minPts = ncol(data_mod)+1)
  cl63_db<-dbscan(data_mod, eps = 119, minPts = ncol(data_mod)+1)
  clall3_db<-cbind(cl53_db$cluster, cl64_db$cluster)
  gap_statInd3_db<-index.Gap(data_mod, clall3_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
  cat("Gap statistic con dbscan su terzo dataset: ", gap_statInd3_db$gap, "\n", sep = "")
}

if(filename =="journal.pone.0175818_S1Dataset_Spain_cardiac_arrest_EDITED..csv" ){
  data<- read.csv(filename)
  data_mod<-na.omit(data)
  kNNdistplot(data_mod, k=ncol(data_mod)+1)
  abline(h=8.7, col="red", lty=2)
  cl54_db<-dbscan(data_mod, eps=8.7, minPts = ncol(data_mod)+1)
  cl64_db<-dbscan(data_mod, eps = 8.7, minPts = ncol(data_mod)+1)
  clall4_db<-cbind(cl54_db$cluster, cl64_db$cluster)
  gap_statInd4_db<-index.Gap(data_mod, clall4_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
  cat("Gap statistic con dbscan su quarto dataset: ", gap_statInd4_db$gap, "\n", sep = "")
}

if(filename == "Takashi2019_diabetes_type1_dataset_preprocessed.csv"){
  data<- read.csv(filename)
  data_mod<-na.omit(data)
  kNNdistplot(data_mod, k=ncol(data_mod)+1)
  abline(h=29, col="red", lty=2)
  cl55_db<-dbscan(data_mod, eps=29, minPts = ncol(data_mod)+1)
  cl65_db<-dbscan(data_mod, eps = 29, minPts = ncol(data_mod)+1)
  clall5_db<-cbind(cl55_db$cluster, cl65_db$cluster)
  gap_statInd5_db<-index.Gap(data_mod, clall5_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
  cat("Gap statistic con dbscan su quarto dataset: ", gap_statInd5_db$gap, "\n", sep = "")
}

eps2 = ceiling(nrow(data_mod)/2)
#eps2=2
dbs12<-dbscan(data_mod, eps = eps2, minPts = ncol(data_mod)+1)
dbs22<-dbscan(data_mod, eps=eps2, minPts = ncol(data_mod)+1)
clustertot12_db<-cbind(dbs12$cluster, dbs22$cluster)
gap_statInd12_db<-index.Gap(data_mod, clustertot12_db, reference.distribution = "unif", B=bootstrap_clus, centrotypes = "centroids")
cat("Gap statistic con dbscan su primo dataset utilizzando un diverso eps: ", gap_statInd12_db$gap, "\n", sep = "")







