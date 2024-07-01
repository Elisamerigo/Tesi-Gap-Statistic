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
data<- read.csv("10_7717_peerj_5665_dataYM2018_neuroblastoma.csv")
data<- read.csv("journal.pone.0148699_S1_Text_Sepsis_SIRS_EDITED.csv")
data<- read.csv("journal.pone.0158570_S2File_depression_heart_failure.csv")
data<- read.csv("journal.pone.0175818_S1Dataset_Spain_cardiac_arrest_EDITED..csv")
data<- read.csv("Takashi2019_diabetes_type1_dataset_preprocessed.csv")
View(data)
data_mod<-na.omit(data)
View(data_mod)

n_cluster<- 2
gap_i<-array()
#Calcolo il k-means con k=2 con index.Gap
 while (n_cluster<5) {
    kmeans1_k2<-kmeans(data_mod, centers = i)
    kmeans2_k2<-kmeans(data_mod, centers = i)
    clustertot_k2<-cbind(kmeans1_k2$cluster, kmeans2_k2$cluster)
    gap_statInd<-index.Gap(data_mod, clustertot_k2, reference.distribution = "unif", B=10, ,method = "k-means", centrotypes = "centroids")
    gap_i[n_cluster]<-gap_statInd$gap
    cat("Gap statistic con index.Gap utilizzando il k-means con k = ", n_cluster, " vale: ", gap_i[n_cluster], "\n", sep = "")
    gap_statClus<- clusGap(data_mod, FUN=kmeans, nstart=25, K.max=n_cluster, B=50)
    #cat("Gap statistic con clusGap utilizzando il k-means con k = ", n_cluster, " vale: ", sep = "")
    #print(gap_statClus)
    as.data.frame(gap_statClus$Tab)
    val_gap<-(as.data.frame(gap_statClus$Tab))$gap
    mean(val_gap)
    cat("Gap statistic con clusGap facendo la media dei valori utilizzando il k-means con k = ", n_cluster, " vale: ", mean(val_gap), "\n", sep = "")
   n_cluster=n_cluster+1


}


#Calcolo il cluster gerarchico con index.Gap
data_mod_sca<- data_mod %>% scale() %>% as.data.frame()
distanza<-dist(data_mod_sca,method = "euclidean")
distanza2<-dist(data_mod_sca,method = "manhattan")

max_dist<-agnes(data_mod_sca, method = "complete")
cat("Risultato con complete linkage: ", max_dist$ac, "\n", sep = "")
min_dist<-agnes(data_mod_sca, method = "single")
cat("Risultato con single linkage: ", min_dist$ac, "\n", sep = "")
average_dist<-agnes(data_mod_sca, method = "average")
cat("Risultato con average linkage: ", average_dist$ac, "\n", sep = "")
ward_meth<-agnes(data_mod_sca, method = "ward")
cat("Risultato con ward linkage: ", ward_meth$ac, "\n", sep = "")

#Calcolo il cluster gerarchico con clusGap
gap_statClus_hie<- clusGap(data_mod_sca, FUN=hcut, K.max=2, B=50, verbose = FALSE)
as.data.frame(gap_statClus_hie$Tab)
val_gap_hie<-(as.data.frame(gap_statClus_hie$Tab))$gap
mean(val_gap_hie)
cat("Valore medio gap con clusGap: ", mean(val_gap_hie), "\n", sep = "")


#Calcolo dbscan con index.Gap
kNNdistplot(data1_mod, k=13)
abline(h=4.7, col="red", lty=2)
cl51_db<-dbscan(data1_mod, eps = 4.7, minPts = 13)
cl61_db<-dbscan(data1_mod, eps = 4.7, minPts = 13)
clall1_db<-cbind(cl51_db$cluster, cl61_db$cluster)
gap_statInd1_db<- index.Gap(data1_mod, clall1_db, reference.distribution = "unif", B=10, method = "k-means", centrotypes = "centroids")
print(gap_statInd1_db$gap)

#Calcolo dbscan con clusGap
gap_statClus1_db<-clusGap(data1_mod, FUN=dbscan, K.max = 2, B=50)
print(gap_statClus1_db)


fgs1<-fviz_gap_stat(gap_statClus1_db, linecolor = "steelblue", maxSE = list(method =
                                                                       "firstSEmax", SE.factor = 1))
fviz_gap_stat(gap_statClus1_db)






#Importo il secondo dataset

View(data2)
data2_mod<-na.omit(data2)
View(data2_mod)

#Calcolo il k-means con k=2 con index.Gap
cl12_k2<-kmeans(data2_mod, centers = 2)
cl22_k2<-kmeans(data2_mod, centers = 2)
clall2_k2<-cbind(cl12_k2$cluster, cl22_k2$cluster)
gap_statInd2_di_k2<-index.Gap(data2_mod, clall2_k2, reference.distribution = "unif", B=10, ,method = "k-means", centrotypes = "centroids")
print("Gap statistic con index.Gap utilizzando il k-means con k=2")
print(gap_statInd2_di_k2$gap)

#Calcolo il k-means con k=3 con index.Gap
cl12_k3<-kmeans(data2_mod, centers = 3)
cl22_k3<-kmeans(data2_mod, centers = 3)
clall2_k3<-cbind(cl12_k3$cluster, cl22_k3$cluster)
gap_statInd2_di_k3<-index.Gap(data2_mod, clall2_k3, reference.distribution = "unif", B=10, ,method = "k-means", centrotypes = "centroids")
print("Gap statistic con index.Gap utilizzando il k-means con k=3")
print(gap_statInd2_di_k3$gap)

#Calcolo il k-means con k=4 con index.Gap
cl12_k4<-kmeans(data2_mod, centers = 3)
cl22_k4<-kmeans(data2_mod, centers = 3)
clall2_k4<-cbind(cl12_k4$cluster, cl22_k4$cluster)
gap_statInd2_di_k4<-index.Gap(data2_mod, clall2_k4, reference.distribution = "unif", B=10, ,method = "k-means", centrotypes = "centroids")
print("Gap statistic con index.Gap utilizzando il k-means con k=4")
print(gap_statInd2_di_k4$gap)

#Calcolo il k-means con k=2 con clusGap
gap_statClus2_k2<- clusGap(data2_mod, FUN=kmeans, nstart=25, K.max=2, B=50)
print("Gap statistic con clusGap utilizzando il k-means con k=2")
print(gap_statClus2_k2)

#Calcolo il k-means con k=3 con clusGap
gap_statClus2_k3<- clusGap(data2_mod, FUN=kmeans, nstart=25, K.max=3, B=50)
print("Gap statistic con clusGap utilizzando il k-means con k=3")
print(gap_statClus2_k3)

#Calcolo il k-means con k=4 con clusGap
gap_statClus2_k4<- clusGap(data2_mod, FUN=kmeans, nstart=25, K.max=4, B=50)
print("Gap statistic con clusGap utilizzando il k-means con k=4")
print(gap_statClus2_k4)

#Calcolo il cluster gerarchico con index.Gap
data2_mod_sca<- data2_mod %>% scale() %>% as.data.frame()
distanza2<-dist(data2_mod_sca,method = "euclidean")
cl32_hie<-hclust(distanza2, method = "ward.D2")
cl42_hie<-hclust(distanza2, method = "ward.D2")
clall2_hie<-rbind(cl32_hie$merge, cl42_hie$merge)
gap_statInd2_hie<-index.Gap(data2_mod, clall2_hie, reference.distribution = "unif", B=10, method = "ward.D2", centrotypes = "centroids")

#Calcolo il cluster gerarchico con clusGap
gap_statClus2_hie<- clusGap(data2_mod_sca, FUN=hcut, K.max=2, B=50, verbose = FALSE)


#Calcolo dbscan con index.Gap
kNNdistplot(data2_mod, k=17)
abline(h=77, col="red", lty=2)
cl52_db<-dbscan(data2_mod, eps = 77, minPts = 17)
cl62_db<-dbscan(data2_mod, eps = 77, minPts = 17)
clall2_db<-cbind(cl52_db$cluster, cl62_db$cluster)
gap_statInd2_db<- index.Gap(data2_mod, clall2_db, reference.distribution = "unif", B=10, method = "k-means", centrotypes = "centroids")
print(gap_statInd2_db$gap)

#Calcolo dbscan con clusGap
gap_statClus2_db<-clusGap(data2_mod, FUN=dbscan, K.max = 2, B=50)
print(gap_statClus2_db)



#Importo il terzo dataset

View(data3)
data3_mod<-na.omit(data3)
View(data3_mod)

#Calcolo il k-means con k=2 con index.Gap
cl13_k2<-kmeans(data3_mod, centers = 2)
cl23_k2<-kmeans(data3_mod, centers = 2)
clall3_k2<-cbind(cl13_k2$cluster, cl23_k2$cluster)
gap_statInd3_di_k2<-index.Gap(data3_mod, clall3_k2, reference.distribution = "unif", B=10, ,method = "k-means", centrotypes = "centroids")
print("Gap statistic con index.Gap utilizzando il k-means con k=2")
print(gap_statInd3_di_k2$gap)

#Calcolo il k-means con k=3 con index.Gap
cl13_k3<-kmeans(data3_mod, centers = 3)
cl23_k3<-kmeans(data3_mod, centers = 3)
clall3_k3<-cbind(cl13_k3$cluster, cl23_k3$cluster)
gap_statInd3_di_k3<-index.Gap(data3_mod, clall3_k3, reference.distribution = "unif", B=10, ,method = "k-means", centrotypes = "centroids")
print("Gap statistic con index.Gap utilizzando il k-means con k=3")
print(gap_statInd3_di_k3$gap)

#Calcolo il k-means con k=4 con index.Gap
cl13_k4<-kmeans(data3_mod, centers = 3)
cl23_k4<-kmeans(data3_mod, centers = 3)
clall3_k4<-cbind(cl13_k4$cluster, cl23_k4$cluster)
gap_statInd3_di_k4<-index.Gap(data3_mod, clall3_k4, reference.distribution = "unif", B=10, ,method = "k-means", centrotypes = "centroids")
print("Gap statistic con index.Gap utilizzando il k-means con k=4")
print(gap_statInd3_di_k4$gap)

#Calcolo il k-means con k=2 con clusGap
gap_statClus3_k2<- clusGap(data3_mod, FUN=kmeans, nstart=25, K.max=2, B=50)
print("Gap statistic con clusGap utilizzando il k-means con k=2")
print(gap_statClus3_k2)

#Calcolo il k-means con k=3 con clusGap
gap_statClus3_k3<- clusGap(data3_mod, FUN=kmeans, nstart=25, K.max=3, B=50)
print("Gap statistic con clusGap utilizzando il k-means con k=3")
print(gap_statClus3_k3)

#Calcolo il k-means con k=4 con clusGap
gap_statClus3_k4<- clusGap(data3_mod, FUN=kmeans, nstart=25, K.max=4, B=50)
print("Gap statistic con clusGap utilizzando il k-means con k=4")
print(gap_statClus3_k4)

#Calcolo il cluster gerarchico con index.Gap
#data1_mod=data1_mod[,-6]
#nuovo_dataset<-as.numeric(data1_mod)
data3_mod_sca<- data3_mod %>% scale() %>% as.data.frame()
distanza3<-dist(data3_mod_sca,method = "euclidean")
cl33_hie<-hclust(distanza3, method = "ward.D2")
cl43_hie<-hclust(distanza3, method = "ward.D2")
clall3_hie<-rbind(cl33_hie$merge, cl43_hie$merge)
gap_statInd3_hie<-index.Gap(data3_mod, clall3_hie, reference.distribution = "unif", B=10, method = "ward.D2", centrotypes = "centroids")

#Calcolo il cluster gerarchico con clusGap
gap_statClus3_hie<- clusGap(data3_mod_sca, FUN=hcut, K.max=2, B=50, verbose = FALSE)


#Calcolo dbscan con index.Gap
kNNdistplot(data3_mod, k=17)
abline(h=128, col="red", lty=2)
cl53_db<-dbscan(data3_mod, eps = 128, minPts = 17)
cl63_db<-dbscan(data3_mod, eps = 128, minPts = 17)
clall3_db<-cbind(cl53_db$cluster, cl63_db$cluster)
gap_statInd3_db<- index.Gap(data3_mod, clall3_db, reference.distribution = "unif", B=10, method = "k-means", centrotypes = "centroids")
print(gap_statInd3_db$gap)

#Calcolo dbscan con clusGap
gap_statClus3_db<-clusGap(data3_mod, FUN=dbscan, K.max = 2, B=50)
print(gap_statClus3_db)




#Importo il quarto dataset

View(data4)
data4_mod<-na.omit(data4)
View(data4_mod)

#Calcolo il k-means con k=2 con index.Gap
cl14_k2<-kmeans(data4_mod, centers = 2)
cl24_k2<-kmeans(data4_mod, centers = 2)
clall4_k2<-cbind(cl14_k2$cluster, cl24_k2$cluster)
gap_statInd4_di_k2<-index.Gap(data4_mod, clall4_k2, reference.distribution = "unif", B=10, ,method = "k-means", centrotypes = "centroids")
print("Gap statistic con index.Gap utilizzando il k-means con k=2")
print(gap_statInd4_di_k2$gap)

#Calcolo il k-means con k=3 con index.Gap
cl14_k3<-kmeans(data4_mod, centers = 3)
cl24_k3<-kmeans(data4_mod, centers = 3)
clall4_k3<-cbind(cl14_k3$cluster, cl24_k3$cluster)
gap_statInd4_di_k3<-index.Gap(data4_mod, clall4_k3, reference.distribution = "unif", B=10, ,method = "k-means", centrotypes = "centroids")
print("Gap statistic con index.Gap utilizzando il k-means con k=3")
print(gap_statInd4_di_k3$gap)

#Calcolo il k-means con k=4 con index.Gap
cl14_k4<-kmeans(data4_mod, centers = 3)
cl24_k4<-kmeans(data4_mod, centers = 3)
clall4_k4<-cbind(cl14_k4$cluster, cl24_k4$cluster)
gap_statInd4_di_k4<-index.Gap(data4_mod, clall4_k4, reference.distribution = "unif", B=10, ,method = "k-means", centrotypes = "centroids")
print("Gap statistic con index.Gap utilizzando il k-means con k=4")
print(gap_statInd4_di_k4$gap)

#Calcolo il k-means con k=2 con clusGap
gap_statClus4_k2<- clusGap(data4_mod, FUN=kmeans, nstart=25, K.max=2, B=50)
print("Gap statistic con clusGap utilizzando il k-means con k=2")
print(gap_statClus4_k2)

#Calcolo il k-means con k=3 con clusGap
gap_statClus4_k3<- clusGap(data4_mod, FUN=kmeans, nstart=25, K.max=3, B=50)
print("Gap statistic con clusGap utilizzando il k-means con k=3")
print(gap_statClus4_k3)

#Calcolo il k-means con k=4 con clusGap
gap_statClus4_k4<- clusGap(data4_mod, FUN=kmeans, nstart=25, K.max=4, B=50)
print("Gap statistic con clusGap utilizzando il k-means con k=4")
print(gap_statClus4_k4)

#Calcolo il cluster gerarchico con index.Gap
#data1_mod=data1_mod[,-6]
#nuovo_dataset<-as.numeric(data1_mod)
data4_mod_sca<- data4_mod %>% scale() %>% as.data.frame()
distanza4<-dist(data4_mod_sca,method = "euclidean")
cl34_hie<-hclust(distanza4, method = "ward.D2")
cl44_hie<-hclust(distanza4, method = "ward.D2")
clall4_hie<-rbind(cl34_hie$merge, cl44_hie$merge)
gap_statInd4_hie<-index.Gap(data4_mod, clall4_hie, reference.distribution = "unif", B=10, method = "ward.D2", centrotypes = "centroids")

#Calcolo il cluster gerarchico con clusGap
gap_statClus4_hie<- clusGap(data4_mod_sca, FUN=hcut, K.max=2, B=50, verbose = FALSE)


#Calcolo dbscan con index.Gap
kNNdistplot(data4_mod, k=11)
abline(h=7.5, col="red", lty=2)
cl54_db<-dbscan(data4_mod, eps = 7.5, minPts = 11)
cl64_db<-dbscan(data4_mod, eps = 7.5, minPts = 11)
clall4_db<-cbind(cl54_db$cluster, cl64_db$cluster)
gap_statInd4_db<- index.Gap(data4_mod, clall4_db, reference.distribution = "unif", B=10, method = "k-means", centrotypes = "centroids")
print(gap_statInd4_db$gap)

#Calcolo dbscan con clusGap
gap_statClus4_db<-clusGap(data4_mod, FUN=dbscan, K.max = 2, B=50)
print(gap_statClus4_db)



#Importo il quinto dataset

View(data5)
data5_mod<-na.omit(data5)
View(data5_mod)

#Calcolo il k-means con k=2 con index.Gap
cl15_k2<-kmeans(data5_mod, centers = 2)
cl25_k2<-kmeans(data5_mod, centers = 2)
clall5_k2<-cbind(cl15_k2$cluster, cl25_k2$cluster)
gap_statInd5_di_k2<-index.Gap(data5_mod, clall5_k2, reference.distribution = "unif", B=10, ,method = "k-means", centrotypes = "centroids")
print("Gap statistic con index.Gap utilizzando il k-means con k=2")
print(gap_statInd5_di_k2$gap)

#Calcolo il k-means con k=3 con index.Gap
cl15_k3<-kmeans(data5_mod, centers = 3)
cl25_k3<-kmeans(data5_mod, centers = 3)
clall5_k3<-cbind(cl15_k3$cluster, cl25_k3$cluster)
gap_statInd5_di_k3<-index.Gap(data5_mod, clall5_k3, reference.distribution = "unif", B=10, ,method = "k-means", centrotypes = "centroids")
print("Gap statistic con index.Gap utilizzando il k-means con k=3")
print(gap_statInd5_di_k3$gap)

#Calcolo il k-means con k=4 con index.Gap
cl15_k4<-kmeans(data5_mod, centers = 3)
cl25_k4<-kmeans(data5_mod, centers = 3)
clall5_k4<-cbind(cl15_k4$cluster, cl25_k4$cluster)
gap_statInd5_di_k4<-index.Gap(data5_mod, clall5_k4, reference.distribution = "unif", B=10, ,method = "k-means", centrotypes = "centroids")
print("Gap statistic con index.Gap utilizzando il k-means con k=4")
print(gap_statInd5_di_k4$gap)

#Calcolo il k-means con k=2 con clusGap
gap_statClus5_k2<- clusGap(data5_mod, FUN=kmeans, nstart=25, K.max=2, B=50)
print("Gap statistic con clusGap utilizzando il k-means con k=2")
print(gap_statClus5_k2)

#Calcolo il k-means con k=3 con clusGap
gap_statClus5_k3<- clusGap(data5_mod, FUN=kmeans, nstart=25, K.max=3, B=50)
print("Gap statistic con clusGap utilizzando il k-means con k=3")
print(gap_statClus5_k3)

#Calcolo il k-means con k=4 con clusGap
gap_statClus5_k4<- clusGap(data5_mod, FUN=kmeans, nstart=25, K.max=4, B=50)
print("Gap statistic con clusGap utilizzando il k-means con k=4")
print(gap_statClus5_k4)

#Calcolo il cluster gerarchico con index.Gap
#data1_mod=data1_mod[,-6]
#nuovo_dataset<-as.numeric(data1_mod)
data5_mod_sca<- data5_mod %>% scale() %>% as.data.frame()
distanza5<-dist(data5_mod_sca,method = "euclidean")
cl35_hie<-hclust(distanza5, method = "ward.D2")
cl45_hie<-hclust(distanza5, method = "ward.D2")
clall5_hie<-rbind(cl35_hie$merge, cl45_hie$merge)
gap_statInd5_hie<-index.Gap(data5_mod, clall5_hie, reference.distribution = "unif", B=10, method = "ward.D2", centrotypes = "centroids")

#Calcolo il cluster gerarchico con clusGap
gap_statClus5_hie<- clusGap(data5_mod_sca, FUN=hcut, K.max=2, B=50, verbose = FALSE)


#Calcolo dbscan con index.Gap
kNNdistplot(data5_mod, k=21)
abline(h=26.5, col="red", lty=2)
cl55_db<-dbscan(data5_mod, eps = 26.5, minPts = 21)
cl65_db<-dbscan(data5_mod, eps = 26.5, minPts = 21)
clall5_db<-cbind(cl55_db$cluster, cl65_db$cluster)
gap_statInd5_db<- index.Gap(data5_mod, clall5_db, reference.distribution = "unif", B=10, method = "k-means", centrotypes = "centroids")
print(gap_statInd5_db$gap)

#Calcolo dbscan con clusGap
gap_statClus5_db<-clusGap(data5_mod, FUN=dbscan, K.max = 2, B=50)
print(gap_statClus5_db)