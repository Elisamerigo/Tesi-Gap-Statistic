library(cluster)
library(MASS)
library(clusterSim)
library(NbClust)
library(factoextra)
A<-matrix(c(0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1), 8,4)
cl1<-kmeans(A, centers = 2)
cl2<-kmeans(A, centers = 2)
clall<-cbind(cl1$cluster, cl2$cluster)
gap_stat1<-index.Gap(A,clall,reference.distribution = "unif", B=10, method="k-means", centrotypes = "centroids")
print(gap_stat1, method = "firstmax")

fviz_cluster(cl1, A,
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
fviz_cluster(cl2, A,
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
fviz_nbclust(A,kmeans,nstart=25, method="gap_stat",k.max = 2, nboot = 10) + labs(subtitle = "Gap statistic method")

settima_riga<-matrix(c(0,0,0,0,1,1,0.5,1,0,0,0,0,1,1,0.3,1,0,0,0,0,1,1,0.9,1,0,0,0,0,1,1,0.1,1), 8,4)
cl3<-kmeans(settima_riga, centers=2)
cl4<-kmeans(settima_riga, centers = 2)
clall2<-cbind(cl3$cluster, cl4$cluster)
gap_stat2<-index.Gap(settima_riga,clall2,reference.distribution = "unif", B=10, method="k-means", centrotypes = "centroids")
print(gap_stat2, method = "firstmax")
fviz_cluster(cl3, B,
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "euclid", 
             ggtheme = theme_bw()
)
fviz_cluster(cl4, B,
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "euclid", 
             ggtheme = theme_bw()
)
fviz_nbclust(settima_riga,kmeans,nstart=25, method="gap_stat",k.max = 2, nboot = 10) + labs(subtitle = "Gap statistic method")

ottava_riga<-matrix(c(0,0,0,0,1,1,0.5,0.2,0,0,0,0,1,1,0.3,0.6,0,0,0,0,1,1,0.9,0.4,0,0,0,0,1,1,0.1,0.8), 8,4)
cl5<-kmeans(ottava_riga, centers=2)
cl6<-kmeans(ottava_riga, centers = 2)
clall3<-cbind(cl5$cluster, cl6$cluster)
gap_stat3<-index.Gap(ottava_riga,clall3,reference.distribution = "unif",B=10,method = "k-means", centrotypes = "centroids")
print(gap_stat3, method = "firstmax")
fviz_cluster(cl5, ottava_riga,
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
fviz_cluster(cl6, ottava_riga,
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

fviz_nbclust(ottava_riga,kmeans,nstart=25, method="gap_stat",k.max = 2, nboot = 10) + labs(subtitle = "Gap statistic method")

sesta_riga<-matrix(c(0,0,0,0,1,0.8,0.5,0.2,0,0,0,0,1,0.9,0.3,0.6,0,0,0,0,1,0.1,0.9,0.4,0,0,0,0,1,0.5,0.1,0.8), 8,4)
cl7<-kmeans(sesta_riga, centers=2)
cl8<-kmeans(sesta_riga, centers = 2)
clall4<-cbind(cl7$cluster, cl8$cluster)
gap_stat4<-index.Gap(sesta_riga, clall4,reference.distribution = "unif", B=10, method="k-means", centrotypes = "centroids")
print(gap_stat4, method = "firstmax")
fviz_cluster(cl7, sesta_riga,
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
fviz_cluster(cl8, sesta_riga,
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
fviz_nbclust(sesta_riga,kmeans,nstart=25, method="gap_stat",k.max = 2, nboot = 10) + labs(subtitle = "Gap statistic method")


quinta_riga<-matrix(c(0,0,0,0,0.3,0.8,0.5,0.2,0,0,0,0,0.7,0.9,0.3,0.6,0,0,0,0,0.5,0.1,0.9,0.4,0,0,0,0,0.3,0.5,0.1,0.8), 8,4)
cl9<-kmeans(quinta_riga, centers = 2)
cl10<-kmeans(quinta_riga, centers = 2)
clall5<-cbind(cl9$cluster, cl10$cluster)
gap_stat5<-index.Gap(quinta_riga,clall5,reference.distribution = "unif", B=10, method = "k-means", centrotypes = "centroids")
print(gap_stat5, method = "firstmax")
fviz_cluster(cl9, quinta_riga,
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
fviz_cluster(cl10, quinta_riga,
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
fviz_nbclust(quinta_riga,kmeans,nstart=25, method="gap_stat",k.max = 2, nboot = 10) + labs(subtitle = "Gap statistic method")
