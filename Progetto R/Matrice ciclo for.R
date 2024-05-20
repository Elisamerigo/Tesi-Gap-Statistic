library(cluster)
library(MASS)
library(clusterSim)

#Matrice 10x10
matrice <- matrix(data= 0,nrow = 10, ncol = 10)
for (i in 1:10) {
  for (j in 1:10) {
    if(i>5){
      matrice[i,j]<-1
      
    }
  }
}
for (f in 1:10) {
  for (g in 1:10) {
    if(f>5)
      matrice[f,g]<-sample(seq(0.1,0.9,0.1),1)
      if(f>5 & g==10)
        print(matrice)
        
  }
  if(f==1){
    print(matrice)
    cl1<-kmeans(matrice, centers = 2)
    cl2<-kmeans(matrice, centers = 2)
    clall<-cbind(cl1$cluster, cl2$cluster)
    gap_stat1<-index.Gap(matrice,clall,reference.distribution = "unif",B=10, method = "k-means",centrotypes = "centroids")
    print(gap_stat1) 
    
  }
  if(f>5){
    cl3<-kmeans(matrice, centers = 2)
    cl4<-kmeans(matrice, centers = 2)
    clall1<-cbind(cl3$cluster, cl4$cluster)
    gap_stat2<-index.Gap(matrice,clall1,reference.distribution = "unif",B=10,method = "k-means", centrotypes = "centroids")
    print(gap_stat2)
    
  }
}

#Matrice 100x100
options(max.print = 10000)
matrice2 <- matrix(data= 0,nrow = 100, ncol = 100)
for (i in 1:100) {
  for (j in 1:100) {
    if(i>50){
      matrice2[i,j]<-1
      
    }
  }
}
for (f in 1:100) {
  for (g in 1:100) {
    if(f>50)
      matrice2[f,g]<-sample(seq(0.1,0.9,0.1),1)
    #if(f>50 & g==100)
      #print(matrice2)
    
  }
  if(f==1){
    #print(matrice2)
    cl5<-kmeans(matrice2, centers = 2)
    cl6<-kmeans(matrice2, centers = 2)
    clall2<-cbind(cl5$cluster, cl6$cluster)
    gap_stat3<-index.Gap(matrice2,clall2,reference.distribution = "unif",B=10, method = "k-means",centrotypes = "centroids")
    print(gap_stat3) 
    
  }
  if(f>50){
    cl7<-kmeans(matrice2, centers = 2)
    cl8<-kmeans(matrice2, centers = 2)
    clall3<-cbind(cl7$cluster, cl8$cluster)
    gap_stat4<-index.Gap(matrice2,clall3,reference.distribution = "unif",B=10,method = "k-means", centrotypes = "centroids")
    print(gap_stat4)
    
  }
}
