library(cluster)
library(MASS)
library(clusterSim)
library(ggplot2)

#Matrice 10x10
matrice <- matrix(data= 0,nrow = 10, ncol = 10)
for (i in 1:10) {
  for (j in 1:10) {
    if(i>5){
      matrice[i,j]<-1
      
    }
  }
}
print(matrice)
cl1<-kmeans(matrice, centers = 2)
cl2<-kmeans(matrice, centers = 2)
clall<-cbind(cl1$cluster, cl2$cluster)
gap_stat1<-index.Gap(matrice,clall,reference.distribution = "unif",B=10, method = "k-means",centrotypes = "centroids")
print("Gap con index")
print(gap_stat1)
gap_statC<-clusGap(matrice, FUN=kmeans, nstart=25, K.max=2, B=10)
print("Gap con clus")
print(gap_statC)


gap_ind<-array()
n_righe<-array()
gap_clus1<-array()
gap_clus2<-array()
numero_da_prendere<-1
valore_min<-0
valore_max<-1
d<-array(0, dim = c(1,10))
cont<-0

while(d[10]==0){
  trovato<-FALSE
  riga_casuale<-sample(nrow(matrice),1)
  print(riga_casuale)
  for (k in 1:10) {
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
print(d)
for (f in d) {
  for (g in 1:10) {
      matrice[f,g]<-runif(numero_da_prendere, valore_min, valore_max)
      if (g==10){
        print(matrice)
        cl3<-kmeans(matrice, centers = 2)
        cl4<-kmeans(matrice, centers = 2)
        clall1<-cbind(cl3$cluster, cl4$cluster)
        gap_stat2<-index.Gap(matrice,clall1,reference.distribution = "unif",B=10,method = "k-means", centrotypes = "centroids")
        print("Gap con index")
        print(gap_stat2$gap)
        gap_ind[f]<-(gap_stat2$gap)
        n_righe[f]<-f
        gap_statC2<- clusGap(matrice, FUN=kmeans, nstart=25, K.max=2, B=10)
        print("Gap con clus")
        print(gap_statC2)
        gap_clus1[f]<-(gap_statC2$Tab[1,3])
        gap_clus2[f]<-(gap_statC2$Tab[2,3])
      }
    }
    
    }

#Creo dataframe per index.Gap
gapI<-c(gap_stat1$gap, gap_ind)
n_righe2<-c(n_righe,11)
my_data<-data.frame(X=n_righe2, Y=gapI)
print(my_data)
#Stampo il grafico con index.gap
ggplot(my_data, aes(x=n_righe2, y=gapI))+geom_point(shape=20, color="blue")+geom_smooth(method = lm, color="red")+labs(title = "Andamento Gap statistic con index.Gap", x="numero righe manipolate", y="Gap")

#Creo dataframe per clusGap
gapC1<-c(gap_statC$Tab[1,3], gap_clus1)
gapC2<-c(gap_statC$Tab[2,3], gap_clus2)
my_data2<-data.frame(X=n_righe2, Y=gapC1, Z=gapC2)
print(my_data2)
#Stampo il grafico con clusGap
ggplot(my_data, aes(x=n_righe2, y=gapC1))+geom_point(shape=20, color="blue")+geom_smooth(method = lm, color="red")+labs(title = "Andamento Gap statistic con clusGap", x="numero righe manipolate", y="Gap")
ggplot(my_data, aes(x=n_righe2, y=gapC2))+geom_point(shape=20, color="blue")+geom_smooth(method = lm, color="red")+labs(title = "Andamento Gap statistic con clusGap", x="numero righe manipolate", y="Gap")

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
cl5<-kmeans(matrice2, centers = 2)
cl6<-kmeans(matrice2, centers = 2)
clall2<-cbind(cl5$cluster, cl6$cluster)
gap_stat3<-index.Gap(matrice2,clall2,reference.distribution = "unif",B=10, method = "k-means",centrotypes = "centroids")
print("Gap con index")
print(gap_stat3)
gap_statC3<- clusGap(matrice2, FUN=kmeans, nstart=25, K.max=2, B=10)
print("Gap con clus")
print(gap_statC3)

gap_ind2<-array()
n_righe3<-array()
gap_clus3<-array()
gap_clus4<-array()
for (f in 1:100) {
  for (g in 1:100) {
      matrice2[f,g]<-runif(1,0,1)
    if(g==100){
      cl7<-kmeans(matrice2, centers = 2)
      cl8<-kmeans(matrice2, centers = 2)
      clall3<-cbind(cl7$cluster, cl8$cluster)
      gap_stat4<-index.Gap(matrice2,clall3,reference.distribution = "unif",B=10,method = "k-means", centrotypes = "centroids")
      print("Gap con index")
      print(gap_stat4)
      gap_ind2[f]<-(gap_stat4$gap)
      n_righe3[f]<-f
      gap_statC4<- clusGap(matrice2, FUN=kmeans, nstart=25, K.max=2, B=10)
      print("Gap con clus")
      print(gap_statC4)
      gap_clus3[f]<-(gap_statC4$Tab[1,3])
      gap_clus4[f]<-(gap_statC4$Tab[2,3])
    }
  }
}
#Creo dataframe per index.Gap
gapI2<-c(gap_stat3$gap, gap_ind2)
n_righe4<-c(n_righe3,101)
my_data3<-data.frame(X=n_righe4, Y=gapI2)
print(my_data3)
#Stampo il grafico con index.gap
ggplot(my_data3, aes(x=n_righe4, y=gapI2))+geom_point(shape=20, color="blue")+geom_smooth(method = lm, color="red")+labs(title = "Andamento Gap statistic con index.Gap", x="numero righe manipolate", y="Gap")

#Creo dataframe per clusGap
gapC3<-c(gap_statC3$Tab[1,3], gap_clus3)
gapC4<-c(gap_statC4$Tab[2,3], gap_clus4)
my_data4<-data.frame(X=n_righe4, Y=gapC3, Z=gapC4)
print(my_data4)
gapC5<-c(gapC3, gapC4)
n_righe5<-c(n_righe4, n_righe4)
my_data5<-data.frame(X=n_righe5, Y=gapC5)
#Stampo il grafico con clusGap
ggplot(my_data4, aes(x=n_righe4, y=gapC3))+geom_point(shape=20, color="blue")+geom_smooth(method = lm, color="red")+labs(title = "Andamento Gap statistic con clusGap", x="numero righe manipolate", y="Gap")
ggplot(my_data4, aes(x=n_righe4, y=gapC4))+geom_point(shape=20, color="blue")+geom_smooth(method = lm, color="red")+labs(title = "Andamento Gap statistic con clusGap", x="numero righe manipolate", y="Gap")

ggplot(my_data5, aes(x=n_righe5, y=gapC5))+geom_point(shape=20, color="blue")+geom_smooth(method = lm, color="red")+labs(title = "Andamento Gap statistic con clusGap", x="numero righe manipolate", y="Gap")

