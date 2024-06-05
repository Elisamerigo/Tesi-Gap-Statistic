library(cluster)
library(ggplot2)
library(ggpubr)
library(factoextra)
A<-matrix(c(0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1), 8,4)
gap_stat1<-clusGap(A, FUN=kmeans, nstart=25, K.max=2, B=10)
print(gap_stat1, method = "firstmax")
kmeans_model1<-kmeans(A, centers = 2)
fviz_cluster(kmeans_model1, A,
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
fgs1<-fviz_gap_stat(gap_stat1, linecolor = "steelblue", maxSE = list(method =
                                                                "firstSEmax", SE.factor = 1))
fviz_gap_stat(gap_stat1)

#Matrice nuova con settima riga cambiata
B<-matrix(c(0,0,0,0,1,1,0.5,1,0,0,0,0,1,1,0.3,1,0,0,0,0,1,1,0.9,1,0,0,0,0,1,1,0.1,1), 8,4)
gap_stat2<-clusGap(B, FUN=kmeans, nstart=25, K.max=2, B=10)
print(gap_stat2, method = "firstmax")
kmeans_model2<-kmeans(B, centers = 2)
fviz_cluster(kmeans_model2, B,
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "euclid", 
             ggtheme = theme_bw()
)
fgs2<-fviz_gap_stat(gap_stat2, linecolor = "steelblue", maxSE = list(method =
                                                                     "firstSEmax", SE.factor = 1))
fviz_gap_stat(gap_stat2)

#Matrice nuova con ottava riga cambiata
C<-matrix(c(0,0,0,0,1,1,0.5,0.2,0,0,0,0,1,1,0.3,0.6,0,0,0,0,1,1,0.9,0.4,0,0,0,0,1,1,0.1,0.8), 8,4)
gap_stat3<-clusGap(C, FUN=kmeans, nstart=25, K.max=2, B=10)
print(gap_stat3, method = "firstmax")
kmeans_model3<-kmeans(C, centers = 2)
fviz_cluster(kmeans_model3, C,
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
fgs3<-fviz_gap_stat(gap_stat3, linecolor = "steelblue", maxSE = list(method =
                                                                     "firstSEmax", SE.factor = 1))
fviz_gap_stat(gap_stat3)

#Matrice nuova con sesta riga cambiata
D<-matrix(c(0,0,0,0,1,0.8,0.5,0.2,0,0,0,0,1,0.9,0.3,0.6,0,0,0,0,1,0.1,0.9,0.4,0,0,0,0,1,0.5,0.1,0.8), 8,4)
gap_stat4<-clusGap(D, FUN=kmeans, nstart=25, K.max=2, B=10)
print(gap_stat4, method = "firstmax")
kmeans_model4<-kmeans(D, centers = 2)
fviz_cluster(kmeans_model4, D,
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
fgs4<-fviz_gap_stat(gap_stat4, linecolor = "steelblue", maxSE = list(method =
                                                                     "firstSEmax", SE.factor = 1))
fviz_gap_stat(gap_stat4)

#Matrice nuova con quinta riga cambiata
E<-matrix(c(0,0,0,0,0.3,0.8,0.5,0.2,0,0,0,0,0.7,0.9,0.3,0.6,0,0,0,0,0.5,0.1,0.9,0.4,0,0,0,0,0.3,0.5,0.1,0.8), 8,4)
gap_stat5<-clusGap(E, FUN=kmeans, nstart=25, K.max=2, B=10)
print(gap_stat5, method = "firstmax")
kmeans_model5<-kmeans(E, centers = 2)
fviz_cluster(kmeans_model5, E,
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
fgs5<-fviz_gap_stat(gap_stat5, linecolor = "steelblue", maxSE = list(method =
                                                                     "firstSEmax", SE.factor = 1))
fviz_gap_stat(gap_stat5)


F<-A+1
gap_stat6<-clusGap(F, FUN=kmeans, nstart=25, K.max=2, B=10)
print(gap_stat6, method = "firstmax")
kmeans_model6<-kmeans(F, centers = 2)
fviz_cluster(kmeans_model1, F,
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
fgs1<-fviz_gap_stat(gap_stat6, linecolor = "steelblue", maxSE = list(method =
                                                                       "firstSEmax", SE.factor = 1))
fviz_gap_stat(gap_stat6)

