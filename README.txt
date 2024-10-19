README

Nella cartella "bin" sono presenti quattro scripts in linguaggio R e cinque files con estensione csv.

DatasetsRealiCliniciDbscan:

Qui calcolo l'algoritmo DBSCAN sui datasets reali di cartelle cliniche elettroniche. 
Per ogni set di dati, bisogna prima importare i files con estensione csv, che corrispondono a tali cartelle cliniche. Successivamente, su ognuno, calcolo la matrice di distanza del kNN, tramite la funzione "kNNdist". Applico la funzione "sort" per riordinare i dati e in seguito creo un dataframe che passo poi alla funzione ggplot, per poter avere il grafico e tracciare su di esso il punto di gomito. Una volta trovato tale punto (che corrisponde all'epsilon) mi calcolo l'algoritmo DBSCAN tramite la funzione "dbscan" e lo applico due volte, ottenendo come uno dei risultati i vari cluster presenti: per visualizzarli uso "$cluster". Poi applico il "cbind" per unire tra di loro i clusters e calcolo la Gap Statistic con la funzione "index.Gap".

DatasetsRealiCliniciK-meansCg:

In questo file calcolo l'algoritmo K-Means e cluster gerarchico su i datasets reali. Per prima cosa ho importato tutti i set di dati e creato un ciclo for per posizionarli in un array. All'interno di tale ciclo ho creato un altro ciclo for, dentro al quale ho posizionato un ciclo while per poter calcolarmi il K-Means su ciascun datasets, cambiando il numero di centroidi di volta in volta: da 2 a 3 e infine a 4.
Qui ho utilizzato due funzioni diverse di K-Means: una fa parte del pacchetto "stats" ed è kmeans(), che calcola l'algoritmo utilizzando di default la distanza Euclidea. L'altra è KMEANS_FUNCTION() del pacchetto "KMEANS.KNN" che calcola tale algoritmo usando la distanza Manhattan. Una volta finito il ciclo while, si passa ad un nuovo ciclo for in cui calcolo su ciascun datasets il cluster gerarchico utilizzando i suoi quattro metodi e le due metriche di distanza. Questo è possibile perchè ho creato due array contenti metodi e distanze. Ogni volta che calcolo un algoritmo, poi utilizzo la funzione "index.Gap" per trovare il valore della Gap Statistic; nel K-Means ho usato anche la funzione "clusGap"

Matrici0e1:
Questo file è stato creato per testare la metrica in varie situazione: l'idea alla base è quella di creare una matrice e calcolare la Gap Statistic ogni volta che si cambia una riga della matrice stessa. 
Per prima cosa ho creato una matrice contenente metà righe di 0 e metà di 1: questo è un codice parametrico, vuol dire che la dimensione della matrice la si può scegliere all'inizio e cambiarla a piacere. Dopo aver creato tale matrice, ho calcolato la Gap Statistic su di essa utilizzando sia "index.Gap" che "clusGap". Su quest'ultima ho attuato la media dei valori finali per poterne trovare solo uno. Successivamente ho creato un array vuoto, di dimensione pari al numero delle righe della matrice e l'ho riempito con valori casuali tra 1 e il numero delle righe: in questo modo capisco quali righe cambiare della matrice e con che ordine. Una volta che l'array è pieno, tramite il ciclo for, faccio in modo di cambiare la riga corrispondente al primo valore estratto nell'array, modificandola con valori compresi tra 0 e 1 e calcolo la Gap Statistic su questa matrice modificata. Il suo valore lo salvo in un altro array. Proseguo in questo modo fino a quando non ottengo una matrice completamente cambiata. Successivamente disegno il grafico dei valori della metrica e la linea di regressione per capire il loro andamento. I due grafici differiscono tra loro per il primo punto.

SanityCheck:

Qui attuo delle verifiche per capire il funzionamento della metrica in esame. Prima mi creo una matrice 6x6 con tre righe di 0 e tre di 1 e mi calcolo la Gap Statistic. Successivamente riporto la matrice in un grafico per vedere la distribuzione dei punti. Nel secondo caso, invece, mi creo sempre una matrice 6x6 cambiando di volta in volta le righe con valori casuali tra 8 e 9. Mi calcolo la Gap Statistic sulla matrice finale e riporto ogni valore in un grafico per vedere la loro distribuzione.

Nella cartella "results" sono presenti quattro files in PDF con i risultati a cui sono giunta.
