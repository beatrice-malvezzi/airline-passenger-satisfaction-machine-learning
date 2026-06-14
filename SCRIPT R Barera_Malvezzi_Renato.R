# Clean the environment
rm(list=ls()); graphics.off(); cat("\014")

library(pROC)
library(ROCR)
library(caret)
library(Matrix)
library("tictoc")
library(e1071)
library("JOUSBoost")
library(caret)
library(adabag)
library(randomForest)
library(ggplot2)
library(ggcorrplot)
library(gridExtra)
library(skimr)
library(DataExplorer)
library("factoextra")
library(mice)
library(tidyverse)
library(corrplot)
library(class)
library(caret)
library(ISLR)
library(stats)
library(EFAtools)
library(clValid)
library(dbscan)

#funzioni
calc_class_err = function(actual, predicted) { mean(actual != predicted) }
accuracy = function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
# CARICO IL DATASET -----------------------------------
train<-read.csv("test.csv")
train<-train[,-c(1:2)]
#rimuoviamo le prime due variabili perchè non servono

# PULISCO E TRASFORMO IL DATASET -----------------------------------------------
# Le variabili qualitative le trasformo 
transform_gender <- function(x) {
  if (x == 'Female') {return(1)} 
  else if (x == 'Male') {return(0)} 
  else {return(-1)}
}
transform_customer_type <- function(x) {
  if (x == 'Loyal Customer') {return(1)} 
  else if (x == 'disloyal Customer') {return(0)} 
  else {return(-1)}
}
transform_travel_type <- function(x) {
  if (x == 'Business travel') {return(1)} 
  else if (x == 'Personal Travel') {return(0)} 
  else {return(-1)}
}
transform_class <- function(x) {
  if (x == 'Business') {return(2)} 
  else if (x == 'Eco Plus') {return(1)} 
  else if (x == 'Eco') {return(0)} 
  else {return(-1)}
}
transform_satisfaction <- function(x) {
  if (x == 'satisfied') {return(1)} 
  else if (x == 'neutral or dissatisfied') {return(0)} 
  else {return(-1)}
}

process_data <- function(df) {
  df <- df[, !(names(df) %in% c('Unnamed: 0', 'id'))]
  df$Gender <- sapply(df$Gender, transform_gender)
  df$Customer.Type <- sapply(df$Customer.Type, transform_customer_type)
  df$Type.of.Travel <- sapply(df$Type.of.Travel, transform_travel_type)
  df$Class <- sapply(df$Class, transform_class)
  df$satisfaction <- sapply(df$satisfaction, transform_satisfaction)
  return(df)
}

train <- process_data(train)


# MISSING VALUES ANALYSIS ------------------------------------------------------
set.seed(123)
skim(train)
plot_missing(train,title='NA in Train dataset Before Mice')
# Verifico la presenza di valori mancanti nella variabile "Arrival Delay in Minutes" 
sum(is.na(train$Arrival.Delay.in.Minutes))
# Si può pensare di imputare i missing values della variabile "Arrival Delay in Minutes"
# utilizzando il campionamento multiplo: er ogni variabile con dati mancanti, mice crea un
# modello di regressione che predice il valore mancante basandosi sulle altre variabili del dataset.
miceresult <- mice::mice(train,seed=123,m=5) # genera m imputazioni per i dati mancanti
miceresult
train <- mice::complete(miceresult,1) # viene selezionata la prima imputazione
plot_missing(train,title='NA in Train dataset After Mice') # ora non ci sono più missing 

#abbiamo notato che nelle variabili di valutazione ci sono dei valori nulli
#leggendo su kaggle abbiamo visto che si tratta di situazioni in cui
#il rispondente non risponde a quella domanda quindi di fatto un missing value
#come lo trattiamo?
#possiamo rimpiazzare gli zero con il 3 che rappresenta di fatto la risposta neutrale
mediane<-apply(train, 2, median)
for(i in 7:20){
  for(j in 1:nrow(train)){
    if(train[j,i]==0){
      train[j,i]<-mediane[i]
    }
 }
}

table(train$Inflight.wifi.service)/nrow(train)

set.seed(123)
training_inx <- sample(nrow(train), nrow(train)*0.75)
training <- train[training_inx,]
test<- train[-training_inx, ]
train<-training

X_train = train[,1:22]
Y_train = train[, 23]

X_test = test[, 1:22]
Y_test = test[, 23]

#GRAFICI -----------------------------------------------------------------------
#Boxplot
b1<-ggplot(train, aes(x=Age))+
  geom_boxplot( fill="lightblue")+
  coord_flip()+
  labs(title="Boxplot per la variabile Age")+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    colour = "cadetblue"
  ))


b2<-ggplot(train, aes(x=Flight.Distance))+
  geom_boxplot(fill="lightblue")+
  coord_flip()+
  labs(title="Boxplot per la variabile Flight.distance")+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    colour = "cadetblue"
  ))
b3<-ggplot(train, aes(x=Departure.Delay.in.Minutes))+
  geom_boxplot(fill="lightblue")+
  coord_flip()+
  labs(title="Boxplot per la variabile Departure.Delay.in.Minutes")+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    colour = "cadetblue"
  ))
b4<-ggplot(train, aes(x=Arrival.Delay.in.Minutes))+
  geom_boxplot(fill="lightblue")+
  coord_flip()+
  labs(title="Boxplot per la variabile Arrival.Delay.in.Minutes")+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    colour = "cadetblue"
  ))

b5<-ggplot(train, aes(x=Inflight.wifi.service))+
  geom_boxplot(fill="lightblue")+
  coord_flip()+
  labs(title="Boxplot per la variabile Inflight.wifi.service")+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    colour = "cadetblue"
  ))

b6<-ggplot(train, aes(x=Departure.Arrival.time.convenient))+
  geom_boxplot(fill="lightblue")+
  coord_flip()+
  labs(title="Boxplot per la variabile Departure.Arrival.time.convenient")+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    colour = "cadetblue"
  ))

b7<-ggplot(train, aes(x=Ease.of.Online.booking))+
  geom_boxplot(fill="lightblue")+
  coord_flip()+
  labs(title="Boxplot per la variabile Ease.of.Online.booking")+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    colour = "cadetblue"
  ))

b8<-ggplot(train, aes(x=Gate.location))+
  geom_boxplot(fill="lightblue")+
  coord_flip()+
  labs(title="Boxplot per la variabile Gate.location")+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    colour = "cadetblue"
  ))

b9<-ggplot(train, aes(x=Food.and.drink))+
  geom_boxplot(fill="lightblue")+
  coord_flip()+
  labs(title="Boxplot per la variabile Food.and.drink")+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    colour = "cadetblue"
  ))
grid.arrange(b1,b2,b3,b4,b5,b6,b7,b8,b9, nrow=3)

b10<-ggplot(train, aes(x=Online.boarding))+
  geom_boxplot(fill="lightblue")+
  coord_flip()+
  labs(title="Boxplot per la variabile Online.boarding")+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    colour = "cadetblue"
  ))

b11<-ggplot(train, aes(x=Seat.comfort))+
  geom_boxplot(fill="lightblue")+
  coord_flip()+
  labs(title="Boxplot per la variabile Seat.comfort")+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    colour = "cadetblue"
  ))

b12<-ggplot(train, aes(x=Inflight.entertainment))+
  geom_boxplot(fill="lightblue")+
  coord_flip()+
  labs(title="Boxplot per la variabile Inflight.entertainment")+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    colour = "cadetblue"
  ))

b13<-ggplot(train, aes(x=On.board.service))+
  geom_boxplot(fill="lightblue")+
  coord_flip()+
  labs(title="Boxplot per la variabile On.board.service")+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    colour = "cadetblue"
  ))

b14<-ggplot(train, aes(x=Leg.room.service))+
  geom_boxplot(fill="lightblue")+
  coord_flip()+
  labs(title="Boxplot per la variabile Leg.room.service")+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    colour = "cadetblue"
  ))

b15<-ggplot(train, aes(x=Baggage.handling))+
  geom_boxplot(fill="lightblue")+
  coord_flip()+
  labs(title="Boxplot per la variabile Baggage.handling")+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    colour = "cadetblue"
  ))

b16<-ggplot(train, aes(x=Checkin.service))+
  geom_boxplot(fill="lightblue")+
  coord_flip()+
  labs(title="Boxplot per la variabile Checkin.service")+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    colour = "cadetblue"
  ))

b17<-ggplot(train, aes(x=Inflight.service))+
  geom_boxplot(fill="lightblue")+
  coord_flip()+
  labs(title="Boxplot per la variabile Inflight.service")+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    colour = "cadetblue"
  ))

b18<-ggplot(train, aes(x=Cleanliness))+
  geom_boxplot(fill="lightblue")+
  coord_flip()+
  labs(title="Boxplot per la variabile Cleanliness")+
  theme(plot.title = element_text(
    size = 14,
    face = "bold",
    colour = "cadetblue"
  ))
grid.arrange(b10,b11,b12,b13,b14,b15,b16,b17,b18, nrow=3)
#OUTLIER----------------------
#Notiamo degli outlier soprattutto nelle variabili"Departure.Delay.in.Minutes"e "Arrival.Delay.in.Minutes" 
outd<-boxplot.stats(train$Departure.Delay.in.Minutes)$out
outa<-boxplot.stats(train$Arrival.Delay.in.Minutes)$out

outa50<-outa[order(outa, decreasing=T)[1:50]]
outd50<-outd[order(outd, decreasing=T)[1:50]]

outa_index<-c()
outd_index<-c()
for(i in 1:nrow(train)){
  for(j in 1:50){
    if(train[i,"Departure.Delay.in.Minutes"]==outd50[j]){
      outd_index<-c(outd_index, i)
    }
    if(train[i,"Arrival.Delay.in.Minutes"]==outa50[j]){
      outa_index<-c(outa_index, i)
    }
  }
}
uguali<-c()
for(i in 1:51){
  for(j in 1:50){
    if(unique(outd_index)[i]==unique(outa_index)[j]){
      uguali<-c(uguali,1)
    }
  }
}
sum(uguali) #45
#esaminando i primi 50 outlier delle due variabili vediamo che il 90% si riferiscono
#alla stessa riga del dataset e quindi allo stesso passeggero
#questo ha senso poichè se un volo parte con ritardo allora molto probabilmente
#arriverà a destinazione in ritardo
#vediamo che però nonostante questo la percentuale di soddisfatti e insoddisfatti
#non viene impattata negli outlier
table(train[unique(outa_index),"satisfaction"])/length(unique(outa_index))
table(train[unique(outd_index),"satisfaction"])/length(unique(outd_index))

# CORRELAZIONE ------------------------------------------
# Calcola la correlazione tra le colonne numeriche
X_num_trainz<-X_train[,c(3,6:22)]

correlation_matrix <- cor(X_num_trainz)

p<-ggcorrplot(correlation_matrix, type = 'lower' , lab=T, lab_size=2)
p+labs(title="Corrplot between numerical features")+
  theme(plot.title = element_text(
    size = 14,
   face = "bold",
    colour = "cadetblue"
 ))
correlation_matrix # Visualizzo la matrice di correlazione

#arrival delay in minutes è molto correlato con departure delay in minutes
#rimuoviamo una delle due?

# NORMALIZZAZIONE --------------------------------------------------------------
X_num_train<-X_train[,c(3,6,21,22)]
X_num_train <- as.data.frame(scale(X_num_train))
X_train1<-cbind(X_num_train, X_train[,-c(3,6,21,22)])

X_num_test<-X_test[,c(3,6,21,22)]
X_num_test <- as.data.frame(scale(X_num_test))
X_test1<-cbind(X_num_test, X_test[,-c(3,6,21,22)])

#CLASSIFICAZIONE 
#Studiamo la variabile risposta------------
#sbilanciamento variabile risposta
prop.table(table(Y_train))*100
#non sono perfettamente bilanciate pero il 56% degli utenti risulta neutrale o insoddisfatto
#questo può essere dovuto al fatto che in tale classe è come 
#se ci fossero due sottoclassi: quelli neutrali 
#e sia quelli insoddisfatti


#DBSCAN----------------
set.seed(123)
colnames(train)
train_dbs<-train[,c(3,6,7:21)]
normalized_train_dbs <- scale(train_dbs)
distances <- dist(normalized_train_dbs)
sorted_distances <- sort(distances, decreasing = TRUE)

par(mfrow=c(1,1))
kNNdistplot(normalized_train_dbs, k = 18)
abline(h=3.5, col = "cadetblue", lty=2)
set.seed(123)
tic()
dbscan.best<-dbscan(normalized_train_dbs, eps=3.5, minPts=18) 
toc()
print(dbscan.best)
out<-which(dbscan.best$cluster==0) #82
ug<-c()
for( i in 1:length(out)){
  for(j in 1:length(outd_index)){
    if(out[i]==outd_index[j]){
      ug<-c(ug,out[i])
    }
  }
}
length(unique(ug)) #all'interno dei noisy points ben 47 osservazioni sono outliers della variabile #departure

#pairs(normalized_train_dbs, col = dbscan.best$cluster + 1L)
#hullplot(x = normalized_train_dbs,cl=dbscan.best, alpha=0.1,pch=19, col=c("blue", "lightblue")) # grafico delle aree convesse
#eps_values <- c(seq(3,4,0.25))
#MinPts_values <- c(16,17,18,19,20)

#best_silhouette <- -1
#best_eps <- 0
#best_MinPts <- 0
#for (eps in eps_values) {
#  for (MinPts in MinPts_values) {
#    dbscan_result <- dbscan(normalized_train_dbs, eps = eps, MinPts = MinPts)
    
#    if (length(unique(dbscan_result$cluster)) > 1) {
#      silhouette <- silhouette(dbscan_result$cluster, distances)
#      
#      if (mean(silhouette[, "sil_width"]) > best_silhouette) {
#        best_silhouette <- mean(silhouette[, "sil_width"])
#        best_eps <- eps
#        best_MinPts <- MinPts
#      }
#    }
#  }
#}

#print(paste("Best eps:", best_eps))
#print(paste("Best MinPts:", best_MinPts))
#print(paste("Best Silhouette:"), best_silhouette)
dbscan.res <- dbscan(scale(train_dbs), eps=0.3, minPts=5) 
#print( dbscan.res )
#plot(train_dbs, pch=19)
#k <- max(dbscan.best$cluster)
#if( k<=3 ) {
#  clrs <- c("blue","green3","red")  
#} else {
#  clrs <- rainbow( k )
#}

#for( i in 1:k ) {
#  points(train_dbs[which(dbscan.best$cluster==i),,drop=F],col=clrs[i],pch=19)
#}

train<-train[-out,]
X_train1<-X_train1[-out,]
Y_train<-Y_train[-out]
X_num_train<-X_num_train[-out,]
X_num_trainz<-X_num_trainz[-out,]
X_train<-X_train[-out,]


# Test di sfericità di Bartlett
bartlett <- EFAtools::BARTLETT(X_train1[,c(9:22)])
print(bartlett)
# Test di adattabilità di Kaiser-Meyer-Olkin (KMO)
kmo <- psych::KMO(X_train1[,c(9:22)])
print(kmo)

#PCA solo quelle di valutazione
X_train1V<-X_train1[,c(9:22)]
X_trainpcaV<-princomp(X_train1V)
fviz_eig(X_trainpcaV, ncp=14)
V=X_trainpcaV$loadings
Y = X_trainpcaV$scores
matri<-cor(Y, X_train1V)
corrplot(matri)
colnames(Y)[1:7]=c("inflight experience","digital services", "service quality","online boarding","check-in service","leg-room service","timeliness and accessibility")
X_test1V<-X_test1[,c(9:22)]
X_testpcaV<-princomp(X_test1V)
fviz_eig(X_testpcaV, ncp=14)
V_t=X_testpcaV$loadings
Y_t = X_testpcaV$scores
matri_t<-cor(Y_t, X_test1V)
corrplot(matri_t)

#K-MEANS-------------
#scegliamo il k ottimale sulla base del rapporto beetween e total
#results <- apply(t(2:10), 2, function(k){kmeans(Y[,1:10], k, iter.max = 10000000)})
#ctrl<-numeric()
#for (res in results){
#  cat("k=", max(res$cluster), "\n")
#  cat("betweenSS/totSS=", res$betweenss/res$totss, "\n")
#  ctrl[max(res$cluster)-1] <- res$tot.withinss/res$totss
#}
#plot(x=2:10, y=ctrl, ylab="tot.withinss/totss", type="l", xlab="K")
#abline(h=ctrl[3], lty=2, col="cadetblue")
tic()
km4 <- kmeans(x=Y[,1:7], centers = 4)
toc()
km4$cluster
CLU1 <- which(km4$cluster == 1)
CLU2 <- which(km4$cluster == 2)
CLU3 <- which(km4$cluster == 3)
CLU4 <- which(km4$cluster == 4)
table(Y_train[CLU1]) #soddisfatti
table(Y_train[CLU2]) #insoddisfatti
table(Y_train[CLU3]) #insoddisfatti
table(Y_train[CLU4]) #entrambi

par(mfrow=c(2,2))
plot(Y[,1], Y[,2])
points(Y[CLU1,1], Y[CLU1,2], col="blue")
plot(Y[,1], Y[,2])
points(Y[CLU2,1], Y[CLU2,2], col="red")
plot(Y[,1], Y[,2])
points(Y[CLU3,1], Y[CLU3,2], col="green")
plot(Y[,1], Y[,2])
points(Y[CLU4,1], Y[CLU4,2], col="yellow")

par(mfrow=c(1,1))
#pairs(Y[,1:7], col=km4$cluster + 1L)


set.seed(123)
sils =numeric()
kappas <- 2:10
for (k in kappas){
  res = kmeans(Y[,1:7], centers = k)
  sil = silhouette(res$cluster, dist(Y[,1:7]))
  plot(sil, main = (paste("k=",k )))
  abline(v=summary(sil)$avg.width, col="red", lty=2, lwd=2)
  sils = c(sils, summary(sil)$avg.width)
}
par(mfrow=c(1,1))
plot(kappas, sils, type="o")

set.seed(2)
tic()
km2 <- kmeans(x=Y[,1:7], centers = 2)
toc()
CLU1 <- which(km2$cluster == 1)
CLU2 <- which(km2$cluster == 2)
table(Y_train[CLU1]) / length(CLU1)
table(Y_train[CLU2]) / length(CLU2)
mC<-apply(X_train1[CLU1,c(9:22)], 2, mean)
mD<-apply(X_train1[CLU2,c(9:22)], 2, mean)
plot(mC, mD, col="cadetblue", pch=19, ylim=c(2,5), xlim=c(2,5), xlab="media di valutazione del gruppo 1", ylab="media di valutazione del gruppo 2")
text(mC, mD, names(mC), cex=0.45, pos=c(1,1,4,4,1,1,1,2,3,2,2,1,1), col="cadetblue")
par(mfrow=c(2,1))
plot(Y[,1], Y[,2])
points(Y[CLU1,1], Y[CLU1,2], col="blue")
plot(Y[,1], Y[,2])
points(Y[CLU2,1], Y[CLU2,2], col="red")

#pairs(X_train1, col=km2$cluster + 1L)


#HIERARCHICAL CLUSTER ALGORITHM----------------

d1=dist(Y[,1:7], method="manhattan")
set.seed(123)
h.resN1=hclust(d=d1, method="ward.D2")
plot(h.resN1, main="manhattan")
abline(h=500, col="cadetblue", lty=3, lwd=3) #due gruppi
for(k in 2:5){
  cluster=cutree(h.resN1, k)
  plot(Y[,1],Y[,2], pch=19, main=paste0("manhattan for k=", k,))
  clrs=rainbow(k)
  points(Y[,1], Y[,2], col=clrs[cluster], pch=19)
}
kappas <- 2:5
sils<-numeric()
for (k in kappas){
  res = cutree(h.resN1, k=k)
  sil = silhouette(res, d1)
  sils = c(sils, summary(sil)$avg.width)
}
plot(kappas, sils, type="o", main="manhattan")
n1<-cutree(h.resN1, k=2)
rowa<-which(n1==1)
rowb<-which(n1==2)
table(train[rowa,23])/length(rowa)*100 #the majority: satisfied
table(train[rowb,23])/length(rowb)*100 #the majority: neutral/unsatisfied

hist(Y[rowb,1], col="blue", ylim=c(0,3000))
hist(Y[rowa,1], add=T, col="cadetblue")

d2=dist(Y[,1:7], method="euclidean")
set.seed(123)
tic()
h.resN2=hclust(d=d2, method="ward.D2")
toc()
plot(h.resN2, main="euclidean")
abline(h=300, col="cadetblue", lty=3, lwd=3)
kappas <- 2:5
sils<-numeric()
for (k in kappas){
  res = cutree(h.resN2, k=k)
  sil = silhouette(res, d2)
  sils = c(sils, summary(sil)$avg.width)
}

n2<-cutree(h.resN2, k=2)
row1<-which(n2==1)
row2<-which(n2==2)
table(train[row1,23])/length(row1)*100 #the majority: satisfied
table(train[row2,23])/length(row2)*100 #the majority: neutral/unsatisfied
m1<-apply(X_train1[row1,c(9:22)], 2, mean)
m2<-apply(X_train1[row2,c(9:22)], 2, mean)
plot(Y[1,], Y[2,])
plot(m1, m2, col="cadetblue", pch=19, ylim=c(2,5), xlim=c(2,5), xlab="rating average group 1", ylab="rating average group 2")
text(m1, m2, names(m1), cex=0.45, pos=c(1,1,4,4,1,1,1,2,3,2,2,1,1), col="cadetblue")
names(m1)

#RANDOM FOREST---------
#multivariata
dati<-cbind(X_train1, Y_train)
colnames(dati)
datit<-X_test1
for(i in 5:8){dati[,i]<-as.factor(dati[,i])}
for(i in 5:8){datit[,i]<-as.factor(datit[,i])}
colnames(datit)
dati$Y_train<-as.factor(dati$Y_train)
(rf_fit <- randomForest(x= dati[,-c(4,23)], y = dati[,23], 
                        xtest= datit[,-4], ytest = as.factor(Y_test) ))
plot(rf_fit, main="relation between errors and number of trees")
abline(v=80,lwd=2, lty=2, col="cadetblue")
#scegliamo un numero di alberi pari a 80
tic()
bag.tree <- randomForest(Y_train~., data=dati[,-4], ntree=80, importance=T)
toc()
bag.tree
par(mfrow=c(1,1))
plot(bag.tree)
varImpPlot(bag.tree, main="Random forest with 80 trees")
table(predict(bag.tree))
confusionMatrix(predict(bag.tree), dati$Y_train)
confusionMatrix(predict(bag.tree, newdata = datit), as.factor(Y_test))
predROC2 <- ROCR::prediction(as.numeric(predict(bag.tree, newdata = datit)), Y_test)
perfROC2 <- performance(predROC2,"tpr","fpr")
plot(perfROC2, colorize=T, main="ROC curve for Random Forest")
roc_object <- roc( Y_test, as.numeric(predict(bag.tree, newdata = datit)))
AUC2<-auc(roc_object)
#KNN-------------------
# random seed necessario perché R assegnerà a caso valori con uguale probabilità
set.seed(123)

# cerco valore ottimo di K calcolando per più valori di K il tasso d’errore sul 
# test set e scelgo il valore di K che minimizza l'errore
k_to_try = 1:100
err_k = rep(x = 0, times = length(k_to_try))
for (i in seq_along(k_to_try)) {
  pred = knn(train = X_train1, test = X_test1, cl = as.factor(Y_train), k = k_to_try[i])
  err_k[i] = calc_class_err(as.factor(Y_test), pred) }

## test error Vs k
par(mfrow=c(1,1))
plot(err_k, type = "b", col = "black", cex = 1, pch = 20,
     xlab = "k, number of neighbors", ylab = "Test error", 
     main = "(Test) Error Rate vs Neighbors")

# Minimum error
abline(h = min(err_k), col = "dodgerblue", lty = 3)
abline(h=err_k[order(err_k, decreasing = F)][3], col="blue", lty=3)
min(err_k) #Valore di k che dà l'errore minimo K=10
max(which(err_k == min(err_k)))

#noi però vogliamo un numero dispari
err_k[order(err_k, decreasing = F)][3] 
which(err_k == err_k[order(err_k, decreasing = F)][3]) # Valore di k che dà il secondo errore piu piccolo
# Scelta del k più alto

# KNN con K=9 
tic()
pred9 = knn(train = X_train1, test = X_test1, cl = as.factor(Y_train), k = 9, prob = TRUE)
toc()

confusionMatrix(data =pred9, reference = as.factor(Y_test), positive = "1") 

predROC1 <- ROCR::prediction(as.numeric(pred9), Y_test)
perfROC1 <- performance(predROC1,"tpr","fpr")
plot(perfROC1,colorize=TRUE, main="ROC curve for KNN-9")
roc_object1 <- roc( Y_test, as.numeric(pred9))
AUC1<-auc(roc_object1)

# test error rate
test_error_rate = calc_class_err(actual = as.factor(Y_test), predicted = pred)
test_error_rate
test_error_rate*100

# confusion matrix o matrice di confusione
conf_matrix = table(pred, as.factor(Y_test))
conf_matrix
# Accuracy
accuracy(conf_matrix)
100-accuracy(conf_matrix)


#ADABOOST--------------
set.seed(123)
#model_adaboost <- adabag::boosting(Y_train~., data=dati[,-4], boos=TRUE, mfinal=50)
#summary(model_adaboost)
set.seed(123)
datiab<-cbind(X_train1, Y_train)
datiab$Y_train<-ifelse(Y_train==1,1,-1)
td<-c(seq(2,13, by=1))
acc<-numeric()
set.seed(123)
for(t in td){
  mod<-adaboost(as.matrix(datiab[,c(1:3,9:22)]), datiab$Y_train, tree_depth = td[t], n_rounds = 100, verbose = TRUE, control = NULL)
  acc[t]<-accuracy(mod$confusion_matrix)
}
par(mfrow=c(1,2))
plot(td, acc, type="b", main="Accuracy according to tree_depth")
abline(v=7, lty=2, lwd=2, col="cadetblue")
acc1<-numeric()
nc<-seq(10,100, by=10)
for(t in 1:length(nc)){
  mod<-adaboost(as.matrix(datiab[,c(1:3,9:22)]), datiab$Y_train, tree_depth = 7, n_rounds = nc[t], verbose = TRUE, control = NULL)
  acc1[t]<-accuracy(mod$confusion_matrix)
}
plot(nc,acc1, main="Accuracy according to n_rounds")
order(acc1, decreasing = T)
abline(v=90, col="cadetblue", lty=2, lwd=2)
set.seed(123)
tic()
modab_best<-adaboost(as.matrix(datiab[,c(1:3,9:22)]), datiab$Y_train, tree_depth = 7, n_rounds =90, verbose = TRUE, control = NULL)
toc()

accuracy(modab_best$confusion_matrix)
modab_best$confusion_matrix

datiabt<-cbind(X_test1, Y_test)
datiabt$Y_test<-ifelse(Y_test==1,1,-1)
pred_test = predict(modab_best, X_test1)
confusionMatrix(data = as.factor(pred_test), reference = as.factor(datiabt$Y_test), positive = "1") 
accuracy(table(as.factor(pred_test), as.factor(datiabt$Y_test)))
acc_test<-numeric()

set.seed(123)

td
for(t in td){
  mod_test<-adaboost(as.matrix(datiab[,c(1:3,9:22)]), datiab$Y_train, tree_depth = td[t], n_rounds = 50, verbose = TRUE, control = NULL)
  pred_testz<-predict(mod_test, X_test1)
  acc_test[t]<-accuracy(table(as.factor(pred_testz), as.factor(datiabt$Y_test)))
}

par(mfrow=c(1,2))
plot(td,acc_test, type="b", main="Test accuracy according to tree_depth")
abline(v=11, lty=2, lwd=2, col="cadetblue")
which.max(acc_tes)
acc_test1<-numeric()
nc<-seq(10,100, by=10)
for(t in 1:length(nc)){
  mod_testz<-adaboost(as.matrix(datiab[,c(1:3,9:22)]), datiab$Y_train, tree_depth = 11, n_rounds = nc[t], verbose = TRUE, control = NULL)
  pred_testz<-predict(mod_testz, X_test1)
  acc_test1[t]<-accuracy(table(as.factor(pred_testz), as.factor(datiabt$Y_test)))
}

plot(nc,acc_test1, type="b", main="Test accuracy according to n_rounds")
abline(v=90, lty=2, lwd=2, col="cadetblue")

tic()
modab_best<-adaboost(as.matrix(datiab[,c(1:3,9:22)]), datiab$Y_train, tree_depth =11, n_rounds =90 , verbose = TRUE, control = NULL)
toc()
modab_best$confusion_matrix
confusionMatrix(data = as.factor(datiab$Y_train), reference = as.factor(datiab$Y_train), positive = "1") 
pred_test = predict(modab_best, X_test1)
confusionMatrix(data = as.factor(pred_test), reference = as.factor(datiabt$Y_test), positive = "1") 
confusionMatrix(data = as.factor(pred_test), reference = as.factor(datiabt$Y_test), positive = "1") 
accuracy(table(as.factor(pred_test), as.factor(datiabt$Y_test)))
predROC3 <- ROCR::prediction(as.numeric(pred_test), Y_test)
perfROC3 <- performance(predROC3,"tpr","fpr")
plot(perfROC3,colorize=TRUE, main="ROC curve for Adaboost")

roc_object3 <- roc( Y_test, as.numeric(pred_test))
AUC3<-auc(roc_object3)

#SVM ------------------
datasv<-X_train1[,-4]
Ysv<-as.factor(Y_train)
datasv<-cbind(datasv, Ysv)
set.seed(123)
datasvt<-X_test1[,-4]
Ysvt<-as.factor(Y_test)
datasvt<-cbind(datasvt, Ysvt)
#cost<-seq(10,100, by=10)
acci<-numeric()
#for(i in length(cost)){
#  svmi <- svm(Ysv ~ ., data = datasv, kernel = "linear", cost = cost[i])
#  predi<-predict(svmi, newdata = datasvt)
#  acci[i]<-accuracy(table(predi,Ysvt))
#}
plot(cost, acci)
tic()
svm.best <- svm(Ysv ~ ., data = datasv, kernel = "linear", cost = 0.5)
toc()
svm.train<-predict(svm.best)
confusionMatrix(data = svm.train, reference = Ysv, positive = "1")

svm.pred <- predict(svm.best, newdata = datasvt)
confusionMatrix(data = svm.pred, reference = Ysvt, positive = "1")
summary(svm.best)
predROC4 <- ROCR::prediction(as.numeric(svm.pred), Y_test)
perfROC4 <- performance(predROC4,"tpr","fpr")
plot(perfROC4,colorize=TRUE, main="ROC curve for SVM")
roc_object4 <- roc(Y_test, as.numeric(svm.pred))
AUC4<-auc(roc_object4)

par(mfrow=c(1,4))
plot(perfROC1,colorize=TRUE, main=paste0("KNN-9 AUC=", round(AUC1,3)))
plot(perfROC2, colorize=T, main=paste0("RANDOM FOREST AUC=", round(AUC2,3)))
plot(perfROC3,colorize=TRUE, main=paste0("ADABOOST AUC=", round(AUC3,3)))
plot(perfROC4,colorize=TRUE, main=paste0("SVM AUC=", round(AUC4,3)))

