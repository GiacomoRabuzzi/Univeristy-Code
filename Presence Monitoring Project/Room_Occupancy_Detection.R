### ROOM OCCUPANCY DETECTION ###

rm(list=ls()); graphics.off(); cat("\014")

library(farff)
library(caret)
library(e1071)
library(readr)
library(lubridate)
library(tidyverse)
library(reshape2)
library(corrplot)
library(GGally)
library(gridExtra)
library(kernlab)
library(cluster)
library(EMCluster)
library(dbscan)
library(factoextra)
library(mlr)
library(mlrMBO)
library(randomForest)
library(lhs)
library(waffle)
library(grid)
library(mclust)
library(class)
library(nnet)
library(deepnet)
library(rgenoud)
library(VGAM)
library(tictoc) #per valutazione efficienza

set.seed(123)

# CARICAMENTO DEL DATASET
df <- read.csv('Occupancy_Estimation.csv',header = T)

df$datetime <- paste(df$Date, df$Time)
df$datetime <- ymd_hms(df$datetime)

df%>%
  dplyr::select(-Date,-Time) -> df

df$hour_of_day <- hour(df$datetime)
df$day <- day(df$datetime)

df%>%
  dplyr::select(datetime,day,hour_of_day,everything()) -> df

str(df)


# ANALISI DELLE VARIABILI
summary(df)

# unbalanced classes
table(df$Room_Occupancy_Count)

par(mfrow=c(1,3))
for (i in c(4,8,12)){
  boxplot(df[,i]~df[,20],xlab = colnames(df)[i],ylab = NULL)
}

for (i in c(5,9,13)){
  boxplot(df[,i]~df[,20],xlab = colnames(df)[i],ylab = NULL)
}

for (i in c(6,10,14)){
  boxplot(df[,i]~df[,20],xlab = colnames(df)[i],ylab = NULL)
}

for (i in c(7,11,15)){
  boxplot(df[,i]~df[,20],xlab = colnames(df)[i],ylab = NULL)
}

par(mfrow=c(1,2))
for (i in c(16,17)){
  boxplot(df[,i]~df[,20],xlab = colnames(df)[i],ylab = NULL)
}

par(mfrow=c(1,2))

table(df[,18],df[,20])
tab1 <- table(df[,18],df[,20])
sums1 <- colSums(table(df[,18],df[,20]))
tab1 <- sweep(tab1, 2, sums1, "/")
melted_t1 <- melt(tab1)
colnames(melted_t1)[1:2] <- c('PIR','Room_Occupancy_Count')
melted_t1$PIR <- as.factor(melted_t1$PIR)

ggplot(melted_t1, aes(x = PIR, y = Room_Occupancy_Count, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red3") +
  labs(x = "PIR", y = "Presenze", fill = "Frequenza") +
  theme_minimal()


table(df[,19],df[,20])
tab2 <- table(df[,19],df[,20])
sums2 <- colSums(table(df[,19],df[,20]))
tab2 <- sweep(tab2, 2, sums2, "/")
melted_t2 <- melt(tab2)
colnames(melted_t2)[1:2] <- c('PIR','Room_Occupancy_Count')
melted_t2$PIR <- as.factor(melted_t2$PIR)

ggplot(melted_t2, aes(x = PIR, y = Room_Occupancy_Count, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red3") +
  labs(x = "PIR", y = "Presenze", fill = "Frequenza") +
  theme_minimal()



#ANALISI CORRELAZIONE
cor(df[,-c(1,2,3,20)])
par(mfrow=c(1,1))

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(df[,-c(1,2,3,20)]), method="color", col=col(200),  
         type="upper", 
         addCoef.col = "black",
         tl.col="black", tl.srt=45,
         diag=FALSE, cl.cex = 0.8
)

# per tipo di variabile
#ggpairs(df[,4:7])
#ggpairs(df[,8:11])
#ggpairs(df[,12:15])
#ggpairs(df[,16:17])

# per nodo
#ggpairs(df[,c(4,8,12)])
#ggpairs(df[,c(5,9,13)])
#ggpairs(df[,c(6,10,14)])


# ANALISI VALORI MANCANTI
sum(is.na(df))


# ANALISI SERIE STORICA
df23 <- df[(df$datetime >= '2017-12-23 11:00:00 UTC' & df$datetime <= '2017-12-23 22:00:00 UTC'),]

g1 <- ggplot(data=df23)+
  geom_line(aes(x = datetime,y = S1_Temp))+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )+
  ggtitle("S1_Temp")

g2 <- ggplot(data=df23)+
  geom_line(aes(x = datetime,y = S1_Light))+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )+
  ggtitle("S1_Light")

g3 <- ggplot(data=df23)+
  geom_line(aes(x = datetime,y = S1_Sound))+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )+
  ggtitle("S1_Sound")

g4 <- ggplot(data=df23)+
  geom_line(aes(x = datetime,y = S5_CO2))+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )+
  ggtitle("S5_CO2")

g5 <- ggplot(data=df23)+
  geom_line(aes(x = datetime,y = S5_CO2_Slope))+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )+
  ggtitle("S5_CO2_Slope")

g6 <- ggplot(data=df23)+
  geom_line(aes(x = datetime,y = S6_PIR))+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )+
  ggtitle("S6_PIR")

g7 <- ggplot(data=df23)+
  geom_line(aes(x = datetime,y = Room_Occupancy_Count))+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )+
  ggtitle("Room_Occupancy_Count")

grid.arrange(g1,g2,g3,g4,g5,g6,g7,ncol=1)


# OUTLIERS DETECTION
df_id <- df
df_id$id <- 1:dim(df_id)[1]

df0 <- df_id[,-c(1,2,3,18,19)]%>%
  filter(Room_Occupancy_Count==0)%>%
  dplyr::select(-Room_Occupancy_Count)

df1 <- df_id[,-c(1,2,3,18,19)]%>%
  filter(Room_Occupancy_Count==1)%>%
  dplyr::select(-Room_Occupancy_Count)

df2 <- df_id[,-c(1,2,3,18,19)]%>%
  filter(Room_Occupancy_Count==2)%>%
  dplyr::select(-Room_Occupancy_Count)

df3 <- df_id[,-c(1,2,3,18,19)]%>%
  filter(Room_Occupancy_Count==3)%>%
  dplyr::select(-Room_Occupancy_Count)

m0 <- colMeans(df0[,-15])
m1 <- colMeans(df1[,-15])
m2 <- colMeans(df2[,-15])
m3 <- colMeans(df3[,-15])

par(mfrow=c(1,4))
boxplot(sqrt(mahalanobis(df0[,-15],m0,cov(df0[,-15]))))
boxplot(sqrt(mahalanobis(df1[,-15],m1,cov(df1[,-15]))))
boxplot(sqrt(mahalanobis(df2[,-15],m2,cov(df2[,-15]))))
boxplot(sqrt(mahalanobis(df3[,-15],m3,cov(df3[,-15]))))
par(mfrow=c(1,1))

# possibili outliers
which.out <- c(df0[sqrt(mahalanobis(df0[,-15],m0,cov(df0[,-15])))>60,15],
df1[sqrt(mahalanobis(df1[,-15],m1,cov(df1[,-15])))>20,15],
df2[sqrt(mahalanobis(df2[,-15],m2,cov(df2[,-15])))>15,15],
df3[sqrt(mahalanobis(df3[,-15],m3,cov(df3[,-15])))>15,15])

which.out <- which.out[order(which.out)]

df[which.out,]
# 347 dopo passaggio da 3 a 0 persone in stanza, valori ancora alti di co2 ma brusca diminuzione luce
# 607 prima di passaggio da 1 a 2 persone, aumento luce in sensore 3 improvviso
# 957 e 959 passaggio da 3 a 0 e riduzione immediata della luce
# 3252 prima di ingresso in stanza luce e rilevazione movimento (apertura porta?)
# 3626 non da rimuovere, nulla di particolare
# 3764 dopo passaggio da 2 a 0, ancora suono molto alto e 0 luce (rumore fuori porta?)
# 8340 prima passaggio da 3 a 2 solo luce bassa
# rimozione poichè relative a passaggi
df <- df[-which.out[-6],]


# DIMENSIONALITY REDUCTION
pca0 <- princomp(x = df[,4:19],cor = T,scores = T)
summary(pca0)
fviz_eig(pca0, addlabels = TRUE)

fviz_pca_var(pca0, col.var = "black")
plot(cumsum((pca0$sdev)^2/sum((pca0$sdev)^2)),type='o',lwd=3,col='grey60',ylim=c(0,1))
abline(h=0.9,lty=2,lwd=2,col='red')
# 8 componenti da considerare

df.red <- as.data.frame(cbind(pca0$scores[,1:8],Room_Occupancy_Count=df$Room_Occupancy_Count))


purity <- function(classes,clusters){
  mt <- melt(table(clusters,classes))
  pj <- tapply(X = mt[,3],INDEX = mt[,1],FUN = max)
  pi <- tapply(X = mt[,3],INDEX = mt[,2],FUN = max)
  (1/length(classes))*(sum(pj)+sum(pi))/2
}

# ITERATIVE DISTANCE BASED CLUSTERING

# K-medie
km.res <- kmeans(x = df.red,centers = 4,nstart = 10)
km.res
(tab <- table(km.res$cluster,df$Room_Occupancy_Count))
purity(df$Room_Occupancy_Count,km.res$cluster)
sil <- silhouette(km.res$cluster, dist(df.red[,-9]))
mean(sil[,'sil_width'])

get_legend <- function(myggplot) {
  tmp <- ggplotGrob(myggplot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

create_waffle <- function(data, title) {
  waffle(
    parts = setNames(object = data$value, data$category),
    rows = 25,
    size = 1,
    legend_pos = "none"
  ) + ggtitle(title) + theme(plot.title = element_text(size = 10))
}

df.w <- melt(tab)[1:4, 3] / 10
waffle_data <- data.frame(
  category = c("I", "II", "III", "IV"),
  value = df.w
)
w1 <- create_waffle(waffle_data, '0 presenze')

df.w <- melt(tab)[5:8, 3]
waffle_data <- data.frame(
  category = c("I", "II", "III", "IV"),
  value = df.w
)
w2 <- create_waffle(waffle_data, '1 presenza')

df.w <- melt(tab)[9:12, 3]
waffle_data <- data.frame(
  category = c("I", "II", "III", "IV"),
  value = df.w
)
w3 <- create_waffle(waffle_data, '2 presenze')

df.w <- melt(tab)[13:16, 3]
waffle_data <- data.frame(
  category = c("I", "II", "III", "IV"),
  value = df.w
)
w4 <- create_waffle(waffle_data, '3 presenze')

legend_data <- data.frame(
  category = c("I", "II", "III", "IV"),
  value = c(1, 1, 1, 1)
)
legend_waffle <- waffle(
  parts = setNames(object = legend_data$value, legend_data$category),
  rows = 1,
  size = 1,
  legend_pos = "bottom"
)
common_legend <- get_legend(legend_waffle)

combined <- arrangeGrob(
  grobs = list(w1, w2, w3, w4),
  ncol = 2
)

title_grob <- textGrob(
  "K-Medie",
  gp = gpar(fontsize = 20)
)

grid.arrange(title_grob, combined, common_legend, ncol = 1, heights = c(0.1, 0.8, 0.1))


# K-medoidi
pam.res<- pam(x = df.red,k = 4,metric = 'manhattan',stand = F) # anche euclidean non cambia
table(pam.res$cluster,df$Room_Occupancy_Count)
purity(df$Room_Occupancy_Count,pam.res$clustering)
sil <- silhouette(pam.res$cluster, dist(df.red[,-9]))
mean(sil[,'sil_width'])
# forme più irregolari meno sensibili a outlier
# funziona infatti meno bene


# Kernel-clusetring
sub.df.red <- df.red[sample(1:dim(df.red)[1],size = round(dim(df.red)[1]/5,0),replace = F),]

obj.fun <- makeSingleObjectiveFunction(
  name = "Clustering-kkmeans",
  fn = function(x) {
    sig <- list(x)
    ds <- as.matrix(sub.df.red)
    
    cluster <- kkmeans(ds[,-9],centers=4,kernel='rbfdot',kpar=sig)
    
    purity <- purity(cluster@.Data, ds[, 9])
    return(purity)
    #sil <- silhouette(cluster@.Data, dist(ds[,-9]))
    #return(mean(sil[,'sil_width']))
    
  },
  minimize = FALSE,
  par.set = makeParamSet( 
    makeNumericParam("Sigma", lower = 0.1, upper = 100)
  )
)

des <- generateDesign(n = 5, getParamSet(obj.fun), fun = lhs::randomLHS)
des$y <- apply(des, 1, obj.fun)

psm <- makeLearner("regr.randomForest", predict.type = "se", ntree = 500,
                   mtry = round(length(getParamSet(obj.fun)) / 3, 0))

control <- makeMBOControl()
control <- setMBOControlTermination(control, iters = 20)
control <- setMBOControlInfill(control, crit = makeMBOInfillCritEI()) # EI

run <- mbo(obj.fun, design = des, learner = psm, control = control, show.info = TRUE)

best.seen <- getOptPathY(run$opt.path)

run$x
run$y

plot(cummax(best.seen), type = "o", lwd = 3, col = "blue",
     ylim = c(min(best.seen), max(best.seen)), ylab = "Best Seen", xlab = "Trials")
lines(best.seen, type = "o", lty = 2, col = "green", lwd = 3)
legend("bottomright", legend = c("Best Seen", "Purity"), col = c("blue", "green"), lty = 1:2, lwd = 3, pch = 1)

kern.res <- kkmeans(as.matrix(df.red[,-9]),centers=4,kernel='rbfdot',kpar=list(run$x$Sigma))
kern.res
table(kern.res@.Data,df.red$Room_Occupancy_Count)
purity(kern.res@.Data,df$Room_Occupancy_Count)
sil <- silhouette(kern.res@.Data, dist(df.red[,-9]))
mean(sil[,'sil_width'])
# coefficiente sigma settato alto implica campana gaussiana stretta
# inoltre i cluster ottenuti sono pessimi, meglio kernel lineare
# non c'è struttura complessa


# CLUSTERING GERARCHICO
di.1 <- dist(df.red[,-9],method = 'euclidean') # clusetrs sferici
h.res <- hclust(di.1,method = 'ward.D2')  # clusters sferici e compatti, poichè minimizza varianza within
plot(h.res,labels=F)
rect.hclust(h.res,4) # due cluster simili (quelli con 2 o 3 presenze)
cls.h <- cutree(h.res,k = 4)
(tab <- table(cls.h,df$Room_Occupancy_Count))
purity(cls.h,df$Room_Occupancy_Count)
sil <- silhouette(cls.h, dist(df.red[,-9]))
mean(sil[,'sil_width'])

df.w <- melt(tab)[1:4, 3] / 10
waffle_data <- data.frame(
  category = c("I", "II", "III", "IV"),
  value = df.w
)
w1 <- create_waffle(waffle_data, '0 presenze')

df.w <- melt(tab)[5:8, 3]
waffle_data <- data.frame(
  category = c("I", "II", "III", "IV"),
  value = df.w
)
w2 <- create_waffle(waffle_data, '1 presenza')

df.w <- melt(tab)[9:12, 3]
waffle_data <- data.frame(
  category = c("I", "II", "III", "IV"),
  value = df.w
)
w3 <- create_waffle(waffle_data, '2 presenze')

df.w <- melt(tab)[13:16, 3]
waffle_data <- data.frame(
  category = c("I", "II", "III", "IV"),
  value = df.w
)
w4 <- create_waffle(waffle_data, '3 presenze')

legend_data <- data.frame(
  category = c("I", "II", "III", "IV"),
  value = c(1, 1, 1, 1)
)
legend_waffle <- waffle(
  parts = setNames(object = legend_data$value, legend_data$category),
  rows = 1,
  size = 1,
  legend_pos = "bottom"
)
common_legend <- get_legend(legend_waffle)

combined <- arrangeGrob(
  grobs = list(w1, w2, w3, w4),
  ncol = 2
)

title_grob <- textGrob(
  "Clustering gerarchico",
  gp = gpar(fontsize = 20)
)

grid.arrange(title_grob, combined, common_legend, ncol = 1, heights = c(0.1, 0.8, 0.1))


# MODEL BASED CLUSTERING
init.mb <- init.EM(df.red[,-9],nclass = 4,lab = df.red[,9])
em.res <- emcluster(df.red[,-9],emobj = init.mb, assign.class = T) #VVV
em.res
(tab <- table(em.res$class,df$Room_Occupancy_Count))
purity(em.res$class,df$Room_Occupancy_Count)
sil <- silhouette(em.res$class, dist(df.red[,-9]))
mean(sil[,'sil_width'])

df.w <- melt(tab)[1:4, 3] / 10
waffle_data <- data.frame(
  category = c("I", "II", "III", "IV"),
  value = df.w
)
w1 <- create_waffle(waffle_data, '0 presenze')

df.w <- melt(tab)[5:8, 3]
waffle_data <- data.frame(
  category = c("I", "II", "III", "IV"),
  value = df.w
)
w2 <- create_waffle(waffle_data, '1 presenza')

df.w <- melt(tab)[9:12, 3]
waffle_data <- data.frame(
  category = c("I", "II", "III", "IV"),
  value = df.w
)
w3 <- create_waffle(waffle_data, '2 presenze')

df.w <- melt(tab)[13:16, 3]
waffle_data <- data.frame(
  category = c("I", "II", "III", "IV"),
  value = df.w
)
w4 <- create_waffle(waffle_data, '3 presenze')

legend_data <- data.frame(
  category = c("I", "II", "III", "IV"),
  value = c(1, 1, 1, 1)
)
legend_waffle <- waffle(
  parts = setNames(object = legend_data$value, legend_data$category),
  rows = 1,
  size = 1,
  legend_pos = "bottom"
)
common_legend <- get_legend(legend_waffle)

combined <- arrangeGrob(
  grobs = list(w1, w2, w3, w4),
  ncol = 2
)

title_grob <- textGrob(
  "Model based clustering (struttura VVV)",
  gp = gpar(fontsize = 20)
)

grid.arrange(title_grob, combined, common_legend, ncol = 1, heights = c(0.1, 0.8, 0.1))


# DIVISIONE DATASET
set.seed(321)

ixs <- createDataPartition(y = df$Room_Occupancy_Count,times = 1,p = 0.8)

dataset <- df[ixs$Resample1,]
testset <- df[-ixs$Resample1,]

table(dataset$Room_Occupancy_Count)/dim(dataset)[1]

table(testset$Room_Occupancy_Count)/dim(testset)[1]


# DIMENSIONALITY REDUCTION
pca <- princomp(x = dataset[,4:19],cor = T,scores = T)
summary(pca)
fviz_eig(pca, addlabels = TRUE)
fviz_pca_var(pca, col.var = "black")
plot(cumsum((pca$sdev)^2/sum((pca$sdev)^2)),type='o',lwd=3,col='grey60',ylim=c(0,1))
abline(h=0.9,lty=2,lwd=2,col='red')
# si considerano 8 componenti, si è dimezzato il numero da 16

dt.red <- as.data.frame(cbind(pca$scores[,1:8],Room_Occupancy_Count=dataset$Room_Occupancy_Count))
tt.red <- as.data.frame(cbind(predict(pca,testset[,-c(1,2,3,20)])[,1:8],Room_Occupancy_Count=testset$Room_Occupancy_Count))
dt.red$Room_Occupancy_Count <- as.factor(dt.red$Room_Occupancy_Count)
tt.red$Room_Occupancy_Count <- as.factor(tt.red$Room_Occupancy_Count)


# KNN
make_f1_measures <- function(class) {
  makeMeasure(
    id = paste0("f1_class_", class),
    minimize = FALSE,
    properties = c("classif", "classif.multi"),
    fun = function(task, model, pred, feats, extra.args) {
      cm <- confusionMatrix(data = factor(pred$data$response, levels = levels(pred$data$truth)),
                            reference = factor(pred$data$truth, levels = levels(pred$data$truth)))
      precision <- cm$byClass[class + 1, "Precision"]
      recall <- cm$byClass[class + 1, "Recall"]
      f1 <- 2 * (precision * recall) / (precision + recall)
      if (is.na(f1)) {f1=0}
      return(f1)
    }
  )
}

n_classes <- length(unique(dt.red$Room_Occupancy_Count))
f1_measures <- lapply(0:(n_classes - 1), make_f1_measures)

obj.fun <- makeSingleObjectiveFunction(
  name = "Classification-knn",
  fn = function(x) {
    k <- x[1]
    ds <- dt.red
    learner <- makeLearner("classif.knn", predict.type="response", k = k)
    task <- makeClassifTask(data = ds, target = "Room_Occupancy_Count")
    res <- resample(learner, task, resampling = makeResampleDesc("CV", iters = 5), measures = f1_measures)
    
    mean_f1 <- mean(sapply(f1_measures, function(f1_measure) {
      value <- res$measures.test[[f1_measure$id]]
      return(value)
    }))
    return(mean_f1)
  },
  minimize = FALSE,
  par.set = makeParamSet(
    makeIntegerParam("k", lower = 1, upper = 100)
  )
)

des <- generateDesign(n = 5, getParamSet(obj.fun), fun = lhs::randomLHS)
des$y <- apply(des, 1, obj.fun)

psm <- makeLearner("regr.randomForest", predict.type = "se", ntree = 500, mtry = round(length(getParamSet(obj.fun)) / 3, 0))

control <- makeMBOControl()
control <- setMBOControlTermination(control, iters = 25)
control <- setMBOControlInfill(control, crit = makeMBOInfillCritEI()) # EI

run <- mbo(obj.fun, design = des, learner = psm, control = control, show.info = TRUE)

best.seen <- getOptPathY(run$opt.path)

plot(cummax(best.seen), type = "o", lwd = 3, col = "blue",
     ylim = c(min(best.seen), max(best.seen)), ylab = "Best Seen", xlab = "Trials")
lines(best.seen, type = "o", lty = 2, col = "green", lwd = 3)
legend("bottomright", legend = c("Best Seen", "Accuracy"), col = c("blue", "green"), lty = 1:2, lwd = 3, pch = 1)

run$x
run$y

# train error
knn.clast <- knn(train = dt.red[,-9], test = dt.red[,-9], cl = dt.red[,9], k = run$x$k)
1-mean(knn.clast==dt.red[,9])

# test error
knn.clas <- knn(train = dt.red[,-9], test = tt.red[,-9], cl = dt.red[,9], k = run$x$k)
confusionMatrix(data = knn.clas,reference=tt.red[,9])
1-mean(knn.clas==tt.red[,9])

cm <- confusionMatrix(data = knn.clas,reference=tt.red[,9])
(F1 <- as.numeric(cm$byClass[,7]))
(BA <- as.numeric(cm$byClass[,11]))


# SVM
make_f1_avg <- function(n_classes) {
  makeMeasure(
    id = "f1_avg",
    minimize = FALSE,
    properties = c("classif", "classif.multi"),
    fun = function(task, model, pred, feats, extra.args) {
      cm <- confusionMatrix(data = factor(pred$data$response, levels = levels(pred$data$truth)),
                            reference = factor(pred$data$truth, levels = levels(pred$data$truth)))
      f1_scores <- numeric(n_classes)
      for (class in 1:n_classes) {
        precision <- cm$byClass[class, "Precision"]
        recall <- cm$byClass[class, "Recall"]
        f1 <- 2 * (precision * recall) / (precision + recall)
        if (is.na(f1)) { f1 = 0 }
        f1_scores[class] <- f1
      }
      avg_f1 <- mean(f1_scores, na.rm = TRUE)
      return(avg_f1)
    }
  )
}

n_classes <- length(unique(dt.red$Room_Occupancy_Count))
f1_avg_measure <- make_f1_avg(n_classes)

par.set <- makeParamSet(
  makeDiscreteParam('kernel', values = c('linear', 'radial')),
  makeNumericParam('cost', lower = -2, upper = 2, trafo = function(x) 10^x),
  makeNumericParam('gamma', lower = -2, upper = 2, trafo = function(x) 10^x,
                   requires = quote(kernel == 'radial'))
)

cntrl <- makeMBOControl()
cntrl <- setMBOControlTermination(cntrl, iter = 25)
tune.ctrl <- makeTuneControlMBO(mbo.control = cntrl)

task <- makeClassifTask(data = dt.red, target = 'Room_Occupancy_Count')

run <- tuneParams(makeLearner('classif.svm'), task, cv5, measure = f1_avg_measure,
                  par.set = par.set, control = tune.ctrl, show.info = TRUE)

best.seen <- getOptPathY(run$opt.path)

plot(cummax(best.seen), type = "o", lwd = 3, col = "blue",
     ylim = c(min(best.seen), max(best.seen)), ylab = "Best Seen", xlab = "Trials")
lines(best.seen, type = "o", lty = 2, col = "green", lwd = 3)
legend("bottomright", legend = c("Best Seen", "Accuracy"), col = c("blue", "green"), lty = 1:2, lwd = 3, pch = 1)

run$x
run$y

# train error
svm.clas <- svm(Room_Occupancy_Count~., data = dt.red,scale = T,
                type='C-classification',cost=run$x$cost, kernel=run$x$kernel,gamma=run$x$gamma)
clas.pred.svmt <- predict(svm.clas)
1-mean(clas.pred.svmt==dt.red[,9])

# test error
clas.pred.svm <- predict(svm.clas,newdata=tt.red[,-9])
confusionMatrix(data = clas.pred.svm,reference=tt.red[,9])
1-mean(clas.pred.svm==tt.red[,9])

cm <- confusionMatrix(data = clas.pred.svm,reference=tt.red[,9])
(F1 <- as.numeric(cm$byClass[,7]))
(BA <- as.numeric(cm$byClass[,11]))

svm.clas$tot.nSV
dim(dt.red)[1]


# RANDOM FOREST
ps <- makeParamSet(
  makeIntegerParam("ntree", lower = 1,upper = 500),
  makeIntegerParam("mtry", lower = 2,upper = 8)
)

cntrl <- makeMBOControl()
cntrl <- setMBOControlTermination(cntrl, iter = 25)
tune.ctrl <- makeTuneControlMBO(mbo.control = cntrl)

task<-makeClassifTask(data=dt.red,target='Room_Occupancy_Count')

run <- tuneParams(makeLearner("classif.randomForest"), task = task, resampling = cv5, par.set = ps, control = tune.ctrl, measures = f1_avg_measure)

best.seen <- getOptPathY(run$opt.path)

plot(cummax(best.seen), type = "o", lwd = 3, col = "blue",
     ylim = c(min(best.seen), max(best.seen)), ylab = "Best Seen", xlab = "Trials")
lines(best.seen, type = "o", lty = 2, col = "green", lwd = 3)
legend("bottomright", legend = c("Best Seen", "Accuracy"), col = c("blue", "green"), lty = 1:2, lwd = 3, pch = 1)

run$x
run$y

# train error
mod.RF <- randomForest(Room_Occupancy_Count~., data = dt.red,ntree=run$x$ntree,mtry=run$x$mtry)
pred.RFt <- predict(mod.RF)
1-mean(pred.RFt==dt.red[,9])

# test error
pred.RF <- predict(mod.RF,newdata=tt.red[,-9])
1-mean(pred.RF==tt.red[,9])

cm <- confusionMatrix(data = pred.RF,reference=tt.red[,9])
(F1 <- as.numeric(cm$byClass[,7]))
(BA <- as.numeric(cm$byClass[,11]))


# RETE NEURALE
param_set <- makeParamSet(
  makeIntegerParam("size", lower = 2,upper = 16),
  makeNumericParam("decay", lower = 0, upper = 0.1)
)

learner <- makeLearner("classif.nnet", predict.type = "prob", par.vals = list(maxit=200))

task <- makeClassifTask(data = dt.red, target = "Room_Occupancy_Count")

cntrl <- makeMBOControl()
cntrl <- setMBOControlTermination(cntrl, iter = 25)
tune.ctrl <- makeTuneControlMBO(mbo.control = cntrl)

run <- tuneParams(learner = learner,task = task,resampling = cv5,par.set = param_set,control = tune.ctrl,measures = f1_avg_measure)

best.seen <- getOptPathY(run$opt.path)

plot(cummax(best.seen), type = "o", lwd = 3, col = "blue",
     ylim = c(min(best.seen), max(best.seen)), ylab = "Best Seen", xlab = "Trials")
lines(best.seen, type = "o", lty = 2, col = "green", lwd = 3)
legend("bottomright", legend = c("Best Seen", "Accuracy"), col = c("blue", "green"), lty = 1:2, lwd = 3, pch = 1)

run$x
run$y

learner_tuned <- setHyperPars(learner, par.vals = run$x)

model <- train(learner_tuned, task)

pred.nnt <- predict(model, newdata = dt.red[,-9])
pred.nnt <- pred.nnt$data$response
1-mean(pred.nnt==dt.red[,9])

pred.nn <- predict(model, newdata = tt.red[,-9])
pred.nn <- pred.nn$data$response
1-mean(pred.nn==tt.red[,9])

cm <- confusionMatrix(data = pred.nn,reference=tt.red[,9])
(F1 <- as.numeric(cm$byClass[,7]))
(BA <- as.numeric(cm$byClass[,11]))


# PROPORTIONAL ODDS
dt.red.po <- dt.red
dt.red.po$Room_Occupancy_Count <- as.ordered(dt.red.po$Room_Occupancy_Count)

mod.propodds <- vglm(data=dt.red.po, Room_Occupancy_Count~., cumulative(parallel = T))
summary(mod.propodds)

pred.propodds <-  predict(mod.propodds, type="response")
a <- vector(length=nrow(pred.propodds))
for (i in 1:nrow(pred.propodds)){
  a[i] <- which.max(pred.propodds[i,])-1}
a <- as.factor(a)
confusionMatrix(a, dt.red$Room_Occupancy_Count)
1-mean(a==dt.red$Room_Occupancy_Count)

test.pred.propodds <- predict(mod.propodds, type="response", newdata = tt.red[,-9])
b <- vector(length=nrow(test.pred.propodds))
for (i in 1:nrow(test.pred.propodds)){
  b[i] <- which.max(test.pred.propodds[i,])-1}
b <- as.factor(b)
confusionMatrix(b, tt.red$Room_Occupancy_Count)
1-mean(b==tt.red$Room_Occupancy_Count)

cm <- confusionMatrix(b, tt.red$Room_Occupancy_Count)
(F1 <- as.numeric(cm$byClass[,7]))
(BA <- as.numeric(cm$byClass[,11]))
