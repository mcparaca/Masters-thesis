library(tidyverse)
library("readxl")
library(psych)
library(arsenal)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(outliers)
library(EnvStats)
library(corrplot)
library(DataExplorer)
library(ipred)
library(klaR)
library(mlbench)
library(ROCR)
library(rpart.plot)
library(e1071)
library(randomForest)
library(stats)
library(factoextra)
library(cluster)

#loading the data and converting it to the dataframe
data <- read_excel("C:/Users/user/Desktop/Applied mathematics/Semestr 2/Data Mining/Project/project_data.xlsx")
df_cancer <- data.frame(data)

#checking the size of the data
df.size <- dim(data)

#saving the names of attributes
df.names <-names(df_cancer)
cat<- df_cancer$diagnosis

#checking the data type of each attribute
summary <- summary(df_cancer)

#changing the attributes into correct data type
df_cancer <- as.data.frame(sapply(df_cancer[, -which(names(df_cancer) == "diagnosis")], as.numeric))
df_cancer$diagnosis <- cat

#checking if there are NAs in the data
number_of_NA <- sum(is.na(data))

#basic numerical characteristics of the attributes
characteristics <- as.data.frame(describe(df_cancer))
characteristics<-characteristics[2:31,]

png("summary_general.png", height=1000, width=1500)
p<-tableGrob(characteristics)
grid.arrange(p)
dev.off()


#divide into 10 categories that describe the same tumor feature
radius <- df_cancer[c("radius_mean", "radius_se", "radius_worst")]
texture<- df_cancer[c("texture_mean", "texture_se", "texture_worst")]
perimeter<-df_cancer[c("perimeter_mean", "perimeter_se", "perimeter_worst")]
area <-df_cancer[c("area_mean", "area_se", "area_worst")]
smoothness <-df_cancer[c("smoothness_mean", "smoothness_se", "smoothness_worst")]
compactness <-df_cancer[c("compactness_mean", "compactness_se", "compactness_worst")]
concavity <-df_cancer[c("concavity_mean", "concavity_se", "concavity_worst")]
concave.points <-df_cancer[c("concave.points_mean", "concave.points_se", "concave.points_worst")]
symmetry<- df_cancer[c("symmetry_mean", "symmetry_se", "symmetry_worst")]
fractal_dimension <-df_cancer[c("fractal_dimension_", "fractal_dimension_se", "fractal_dimension_worst")]
diagnosis <- df_cancer$diagnosis

#and select only the mean parameters of the tumors  - without its se and worst features
df_parameters <- df_cancer[c("radius_mean","texture_mean","perimeter_mean","area_mean", "smoothness_mean", "compactness_mean",
                             "concavity_mean","concave.points_mean","symmetry_mean","fractal_dimension_")]
df_parameters<-as.data.frame(df_parameters)
df_parameters_cat <- df_parameters
df_parameters_cat$diagnosis <- df_cancer$diagnosis




#plotting the density for each attribute, grupped by  the feature
my_plots <- lapply(names(diagnosis), function(var_x){
  p <- 
    ggplot(diagnosis) +
    aes_string(var_x)
  
  if(is.numeric(diagnosis[[var_x]])) {
    p <- p + geom_density()
    
  } else {
    p <- p + geom_bar()
  } 
  
})

plot_grid(plotlist = my_plots)

#barplot and pie chart for categorical attribute
ggplot(df_cancer) +
  aes(diagnosis)+geom_bar()

ggplot(df_cancer, aes(x="", y=diagnosis, fill = diagnosis)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

#plotting boxplots each attribute, grupped by the feature
my_plots_box <- lapply(names(fractal_dimension), function(var_x){
  p <- 
    ggplot(fractal_dimension) +
    aes_string(var_x)
  
  if(is.numeric(fractal_dimension[[var_x]])) {
    p <- p + geom_boxplot()
    
  } else {
    p <- p + geom_bar()
  } 
  
})

plot_grid(plotlist = my_plots_box)

#exemplary scatter plots

radius.scatter <- df_parameters$radius_mean
perimeter.scatter <- df_parameters$perimeter_mean
smoothness.scatter <- df_parameters$smoothness_mean
texture.scatter <- df_parameters$texture_mean
area.scatter <- df_parameters$area_mean
compactness.scatter <- df_parameters$compactness_mean
concavity.scatter <- df_parameters$concavity_mean
concave.points.scatter <- df_parameters$concave.points_mean
symmetry.scatter <- df_parameters$symmetry_mean
fractal_dimension.scatter <- df_parameters$fractal_dimension_


pairs(cbind(radius.scatter, perimeter.scatter, smoothness.scatter))
pairs(cbind(concavity.scatter, symmetry.scatter, compactness.scatter))
pairs(cbind(area.scatter, texture.scatter, fractal_dimension.scatter))

#detecting outliers
test.out.radius <- as.data.frame(rosnerTest(df_cancer$radius_mean,k = 4)$all.stats)
df.outliers <- test.out.radius[, c(5,8)]
colnames(df.outliers)<- c("observation number", "is_outlier")

test.out.texture <- as.data.frame(rosnerTest(df_cancer$texture_mean,k = 4)$all.stats)
df1 <-test.out.texture[, c(5,8)]
colnames(df1)<-c("observation number", "is_outlier")
df.outliers<-bind_rows(df.outliers, df1)

test.out.perimeter <- as.data.frame(rosnerTest(df_cancer$perimeter_mean,k = 4)$all.stats)
df1 <-test.out.perimeter[, c(5,8)]
colnames(df1)<-c("observation number", "is_outlier")
df.outliers<-bind_rows(df.outliers, df1)

test.out.area <- as.data.frame(rosnerTest(df_cancer$area_mean,k = 4)$all.stats)
df1 <-test.out.area[, c(5,8)]
colnames(df1)<-c("observation number", "is_outlier")
df.outliers<-bind_rows(df.outliers, df1)

test.out.smoothness <- as.data.frame(rosnerTest(df_cancer$smoothness_mean,k = 4)$all.stats)
df1 <-test.out.smoothness[, c(5,8)]
colnames(df1)<-c("observation number", "is_outlier")
df.outliers<-bind_rows(df.outliers, df1)

test.out.compactness <- as.data.frame(rosnerTest(df_cancer$compactness_mean,k = 4)$all.stats)
df1 <-test.out.compactness[, c(5,8)]
colnames(df1)<-c("observation number", "is_outlier")
df.outliers<-bind_rows(df.outliers, df1)

test.out.concavity <- as.data.frame(rosnerTest(df_cancer$concavity_mean,k = 4)$all.stats)
df1 <-test.out.concavity[, c(5,8)]
colnames(df1)<-c("observation number", "is_outlier")
df.outliers<-bind_rows(df.outliers, df1)

test.out.concave.points <- as.data.frame(rosnerTest(df_cancer$concave.points_mean,k = 4)$all.stats)
df1 <-test.out.concave.points[, c(5,8)]
colnames(df1)<-c("observation number", "is_outlier")
df.outliers<-bind_rows(df.outliers, df1)

test.out.symmetry <- as.data.frame(rosnerTest(df_cancer$symmetry_mean,k = 4)$all.stats)
df1 <-test.out.symmetry[, c(5,8)]
colnames(df1)<-c("observation number", "is_outlier")
df.outliers<-bind_rows(df.outliers, df1)

test.out.fractal_dimension <- as.data.frame(rosnerTest(df_cancer$fractal_dimension_,k = 4)$all.stats)
df1 <-test.out.fractal_dimension[, c(5,8)]
colnames(df1)<-c("observation number", "is_outlier")
df.outliers<-bind_rows(df.outliers, df1)

rownames(df.outliers)<-c("radius1", "radius2", "radius3", "radius4","texture1", "texture2", "texture3", "texture4","perimeter1", "perimeter2", "perimeter3", "perimeter4",
                         "area1", "area2", "area3", "area4","smoothness1", "smoothness2", "smoothness3", "smoothness4","compactness1", "compactness2", "compactness3", "compactness4",
                         "concavity1", "concavity2", "concavity3", "concavity4","concave.points1", "concave.points2", "concave.points3", "concave.points4","symmetry1", "symmetry2", "symmetry3", "symmetry4",
                         "fractal_dimension1", "fractal_dimension2", "fractal_dimension3", "fractal_dimension4")


png("outliers.png", height=2000, width=500)
p<-tableGrob(df.outliers)
grid.arrange(p)
dev.off()

#Checking which observations had more than one occurence in the previous table
aggr<-as.data.frame(aggregate(df.outliers$is_outlier, list(df.outliers$`observation number`), FUN=sum)) 
colnames(aggr)<-c("observation number", "times being an outlier")
aggr <-filter(aggr, `times being an outlier` > 0)

#summary of outliers
png("outliers_summary.png", height=2000, width=500)
p<-tableGrob(aggr)
grid.arrange(p)
dev.off()

# now we can check a correlation between the mean parameters as a corr matrix
rquery.cormat<-function(x,
                        type=c('lower', 'upper', 'full', 'flatten'),
                        graph=TRUE,
                        graphType=c("correlogram", "heatmap"),
                        col=NULL, ...)
{
  cor.pmat <- function(x, ...) {
    mat <- as.matrix(x)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  # Get lower triangle of the matrix
  getLower.tri<-function(mat){
    upper<-mat
    upper[upper.tri(mat)]<-""
    mat<-as.data.frame(upper)
    mat
  }
  # Get upper triangle of the matrix
  getUpper.tri<-function(mat){
    lt<-mat
    lt[lower.tri(mat)]<-""
    mat<-as.data.frame(lt)
    mat
  }
  # Get flatten matrix
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut],
      p = pmat[ut]
    )
  }
  # Define color
  if (is.null(col)) {
    col <- colorRampPalette(
      c("#67001F", "#B2182B", "#D6604D", "#F4A582",
        "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
        "#4393C3", "#2166AC", "#053061"))(200)
    col<-rev(col)
  }
  
  # Correlation matrix
  cormat<-signif(cor(x, use = "complete.obs", ...),2)
  pmat<-signif(cor.pmat(x, ...),2)
  # Reorder correlation matrix
  ord<-corrMatOrder(cormat, order="hclust")
  cormat<-cormat[ord, ord]
  pmat<-pmat[ord, ord]
  # Replace correlation coeff by symbols
  sym<-symnum(cormat, abbr.colnames=FALSE)
  # Correlogram
  if(graph & graphType[1]=="correlogram"){
    corrplot(cormat, type=ifelse(type[1]=="flatten", "lower", type[1]),
             tl.col="black", tl.srt=45,col=col,...)
  }
  else if(graphType[1]=="heatmap")
    heatmap(cormat, col=col, symm=TRUE)
  # Get lower/upper triangle
  if(type[1]=="lower"){
    cormat<-getLower.tri(cormat)
    pmat<-getLower.tri(pmat)
  }
  else if(type[1]=="upper"){
    cormat<-getUpper.tri(cormat)
    pmat<-getUpper.tri(pmat)
    sym=t(sym)
  }
  else if(type[1]=="flatten"){
    cormat<-flattenCorrMatrix(cormat, pmat)
    pmat=NULL
    sym=NULL
  }
  list(r=cormat, p=pmat, sym=sym)
}

rquery.cormat(df_parameters,type="full")
cor.matrix <- rquery.cormat(df_parameters,type="full")$r

png("cor_matrix_values.png", height=2000, width=2000)
p<-tableGrob(cor.matrix)
grid.arrange(p)
dev.off()




#We can group the data by the diagnosis category

df_parameters_benign <- filter(df_parameters_cat, diagnosis == "B")
df_parameters_malignant <- filter(df_parameters_cat, diagnosis == "M")

#at first summary for both categories

summary_benign<-describe(df_parameters_benign[, -which(names(df_parameters_benign) == "diagnosis")])
summary_malignant<-describe(df_parameters_malignant[, -which(names(df_parameters_malignant) == "diagnosis")])

png("summary_benign.png", height=1000, width=1500)
p<-tableGrob(summary_benign)
grid.arrange(p)
dev.off()

png("summary_malignant.png", height=1000, width=1500)
p<-tableGrob(summary_malignant)
grid.arrange(p)
dev.off()

####difference of densities


cat_plot_radius <- ggplot(NULL, aes(radius_mean)) + 
  geom_density(data = df_parameters_benign, aes(color=diagnosis),size=2) +
  geom_density(data = df_parameters_malignant, aes(color=diagnosis),size=2)

cat_plot_texture <- ggplot(NULL, aes(texture_mean)) + 
  geom_density(data = df_parameters_benign, aes(color=diagnosis),size=2) +
  geom_density(data = df_parameters_malignant, aes(color=diagnosis),size=2)

cat_plot_perimeter<- ggplot(NULL, aes(perimeter_mean)) + 
  geom_density(data = df_parameters_benign, aes(color=diagnosis),size=2) +
  geom_density(data = df_parameters_malignant, aes(color=diagnosis),size=2)

cat_plot_area <- ggplot(NULL, aes(area_mean)) + 
  geom_density(data = df_parameters_benign, aes(color=diagnosis),size=2) +
  geom_density(data = df_parameters_malignant, aes(color=diagnosis),size=2)


grid.arrange(cat_plot_radius, cat_plot_texture,cat_plot_perimeter,cat_plot_area, ncol = 2, nrow=2)

cat_plot_smoothness <- ggplot(NULL, aes(smoothness_mean)) + 
  geom_density(data = df_parameters_benign, aes(color=diagnosis),size=2) +
  geom_density(data = df_parameters_malignant, aes(color=diagnosis),size=2)

cat_plot_compactness <- ggplot(NULL, aes(compactness_mean)) + 
  geom_density(data = df_parameters_benign, aes(color=diagnosis),size=2) +
  geom_density(data = df_parameters_malignant, aes(color=diagnosis),size=2)

cat_plot_concavity<- ggplot(NULL, aes(concavity_mean)) + 
  geom_density(data = df_parameters_benign, aes(color=diagnosis),size=2) +
  geom_density(data = df_parameters_malignant, aes(color=diagnosis),size=2)

cat_plot_concave.points <- ggplot(NULL, aes(concave.points_mean)) + 
  geom_density(data = df_parameters_benign, aes(color=diagnosis),size=2) +
  geom_density(data = df_parameters_malignant, aes(color=diagnosis),size=2)


grid.arrange(cat_plot_smoothness, cat_plot_compactness,cat_plot_concavity,cat_plot_concave.points, ncol = 2, nrow=2)


cat_plot_symmetry<- ggplot(NULL, aes(symmetry_mean)) + 
  geom_density(data = df_parameters_benign, aes(color=diagnosis),size=2) +
  geom_density(data = df_parameters_malignant, aes(color=diagnosis),size=2)

cat_plot_fractal_dimension <- ggplot(NULL, aes(fractal_dimension_)) + 
  geom_density(data = df_parameters_benign, aes(color=diagnosis),size=2) +
  geom_density(data = df_parameters_malignant, aes(color=diagnosis),size=2)


grid.arrange(cat_plot_symmetry, cat_plot_fractal_dimension, nrow=2)

#and boxplots

plot_boxplot(df_parameters_cat, by="diagnosis")


#the difference of the summary values (how many times the values in M are bigger than in B)
#this shows us the impact that each parameters might have for the tumor to be classified as M instead of B

M <- merge(summary_malignant,summary_benign,by="vars")

S <- M[,grepl("*\\.x$",names(M))] / M[,grepl("*\\.y$",names(M))]

value_diff<-as.data.frame(cbind(M[,1,drop=FALSE],S))
row.names(value_diff) <- row.names(summary_malignant)
value_diff<-value_diff[c("mean.x","sd.x","max.x","range.x")]

png("val_diff.png", height=1000, width=1500)
p<-tableGrob(value_diff)
grid.arrange(p)
dev.off()



###################################################################
#CLASSIFICATION


#At first we manually divide the data into trainig and test set
# we would like to focus mainly on the general parameters - we chose the means of factors and reject the se and worst features
smp_size <- floor(0.7 * nrow(df_parameters_cat))
set.seed(123)
train_ind <- sample(seq_len(nrow(df_parameters_cat)), size = smp_size)
train <- df_parameters_cat[train_ind, ]
test <- df_parameters_cat[-train_ind, ]

#LDA

#classification rule for all variables
cancer.lda.all  <- lda(diagnosis~., data=df_parameters_cat, subset = train_ind)


#1)we can also create our own classification rule based on the cor.matrix and value_diff we pick the variables that are uncorrelated
#and their mean values are bigger for M than for B i.e.: area_mean, concavity_mean, and texture_mean
#2) we can pick the variables regardless of their correlation and just focus on their mean values from the considered previously table
cancer.lda.chosen  <- lda(diagnosis~area_mean + concavity_mean + texture_mean, data=df_parameters_cat, subset = train_ind)
cancer.lda.picked  <- lda(diagnosis~area_mean + concavity_mean + compactness_mean + concave.points_mean, data=df_parameters_cat, subset = train_ind)


#predictions
(prediction.lda.all  <-  predict(cancer.lda.all, test)$class)
(prediction.lda.chosen  <-  predict(cancer.lda.chosen, test)$class)
(prediction.lda.picked  <-  predict(cancer.lda.picked, test)$class)


prediction.lda.posterior.all  <-  predict(cancer.lda.all, test)$posterior[,2]
prediction.lda.posterior.chosen  <-  predict(cancer.lda.chosen, test)$posterior[,2]
prediction.lda.posterior.picked  <-  predict(cancer.lda.picked, test)$posterior[,2]



#real values of diagnosis form test set
real.labels.cancer <- df_parameters_cat$diagnosis[-train_ind]

#confusion matrices
(conf.mat.lda.all <- table(prediction.lda.all, real.labels.cancer))
(conf.mat.lda.chosen <- table(prediction.lda.chosen, real.labels.cancer))
(conf.mat.lda.picked <- table(prediction.lda.picked, real.labels.cancer))
row.names(conf.mat.lda.all)<- c("predicted B", "predicted M")
row.names(conf.mat.lda.chosen)<- c("predicted B", "predicted M")
row.names(conf.mat.lda.picked)<- c("predicted B", "predicted M")



#misclassifcation error
n.test <- dim(test)[1]
(error.lda.all <- (n.test-sum(diag(conf.mat.lda.all)))/n.test)
(error.lda.chosen <- (n.test-sum(diag(conf.mat.lda.chosen)))/n.test)
(error.lda.picked <- (n.test-sum(diag(conf.mat.lda.picked)))/n.test)


png("conf_mat_lda_all.png", height=1000, width=1500)
p<-tableGrob(conf.mat.lda.all)
grid.arrange(p)
dev.off()


png("conf_mat_lda_chosen.png", height=1000, width=1500)
p<-tableGrob(conf.mat.lda.chosen)
grid.arrange(p)
dev.off()

png("conf_mat_lda_picked.png", height=1000, width=1500)
p<-tableGrob(conf.mat.lda.picked)
grid.arrange(p)
dev.off()

##QDA

#the same as in LDA
cancer.qda.all  <- qda(diagnosis~., data=df_parameters_cat, subset = train_ind)
cancer.qda.chosen  <- qda(diagnosis~area_mean + concavity_mean + texture_mean, data=df_parameters_cat, subset = train_ind)
cancer.qda.picked  <- qda(diagnosis~area_mean + concavity_mean + compactness_mean + concave.points_mean, data=df_parameters_cat, subset = train_ind)


#predictions
(prediction.qda.all  <-  predict(cancer.qda.all, test)$class)
(prediction.qda.chosen  <-  predict(cancer.qda.chosen, test)$class)
(prediction.qda.picked  <-  predict(cancer.qda.picked, test)$class)

prediction.qda.posterior.all  <-  predict(cancer.qda.all, test)$posterior[,2]
prediction.qda.posterior.chosen  <-  predict(cancer.qda.chosen, test)$posterior[,2]
prediction.qda.posterior.picked  <-  predict(cancer.qda.picked, test)$posterior[,2]

#confusion matrices
(conf.mat.qda.all <- table(prediction.qda.all, real.labels.cancer))
(conf.mat.qda.chosen <- table(prediction.qda.chosen, real.labels.cancer))
(conf.mat.qda.picked <- table(prediction.qda.picked, real.labels.cancer))
row.names(conf.mat.qda.all)<- c("predicted B", "predicted M")
row.names(conf.mat.qda.chosen)<- c("predicted B", "predicted M")
row.names(conf.mat.qda.picked)<- c("predicted B", "predicted M")

#misclassifcation error
n.test <- dim(test)[1]
(error.qda.all <- (n.test-sum(diag(conf.mat.qda.all)))/n.test)
(error.qda.chosen <- (n.test-sum(diag(conf.mat.qda.chosen)))/n.test)
(error.qda.picked <- (n.test-sum(diag(conf.mat.qda.picked)))/n.test)

png("conf_mat_qda_all.png", height=1000, width=1500)
p<-tableGrob(conf.mat.qda.all)
grid.arrange(p)
dev.off()


png("conf_mat_qda_chosen.png", height=1000, width=1500)
p<-tableGrob(conf.mat.qda.chosen)
grid.arrange(p)
dev.off()

png("conf_mat_qda_picked.png", height=1000, width=1500)
p<-tableGrob(conf.mat.qda.picked)
grid.arrange(p)
dev.off()

#Decision boundaries: LDA vs QDA for area and concavity

drawparti(train$diagnosis, x=train$area_mean, y=train$concavity_mean, method = "lda", xlab="area", ylab="concavity")
drawparti(train$diagnosis, x=train$area_mean, y=train$concavity_mean, method = "qda", xlab="area", ylab="concavity")


####kNN
#model
cancer.knn.all <- ipredknn(diagnosis ~ ., data=train, k=5)
cancer.knn.chosen <- ipredknn(diagnosis ~ area_mean + concavity_mean + texture_mean, data=train, k=5)
cancer.knn.picked <- ipredknn(diagnosis ~ area_mean + concavity_mean + compactness_mean + concave.points_mean, data=train, k=5)

#predict
prediction.knn.all <- predict(cancer.knn.all,test, type="class")
prediction.knn.chosen <- predict(cancer.knn.chosen,test, type="class")
prediction.knn.picked <- predict(cancer.knn.picked,test, type="class")

prediction.knn.probs.all <- predict(cancer.knn.all, newdata=test, type="prob")
prediction.knn.probs.chosen <- predict(cancer.knn.chosen, newdata=test, type="prob")
prediction.knn.probs.picked <- predict(cancer.knn.picked, newdata=test, type="prob")

prediction.knn.all<-ifelse(prediction.knn.all=="B",0,1)
prediction.knn.chosen<-ifelse(prediction.knn.chosen=="B",0,1)
prediction.knn.picked<-ifelse(prediction.knn.picked=="B",0,1)



(conf.mat.knn.all1 <- table(prediction.knn.probs.all, real.labels.logit))
(conf.mat.knn.chosen1 <- table(prediction.knn.probs.chosen, real.labels.logit))
(conf.mat.knn.picked1 <- table(prediction.knn.probs.picked, real.labels.logit))





# confusion matrix
(conf.mat.knn.all <- table(prediction.knn.all, real.labels.cancer))
(conf.mat.knn.chosen <- table(prediction.knn.chosen, real.labels.cancer))
(conf.mat.knn.picked <- table(prediction.knn.picked, real.labels.cancer))
row.names(conf.mat.knn.all)<- c("predicted B", "predicted M")
row.names(conf.mat.knn.chosen)<- c("predicted B", "predicted M")
row.names(conf.mat.knn.picked)<- c("predicted B", "predicted M")

png("conf_mat_knn_all.png", height=1000, width=1500)
p<-tableGrob(conf.mat.knn.all)
grid.arrange(p)
dev.off()
png("conf_mat_knn_chosen.png", height=1000, width=1500)
p<-tableGrob(conf.mat.knn.chosen)
grid.arrange(p)
dev.off()
png("conf_mat_knn_picked.png", height=1000, width=1500)
p<-tableGrob(conf.mat.knn.picked)
grid.arrange(p)
dev.off()

# misclassification error
error.knn.all<-(n.test - sum(diag(conf.mat.knn.all))) / n.test
error.knn.chosen<-(n.test - sum(diag(conf.mat.knn.chosen))) / n.test
error.knn.picked<-(n.test - sum(diag(conf.mat.knn.picked))) / n.test

#######now we can check for what k the misclassification error for all features in our model will be best
check_k <- function(kk){
  errors <-data.frame()
  con_mat <-table(predict(ipredknn(diagnosis ~ ., data=train, k=kk),test, type="class"), real.labels.cancer)
  error<-(n.test - sum(diag(con_mat))) / n.test
  return(error)
  }
errors <- c()
k_s<-c()
for (k in 1:20){
  error <-check_k(k)
  errors<-c(errors, error)
  k_s<-c(k_s,k)
}
error.df <- as.data.frame(k_s)
error.df$errors <-errors

png("which_k_in_knn.png", height=1000, width=1500)
p<-tableGrob(error.df)
grid.arrange(p)
dev.off()


#####Logistic Regression 
#we denote Malignant tumor as 1 and Benign as 0
train_b <- train %>% 
  mutate(binary_diagnosis = if_else(diagnosis == "B", 0, 1))
test_b <- test %>% 
  mutate(binary_diagnosis = if_else(diagnosis == "B", 0, 1))

cancer.logit.all <-  glm(binary_diagnosis~., data=train_b, family=binomial(link="logit"))
cancer.logit.chosen <-  glm(binary_diagnosis~area_mean + concavity_mean + texture_mean, data=train_b, family=binomial(link="logit"))
cancer.logit.picked <-  glm(binary_diagnosis~area_mean + concavity_mean + compactness_mean + concave.points_mean, data=train_b, family=binomial(link="logit"))


#prediction
prediction.logit.all <- predict(cancer.logit.all,test_b, type = "response")
prediction.logit.chosen <- predict(cancer.logit.chosen,test_b, type = "response")
prediction.logit.picked <- predict(cancer.logit.picked,test_b, type = "response")


prob2labels <- function(probs, cutoff)
{
  classes <- rep(0,length(probs))
  classes[probs>cutoff] <- 1
  return(as.factor(classes))
}

# real and predicted labels
real.labels.logit <-test_b$binary_diagnosis
(pred.logit.all <- prob2labels(prediction.logit.all, cutoff=0.5))
(pred.logit.chosen <- prob2labels(prediction.logit.chosen, cutoff=0.5))
(pred.logit.picked <- prob2labels(prediction.logit.picked, cutoff=0.5))

# confusion matrix
(conf.mat.logit.all <- table(pred.logit.all, real.labels.logit))
(conf.mat.logit.chosen <- table(pred.logit.chosen, real.labels.logit))
(conf.mat.logit.picked <- table(pred.logit.picked, real.labels.logit))
row.names(conf.mat.logit.all)<- c("predicted B", "predicted M")
row.names(conf.mat.logit.chosen)<- c("predicted B", "predicted M")
row.names(conf.mat.logit.picked)<- c("predicted B", "predicted M")

# misclassification error
error.log.all<-(n.test - sum(diag(conf.mat.logit.all))) / n.test
error.log.chosen<-(n.test - sum(diag(conf.mat.logit.chosen))) / n.test
error.log.picked<-(n.test - sum(diag(conf.mat.logit.picked))) / n.test


png("conf_mat_logit_all.png", height=1000, width=1500)
p<-tableGrob(conf.mat.logit.all)
grid.arrange(p)
dev.off()
png("conf_mat_logit_chosen.png", height=1000, width=1500)
p<-tableGrob(conf.mat.logit.chosen)
grid.arrange(p)
dev.off()
png("conf_mat_logit_picked.png", height=1000, width=1500)
p<-tableGrob(conf.mat.logit.picked)
grid.arrange(p)
dev.off()


#####ROC
#for LDA

cancer.ROCR.LDA.all.p <- prediction(prediction.lda.posterior.all, real.labels.cancer)
cancer.ROCR.LDA.all <- performance(cancer.ROCR.LDA.all.p, "tpr", "fpr")

cancer.ROCR.LDA.chosen.p <- prediction(prediction.lda.posterior.chosen, real.labels.cancer)
cancer.ROCR.LDA.chosen <- performance(cancer.ROCR.LDA.chosen.p, "tpr", "fpr")

cancer.ROCR.LDA.picked.p <- prediction(prediction.lda.posterior.picked, real.labels.cancer)
cancer.ROCR.LDA.picked <- performance(cancer.ROCR.LDA.picked.p, "tpr", "fpr")


#for QDA
cancer.ROCR.QDA.all.p <- prediction(prediction.qda.posterior.all, real.labels.cancer)
cancer.ROCR.QDA.all <- performance(cancer.ROCR.QDA.all.p, "tpr", "fpr")

cancer.ROCR.QDA.chosen.p <- prediction(prediction.qda.posterior.chosen, real.labels.cancer)
cancer.ROCR.QDA.chosen <- performance(cancer.ROCR.QDA.chosen.p, "tpr", "fpr")

cancer.ROCR.QDA.picked.p <- prediction(prediction.qda.posterior.picked, real.labels.cancer)
cancer.ROCR.QDA.picked <- performance(cancer.ROCR.QDA.picked.p, "tpr", "fpr")


#for knn

cancer.ROCR.knn.all.p <- prediction(prediction.knn.all, real.labels.cancer)
cancer.ROCR.knn.all <- performance(cancer.ROCR.knn.all.p, "tpr", "fpr")

cancer.ROCR.knn.chosen.p <- prediction(prediction.knn.chosen, real.labels.cancer)
cancer.ROCR.knn.chosen <- performance(cancer.ROCR.knn.chosen.p, "tpr", "fpr")

cancer.ROCR.knn.picked.p <- prediction(prediction.knn.picked, real.labels.cancer)
cancer.ROCR.knn.picked <- performance(cancer.ROCR.knn.picked.p, "tpr", "fpr")


#for logistic regression
cancer.ROCR.logit.all.p <- prediction(prediction.logit.all, real.labels.logit)
cancer.ROCR.logit.all <- performance(cancer.ROCR.logit.all.p, "tpr", "fpr")

cancer.ROCR.logit.chosen.p <- prediction(prediction.logit.chosen, real.labels.logit)
cancer.ROCR.logit.chosen <- performance(cancer.ROCR.logit.chosen.p, "tpr", "fpr")

cancer.ROCR.logit.picked.p <- prediction(prediction.logit.picked, real.labels.logit)
cancer.ROCR.logit.picked <- performance(cancer.ROCR.logit.picked.p, "tpr", "fpr")


###ROC plots

#for all
plot(cancer.ROCR.LDA.all, col="red", lwd=2)
grid()
plot(cancer.ROCR.QDA.all,add = T, col="blue", lwd=2)
plot(cancer.ROCR.knn.all, add = T, col="green", lwd=2)
plot(cancer.ROCR.logit.all, add = T, col="purple", lwd=2)
legend("bottomright",lty=c(1,1,1,1), lwd=2, col=c("green","red","blue","purple"),
       legend=c("kNN", "LDA","QDA","Logistic Regression"), bg="azure2", cex=0.7)

#for chosen
plot(cancer.ROCR.LDA.chosen, col="red", lwd=2)
grid()
plot(cancer.ROCR.QDA.chosen,add = T, col="blue", lwd=2)
plot(cancer.ROCR.knn.chosen, add = T, col="green", lwd=2)
plot(cancer.ROCR.logit.chosen, add = T, col="purple", lwd=2)
legend("bottomright",lty=c(1,1,1,1), lwd=2, col=c("green","red","blue","purple"),
       legend=c("kNN", "LDA","QDA","Logistic Regression"), bg="azure2", cex=0.7)

plot(cancer.ROCR.LDA.picked, col="red", lwd=2)
grid()
plot(cancer.ROCR.QDA.picked,add = T, col="blue", lwd=2)
plot(cancer.ROCR.knn.picked, add = T, col="green", lwd=2)
plot(cancer.ROCR.logit.picked, add = T, col="purple", lwd=2)
legend("bottomright",lty=c(1,1,1,1), lwd=2, col=c("green","red","blue","purple"),
       legend=c("kNN", "LDA","QDA","Logistic Regression"), bg="azure2", cex=0.7)


#AUC values
(AUC.LDA.all <- performance(cancer.ROCR.LDA.all.p, "auc")@y.values[[1]])
(AUC.LDA.chosen <- performance(cancer.ROCR.LDA.chosen.p, "auc")@y.values[[1]])
(AUC.LDA.picked <- performance(cancer.ROCR.LDA.picked.p, "auc")@y.values[[1]])

(AUC.QDA.all <- performance(cancer.ROCR.QDA.all.p, "auc")@y.values[[1]])
(AUC.QDA.chosen <- performance(cancer.ROCR.QDA.chosen.p, "auc")@y.values[[1]])
(AUC.QDA.picked <- performance(cancer.ROCR.QDA.picked.p, "auc")@y.values[[1]])

(AUC.knn.all <- performance(cancer.ROCR.knn.all.p, "auc")@y.values[[1]])
(AUC.knn.chosen <- performance(cancer.ROCR.knn.chosen.p, "auc")@y.values[[1]])
(AUC.knn.picked <- performance(cancer.ROCR.knn.picked.p, "auc")@y.values[[1]])

(AUC.logit.all <- performance(cancer.ROCR.logit.all.p, "auc")@y.values[[1]])
(AUC.logit.chosen <- performance(cancer.ROCR.logit.chosen.p, "auc")@y.values[[1]])
(AUC.logit.picked <- performance(cancer.ROCR.logit.picked.p, "auc")@y.values[[1]])

#Regression trees

cancer.tree.complex.all  <- rpart(diagnosis~., data=train, control=rpart.control(cp=.01, minsplit=5, maxdepth=20))
cancer.tree.complex.chosen  <- rpart(diagnosis~area_mean + concavity_mean + texture_mean, data=train, control=rpart.control(cp=.01, minsplit=5, maxdepth=20))
cancer.tree.complex.picked  <- rpart(diagnosis~area_mean + concavity_mean + compactness_mean + concave.points_mean, data=train, control=rpart.control(cp=.01, minsplit=5, maxdepth=20))


#cp and pruning
plotcp(cancer.tree.complex.all)
bestcp.all <- cancer.tree.complex.all$cptable[which.min(cancer.tree.complex.all$cptable[,"xerror"]),"CP"]
cancer.tree.complex.pruned.all <- prune(cancer.tree.complex.all, cp = bestcp.all)

plotcp(cancer.tree.complex.chosen)
bestcp.chosen <- cancer.tree.complex.chosen$cptable[which.min(cancer.tree.complex.chosen$cptable[,"xerror"]),"CP"]
cancer.tree.complex.pruned.chosen <- prune(cancer.tree.complex.chosen, cp = bestcp.chosen)

plotcp(cancer.tree.complex.picked)
bestcp.picked <- cancer.tree.complex.picked$cptable[which.min(cancer.tree.complex.picked$cptable[,"xerror"]),"CP"]
cancer.tree.complex.pruned.picked <- prune(cancer.tree.complex.picked, cp = bestcp.picked)


#plots
par(mfrow=c(2,1))
rpart.plot(cancer.tree.complex.all, main="Original tree")
rpart.plot(cancer.tree.complex.pruned.all, main="Pruned tree")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
rpart.plot(cancer.tree.complex.chosen, main="Original tree")
rpart.plot(cancer.tree.complex.pruned.chosen, main="Pruned tree")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
rpart.plot(cancer.tree.complex.picked, main="Original tree")
rpart.plot(cancer.tree.complex.pruned.picked, main="Pruned tree")
par(mfrow=c(1,1))

#The first two trees are the same after pruning  so there is no point in predicting also for them
#but we can comparepredictions for the third one

#predictions
(prediction.tree.all  <-  predict(cancer.tree.complex.all, test,type = "class"))
(prediction.tree.chosen  <-  predict(cancer.tree.complex.chosen, test,type = "class"))
(prediction.tree.picked  <-  predict(cancer.tree.complex.picked, test,type = "class"))
(prediction.tree.pruned.picked  <-  predict(cancer.tree.complex.pruned.picked, test,type = "class"))

#confusion matrices
(conf.mat.tree.all <- table(prediction.tree.all , real.labels.cancer))
(conf.mat.tree.chosen <- table(prediction.tree.chosen, real.labels.cancer))
(conf.mat.tree.picked <- table(prediction.tree.picked, real.labels.cancer))
(conf.mat.tree.pruned.picked <- table(prediction.tree.pruned.picked, real.labels.cancer))
row.names(conf.mat.tree.all)<- c("predicted B", "predicted M")
row.names(conf.mat.tree.chosen)<- c("predicted B", "predicted M")
row.names(conf.mat.tree.picked)<- c("predicted B", "predicted M")
row.names(conf.mat.tree.pruned.picked)<- c("predicted B", "predicted M")

#misclassifcation error
n.test <- dim(test)[1]
(error.tree.all <- (n.test-sum(diag(conf.mat.tree.all)))/n.test)
(error.tree.chosen <- (n.test-sum(diag(conf.mat.tree.chosen)))/n.test)
(error.tree.picked <- (n.test-sum(diag(conf.mat.tree.picked)))/n.test)
(error.tree.pruned.picked <- (n.test-sum(diag(conf.mat.tree.pruned.picked)))/n.test)


png("conf_mat_tree_all.png", height=1000, width=1500)
p<-tableGrob(conf.mat.tree.all)
grid.arrange(p)
dev.off()

png("conf_mat_tree_chosen.png", height=1000, width=1500)
p<-tableGrob(conf.mat.tree.chosen)
grid.arrange(p)
dev.off()

png("conf_mat_tree_picked.png", height=1000, width=1500)
p<-tableGrob(conf.mat.tree.picked)
grid.arrange(p)
dev.off()

png("conf_mat_tree_pruned_picked.png", height=1000, width=1500)
p<-tableGrob(conf.mat.tree.pruned.picked)
grid.arrange(p)
dev.off()


######SVMs
#Here we would like to compare different types of SVMs with particular parameters for all data

svm.linear.all <- svm(binary_diagnosis~., data=train_b, kernel="linear", cost=.1)
svm.ploynomial.all <- svm(binary_diagnosis~., data=train_b, kernel="polynomial", degree = 2)
svm.radial.all <- svm(binary_diagnosis~., data=train_b, kernel="radial", gamma=0.1)

# prediction and accuracy assessment

pred.svm.linear.all <- predict(svm.linear.all, newdata=test_b)
pred.svm.polynomial.all <- predict(svm.ploynomial.all, newdata=test_b)
pred.svm.radial.all <- predict(svm.radial.all, newdata=test_b)

pred.svm.linear.all<-ifelse(pred.svm.linear.all>0.5,1,0)
pred.svm.polynomial.all<-ifelse(pred.svm.polynomial.all>0.5,1,0)
pred.svm.radial.all<-ifelse(pred.svm.radial.all>0.5,1,0)


#confusion matrices
(conf.mat.svm.linear.all <- table(pred.svm.linear.all , real.labels.logit))
(conf.mat.svm.polynomial.all <- table(pred.svm.polynomial.all , real.labels.logit))
(conf.mat.svm.radial.all <- table(pred.svm.radial.all , real.labels.logit))
row.names(conf.mat.svm.linear.all)<- c("predicted B", "predicted M")
row.names(conf.mat.svm.polynomial.all)<- c("predicted B", "predicted M")
row.names(conf.mat.svm.radial.all)<- c("predicted B", "predicted M")

(error.svm.linear.all <- (n.test-sum(diag(conf.mat.svm.linear.all)))/n.test)
(error.svm.polynomial.all <- (n.test-sum(diag(conf.mat.svm.polynomial.all)))/n.test)
(error.svm.radial.all <- (n.test-sum(diag(conf.mat.svm.radial.all)))/n.test)



png("conf_mat_svm_linear_all.png", height=1000, width=1500)
p<-tableGrob(conf.mat.svm.linear.all)
grid.arrange(p)
dev.off()

png("conf_mat_svm_polynomial_all.png", height=1000, width=1500)
p<-tableGrob(conf.mat.svm.polynomial.all)
grid.arrange(p)
dev.off()

png("conf_mat_svm_radial_all.png", height=1000, width=1500)
p<-tableGrob(conf.mat.svm.radial.all)
grid.arrange(p)
dev.off()

####bagging


cancer.bagging.all <- bagging(binary_diagnosis~., data=train_b, nbagg=25, minsplit=1, cp=0)
cancer.bagging.chosen  <- bagging(binary_diagnosis~area_mean + concavity_mean + texture_mean, data=train_b, nbagg=25, minsplit=1, cp=0)
cancer.bagging.picked  <- bagging(binary_diagnosis~area_mean + concavity_mean + compactness_mean + concave.points_mean, data=train_b, nbagg=25, minsplit=1, cp=0)


#predictions
(prediction.bagging.all  <-  predict(cancer.bagging.all, test_b,type = "class"))
(prediction.bagging.chosen  <-  predict(cancer.bagging.chosen, test_b,type = "class"))
prediction.bagging.chosen<-ifelse(prediction.bagging.chosen>0.5,1,0)
(prediction.bagging.picked  <-  predict(cancer.bagging.picked, test_b,type = "class"))
prediction.bagging.picked<-ifelse(prediction.bagging.picked>0.5,1,0)

#confusion matrices
(conf.mat.bagging.all <- table(prediction.bagging.all , real.labels.logit))
(conf.mat.bagging.chosen <- table(prediction.bagging.chosen, real.labels.logit))
(conf.mat.bagging.picked <- table(prediction.bagging.chosen, real.labels.logit))
row.names(conf.mat.bagging.all)<- c("predicted B", "predicted M")
row.names(conf.mat.bagging.chosen)<- c("predicted B", "predicted M")
row.names(conf.mat.bagging.picked)<- c("predicted B", "predicted M")

#misclassifcation error
n.test <- dim(test)[1]
(error.bagging.all <- (n.test-sum(diag(conf.mat.bagging.all)))/n.test)
(error.bagging.chosen <- (n.test-sum(diag(conf.mat.bagging.chosen)))/n.test)
(error.bagging.picked <- (n.test-sum(diag(conf.mat.bagging.picked)))/n.test)


png("conf_mat_bagging_all.png", height=1000, width=1500)
p<-tableGrob(conf.mat.bagging.all)
grid.arrange(p)
dev.off()

png("conf_mat_bagging_chosen.png", height=1000, width=1500)
p<-tableGrob(conf.mat.bagging.chosen)
grid.arrange(p)
dev.off()

png("conf_mat_bagging_picked.png", height=1000, width=1500)
p<-tableGrob(conf.mat.bagging.picked)
grid.arrange(p)
dev.off()


df.errors.all <- data.frame(c(error.lda.all,error.qda.all, error.knn.all,error.log.all,error.tree.all,error.svm.linear.all,error.svm.polynomial.all,error.svm.radial.all,error.bagging.all), row.names = c("error.lda.all","error.qda.all","error.knn.all","error.log.all","error.tree.all","error.svm.linear.all","error.svm.polynomial.all","error.svm.radial.all","error.bagging.all") )
names(df.errors.all)<-"misclassification error"

df.errors.chosen <- data.frame(c(error.lda.chosen,error.qda.chosen, error.knn.chosen,error.log.chosen,error.tree.chosen,error.bagging.chosen), row.names = c("error.lda.chosen","error.qda.chosen","error.knn.chosen","error.log.all","error.tree.chosen","error.bagging.chosen") )
names(df.errors.chosen)<-"misclassification error"

df.errors.picked <- data.frame(c(error.lda.picked,error.qda.picked, error.knn.picked,error.log.picked,error.tree.picked,error.tree.pruned.picked,error.bagging.picked), row.names = c("error.lda.picked","error.qda.picked","error.knn.picked","error.log.picked","error.tree.picked","error.tree.pruned.picked","error.bagging.picked") )
names(df.errors.picked)<-"misclassification error"

png("df.errors.all.png", height=1000, width=1500)
p<-tableGrob(df.errors.all)
grid.arrange(p)
dev.off()

png("df.errors.chosen.png", height=1000, width=1500)
p<-tableGrob(df.errors.chosen)
grid.arrange(p)
dev.off()

png("df.errors.picked.png", height=1000, width=1500)
p<-tableGrob(df.errors.picked)
grid.arrange(p)
dev.off()

###############################################################################PART 2 
###CLUSTERING

#creating more qualitative columns for the data
df_parameters_cat <- df_parameters_cat %>%
  mutate(size_of_the_tumor = case_when(
    area_mean < 500 ~ "small",
    area_mean < 1000 ~ "medium",
    area_mean < 1500 ~ "big",
    area_mean > 1500 ~ "huge",
  ))

df.features <- df_parameters_cat[,1:10] 
df.real.class.labels <- df_parameters_cat[,11]
df.size <- df_parameters_cat[,12]


df.num <- df_parameters_cat %>% 
  mutate(binary_diagnosis = if_else(diagnosis == "B", 2, 1))

df.size.num <- df.num %>% 
  mutate(numsize = case_when(size_of_the_tumor == "small" ~ 2,
                             size_of_the_tumor == "medium" ~ 4,
                             size_of_the_tumor == "big" ~ 3,
                             size_of_the_tumor == "huge" ~ 1))

##kmeans


#for malignant or benign
k <- 2

kmeans.k2 <- kmeans(df.features, centers=2, iter.max=10, nstart=10)
(df.kmeans.labels <- kmeans.k2$cluster)





plot(df.features$area_mean, df.features$concavity_mean, col=df.kmeans.labels, pch=df.real.class.labels)
title ("Clustering using k-means \n color - cluster labels from k-means, symbol - real class labels")
points(kmeans.k2$centers[,c("area_mean","concavity_mean")],pch=16,cex=3,col=1:2)

fviz_cluster(kmeans.k2, df.features, repel = T)
fviz_cluster(kmeans.k2, df.features, ellipse.type="euclid", repel = T)

# for size of the tumor
kk<- 4
kmeans.k4 <- kmeans(df.features, centers=kk, iter.max=10, nstart=10)
(df.size.labels <- kmeans.k4$cluster)



#comparison of clustering vs real labels
tab.kmeans.k2<-(table(df.kmeans.labels , df.num$binary_diagnosis))
tab.kmeans.k4<-(table(df.size.labels , df.size.num$numsize))


matchClasses(tab.kmeans.k2)
compareMatchedClasses(df.kmeans.labels, df.num$binary_diagnosis)$diag
matchClasses(tab.kmeans.k2, method="exact")
compareMatchedClasses(df.kmeans.labels, df.num$binary_diagnosis, method="exact")$diag

matchClasses(tab.kmeans.k4)
compareMatchedClasses(df.size.labels, df.size.num$numsize)$diag
matchClasses(tab.kmeans.k4, method="exact")
compareMatchedClasses(df.size.labels, df.size.num$numsize, method="exact")$diag

plot(df.features$area_mean, df.features$concavity_mean, col=df.size.labels, pch=df.size)
title ("Clustering using k-means \n color - cluster labels from k-means, symbol - real class labels")
points(kmeans.k4$centers[,c("area_mean","concavity_mean")],pch=16,cex=3,col=1:4)

fviz_cluster(kmeans.k4, df.features, repel = T)
fviz_cluster(kmeans.k4, df.features, ellipse.type="normal", repel = TRUE)
fviz_cluster(kmeans.k4, df.features, ellipse.type="euclid", repel = T)

#dissimilarity matrices
df.DissimilarityMatrix <- daisy(df.features)
df.DissimilarityMatrix.mat <- as.matrix(df.DissimilarityMatrix)

#silhouette indices
sil.kmeans.k2 <- silhouette(x=kmeans.k2$cluster, dist=df.DissimilarityMatrix.mat)
sil.kmeans.k4 <- silhouette(x=kmeans.k4$cluster, dist=df.DissimilarityMatrix.mat)

#average silhouette values
(avg.sil.kmeans.k2 <- summary(sil.kmeans.k2)$clus.avg.widths)
(avg.sil.kmeans.k4 <- summary(sil.kmeans.k4)$clus.avg.widths)

#average silhouette values for whole partition
m1.p<-mean(avg.sil.kmeans.k2)
m2.p<-mean(avg.sil.kmeans.k4)

sil.kmeans.k2 <- silhouette(kmeans.k2$cluster, dist(df.features))
fviz_silhouette(sil.kmeans.k2, xlab="K-means")

sil.kmeans.k4 <- silhouette(kmeans.k4$cluster, dist(df.features))
fviz_silhouette(sil.kmeans.k4, xlab="K-means")

Within.kmeans   <- c()
Between.kmeans  <- c()
Total.kmeans    <- c()


#best k 
sil.avg.kmeans<-c()
for (kkk in 1:10){
  km <- kmeans(df.features, centers = kkk, iter.max=10, nstart=10)
  Within.kmeans  <- c(Within.kmeans, km$tot.withinss)	
  Between.kmeans <- c(Between.kmeans, km$betweenss) 
  Total.kmeans   <- c(Total.kmeans,km$totss)
  if (kkk>1){
  ss <- silhouette(km$cluster, dist(df.DissimilarityMatrix.mat))
  sil.avg.kmeans<-c(sil.avg.kmeans,mean(ss[, 3]))}
  }

df.best.silh.ind.kmeans<-data.frame(silhouette_index = sil.avg.kmeans)
rownames(df.best.silh.ind.kmeans) <- c("2","3","4","5","6","7","8","9","10")

png("kmeanssilhouette.png", height=1000, width=1500)
p<-tableGrob(df.best.silh.ind.kmeans)
grid.arrange(p)
dev.off()

y.range <- range(c(Within.kmeans, Between.kmeans, Total.kmeans))
K.range <- 1:10
plot(K.range,  Within.kmeans, col="red", type="b", lwd=2, xlab="K", ylim=y.range, ylab="B/W")
lines(K.range,  Between.kmeans, col="blue", lwd=2, type="b")
lines(K.range,  Total.kmeans, col="black", lwd=2, type="b")
legend(x='right', legend=c("Total SS (total dispersion)", "Between SS (between-cluster dispersion)","Within SS (within-cluster dispersion)"), lwd=2, col=c("black","blue","red"), bg="azure2", cex=0.7)
grid()
title("Comparison of the within-cluster and between-cluster dispersion")


fviz_nbclust(df.features, FUNcluster = kmeans, method="wss", k.max=10)
fviz_nbclust(df.features, FUNcluster = kmeans, method="wss", k.max=10) + geom_vline(xintercept=2, linetype=2)
fviz_nbclust(df.features, FUNcluster = kmeans, method = "silhouette")
fviz_nbclust(df.features, FUNcluster = kmeans, method = "gap")

##PAM

fviz_dist(df.DissimilarityMatrix, order = FALSE)
fviz_dist(df.DissimilarityMatrix, order = TRUE)
df.pam2 <- pam(x=df.DissimilarityMatrix.mat, diss=TRUE, k=2)
X11()
plot(df.pam2)

df.pam4 <- pam(x=df.DissimilarityMatrix.mat, diss=TRUE, k=4)
X11()
plot(df.pam4)



#comparison of clustering vs real labels
table.pam.k2<-(table(df.pam2$clustering , df.num$binary_diagnosis))
table.pam.k4<-(table(df.pam4$clustering , df.size.num$numsize))


matchClasses(table.pam.k2)
compareMatchedClasses(df.pam2$clustering, df.num$binary_diagnosis)$diag
matchClasses(table.pam.k2, method="exact")
compareMatchedClasses(df.pam2$clustering, df.num$binary_diagnosis, method="exact")$diag

matchClasses(table.pam.k4)
compareMatchedClasses(df.pam4$clustering, df.size.num$numsize)$diag
matchClasses(table.pam.k4, method="exact")
compareMatchedClasses(df.pam4$clustering, df.size.num$numsize, method="exact")$diag

cluster.labels <- df.pam2$clustering
plot(df.features$area_mean, df.features$concavity_mean, col=cluster.labels, xlab="area", ylab="concavity",pch=df.real.class.labels)
title("Visualization of cluster analysis results")
ClusterCenters.names <- df.pam2$medoids

cluster.labels <- df.pam4$clustering
plot(df.features$area_mean, df.features$concavity_mean, col=cluster.labels, xlab="area", ylab="concavity",pch=df.size)
title("Visualization of cluster analysis results")
ClusterCenters.names <- df.pam4$medoids


df.numeric.pam2 <- pam(df.features, k=2, metric="euclidean")
(summary(df.numeric.pam2))

df.numeric.pam4 <- pam(df.features, k=4, metric="euclidean")
(summary(df.numeric.pam4))




plot(df.numeric.pam2)
fviz_cluster(df.numeric.pam2,repel = T)
fviz_cluster(df.numeric.pam2, df.features, ellipse.type="norm", repel = T)

plot(df.numeric.pam4)
fviz_cluster(df.numeric.pam4,repel = T)
fviz_cluster(df.numeric.pam4, df.features, ellipse.type="norm", repel = T)


#silhouette indices
sil.pam.k2 <- silhouette(x=df.numeric.pam2, dist=df.DissimilarityMatrix.mat)
sil.pam.k4 <- silhouette(x=df.numeric.pam4, dist=df.DissimilarityMatrix.mat)

#average silhouette values
(avg.sil.pam.k2 <- summary(sil.pam.k2)$clus.avg.widths)
(avg.sil.pam.k4 <- summary(sil.pam.k4)$clus.avg.widths)

#average silhouette values for whole partition
m1.pam<-mean(avg.sil.pam.k2)
m2.pam<-mean(avg.sil.pam.k4)

sil.pam.k2 <- silhouette(df.numeric.pam2$clustering, dist(df.features))
fviz_silhouette(sil.pam.k2, xlab="PAM")

sil.pam.k4 <- silhouette(df.numeric.pam4$clustering, dist(df.features))
fviz_silhouette(sil.pam.k4, xlab="PAM")


#best k
sil.avg.pam<-c()
for (kkk in 2:10){
  km <- pam(x=df.DissimilarityMatrix.mat, diss=TRUE, k=kkk)
  ss <- silhouette(km$cluster, dist(df.features))
  sil.avg.pam<-c(sil.avg.pam,mean(ss[,3]))
}

df.best.silh.ind.pam<-data.frame(silhouette_index = sil.avg.pam)
rownames(df.best.silh.ind.pam) <- c("2","3","4","5","6","7","8","9","10")

png("pamsilhouette.png", height=1000, width=1500)
p<-tableGrob(df.best.silh.ind.pam)
grid.arrange(p)
dev.off()


fviz_nbclust(df.features, FUNcluster = cluster::pam, method="wss", k.max=10)
fviz_nbclust(df.features, FUNcluster = cluster::pam, method="wss", k.max=10) + geom_vline(xintercept=3, linetype=2)
fviz_nbclust(df.features, FUNcluster = cluster::pam, method = "silhouette")
fviz_nbclust(df.features, FUNcluster = cluster::pam, method = "gap")

##AGNES
df.agnes.avg      <- agnes(x=df.DissimilarityMatrix.mat, diss=TRUE, method="average")
df.agnes.single   <- agnes(x=df.DissimilarityMatrix.mat, diss=TRUE, method="single")
df.agnes.complete <- agnes(x=df.DissimilarityMatrix.mat, diss=TRUE, method="complete")

plot(df.agnes.avg,which.plots=2,main="AGNES: average linkage")
plot(df.agnes.single,which.plots=2,main="AGNES: single linkage")
plot(df.agnes.complete,which.plots=2, main="AGNES: complete linkage")

#cutting into clusters
(df.agnes.avg.k2 <- cutree(df.agnes.avg, k=2))
(df.agnes.avg.k4 <- cutree(df.agnes.avg, k=4)) 

(df.agnes.single.k2 <- cutree(df.agnes.single, k=2))
(df.agnes.single.k4 <- cutree(df.agnes.single, k=4)) 

(df.agnes.complete.k2 <- cutree(df.agnes.complete, k=2))
(df.agnes.complete.k4 <- cutree(df.agnes.complete, k=4)) 



cluster.labels <- df.agnes.avg.k2
plot(df.features$area_mean, df.features$concavity_mean, col=cluster.labels, xlab="area", ylab="concavity",pch=df.real.class.labels)
title("Visualization of cluster analysis results")
ClusterCenters.names <- df.pam2$medoids

#comparison of clustering vs real labels
table.agnes.avg.k2<-(table(df.agnes.avg.k2 , df.num$binary_diagnosis))
table.agnes.avg.k4<-(table(df.agnes.avg.k4 , df.size.num$numsize))

table.agnes.single.k2<-(table(df.agnes.single.k2 , df.num$binary_diagnosis))
table.agnes.single.k4<-(table(df.agnes.single.k4 , df.size.num$numsize))

table.agnes.complete.k2<-(table(df.agnes.complete.k2 , df.num$binary_diagnosis))
table.agnes.complete.k4<-(table(df.agnes.complete.k4 , df.size.num$numsize))

matchClasses(table.agnes.avg.k2)
a2<-compareMatchedClasses(df.agnes.avg.k2, df.num$binary_diagnosis)$diag
matchClasses(table.agnes.avg.k2, method="exact")
a4<-compareMatchedClasses(df.agnes.avg.k2, df.num$binary_diagnosis, method="exact")$diag

matchClasses(table.agnes.avg.k4)
b2<-compareMatchedClasses(df.agnes.avg.k4, df.size.num$numsize)$diag
matchClasses(table.agnes.avg.k4, method="exact")
b4<-compareMatchedClasses(df.agnes.avg.k4, df.size.num$numsize, method="exact")$diag

matchClasses(table.agnes.single.k2)
c2<-compareMatchedClasses(df.agnes.single.k2, df.num$binary_diagnosis)$diag
matchClasses(table.agnes.single.k2, method="exact")
c4<-compareMatchedClasses(df.agnes.single.k2, df.num$binary_diagnosis, method="exact")$diag

matchClasses(table.agnes.single.k4)
d2<-compareMatchedClasses(df.agnes.single.k4, df.size.num$numsize)$diag
matchClasses(table.agnes.single.k4, method="exact")
d4<-compareMatchedClasses(df.agnes.single.k4, df.size.num$numsize, method="exact")$diag

matchClasses(table.agnes.complete.k2)
e2<-compareMatchedClasses(df.agnes.complete.k2, df.num$binary_diagnosis)$diag
matchClasses(table.agnes.complete.k2, method="exact")
e4<-compareMatchedClasses(df.agnes.complete.k2, df.num$binary_diagnosis, method="exact")$diag

matchClasses(table.agnes.complete.k4)
f2<-compareMatchedClasses(df.agnes.complete.k4, df.size.num$numsize)$diag
matchClasses(table.agnes.complete.k4, method="exact")
f4<-compareMatchedClasses(df.agnes.complete.k4, df.size.num$numsize, method="exact")$diag

col1<- c(a2,b2,c2,d2,e2,f2)
col2<- c(a4,b4,c4,d4,e4,f4)
tab <- matrix(c(a4,c4,e4,b4,d4,f4), ncol=2, byrow=TRUE)
colnames(tab) <- c('k=2','k=4')
rownames(tab) <- c('average','single','complete')
tab <- as.table(tab)


png("tableagnes.png", height=1000, width=1500)
p<-tableGrob(tab)
grid.arrange(p)
dev.off()


#silhouette indices
sil.agnes.avg.k2 <- silhouette(x=df.agnes.avg.k2, dist=df.DissimilarityMatrix.mat)
sil.agnes.avg.k4 <- silhouette(x=df.agnes.avg.k4, dist=df.DissimilarityMatrix.mat)

sil.agnes.single.k2 <- silhouette(x=df.agnes.single.k2, dist=df.DissimilarityMatrix.mat)
sil.agnes.single.k4 <- silhouette(x=df.agnes.single.k4, dist=df.DissimilarityMatrix.mat)

sil.agnes.complete.k2 <- silhouette(x=df.agnes.complete.k2, dist=df.DissimilarityMatrix.mat)
sil.agnes.complete.k4 <- silhouette(x=df.agnes.complete.k4, dist=df.DissimilarityMatrix.mat)

#average silhouette values
(avg.sil.agnes.avg.k2 <- summary(sil.agnes.avg.k2)$clus.avg.widths)
(avg.sil.agnes.avg.k4 <- summary(sil.agnes.avg.k4)$clus.avg.widths)

(avg.sil.agnes.single.k2 <- summary(sil.agnes.single.k2)$clus.avg.widths)
(avg.sil.agnes.single.k4 <- summary(sil.agnes.single.k4)$clus.avg.widths)

(avg.sil.agnes.complete.k2 <- summary(sil.agnes.complete.k2)$clus.avg.widths)
(avg.sil.agnes.complete.k4 <- summary(sil.agnes.complete.k4)$clus.avg.widths)

#average silhouette values for whole partition
m1<-mean(avg.sil.agnes.avg.k2)
m2<-mean(avg.sil.agnes.avg.k4)
m3<-mean(avg.sil.agnes.single.k2)
m4<-mean(avg.sil.agnes.single.k4)
m5<-mean(avg.sil.agnes.complete.k2)
m6<-mean(avg.sil.agnes.complete.k4)

sil.agnes.avg.k2 <- silhouette(df.agnes.avg.k2, dist(df.features))
fviz_silhouette(sil.agnes.avg.k2, xlab="Agnes - average")

sil.agnes.avg.k4 <- silhouette(df.agnes.avg.k4, dist(df.features))
fviz_silhouette(sil.agnes.avg.k4, xlab="Agnes - average")

sil.agnes.single.k2 <- silhouette(df.agnes.single.k2, dist(df.features))
fviz_silhouette(sil.agnes.single.k2, xlab="Agnes - single")

sil.agnes.single.k4 <- silhouette(df.agnes.single.k4, dist(df.features))
fviz_silhouette(sil.agnes.single.k4, xlab="Agnes - single")

sil.agnes.complete.k2 <- silhouette(df.agnes.complete.k2, dist(df.features))
fviz_silhouette(sil.agnes.complete.k2, xlab="Agnes - complete")

sil.agnes.complete.k4 <- silhouette(df.agnes.complete.k4, dist(df.features))
fviz_silhouette(sil.agnes.complete.k4, xlab="Agnes - complete")


#for which k and which method the silhouette index in the best?
sil.avg<-c()
sil.single <-c()
sil.complete<-c()
for (k_k in 2:10){
  sil.avg<-c(sil.avg, mean(summary(silhouette(x=cutree(df.agnes.avg, k=k_k), dist=df.DissimilarityMatrix.mat))$clus.avg.widths))
  sil.single<-c(sil.single, mean(summary(silhouette(x=cutree(df.agnes.single, k=k_k), dist=df.DissimilarityMatrix.mat))$clus.avg.widths))
  sil.complete<-c(sil.complete, mean(summary(silhouette(x=cutree(df.agnes.complete, k=k_k), dist=df.DissimilarityMatrix.mat))$clus.avg.widths))
}



png("silhouetteagnes.png", height=1000, width=1500)
p<-tableGrob(df.best.silh.ind)
grid.arrange(p)
dev.off()

df.best.silh.ind<-data.frame(average = sil.avg, simple = sil.single, complete = sil.complete)
rownames(df.best.silh.ind) <- c("2","3","4","5","6","7","8","9","10")

fviz_dend(df.agnes.avg, cex=0.4, main="Dendrogram - average linkage")
fviz_dend(df.agnes.single, cex=0.4, main="Dendrogram - single linkage")
fviz_dend(df.agnes.complete, cex=0.4, main="Dendrogram - complete linkage")

fviz_dend(df.agnes.avg, k=2, cex=0.4, show_labels = F)
fviz_dend(df.agnes.single, k=2, cex=0.4, show_labels = F)
fviz_dend(df.agnes.complete, k=2, cex=0.4, show_labels = F)

fviz_dend(df.agnes.avg, k=4, cex=0.4, show_labels = F)
fviz_dend(df.agnes.single, k=4, cex=0.4, show_labels = F)
fviz_dend(df.agnes.complete, k=4, cex=0.4, show_labels = F)

#cluster plot
df.features.agnes <- agnes(df.features, metric="euclidean", method="complete", stand=TRUE)
df.features.agnes.k4 <- cutree(df.features.agnes, k=4)
fviz_dend(df.features.agnes, k=4)
fviz_cluster(list(data=df.features, cluster=df.features.agnes.k4), repel = T)

df.features.agnes.k2 <- cutree(df.features.agnes, k=2)
fviz_dend(df.features.agnes, k=2)
fviz_cluster(list(data=df.features, cluster=df.features.agnes.k2), repel = T)


###DIANA

df.diana <- diana(x=df.DissimilarityMatrix.mat, diss=TRUE)
X11() 
par(cex=0.6)
plot(df.diana, main="DIANA")

(df.diana.k2 <- cutree(df.diana, k=2))
(df.diana.k4 <- cutree(df.diana, k=4))

#comparison of clustering vs real labels
table.diana.k2<-(table(df.diana.k2 , df.num$binary_diagnosis))
table.diana.k4<-(table(df.diana.k4 , df.size.num$numsize))

matchClasses(table.diana.k2)
compareMatchedClasses(df.diana.k2, df.num$binary_diagnosis)$diag
matchClasses(table.diana.k2, method="exact")
compareMatchedClasses(df.diana.k2, df.num$binary_diagnosis, method="exact")$diag

matchClasses(table.diana.k4)
compareMatchedClasses(df.diana.k4, df.size.num$numsize)$diag
matchClasses(table.diana.k4, method="exact")
compareMatchedClasses(df.diana.k4, df.size.num$numsize, method="exact")$diag

#silhouette indices
sil.diana.k2 <- silhouette(x=df.diana.k2, dist=df.DissimilarityMatrix.mat)
sil.diana.k4 <- silhouette(x=df.diana.k4, dist=df.DissimilarityMatrix.mat)

#average silhouette values
(avg.sil.diana.k2 <- summary(sil.diana.k2)$clus.avg.widths)
(avg.sil.diana.k4 <- summary(sil.diana.k4)$clus.avg.widths)

#average silhouette values for whole partition
m1.diana<-mean(avg.sil.diana.k2)
m2.diana<-mean(avg.sil.diana.k4)


png("tablediana.png", height=1000, width=1500)
p<-tableGrob(df.best.silh.ind.dia)
grid.arrange(p)
dev.off()


df.features.diana.k2 <- cutree(df.diana, k=2)
fviz_cluster(list(data=df.features, cluster=df.features.diana.k2), repel = T)

df.features.diana.k4 <- cutree(df.diana, k=4)
fviz_cluster(list(data=df.features, cluster=df.features.diana.k4), repel = T)

sil.diana.k2 <- silhouette(df.diana.k2, dist(df.features))
fviz_silhouette(sil.diana.k2, xlab="diana")

sil.diana.k4 <- silhouette(df.diana.k4, dist(df.features))
fviz_silhouette(sil.diana.k4, xlab="diana")

#best k
sil.dia.avg<-c()
for (kkk in 2:10){
  sil.dia.avg<-c(sil.dia.avg, mean(summary(silhouette(x=cutree(df.diana, k=kkk), dist=df.DissimilarityMatrix.mat))$clus.avg.widths))
 }


df.best.silh.ind.dia<-data.frame(average = sil.dia.avg)
rownames(df.best.silh.ind.dia) <- c("2","3","4","5","6","7","8","9","10")
#best k = 4


fviz_dend(df.diana, k=2, cex=0.4, show_labels = F)
fviz_dend(df.diana, k=4, cex=0.4, show_labels = F)

#partition agreement
K.max <- 10
partition.agreement1 <- numeric(K.max)
partition.agreement2 <- numeric(K.max)
partition.agreement3 <- numeric(K.max)
partition.agreement4 <- numeric(K.max)
partition.agreement5 <- numeric(K.max)
partition.agreement6 <- numeric(K.max)

for (K in 2:K.max)
{
  pam.k <- pam(df.DissimilarityMatrix.mat, k=K)$clustering
  kmeans.k <- kmeans(x=df.features, centers=K, nstart = 10)$cluster
  agnes.avg<- agnes(x=df.DissimilarityMatrix.mat, diss=TRUE, method="average")
  agnes.avg.kk <- cutree(df.agnes.avg, k=K)
  diana <- diana(x=df.DissimilarityMatrix.mat, diss=TRUE)
  diana.kk <- cutree(df.diana, k=K)
  matchClasses(table(pam.k, kmeans.k), method="exact")
  part.agreement1 <- compareMatchedClasses(pam.k, kmeans.k, method="exact")$diag
  partition.agreement1[K] <- part.agreement1
  
  matchClasses(table(agnes.avg.kk, kmeans.k), method="exact")
  part.agreement2 <- compareMatchedClasses(agnes.avg.kk, kmeans.k, method="exact")$diag
  partition.agreement2[K] <- part.agreement2
  
  matchClasses(table(diana.kk, kmeans.k), method="exact")
  part.agreement3 <- compareMatchedClasses(diana.kk, kmeans.k, method="exact")$diag
  partition.agreement3[K] <- part.agreement3
  
  matchClasses(table(pam.k, diana.kk), method="exact")
  part.agreement4 <- compareMatchedClasses(pam.k, diana.kk, method="exact")$diag
  partition.agreement4[K] <- part.agreement4
  
  matchClasses(table(pam.k, agnes.avg.kk), method="exact")
  part.agreement5 <- compareMatchedClasses(pam.k, agnes.avg.kk, method="exact")$diag
  partition.agreement5[K] <- part.agreement5
  
  matchClasses(table(diana.kk, agnes.avg.kk), method="exact")
  part.agreement6 <- compareMatchedClasses(diana.kk, agnes.avg.kk, method="exact")$diag
  partition.agreement6[K] <- part.agreement6
}
par(mfrow=c(3,2))
plot(2:K.max, partition.agreement1[-1], pch="x", type="b", col="green",xlab="K (number of clusters)", ylab="partition agreement")
title("Partition agreement for k-means and PAM")
grid()

plot(2:K.max, partition.agreement2[-1], pch="x", type="b", col="red",xlab="K (number of clusters)", ylab="partition agreement")
title("Partition agreement for k-means and AGNES")
grid()

plot(2:K.max, partition.agreement3[-1], pch="x", type="b", col="blue",xlab="K (number of clusters)", ylab="partition agreement")
title("Partition agreement for k-means and DIANA")
grid()

plot(2:K.max, partition.agreement4[-1], pch="x", type="b", col="orange",xlab="K (number of clusters)", ylab="partition agreement")
title("Partition agreement for DIANA and PAM")
grid()

plot(2:K.max, partition.agreement5[-1], pch="x", type="b", col="purple",xlab="K (number of clusters)", ylab="partition agreement")
title("Partition agreement for AGNES and PAM")
grid()

plot(2:K.max, partition.agreement6[-1], pch="x", type="b", col="magenta",xlab="K (number of clusters)", ylab="partition agreement")
title("Partition agreement for AGNES and DIANA")
grid()


library(NbClust)

NbClust.results.1 <- NbClust(df.features, distance="euclidean", min.nc=2, max.nc=10, method="complete", index="all")

NbClust.results.1$All.index
NbClust.results.1$Best.nc
NbClust.results.1$Best.partition

# visualization of results with the aid of factoextra package
factoextra::fviz_nbclust(NbClust.results.1) + theme_minimal() + ggtitle("Optimal number of clusters")


diss_matrix <- dist(df.features, method = "manhattan", diag = FALSE)
NbClust.results.2 <- NbClust(df.features, diss=diss_matrix, distance=NULL, min.nc=2, max.nc=10, method="complete", index="all")

# visualization of results with the aid of factoextra package
factoextra::fviz_nbclust(NbClust.results.2) + theme_minimal() + ggtitle("Optimal number of clusters")




library(clValid)
library(mclust) 

methods <- c("hierarchical","kmeans", "diana", "fanny", "pam", "clara","method")

# range for number of clusters
K.range <- 2:6

internal.validation <- clValid(df.features, nClust=K.range, clMethods=methods, validation="internal")

summary(internal.validation)
optimalScores(internal.validation)

par(mfrow = c(2, 2))
plot(internal.validation, legend = FALSE, lwd=2)
#plot.new()
legend("center", clusterMethods(internal.validation), col=1:9, lty=1:9, pch=paste(1:9))


stability.validation <- clValid(df.features, nClust=K.range, clMethods=methods, validation="stability")
summary(stability.validation)
optimalScores(stability.validation)

par(mfrow = c(2,2))
plot(stability.validation, measure=c("APN","AD","ADM"), legend=FALSE, lwd=2)
plot.new()
legend("center", clusterMethods(stability.validation), col=1:9, lty=1:9, pch=paste(1:9))
par(mfrow = c(1,1))




#############################################################3
#Dimensionality reduction

##PCA

plot(df.features$radius_mean, df.features$texture_mean,pch = 16)
data.pca <- df.features
data.after.pca <- prcomp(data.pca, retx=T, center=T, scale.=T) 
plot(data.after.pca$x[,1], data.after.pca$x[,2], pch=16)

variance <- (data.after.pca$sdev ^2)/sum(data.after.pca$sdev^2)
cumulative.variance <- cumsum(variance)
barplot(variance)
barplot(cumulative.variance,xlab='amount of variance explained by subsequent principal components')

(eigenvalues <- get_eigenvalue(data.after.pca))

fviz_eig(data.after.pca, addlabels = TRUE)

(variables <- get_pca_var(data.after.pca))


fviz_pca_var(data.after.pca)

corrplot(variables$cor)
corrplot(cor(data.pca))

fviz_contrib(data.after.pca, choice="var", axes=1)
fviz_contrib(data.after.pca, choice="var", axes=1, top=6)

fviz_contrib(data.after.pca, choice="var", axes=2)
fviz_contrib(data.after.pca, choice="var", axes=2, top=6)

fviz_contrib(data.after.pca, choice="var", axes=3)
fviz_contrib(data.after.pca, choice="var", axes=3, top=6)


fviz_pca_var(data.after.pca, col.var ="contrib", gradient.cols=c("#00AFBB","#E7B800","#FC4E07"))



fviz_pca_ind(data.after.pca, col.ind = df_cancer$diagnosis, addEllipses = TRUE, label = 'var')
fviz_pca_ind(data.after.pca, col.ind = df_parameters_cat$size_of_the_tumor, addEllipses = TRUE, label = 'var')

fviz_pca_biplot(data.after.pca, label="var", col.ind = df_parameters_cat$size_of_the_tumor)



##MDS

dissimilarities <- daisy(df.features, stand=T)
dis.matrix <- as.matrix(dissimilarities)

mds.k2 <- cmdscale(dis.matrix, k=2)
dist.mds.k2 <- dist(mds.k2, method="euclidean") # we compute Euclidean distances in the new space
dist.mds.k2 <- as.matrix(dist.mds.k2)

mds.results <- cmdscale(dis.matrix, k=2)
plot(mds.results[,1], mds.results[,2], pch=16)


# STRESS criterion
dis.original <- dis.matrix
STRESS <- sum((dis.original-dist.mds.k2)^2)

# Shepard diagram
plot(dis.original,dist.mds.k2, main="Shepard diagram", cex=0.5, xlab="original distance", ylab="distance after MDS mapping")
abline(coef=c(0,1), col="red", lty=2) # diagonal


d.max <- 9
stress.vec <- numeric(d.max)

par(mfrow=c(3,3))

for (d in 1:d.max)
{
  mds.k <- cmdscale(dis.matrix, k = d)
  dist.mds.k <- dist(mds.k, method="euclidean") # we compute Euclidean distances in the new space
  dis.original <- dis.matrix
  dist.mds.k <- as.matrix(dist.mds.k)
  STRESS <- sum((dis.original-dist.mds.k)^2)
  
  stress.vec[d] <- STRESS
  
  # Shepard diagram
  plot(dis.original,dist.mds.k, main=paste0("Shepard diagram (d=",d,")"),
       cex=0.5, xlab="original distance",  ylab="distance after MDS mapping")
  abline(coef=c(0,1), col="red", lty=2)
  grid()
  legend(x="topleft",legend=paste("STRESS = ",signif(STRESS,3)), bg="azure2")
}
par(mfrow=c(1,1))
plot(1:d.max, stress.vec, lwd=2, type="b", pch=19, xlab="dimension (d)", ylab="STRESS")
title("STRESS vs. dimension")
grid()







