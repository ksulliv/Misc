library(randomForest)
library(tree)
library(class) 
library(car)
library(caret)

rm(list=ls())
Obesity=read.csv(file.choose(),header=T, stringsAsFactors=TRUE) 

#Use explanatory data analysis to examine the predictors. Could you find any variables
#are the potential predictors of Sales from your plots? Justify your answer.
#You can create one pairwise scatterplot for all variables. Or you can create multiple
#scatterplots and boxplots for each variable. 

head(Obesity)
dim(Obesity)
str(Obesity)
summary(Obesity)
names(Obesity)

#create BMI
Obesity$BMI<-Obesity$Weight/Obesity$Height^2

#Convert variables to factors
Obesity$FCVC=round(Obesity$FCVC,0)
Obesity$NCP=round(Obesity$NCP,0)
Obesity$CH2O=round(Obesity$CH2O,0)
Obesity$FAF=round(Obesity$FAF,0)
Obesity$TUE=round(Obesity$TUE,0)

Obesity$FCVC<-as.factor(Obesity$FCVC)
Obesity$NCP<-as.factor(Obesity$NCP)
Obesity$CH2O<-as.factor(Obesity$CH2O)
Obesity$FAF<-as.factor(Obesity$FAF)
Obesity$TUE<-as.factor(Obesity$TUE)

str(Obesity)

#Check for missing values - no missing values
any(is.na(Obesity))

#a. Scatterplot
pairs(Obesity)

#b. Histogram - BMi
hist(Obesity$BMI, breaks=10, col="red", xlab="BMI", xlim=c(0,50), main="Histogram of BMI")

#Boxplot - width shows sample size
par(mfrow=c(1,1))
plot(Obesity$Gender,Obesity$BMI,col="green",varwidth=T,xlab="Gender",ylab="BMI",main="Gender vs BMI",cex.lab=1.5)
plot(Obesity$SMOKE,Obesity$BMI,col="green",varwidth=T,xlab="SMOKE",ylab="BMI",main="SMOKE vs BMI",cex.lab=1.5)
plot(Obesity$CH2O,Obesity$BMI,col="green",varwidth=T,xlab="CH2O",ylab="BMI",main="CH2O vs BMI",cex.lab=1.5)
plot(Obesity$FAF,Obesity$BMI,col="green",varwidth=T,xlab="FAF",ylab="BMI",main="FAF vs BMI",cex.lab=1.5)
plot(Obesity$TUE,Obesity$BMI,col="green",varwidth=T,xlab="TUE",ylab="BMI",main="TUE vs BMI",cex.lab=1.5)

par(mfrow=c(1,4))
plot(Obesity$family_history_with_overweight,Obesity$BMI,col="green",varwidth=T,xlab="family history with overweight",ylab="BMI",main="Family History with Overweight vs BMI",cex.lab=1.5)
plot(Obesity$FAVC,Obesity$BMI,col="green",varwidth=T,xlab="FAVC",ylab="BMI",main="FAVC vs BMI",cex.lab=1.5)
plot(Obesity$FCVC,Obesity$BMI,col="green",varwidth=T,xlab="FCVC",ylab="BMI",main="FCVC vs BMI",cex.lab=1.5)
plot(Obesity$NCP,Obesity$BMI,col="green",varwidth=T,xlab="NCP",ylab="BMI",main="NCP vs BMI",cex.lab=1.5)

par(mfrow=c(1,3))
plot(Obesity$CAEC,Obesity$BMI,col="green",varwidth=T,xlab="CAEC",ylab="BMI",main="CAEC vs BMI",cex.lab=1.5)
plot(Obesity$SCC,Obesity$BMI,col="green",varwidth=T,xlab="SCC",ylab="BMI",main="SCC vs BMI",cex.lab=1.5)
plot(Obesity$MTRANS,Obesity$BMI,col="green",varwidth=T,xlab="MTRANS",ylab="BMI",main="MTRANS vs BMI",cex.lab=1.5)

par(mfrow=c(1,1))
plot(Obesity$Age,Obesity$BMI,col="green",varwidth=T,xlab="Age",ylab="BMI",main="Age vs BMI",cex.lab=1.5)
cor(Obesity$Age, Obesity$BMI)

#Exploration of how features relate to NObeyesdad variable
par(mfrow=c(1,3))
plot(Obesity$NObeyesdad, Obesity$Age,col ="red", varwidth =T, xlab=" Obesity Type ",ylab =" Age  ",main="Age vs Obesity Type",cex.lab=1.5)
plot(Obesity$NObeyesdad, Obesity$Weight,col ="red", varwidth =T, xlab=" Obesity Type ",ylab ="Weight ",main="Weight vs Obesity Type",cex.lab=1.5)
plot(Obesity$NObeyesdad, Obesity$Height,col ="red", varwidth =T, xlab=" Obesity Type ",ylab ="Height ",main="Height vs Obesity Type",cex.lab=1.5)

#Proportions in each category of NObeyesdad
par(mfrow=c(1,1))
barplot(prop.table(table(Obesity$NObeyesdad)))

# Female Respondents
table(Obesity$NObeyesdad[Obesity$Gender == 'Female']) / nrow(Obesity)

# Male Respondents
table(Obesity$NObeyesdad[Obesity$Gender == 'Male']) / nrow(Obesity)

#Obesity Distribution
(table(Obesity$NObeyesdad) / nrow(Obesity))

#Create subset of data for analysis
Obesity_BMI <- subset (Obesity, select = -c(Height,Weight,NObeyesdad))

#Frequency of CALC Factor
table(Obesity_BMI$CALC)
#Only 1 member in always, (we need to exclude from analysis for linear regression
#Not useful, messing up CV 

#Regression Models to predict BMI

#Linear Regression
M1=lm(BMI~., data=Obesity_BMI)
summary(M1) 
vif(M1)

M2=lm(BMI~family_history_with_overweight+FCVC+CAEC+SCC+MTRANS+FAVC+NCP, data=Obesity_BMI)
summary(M2) 
vif(M2)

M3=lm(BMI~Obesity$FAVC*Obesity$NCP, data=Obesity_BMI)
summary(M3)

M4=lm(BMI~family_history_with_overweight+FCVC+CAEC+SCC+MTRANS+FAVC*NCP,data=Obesity_BMI)
summary(M4)

# Model Diagnostic Plots - Outliers
plot(predict(M1), residuals(M1))
plot(predict(M1), rstudent(M1))

plot(predict(M2), residuals(M2))
plot(predict(M2), rstudent(M2))

plot(predict(M3), residuals(M3))
plot(predict(M3), rstudent(M3))

plot(predict(M4), residuals(M4))
plot(predict(M4), rstudent(M4))

#Cross Validation for Model 2 and Model 4
set.seed(2)
k=10
M2CVMSE=rep(0,k)
M4CVMSE=rep(0,k)

folds=sample(1:k,nrow(Obesity_BMI),replace=TRUE)

for(j in 1:k)
{
  M2CV=lm(BMI~family_history_with_overweight+FCVC+CAEC+SCC+MTRANS+FAVC+NCP,data=Obesity_BMI[folds!=j,])
  M2CVMSE[j]=mean((Obesity_BMI$BMI-predict(M2CV,Obesity_BMI))[folds==j]^2)
}

for(j in 1:k)
{
  M4CV=lm(BMI~family_history_with_overweight+FCVC+CAEC+SCC+MTRANS+FAVC*NCP, data=Obesity_BMI[folds!=j,])
  M4CVMSE[j]=mean((Obesity_BMI$BMI-predict(M4CV,Obesity_BMI))[folds==j]^2)
}

## MSE Analysis #select model 4 because it has lowest MSE and highest R2
MeanM2MSE=mean(M2CVMSE) ###CVMSE-M2###
MeanM2MSE
MeanM4MSE=mean(M4CVMSE) ###CVMSE-M4###
MeanM4MSE

summary(M2CV)
summary(M4CV)

#Regression Tree for BMI

set.seed(3) 
train=sample(1:nrow(Obesity_BMI), nrow(Obesity_BMI)*0.8)
tree.obesity=tree(BMI~.,Obesity_BMI,subset=train)
cv.obesity=cv.tree(tree.obesity, K=10)
cv.obesity
plot(cv.obesity$size, cv.obesity$dev, type = "b")

par(mfrow=c(1,1))
prune.obesity=prune.tree(tree.obesity,best=13)
plot(prune.obesity)
text(prune.obesity, pretty=0)

obesity.test=Obesity_BMI[-train, "BMI"]
tree.pred=predict(prune.obesity,newdata=Obesity_BMI[-train,])
rt.mse = mean((tree.pred-obesity.test)^2)
rt.mse
#mean square error of the prediction in the testing set 16.8031 

#Bagging with BMI
set.seed(3)
bag.obesity=randomForest(BMI~.,data=Obesity_BMI,subset=train,mtry=14,importance=TRUE) 
bag.obesity

#eval. performance of bagging by fitting it to the testing set Obesity[-train,]
yhat.bag = predict(bag.obesity,newdata=Obesity_BMI[-train,]) 
bag.mse = mean((yhat.bag-obesity.test)^2)
bag.mse
#MSE of testing set 8.0439
importance(bag.obesity)
varImpPlot(bag.obesity)

#random forest with BMI
#test the MSE of this model by comparing the predicted values 
#with the true values. 
set.seed(3) 
rf.obesity=randomForest(BMI~.,data=Obesity_BMI,subset=train,mtry=5,importance=TRUE) 
yhat.rf = predict(rf.obesity,newdata=Obesity_BMI[-train,])  
rfbmi.accuracy = mean((yhat.rf-obesity.test)^2)  
rfbmi.accuracy
# MSE 8.1300

#Check for importance
importance(rf.obesity) 
varImpPlot(rf.obesity)

#Classification Models to predict NObeyesdad
#Create subset  by removing BMI as BMI was used to create NObeyesdad accorinding to data brief 
Obesity_class <- subset (Obesity, select = -c(BMI))
#Random Forest
set.seed(3) 
Obesity.train=sample(nrow(Obesity_class),1500)

#Test
Obesity.test=Obesity_class[-Obesity.train, ] 
NObeyesdad.test=Obesity_class$NObeyesdad[-Obesity.train]

set.seed(3) #want to keep same sample to compare to tree model
rf.model=randomForest(NObeyesdad~.,data=Obesity_class,subset=Obesity.train,mtry=5,importance=TRUE)

#Predictions
rf.model.pred=predict(rf.model,Obesity.test,type="class")
table(rf.model.pred,NObeyesdad.test)
confusionMatrix(rf.model.pred,NObeyesdad.test)
rf.accuracy = mean(rf.model.pred==NObeyesdad.test) 
rf.accuracy
#Accuracy = .9313

#Check for importance
importance(rf.model) 
varImpPlot(rf.model)

#Classification tree
tree.model=tree(NObeyesdad~.,Obesity_class,subset=Obesity.train)

#plot tree
plot(tree.model)
text(tree.model,pretty=0)

cv.model=cv.tree(tree.model,K=10,FUN=prune.misclass)
plot(cv.model$size, cv.model$dev, type = "b")
cv.model

#Best model has K=17
prune.model=prune.tree(tree.model,best=17)
plot(prune.model)
text(prune.model,pretty=0)

#Predictions
prunetree.pred=predict(prune.model,Obesity.test,type="class")
table(prunetree.pred,NObeyesdad.test)
confusionMatrix(prunetree.pred,NObeyesdad.test)
tree.accuracy=mean(prunetree.pred==NObeyesdad.test)
tree.accuracy
#Prediction Accuracy = .8462

#K-means
#Scatterplots to check if continuous features distinguish classes. Seems like Weight & Height does.
par(mfrow=c(1,3)) 
cols <- c("red","green","blue", "yellow", "black", "purple", "orange") 
plot(Obesity$Height ~ Obesity$Weight, data=Obesity, col=cols[Obesity$NObeyesdad], main="Obesity$Height ~ Obesity$Weight") 
legend(x=6.5, y=4.5, legend=levels(Obesity$NObeyesdad), col=cols, pch=1) 
plot(Obesity$Age ~ Obesity$Weight, data=Obesity, col=cols[Obesity$NObeyesdad], main="Obesity$Age ~ Obesity$Weight") 
legend(x=6.5, y=4.5, legend=levels(Obesity$NObeyesdad), col=cols, pch=1) 
plot(Obesity$Age ~ Obesity$Height, data=Obesity, col=cols[Obesity$NObeyesdad], main="Obesity$Age ~ Obesity$Height") 
legend(x=6.5, y=4.5, legend=levels(Obesity$NObeyesdad), col=cols, pch=1) 

#Get NObeyesdad
Obesity.labs=Obesity_class[,17]
#Select weight and height 
Obesity.data=Obesity_class[,c(3,4)] 

set.seed(3)
#Scale data
sd.data=scale(Obesity.data) 

#Seven clusters
km.out7 = kmeans (sd.data,7, nstart =20) 
km.out7$betweenss 
km.out7$withinss 
km.out7$tot.withinss 
km.out7$totss 
table(km.out7$cluster,Obesity.labs)

#Define cluster purity
purity <- function(clusters, classes) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

cluster.purity = purity(km.out7$cluster,Obesity.labs) #Sanity check 1021/2111 = .4837
cluster.purity
# = .4837
#Not surprised - kmeans can't use categorical data, Obeyesdad has relationships with categorical data like sex, smoke etc

#Hierarchical Clustering
data.sd.dist=dist(sd.data)
hc1=hclust(data.sd.dist) 
hc2=hclust(data.sd.dist, method="average") 
hc3=hclust(data.sd.dist, method="single") 

#Plots for each model
#plot(hc1, main="Complete Linkage", xlab="", sub="",ylab="") 
#plot(hc2, main="Average Linkage", xlab="", sub="",ylab="") 
#plot(hc3, main="Single Linkage", xlab="", sub="",ylab="") 

#Cut dendrogram to obtain 7 clusters. 
hc1.cluster=cutree(hc1,7)
hc2.cluster=cutree(hc2,7)
hc3.cluster=cutree(hc3,7)

#Cluster Purity - Average Linkage performs the best
table(hc1.cluster,Obesity.labs) #Cluster Purity = .4178
hc1.accuracy = purity(hc1.cluster,Obesity.labs) #882/2111
hc1.accuracy
table(hc2.cluster,Obesity.labs) #Cluster Purity = .42911
hc2.accuracy = purity(hc2.cluster,Obesity.labs)  #889/2111
hc2.accuracy
table(hc3.cluster,Obesity.labs) #Cluster Purity = .1772
hc3.accuracy = purity(hc3.cluster,Obesity.labs) #374/2111
hc3.accuracy

#KNN
standardized.Age=scale(Obesity$Age) 
standardized.Height=scale(Obesity$Height)
standardized.Weight=scale(Obesity$Weight) 
#standardized.BMI=scale(Obesity$BMI) 

Input.standard=cbind(standardized.Age,standardized.Height,standardized.Weight) 
accuracy=matrix(0,10,5) 

#Determine k using Cross Validation
set.seed(3) 
folds=sample(1:5,nrow(Input.standard),replace=TRUE) 
for (j in 1:10) 
{ 
  for(i in 1:5) 
  { 
    train.standard=Input.standard[folds!=i,] 
    test.standard=Input.standard[folds==i,] 
    train.truevalue=Obesity$NObeyesdad[folds!=i] 
    test.truevalue=Obesity$NObeyesdad[folds==i] 
    knn.pred=knn(train.standard,test.standard,train.truevalue,k=j) 
    accuracy[j,i]=mean(knn.pred==test.truevalue) 
  } 
}

cv.accuracy=apply(accuracy,1,mean) 
cv.accuracy

#Even though 1nn has highest accuracy, it may overfit on new data so I'll go with 5nn as best k
knn5.accuracy=cv.accuracy[5]
knn5.accuracy
#1NN has highest accuracy of .9569
#supervised work better than non-supervised.

#Dataframe with models and accuracy
accuracy.df <- data.frame(model = c("rf", "tree", "kmeans", "hc", "knn"),
                         accuracy = c(rf.accuracy, tree.accuracy, cluster.purity,hc2.accuracy, knn5.accuracy))

#Plot Classification Models and Accuracy - highest accuracy is random forest
par(mfrow=c(1,1))
bp<-barplot(accuracy.df$accuracy, names.arg = accuracy.df$model, ylab = "Accuracy (%)", xlab = "Type of Statistical Model", ylim= c(0, 1), main="Classification Models Performance")
text(bp, 0, round(accuracy.df$accuracy, 3),cex=1,pos=3)


#Plot Regression models performance - lowest mse is bagging
accuracy.df <- data.frame(model = c("rf", "tree", "bagging", "lm"),
                          mse = c(rfbmi.accuracy, rt.mse, bag.mse, MeanM4MSE))

par(mfrow=c(1,1))
bp<-barplot(accuracy.df$mse, names.arg = accuracy.df$model, ylab = "Mean Squared Error", xlab = "Type of Statistical Model", ylim = c(0,36), main="Regression Models Performance")
text(bp, 0, round(accuracy.df$mse, 3),cex=1,pos=3)
