# NOTE: "lafire" and "lafdtrain" both refer to the updated training data posted on CCLE, and
#       "test" and "lafdtest" both refer to the testing without response data posted on CCLE.

# Code for Tree Model
lafire <- lafire[complete.cases(lafire),]
lafire$year <- as.factor(lafire$year)
lafire$First.in.District <- as.factor(lafire$First.in.District)
lafire[which(lafire$year==2013),1] <- 1
lafire[which(lafire$year==2014),1] <- 2
lafire[which(lafire$year==2015),1] <- 3
lafire[which(lafire$year==2016),1] <- 4
levels(lafire$Unit.Type)[c(3,17,20,21,24,25,27,32,35,37)] <-
  c("other","other","other","other","other","other","other","other","other","other")
tree.lafire <- tree(elapsed_time ~.,lafire)
cv.X <- cv.tree(tree.lafire)
cv.X
prune.tree <- prune.tree(tree.lafire,best=4)
new.predict <- predict(prune.tree,test)
tree.predict <- predict(tree.lafire,test)

# Code for Lasso
i=seq(10,-2,length=100)
grid=10^i
  # Preprocessing Part 
lafdtrain$newUnit.Type=lafdtrain$Unit.Type
levels(lafdtrain$newUnit.Type)[c(3,7,10,12,14,17,20,21,22,24,25,27,32,33,35,37)]="Others"
lafdtrain$NewUnit.Type=lafdtrain$newUnit.Type
levels(lafdtrain$NewUnit.Type)[c(5,7,8,9,10,12,18,20,21,24,26)]="Others"
lafdtrain$newDispatch.Status=lafdtrain$Dispatch.Status
levels(lafdtrain$newDispatch.Status)[c(7,8,11,12)]="Others"
lafdtest$NewUnit.Type=lafdtest$`Unit Type`
levels(lafdtest$NewUnit.Type)[c(3,5,7:14,16,17,20,21,23,26,28,29,30:32,34,36,38)]="Others"
levels(lafdtest$NewUnit.Type)
lafdtest$NewDispatch.Status=as.factor(lafdtest$`Dispatch Status`)
table(lafdtest$'Dispatch Status')

levels(lafdtest$NewDispatch.Status)[c(7,8,11,12)]="Others"
# Coding the NAs in the Dispatch Sequence as 2
lafdtrain$Dispatch.Sequencenona=lafdtrain$Dispatch.Sequence
lafdtrain$Dispatch.Sequencenona[is.na(lafdtrain$Dispatch.Sequencenona)]=2

lafdtrain$Dispatch.Sequence15nona=rep(0,nrow(lafdtrain))
lafdtrain$Dispatch.Sequence15nona[which(lafdtrain$Dispatch.Sequencenona<=15)]=lafdtrain$Dispatch.Sequencenona[which(lafdtrain$Dispatch.Sequencenona<=15)]
lafdtrain$Dispatch.Sequence15upnona=rep(0,nrow(lafdtrain))
lafdtrain$Dispatch.Sequence15upnona[which(lafdtrain$Dispatch.Sequencenona>15)]=lafdtrain$Dispatch.Sequencenona[which(lafdtrain$Dispatch.Sequencenona>15)]

lafdtest$Dispatch.Sequencenona=lafdtest$Dispatch.Sequence
lafdtest$Dispatch.Sequencenona[is.na(lafdtest$Dispatch.Sequencenona)]=2

lafdtest$Dispatch.Sequence15nona=rep(0,nrow(lafdtest))
lafdtest$Dispatch.Sequence15nona[which(lafdtest$Dispatch.Sequencenona<=15)]=lafdtest$Dispatch.Sequencenona[which(lafdtest$Dispatch.Sequencenona<=15)]
lafdtest$Dispatch.Sequence15upnona=rep(0,nrow(lafdtest))
lafdtest$Dispatch.Sequence15upnona[which(lafdtest$Dispatch.Sequencenona>15)]=lafdtest$Dispatch.Sequencenona[which(lafdtest$Dispatch.Sequencenona>15)]

 # Lasso Part
  # Here we used a sample of 100000 observations from the NA-removed version of the original training
  # dataset to do the lasso.
lafdtrain3=lafdtrain[complete.cases(lafdtrain[c(3,4,9,11,13,14,16,17)]),]
set.seed(12345)
lafdtrain3=lafdtrain3[sample(1:nrow(lafdtrain3),100000),]
x5=model.matrix(elapsed_time~.,data=lafdtrain3[c(3,4,9,11,13,14,16,17)])[,-1]
y=lafdtrain3$elapsed_time
lasso.mod5=glmnet(x5,y,alpha=1,lambda=grid)
cv.out6=cv.glmnet(x5,y,alpha=1)
bestlam6=cv.out6$lambda.min
lafdtest$elapsed_time=NULL
newx1=model.matrix(~.,lafdtest[,-c(3,4,9,11,12,14,15)])[,-1]
lasso.pred5=predict(lasso.mod5,s=bestlam6,newx=newx1,type="response")



# Artificial Neural Network
lafire <- lafire[complete.cases(lafire),]
new1 <- lafire[which(lafire$Dispatch.Sequence < 16.346 & lafire$Dispatch.Sequence <
                       8.38764),]
new2 <- lafire[which(lafire$Dispatch.Sequence < 16.346 & lafire$Dispatch.Sequence >=
                       8.38764),]
new3 <- lafire[which(lafire$Dispatch.Sequence >= 16.346 & lafire$Dispatch.Sequence <
                       26.9571),]
new4 <- lafire[which(lafire$Dispatch.Sequence >= 16.346 & lafire$Dispatch.Sequence
                     >= 26.9571),]
library(nnet)
temp_model1 <- nnet(data = new1,
                    elapsed_time~First.in.District+Dispatch.Sequence+PPE.Level,
                    size = 10, linout = T, skip =T, maxit = 10000, decay = 0.001)
temp_model2 <- nnet(data = new2,
                    elapsed_time~First.in.District+Dispatch.Sequence+PPE.Level,
                    size = 10, linout = T, skip =T, maxit = 10000, decay = 0.001)
temp_model3 <- nnet(data = new3,
                    elapsed_time~First.in.District+Dispatch.Sequence+PPE.Level,
                    size = 10, linout = T, skip =T, maxit = 10000, decay = 0.001)
temp_model4 <- nnet(data = new4,
                    elapsed_time~First.in.District+Dispatch.Sequence+PPE.Level,
                    size = 10, linout = T, skip =T, maxit = 10000, decay = 0.001)
test1 <- test[which(test$Dispatch.Sequence < 16.346 & test$Dispatch.Sequence <
                      8.38764),]

test2 <- test[which(test$Dispatch.Sequence < 16.346 & test$Dispatch.Sequence >=
                      8.38764),]
test3 <- test[which(test$Dispatch.Sequence >= 16.346 & test$Dispatch.Sequence <
                      26.9571),]
test4 <- test[which(test$Dispatch.Sequence >= 16.346 & test$Dispatch.Sequence >=
                      26.9571),]
temp_predicted1 <- predict(temp_model1, test1)
temp_predicted2 <- predict(temp_model2, test2)
temp_predicted3 <- predict(temp_model3, test3)
temp_predicted4 <- predict(temp_model4, test4)


# XGBoost
library(Matrix)
library(xgboost)
traindata=na.omit(lafdtrain)
traindata=traindata[,c(3,7,8,9,11,15)]
modelmtx1=sparse.model.matrix(elapsed_time~.-1,data=traindata)
train=xgb.DMatrix(data=modelmtx1,label=traindata$elapsed_time)
best_param = list()
best_min_rmse = Inf
best_min_rmse_index = 0
for (iter in 1:5) {
  param <- list(objective = "reg:linear",
                eval_metric = "rmse",
                max_depth = sample(5:10, 1),
                eta = runif(1, 0, .3),
                gamma = runif(1, 0.0, 0.2),
                subsample = runif(1,0.5,0.8),
                colsample_bytree = runif(1,0.6,0.9)
)
  cv.nround = 100
  cv.nfold = 5
  mdcv <- xgb.cv(data=train, params = param, nthread=6, nfold=cv.nfold,
                 nrounds=cv.nround,verbose = TRUE)
  min_rmse = min(mdcv$evaluation_log[,"test_rmse_mean"])
  min_rmse_index = which.min(as.matrix(mdcv$evaluation_log[,"test_rmse_mean"]))
  if (min_rmse < best_min_rmse) {
    best_min_rmse = min_rmse
    best_min_rmse_index = min_rmse_index
    best_param = param
  }
}
nround = best_min_rmse_index

Training <-xgb.train(params = best_param, data = train, nrounds=nround,watchlist = list(train
                       = train),verbose = TRUE,print_every_n = 1,nthread = 6)
finaltest=lafdtest[,c(3,7,8,9,13)]
sparsemtxtest=sparse.model.matrix(~.-1,data=finaltest)
testdata=xgb.DMatrix(data=sparsemtxtest)
prediction=predict(Training,testdata)

# Making Importance Plots
bst=xgboost(modelmtx1,traindata$elapsed_time,max_depth=9,eta=0.2,gamma=0.12,
            subsample=0.64,colsample_bytree=0.62)
importance_matrix <- xgb.importance(colnames(traindata), model = bst)
xgb.plot.importance(importance_matrix, rel_to_first = TRUE, xlab = "Relative importance")
#install.packages("Ckmeans.1d.dp")
library(Ckmeans.1d.dp)
gg <- xgb.ggplot.importance(importance_matrix, measure = "Frequency", rel_to_first = TRUE)
gg + ggplot2::ylab("Frequency")


# XGBoost Using year as a factor, including First.in.District and using Unit.Type and 
# Dispatch.Status with some levels combined
lafdtrain$year=as.factor(lafdtrain$year)
traindata=na.omit(lafdtrain)
traindata=traindata[,c(3,4,9,11,13,14,15)]
modelmtx1=sparse.model.matrix(elapsed_time~.-1,data=traindata)
train=xgb.DMatrix(data=modelmtx1,label=traindata$elapsed_time)
for (iter in 1:10) {
  param <- list(objective = "reg:linear",
                eval_metric = "rmse",
                max_depth = sample(5:10, 1),
                eta = runif(1, 0, .3),
                gamma = runif(1, 0.0, 0.2),
                subsample = runif(1,0.5,0.8),
                colsample_bytree = runif(1,0.6,0.9)
  )
  cv.nround = 100
  cv.nfold = 5
  mdcv <- xgb.cv(data=train, params = param, nthread=6, nfold=cv.nfold,
                 nrounds=cv.nround,verbose = TRUE)
  min_rmse = min(mdcv$evaluation_log[,"test_rmse_mean"])
  min_rmse_index = which.min(as.matrix(mdcv$evaluation_log[,"test_rmse_mean"]))
  if (min_rmse < best_min_rmse) {
    best_min_rmse = min_rmse
    best_min_rmse_index = min_rmse_index
    best_param = param
  }
}
nround = best_min_rmse_index

Training <-xgb.train(params = best_param, data = train, nrounds=nround,watchlist = list(train
                                       = train),verbose = TRUE,print_every_n = 1,nthread = 6)
finaltest=lafdtest[,c(3,4,9,11,12,13)]
sparsemtxtest=sparse.model.matrix(~.-1,data=finaltest)
testdata=xgb.DMatrix(data=sparsemtxtest)
prediction=predict(Training,testdata)


# library(lubridate)
lafdtrain$creation.time=hms(lafdtrain$Incident.Creation.Time..GMT.)
lafdtrain$creation.hour=hour(lafdtrain$creation.time)

lafdtest$creation.time=hms(lafdtest$Incident.Creation.Time..GMT.)
lafdtest$creation.hour=hour(lafdtest$creation.time)

# Cutting the incident creation time into "day" and "night"
lafdtrain$hourdaynight=rep("day",nrow(lafdtrain))
lafdtrain$hourdaynight[which(lafdtrain$creation.hour<=6)]="night"
lafdtrain$hourdaynight[which(lafdtrain$creation.hour>18)]="night"
table(lafdtrain$hourdaynight)
lafdtest$hourdaynight=rep("day",nrow(lafdtest))
lafdtest$hourdaynight[which(lafdtest$creation.hour<=6)]="night"
lafdtest$hourdaynight[which(lafdtest$creation.hour>18)]="night"
table(lafdtest$hourdaynight)
lafdtrain$hourdaynight=as.factor(lafdtrain$hourdaynight)
lafdtest$hourdaynight=as.factor(lafdtest$hourdaynight)

 # In this case we did not tune the parameters using CV, but instead we picked random parameters 
 # in order to prevent overtuning. 
sparsematrix1=sparse.model.matrix(elapsed_time~.-1,data=lafdtrain[,c(4,9,11,13,14,15,20)])
traindata2=lafdtrain[complete.cases(lafdtrain[c(4,9,11,13,14,15,20)]),c(4,9,11,13,14,15,20)]
bst1=xgboost(data=sparsematrix1,label=traindata2$elapsed_time,max.depth=6,eta=0.3,nthread=6,nround=35)
sparsematrixtest1=sparse.model.matrix(~.-1,data=lafdtest[,c(4,9,11,12,13,18)])
pred1=predict(bst1,sparsematrixtest1)
summary(pred1)
importance_matrix=xgb.importance(colnames(traindata2), model = bst1)
xgb.plot.importance(importance_matrix, rel_to_first = TRUE, xlab = "Relative importance")
#install.packages("Ckmeans.1d.dp")
library(Ckmeans.1d.dp)
gg <- xgb.ggplot.importance(importance_matrix, measure = "Frequency", rel_to_first = TRUE)
gg + ggplot2::ylab("Frequency")

# Including the year variable (used it as a factor)
sparsematrix2=sparse.model.matrix(elapsed_time~.-1,data=lafdtrain[,c(3,4,9,11,13,14,15,20)])
traindata3=lafdtrain[complete.cases(lafdtrain[c(3,4,9,11,13,14,15,20)]),c(3,4,9,11,13,14,15,20)]
bst2=xgboost(data=sparsematrix2,label=traindata3$elapsed_time,max.depth=6,eta=0.3,nthread=6,nround=35)
sparsematrixtest2=sparse.model.matrix(~.-1,data=lafdtest[,c(3,4,9,11,12,13,18)])
pred2=predict(bst2,sparsematrixtest2)
summary(pred2)

importance_matrix <- xgb.importance(colnames(traindata3), model = bst2)
xgb.plot.importance(importance_matrix, rel_to_first = TRUE, xlab = "Relative importance")
#install.packages("Ckmeans.1d.dp")
library(Ckmeans.1d.dp)
gg <- xgb.ggplot.importance(importance_matrix, measure = "Frequency", rel_to_first = TRUE)
gg + ggplot2::ylab("Frequency")

# Cutting the creation time into 4 levels
lafdtrain$hour4levels=rep("1",nrow(lafdtrain))
lafdtrain$hour4levels[which(lafdtrain$creation.hour>=12 & lafdtrain$creation.hour<18)]="2"
lafdtrain$hour4levels[which(lafdtrain$creation.hour>=18)]="3"
lafdtrain$hour4levels[which(lafdtrain$creation.hour>=0 & lafdtrain$creation.hour<6)]="4"

lafdtest$hour4levels=rep("1",nrow(lafdtest))
lafdtest$hour4levels[which(lafdtest$creation.hour>=12 & lafdtest$creation.hour<18)]="2"
lafdtest$hour4levels[which(lafdtest$creation.hour>=18)]="3"
lafdtest$hour4levels[which(lafdtest$creation.hour>=0 & lafdtest$creation.hour<6)]="4"

lafdtrain$hour4levels=as.factor(lafdtrain$hour4levels)
lafdtest$hour4levels=as.factor(lafdtest$hour4levels)
table(lafdtrain$hour4levels)
table(lafdtest$hour4levels)


sparsematrix3=sparse.model.matrix(elapsed_time~.-1,data=lafdtrain[,c(3,4,9,11,13,14,15,21)])
traindata4=lafdtrain[complete.cases(lafdtrain[c(3,4,9,11,13,14,15,21)]),c(3,4,9,11,13,14,15,21)]
bst3=xgboost(data=sparsematrix3,label=traindata4$elapsed_time,max.depth=6,eta=0.3,nthread=6,nround=35)
sparsematrixtest3=sparse.model.matrix(~.-1,data=lafdtest[,c(3,4,9,11,12,13,19)])
pred3=predict(bst3,sparsematrixtest3)

# Without the year variable 
sparsematrix4=sparse.model.matrix(elapsed_time~.-1,data=lafdtrain[,c(4,9,11,13,14,15,21)])
traindata5=lafdtrain[complete.cases(lafdtrain[c(4,9,11,13,14,15,21)]),c(4,9,11,13,14,15,21)]
bst4=xgboost(data=sparsematrix4,label=traindata5$elapsed_time,max.depth=6,eta=0.3,nthread=6,nround=35)
sparsematrixtest4=sparse.model.matrix(~.-1,data=lafdtest[,c(4,9,11,12,13,19)])
pred4=predict(bst4,sparsematrixtest4)
summary(pred4)




# Cutting Dispatch.Sequencenona into <=15 and >15
lafdtrain$Dispatch.Sequence15nona=rep(0,nrow(lafdtrain))
lafdtrain$Dispatch.Sequence15nona[which(lafdtrain$Dispatch.Sequencenona<=15)]=lafdtrain$Dispatch.Sequencenona[which(lafdtrain$Dispatch.Sequencenona<=15)]
table(lafdtrain$Dispatch.Sequence15nona)

lafdtrain$Dispatch.Sequence15upnona=rep(0,nrow(lafdtrain))
lafdtrain$Dispatch.Sequence15upnona[which(lafdtrani$Dispatch.Sequence>15)]=lafdtrain$Dispatch.Sequencenona[which(lafdtrain$Dispatch.Sequencenona>15)]
table(lafdtrain$Dispatch.Sequence15upnona)

lafdtest$Dispatch.Sequence15nona=rep(0,nrow(lafdtest))
lafdtest$Dispatch.Sequence15nona[which(lafdtest$Dispatch.Sequencenona<=15)]=lafdtest$Dispatch.Sequencenona[which(lafdtest$Dispatch.Sequencenona<=15)]
table(lafdtest$Dispatch.Sequence15nona)

lafdtest$Dispatch.Sequence15upnona=rep(0,nrow(lafdtest))
lafdtest$Dispatch.Sequence15upnona[which(lafdtest$Dispatch.Sequence>15)]=lafdtest$Dispatch.Sequencenona[which(lafdtest$Dispatch.Sequencenona>15)]
table(lafdtest$Dispatch.Sequence15upnona)

# Doing Lasso
lafdtrain1=lafdtrain[complete.cases(lafdtrain),]
x=model.matrix(elapsed_time~.,data=lafdtrain[c(3,4,9,11,13,14,21,22,23)])[,-1]
y=lafdtrain1$elapsed_time
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)
cv.out=cv.glmnet(x,y,alpha=1)
bestlam=cv.out$lambda.min
lafdtest$elapsed_time=NULL
newx1=model.matrix(~.,lafdtest[,c(3,4,9,11,12,19,20,21)])[,-1]
lasso.pred3=predict(lasso.mod3,s=bestlam4,newx=newx1,type="response")

# Doing XGBoost
traindata=na.omit(lafdtrain)
traindata=traindata[,c(3,4,9,11,13,14,21,22,23)]
modelmtx1=sparse.model.matrix(elapsed_time~.-1,data=traindata)
train=xgb.DMatrix(data=modelmtx1,label=traindata$elapsed_time)
best_param = list()
best_min_rmse = Inf
best_min_rmse_index = 0
for (iter in 1:10) {
  param <- list(objective = "reg:linear",
                eval_metric = "rmse",
                max_depth = sample(5:10, 1),
                eta = runif(1, 0, .3),
                gamma = runif(1, 0.0, 0.2),
                subsample = runif(1,0.5,0.8),
                colsample_bytree = runif(1,0.6,0.9)
  )
  cv.nround = 100
  cv.nfold = 5
  mdcv <- xgb.cv(data=train, params = param, nthread=6, nfold=cv.nfold,
                 nrounds=cv.nround,verbose = TRUE)
  min_rmse = min(mdcv$evaluation_log[,"test_rmse_mean"])
  min_rmse_index = which.min(as.matrix(mdcv$evaluation_log[,"test_rmse_mean"]))
  if (min_rmse < best_min_rmse) {
    best_min_rmse = min_rmse
    best_min_rmse_index = min_rmse_index
    best_param = param
  }
}
nround = best_min_rmse_index

Training <-xgb.train(params = best_param, data = train, nrounds=nround,watchlist = list(train
                                     = train),verbose = TRUE,print_every_n = 1,nthread = 6)
finaltest=lafdtest[,c(3,4,9,11,12,19,20,21)]
sparsemtxtest=sparse.model.matrix(~.-1,data=finaltest)
testdata=xgb.DMatrix(data=sparsemtxtest)
prediction=predict(Training,testdata)

# Doing Random Forest
rndmfrst1=rpart(elapsed_time~.,data=lafdtrain[c(3,4,9,11,13,14,21,22,23)],method="anova")
predtest1=predict(rndmfrst1,lafdtest[c(3,4,9,11,12,19,20,21)])
summary(predtest1)

 # Using the training dataset without NAs
rndmfrst2=rpart(elapsed_time~.,data=traindata,method="anova")
predtest2=predict(rndmfrst2,lafdtest[c(3,4,9,11,12,19,20,21)])
summary(predtest2)


# Cutting the Datas into Four Parts Using the Cutoff-Points For Dispatch.Sequence suggested by 
# the Tree Model
traindata1 <- traindata[which(traindata$Dispatch.Sequencenona < 16.346 & traindata$Dispatch.Sequencenona <
                       8.38764),]
traindata2 <- traindata[which(traindata$Dispatch.Sequencenona < 16.346 & traindata$Dispatch.Sequencenona >=
                       8.38764),]
traindata3 <- traindata[which(traindata$Dispatch.Sequencenona >= 16.346 & traindata$Dispatch.Sequencenona <
                       26.9571),]
traindata4 <- traindata[which(traindata$Dispatch.Sequencenona >= 16.346 & traindata$Dispatch.Sequencenona
                     >= 26.9571),]


lafdtest1 <- lafdtest[which(lafdtest$Dispatch.Sequencenona < 16.346 & lafdtest$Dispatch.Sequencenona <
                      8.38764),]
lafdtest2 <- lafdtest[which(lafdtest$Dispatch.Sequencenona < 16.346 & lafdtest$Dispatch.Sequencenona >=
                      8.38764),]
lafdtest3 <- lafdtest[which(lafdtest$Dispatch.Sequencenona >= 16.346 & lafdtest$Dispatch.Sequencenona <
                      26.9571),]
lafdtest4 <- lafdtest[which(lafdtest$Dispatch.Sequencenona >= 16.346 & lafdtest$Dispatch.Sequencenona >=
                      26.9571),]


# Doing XGBoost on Each Part
modelmtx1=sparse.model.matrix(elapsed_time~.-1,data=traindata1)
train1=xgb.DMatrix(data=modelmtx1,label=traindata1$elapsed_time)
best_param = list()
best_min_rmse = Inf
best_min_rmse_index = 0
for (iter in 1:10) {
  param <- list(objective = "reg:linear",
                eval_metric = "rmse",
                max_depth = sample(5:10, 1),
                eta = runif(1, 0, .3),
                gamma = runif(1, 0.0, 0.2),
                subsample = runif(1,0.5,0.8),
                colsample_bytree = runif(1,0.6,0.9)
  )
  cv.nround = 100
  cv.nfold = 5
  mdcv <- xgb.cv(data=train, params = param, nthread=6, nfold=cv.nfold,
                 nrounds=cv.nround,verbose = TRUE)
  min_rmse = min(mdcv$evaluation_log[,"test_rmse_mean"])
  min_rmse_index = which.min(as.matrix(mdcv$evaluation_log[,"test_rmse_mean"]))
  if (min_rmse < best_min_rmse) {
    best_min_rmse = min_rmse
    best_min_rmse_index = min_rmse_index
    best_param = param
  }
}
nround = best_min_rmse_index

Training1 <-xgb.train(params = best_param, data = train1, nrounds=nround,watchlist = list(train
                                     = train1),verbose = TRUE,print_every_n = 1,nthread = 6)
finaltest1=lafdtest1[,c(3,4,9,11,12,19,20,21)]
sparsemtxtest1=sparse.model.matrix(~.-1,data=finaltest1)
testdata1=xgb.DMatrix(data=sparsemtxtest1)
prediction1=predict(Training1,testdata1)


modelmtx2=sparse.model.matrix(elapsed_time~.-1,data=traindata2)
train2=xgb.DMatrix(data=modelmtx2,label=traindata2$elapsed_time)
best_param = list()
best_min_rmse = Inf
best_min_rmse_index = 0
for (iter in 1:10) {
  param <- list(objective = "reg:linear",
                eval_metric = "rmse",
                max_depth = sample(5:10, 1),
                eta = runif(1, 0, .3),
                gamma = runif(1, 0.0, 0.2),
                subsample = runif(1,0.5,0.8),
                colsample_bytree = runif(1,0.6,0.9)
  )
  cv.nround = 100
  cv.nfold = 5
  mdcv <- xgb.cv(data=train, params = param, nthread=6, nfold=cv.nfold,
                 nrounds=cv.nround,verbose = TRUE)
  min_rmse = min(mdcv$evaluation_log[,"test_rmse_mean"])
  min_rmse_index = which.min(as.matrix(mdcv$evaluation_log[,"test_rmse_mean"]))
  if (min_rmse < best_min_rmse) {
    best_min_rmse = min_rmse
    best_min_rmse_index = min_rmse_index
    best_param = param
  }
}
nround = best_min_rmse_index

Training2 <-xgb.train(params = best_param, data = train2, nrounds=nround,watchlist = list(train
                                         = train2),verbose = TRUE,print_every_n = 1,nthread = 6)
finaltest2=lafdtest2[,c(3,4,9,11,12,19,20,21)]
sparsemtxtest2=sparse.model.matrix(~.-1,data=finaltest2)
testdata2=xgb.DMatrix(data=sparsemtxtest2)
prediction2=predict(Training2,testdata2)



modelmtx3=sparse.model.matrix(elapsed_time~.-1,data=traindata3)
train3=xgb.DMatrix(data=modelmtx3,label=traindata3$elapsed_time)
best_param = list()
best_min_rmse = Inf
best_min_rmse_index = 0
for (iter in 1:10) {
  param <- list(objective = "reg:linear",
                eval_metric = "rmse",
                max_depth = sample(5:10, 1),
                eta = runif(1, 0, .3),
                gamma = runif(1, 0.0, 0.2),
                subsample = runif(1,0.5,0.8),
                colsample_bytree = runif(1,0.6,0.9)
  )
  cv.nround = 100
  cv.nfold = 5
  mdcv <- xgb.cv(data=train, params = param, nthread=6, nfold=cv.nfold,
                 nrounds=cv.nround,verbose = TRUE)
  min_rmse = min(mdcv$evaluation_log[,"test_rmse_mean"])
  min_rmse_index = which.min(as.matrix(mdcv$evaluation_log[,"test_rmse_mean"]))
  if (min_rmse < best_min_rmse) {
    best_min_rmse = min_rmse
    best_min_rmse_index = min_rmse_index
    best_param = param
  }
}
nround = best_min_rmse_index

Training3 <-xgb.train(params = best_param, data = train3, nrounds=nround,watchlist = list(train
                                        = train3),verbose = TRUE,print_every_n = 1,nthread = 6)
finaltest3=lafdtest3[,c(3,4,9,11,12,19,20,21)]
sparsemtxtest3=sparse.model.matrix(~.-1,data=finaltest3)
testdata3=xgb.DMatrix(data=sparsemtxtest3)
prediction3=predict(Training3,testdata3)



modelmtx4=sparse.model.matrix(elapsed_time~.-1,data=traindata4)
train4=xgb.DMatrix(data=modelmtx4,label=traindata4$elapsed_time)
best_param = list()
best_min_rmse = Inf
best_min_rmse_index = 0
for (iter in 1:10) {
  param <- list(objective = "reg:linear",
                eval_metric = "rmse",
                max_depth = sample(5:10, 1),
                eta = runif(1, 0, .3),
                gamma = runif(1, 0.0, 0.2),
                subsample = runif(1,0.5,0.8),
                colsample_bytree = runif(1,0.6,0.9)
  )
  cv.nround = 100
  cv.nfold = 5
  mdcv <- xgb.cv(data=train, params = param, nthread=6, nfold=cv.nfold,
                 nrounds=cv.nround,verbose = TRUE)
  min_rmse = min(mdcv$evaluation_log[,"test_rmse_mean"])
  min_rmse_index = which.min(as.matrix(mdcv$evaluation_log[,"test_rmse_mean"]))
  if (min_rmse < best_min_rmse) {
    best_min_rmse = min_rmse
    best_min_rmse_index = min_rmse_index
    best_param = param
  }
}
nround = best_min_rmse_index

Training1 <-xgb.train(params = best_param, data = train4, nrounds=nround,watchlist = list(train
                                       = train4),verbose = TRUE,print_every_n = 1,nthread = 6)
finaltest4=lafdtest4[,c(3,4,9,11,12,19,20,21)]
sparsemtxtest4=sparse.model.matrix(~.-1,data=finaltest4)
testdata4=xgb.DMatrix(data=sparsemtxtest4)
prediction4=predict(Training4,testdata4)

xgboostpredcut4=data.frame(lafdtest$row.id,0)
xgboostpredcut4[lafdtest$Dispatch.Sequencenona<=8.38764,]=prediction1
xgboostpredcut4[lafdtest$Dispatch.Sequencenona<=16.346 & lafdtest$Dispatch.Sequencenona>8.38764,]=prediction2
xgboostpredcut4[lafdtest$Dispatch.Sequencenona>16.346 & lafdtest$Dispatch.Sequencenona<=26.9571,]=prediction3
xgboostpredcut4[lafdtest$Dispatch.Sequencenona>26.9571,]=prediction4
summary(xgboostpredcut4)


# Doing Lasso on each part
x1=model.matrix(elapsed_time~.,data=traindata1)[,-1]
y1=traindata1$elapsed_time
lasso.mod1=glmnet(x1,y1,alpha=1,lambda=grid)
cv.out1=cv.glmnet(x1,y1,alpha=1)
bestlam1=cv.out1$lambda.min
lafdtest1$elapsed_time=NULL
newx1=model.matrix(~.,lafdtest1[,c(3,4,9,11,12,19,20,21)])[,-1]
lasso.pred1=predict(lasso.mod1,s=bestlam1,newx=newx1,type="response")

x2=model.matrix(elapsed_time~.,data=traindata2)[,-1]
y2=traindata2$elapsed_time
lasso.mod2=glmnet(x2,y2,alpha=1,lambda=grid)
cv.out2=cv.glmnet(x2,y2,alpha=1)
bestlam2=cv.out2$lambda.min
lafdtest2$elapsed_time=NULL
newx2=model.matrix(~.,lafdtest2[,c(3,4,9,11,12,19,20,21)])[,-1]
lasso.pred2=predict(lasso.mod2,s=bestlam2,newx=newx2,type="response")

x3=model.matrix(elapsed_time~.,data=traindata3)[,-1]
y3=traindata3$elapsed_time
lasso.mod3=glmnet(x3,y3,alpha=1,lambda=grid)
cv.out3=cv.glmnet(x3,y3,alpha=1)
bestlam3=cv.out3$lambda.min
lafdtest3$elapsed_time=NULL
newx3=model.matrix(~.,lafdtest3[,c(3,4,9,11,12,19,20,21)])[,-1]
lasso.pred3=predict(lasso.mod3,s=bestlam3,newx=newx3,type="response")

x4=model.matrix(elapsed_time~.,data=traindata4)[,-1]
y4=traindata4$elapsed_time
lasso.mod4=glmnet(x4,y4,alpha=1,lambda=grid)
cv.out4=cv.glmnet(x4,y4,alpha=1)
bestlam4=cv.out4$lambda.min
lafdtest4$elapsed_time=NULL
newx4=model.matrix(~.,lafdtest4[,c(3,4,9,11,12,19,20,21)])[,-1]
lasso.pred4=predict(lasso.mod4,s=bestlam4,newx=newx4,type="response")


lassopredcut4=data.frame(lafdtest$row.id,0)
lassopredcut4[lafdtest$Dispatch.Sequencenona<=8.38764,]=lasso.pred1
lassopredcut4[lafdtest$Dispatch.Sequencenona<=16.346 & lafdtest$Dispatch.Sequencenona>8.38764,]=lasso.pred2
lassopredcut4[lafdtest$Dispatch.Sequencenona>16.346 & lafdtest$Dispatch.Sequencenona<=26.9571,]=lasso.pred3
lassopredcut4[lafdtest$Dispatch.Sequencenona>26.9571,]=lasso.pred4
summary(lassopredcut4)

# Doing Random Forest
rndmfrst1=rpart(elapsed_time~.,data=traindata1,method="anova")
predtest1=predict(rndmfrst1,lafdtest1[c(3,4,9,11,12,19,20,21)])
summary(predtest1)

rndmfrst2=rpart(elapsed_time~.,data=traindata2,method="anova")
predtest2=predict(rndmfrst2,lafdtest2[c(3,4,9,11,12,19,20,21)])
summary(predtest2)

rndmfrst3=rpart(elapsed_time~.,data=traindata3,method="anova")
predtest3=predict(rndmfrst3,lafdtest3[c(3,4,9,11,12,19,20,21)])
summary(predtest3)

rndmfrst4=rpart(elapsed_time~.,data=traindata4,method="anova")
predtest4=predict(rndmfrst4,lafdtest4[c(3,4,9,11,12,19,20,21)])
summary(predtest4)



rndmfrstpredcut4=data.frame(lafdtest$row.id,0)
rndmfrstpredcut4[lafdtest$Dispatch.Sequencenona<=8.38764,]=predtest1
rndmfrstpredcut4[lafdtest$Dispatch.Sequencenona<=16.346 & lafdtest$Dispatch.Sequencenona>8.38764,]=predtest2
rndmfrstpredcut4[lafdtest$Dispatch.Sequencenona>16.346 & lafdtest$Dispatch.Sequencenona<=26.9571,]=predtest3
rndmfrstpredcut4[lafdtest$Dispatch.Sequencenona>26.9571,]=predtest4
summary(rndmfrstpredcut4)
