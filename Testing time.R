train <- read.csv("~/Desktop/train.csv")


#cleaning

any(is.na(train))
#no missing values

library(dplyr)

train<-train%>%mutate_if(is.character,as.factor)

train<-train[-884,]

train<-train%>%mutate_if(is.integer,as.factor)

train$ID<-as.integer(train$ID)

train<-train[,-1]

train<-select(train,-c(X11,X93,X107,X233,X289,X297,X293,X330,X347,X268,X235))
train$X290<-NULL

#model

library(h2o)

h2o.init()              

data<-as.h2o(train)

splits<-h2o.splitFrame(data=data,ratios = c(0.8,0.1))

train_h20<-splits[[1]]

combine1<-splits[[2]]

combine2<-splits[[3]]

valid<-h2o.rbind(combine1,combine2)


y<-"y"

x<-setdiff(names(train),"y")


aml<-h2o.automl(y=y,x=x,training_frame = train_h20,validation_frame = valid,max_models = 10)


lb<-aml@leaderboard

model_id<-as.data.frame(lb$model_id)[,1]

best_family<-h2o.getModel(grep("StackedEnsemble_BestOfFamily",model_id,value = TRUE)[1])



#new data

test<-test%>%mutate_if(is.character,as.factor)

test<-test%>%mutate_if(is.integer,as.factor)

test$ID<-as.integer(test$ID)

id<-test$ID

test<-test[,-1]

test<-select(test,-c(X11,X93,X107,X233,X289,X297,X293,X330,X347,X268,X235))
test$X290<-NULL


final_test<-as.h2o(test)

pred<-h2o.predict(best_family,newdata=final_test)

pred<-as.data.frame(pred)


submit<-cbind(id,pred)

names(submit)<-c("ID","y")

write.csv(submit,file="submit.csv",row.names = FALSE)
