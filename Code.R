library("FSelector")
library("e1071")
library("caret")

movie <- read.csv("movie.csv")
# selecting attributes
z<-c(3,5,6,10:39,8)
mv<-movie[z]
mv<-cbind(movie[1:3],movie[5:35],movie[4])
# A, train test split

idx<-sample(1:nrow(mv),70)
mvtrn<-mv[idx,]
mvtst<-mv[-idx,]


# Establishing the importance of facebook like feature (Include this chart in Paper)
qplot(Target,log.end.average., data = mvtrn, geom = "boxplot",main='Facebook likes across 3 classes',xlab="Box Office", ylab="Ratio of FB Likes", color=Target)



# With all features
# define training control
train_control <- trainControl(method="cv", number=10)
# fix the parameters of the algorithm
grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
# train the model
model <- train(Target~., data=mv, trControl=train_control, method="ctree")
# summarize results
print(model)

# Use different models and record the results

plot(model$finalModel, uniform=TRUE,  main="Classification Tree for Movie")
text(model$finalModel, use.n=TRUE, all=TRUE, cex=.8)

