claire_classifier <- function(train_data,train_labels,test_data,k,prior,m,ntree,kernel,cost)
{
  library(class)
  library(klaR)
  library(randomForest)
  library(e1071)
  
  l = nrow(test_data)
  num_labels <- length(unique(train_labels))
  if(missing(prior))
  {
    prior <- rep(1/num_labels,num_labels)
  }
  if(missing(k))
  {
    k=3
  }
  if(missing(m))
  {
    m <- floor(sqrt(ncol(train_data)))
  }
  if(missing(ntree))
  {
    ntree=500
  }
  if(missing(kernel))
  {
    kernel="linear"
  }
  if(missing(cost))
  {
    cost=1
  }
  
  knn_classifier <- knn(train_data,test_data,train_labels,k=k) #k nærmeste nabo
  rf_classifier <- randomForest(train_labels~ ., data=train_data,classwt=prior,mtry=m,ntree=ntree) #random forest, ignorant prior
  rf_pred <- predict(rf_classifier,test_data)
  nb_classifier <- NaiveBayes(train_labels~.,data=train_data,usekernel=TRUE,prior=prior,fL=1) #naive bayes, ignorant prior, laplace correction
  nb_pred <- predict(nb_classifier,test_data)
  svm_classifier <- svm(train_labels~.,data=train_data,kernel=kernel,cost=cost) #support vector machine
  svm_pred <- predict(svm_classifier,test_data)
  lda_classifier <- lda(train_labels~.,data=train_data,prior=prior) #LDA, ignorant prior
  lda_pred <- predict(lda_classifier,test_data)
  
  all_classifications = cbind(as.vector.factor(knn_classifier),as.vector.factor(rf_pred),
                              as.vector.factor(nb_pred$class),as.vector.factor(svm_pred),as.vector.factor(lda_pred))
  final = rep("",l)
  for (i in 1:l)
  {
    n = table(all_classifications[i,])
    final[i] = sample(names(n[which(n==max(n))]),1) #majority vote mellom ett eller flere anlegg som har max konsensus
  }
  return(final)
}