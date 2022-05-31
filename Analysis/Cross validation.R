set.seed(2)

iterations = 10
rows = 1000
j = 1
variables = 4
output_cv <- matrix(ncol=variables, nrow=iterations)
output_final <- matrix(ncol=variables, nrow=iterations)

for(i in 1:iterations){
  Train_show <- dat_show_s[-(j:rows),]
  test_show <- dat_show_s[(j:rows),]
  
  
  y_show <- as.numeric(as.factor(Train_show$Breakout))-1
  y.test_show <- as.matrix(as.numeric(as.factor(test_show$Breakout))-1)
  x_show <- as.data.frame(Train_show[,c(2,4)])
  x.test_show <- as.data.frame(test_show[,c(2,4)])
  
  model <- SuperLearner(y_show,
                        x_show,
                        family=binomial(),
                        SL.library=list("SL.glm", "SL.knn", "SL.nnet" , "SL.svm"))
  
  
  predictions_show <- predict.SuperLearner(model, 
                                           newdata = x_final, 
                                           X = x_show, 
                                           Y = y_show, 
                                           onlySL = F)
  
  conv.preds_show <- (ifelse(predictions_show$pred>=0.5,1,0))
  
  # Create the confusion matrix
  cm <- confusionMatrix(as.factor(conv.preds_show),
                        as.factor(y_final))
  output_cv[i,1] <- cm$table[1]
  output_cv[i,2] <- cm$table[2]
  output_cv[i,3] <- cm$table[3]
  output_cv[i,4] <- cm$table[4]
  
  x_final <- as.data.frame(datagg[,c(6,8)])
  y_final <- as.matrix(as.numeric(as.factor(datagg$Breakout))-1)
  
  predictions_show <- predict.SuperLearner(model, newdata = x_final, X = x_show, Y = y_show, onlySL = F)
  
  conv.preds_show <- (ifelse(predictions_show$pred>=0.5,1,0))
  
  # Create the confusion matrix
  cm <- confusionMatrix(as.factor(conv.preds_show), as.factor(y_final))

  output_final[i,1] <- cm$table[1]
  output_final[i,2] <- cm$table[2]
  output_final[i,3] <- cm$table[3]
  output_final[i,4] <- cm$table[4]
  
  j <- j + 1000
  rows <- rows + 1000
  } 

output_cv <- data.frame(output_cv)
output_cv <- output_cv %>% 
  mutate(neg = X1/(X1 + X2),
         pos = X4/(X4 + X3)) %>% 
  select(neg,pos)

output_final <- data.frame(output_final)
output_final <- output_final %>% 
  mutate(neg = X1/(X1 + X2),
         pos = X4/(X4 + X3)) %>% 
  select(neg,pos)


output_final

