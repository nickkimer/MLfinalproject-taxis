hyper_params = list( max_depth = seq(1,10,2) )
#hyper_params = list( max_depth = c(4,6,8,12,16,20) ) ##faster for larger datasets

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## full Cartesian hyper-parameter search
  search_criteria = list(strategy = "Cartesian"),
  
  ## which algorithm to run
  algorithm="gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id="depth_grid",
  
  ## standard model parameters
  x=colnames(training.data.h2o[,which(colnames(training.data.h2o ) %in% c(Originial_and_EF))]), y="FRD_IND",
  training_frame = training.data.h2o, 
  nfolds= 5, 
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 300,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       
  
  ## sample 80% of columns per split
  col_sample_rate = 0.8, 
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "AUC", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                
)

## by default, display the grid search results sorted by increasing logloss (since this is a classification task)
grid                                                                       

## sort the grid models by decreasing AUC
sortedGrid <- h2o.getGrid("depth_grid", sort_by="auc", decreasing = TRUE)    
sortedGrid


LOG_EF_model_path <- h2o.saveModel(object=gbt_model_EF, path=getwd(), force=TRUE)



for (i in 1:5) {
  gbm <- h2o.getModel(sortedGrid@model_ids[[i]])
  print(h2o.auc(h2o.performance(gbm, newdata = test.data.h2o)))
}
gbm <- h2o.getModel(sortedGrid@model_ids[[1]])
h2o.complete_predictions_GBT = h2o.predict(gbm , test.data.h2o)
h2o.complete_predictions_GBT= as.data.frame(h2o.complete_predictions_GBT)
h2o.complete_predictions_GBT= cbind(h2o.complete_predictions_GBT, test.data$FRD_IND)
h2o.complete_predictions_GBT= cbind(h2o.complete_predictions_GBT, test.data$AUTHZN_AMT)
write.csv(h2o.complete_predictions_GBT, "h2o.complete_predictions_GBT.csv")
