lat_long = function(addr){
  require(stringr)
  require(RCurl)
  require(htmltidy)
  require(XML)
  
  # addr = "Anjor society,baner road,pune,maharashtra"
  addr = gsub(" ", "+", addr)
  if(grepl("\\s{1,}", addr)){
    addr = gsub(",", ",+", addr)
  }
  
  url = "https://www.google.com/maps/search/"
  url = paste0(url, addr)
  
  doc.raw <- getURL(url, httpheader = c("User-Agent" = "R(2.10.0)"))
  doc <- tidy_html(doc.raw)
  html <- htmlTreeParse(doc, useInternal = TRUE, asTree = T)
  
  val = xmlToList(html)
  val_text = tryCatch({val[["head"]][[11]][["content"]]},
                      error = function(e){
                        NA
                      })
  st_st = as.numeric(str_locate(val_text, "center="))
  val_text = substr(val_text, (st_st[2]+1), str_length(val_text))
  st_st = as.numeric(str_locate(val_text, "&"))
  val_text = substr(val_text, 1, (st_st[1]-1))
  val_text = as.character(str_split(val_text, "%2C", simplify = T))
  
  return(val_text)
}

# Preparing Sparse Matrix
dtrain = xgb.DMatrix(data = as.matrix(training %>% select(-INSTALLMENT_AMT_TREATED,-ACCT_TYPE1,-`ACCT-TYPE`) %>% mutate_all(as.numeric)),label = training$INSTALLMENT_AMT_TREATED) 
dtest = xgb.DMatrix(data = as.matrix(testing %>% select(-INSTALLMENT_AMT_TREATED,-ACCT_TYPE1,-`ACCT-TYPE`) %>% mutate_all(as.numeric)),label=testing$INSTALLMENT_AMT_TREATED)
watchlist = list(train=dtrain, test=dtest)

# Hyperparameter Tuning with Cross Validation -----------------------------
# Grid Search -------------------------------------------------------------
# searchGridSubCol = expand.grid(subsample = c(0.7, 1),
#                                 colsample_bytree = 1,
#                                 max_depth = c(6, 10, 15),
#                                 min_child = c(1, 2, 3),
#                                 gamma = 0,
#                                 eta = c(0.3, 0.03, 0.001),
#                                 nthread=4
# )
# 
# system.time(
#   maeErrorsHyperparameters = apply(searchGridSubCol, 1, function(parameterList){
#     
#     #Extract Parameters to test
#     currentSubsampleRate = parameterList[["subsample"]]
#     currentColsampleRate = parameterList[["colsample_bytree"]]
#     currentDepth = parameterList[["max_depth"]]
#     currentEta = parameterList[["eta"]]
#     currentMinChild = parameterList[["min_child"]]
#     currentGamma = parameterList[["gamma"]]
#     
#     print(paste0("currentSubsampleRate = ", currentSubsampleRate))
#     print(paste0("currentColsampleRate = ", currentColsampleRate))
#     print(paste0("currentDepth = ", currentDepth))
#     print(paste0("currentEta = ", currentEta))
#     print(paste0("currentMinChild = ", currentMinChild))
#     print(paste0("currentGamma = ", currentGamma))
#     
#     xgboostModelCV = xgb.cv(data =  dtrain, nrounds = 2000, nfold = 5, showsd = TRUE, 
#                              verbose = TRUE, "eval_metric" = "mae",
#                              "objective" = "reg:linear", "max.depth" = currentDepth, "eta" = currentEta,
#                              "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate,
#                              print_every_n = 50, "min_child_weight" = currentMinChild, booster = "gbtree",
#                              "gamma" = currentGamma, early_stopping_rounds = 10)
#     
#     xvalidationScores = as.data.frame(xgboostModelCV$evaluation_log)
#     test_mae = tail(xvalidationScores$test_mae_mean, 1)
#     train_mae = tail(xvalidationScores$train_mae_mean,1)
#     best_iter = xgboostModelCV$best_iteration
#     output = return(c(best_iter, test_mae, train_mae, currentSubsampleRate, currentColsampleRate,
#                        currentDepth, currentEta, currentMinChild, currentGamma))}
#   )
# )
# 
# maeErrorsHyperparameters = as.data.frame(maeErrorsHyperparameters)
# rownames(maeErrorsHyperparameters) = c("best_iter", "test_mae", "train_mae", "currentSubsampleRate",
#                                        "currentColsampleRate", "currentDepth", "currentEta",
#                                        "currentMinChild", "currentGamma")
# col_indx = as.numeric(which.min(maeErrorsHyperparameters["test_mae",]))
# maeErrorsHyperparameters[,col_indx]
# 
# params = list(booster = "gbtree",
#                objective = "reg:linear",
#                eval_metric = "mae",
#                eta=maeErrorsHyperparameters["currentEta",col_indx],
#                gamma=maeErrorsHyperparameters["currentGamma",col_indx],
#                max_depth=maeErrorsHyperparameters["currentDepth",col_indx],
#                min_child_weight=maeErrorsHyperparameters["currentMinChild",col_indx],
#                subsample=maeErrorsHyperparameters["currentSubsampleRate",col_indx],
#                colsample_bytree=maeErrorsHyperparameters["currentColsampleRate",col_indx]
#                )
# 
# xgb = xgb.train (params = params,
#                   data = dtrain,
#                   nrounds=maeErrorsHyperparameters["best_iter",col_indx],
#                   watchlist = list(test=dtest,train=dtrain),
#                   print_every_n = 50,
#                   early_stop_round = 10,
#                   maximize = F)

# Without Grid Search -----------------------------------------------------
params = list(booster = "gbtree",
               objective = "reg:linear",
               eval_metric = "mae",
               eta=0.01,
               max_depth=9,
               gamma=0,
               min_child_weight=2,
               subsample=1,
               colsample_bytree=1,
               nthread=8
              )
set.seed(123)
xgbcv = xgb.cv(params = params,
                data = dtrain,
                nrounds = 2000,
                nfold = 5,
                showsd = T,
                stratified = T,
                print_every_n = 20,
                early_stopping_rounds = 10,
                maximize = F)

set.seed(123)
xgb = xgb.train (params = params,
                  data = dtrain,
                  # nrounds=xgbcv$best_iteration,
                  nrounds=1000,
                  watchlist = list(test=dtest,train=dtrain),
                  print_every_n = 50,
                  early_stop_round = 10,
                  maximize = F)

# Model Prediction and Evaluation -----------------------------------------
evaluation = data.frame(ACCOUNT_TYPE = testing$`ACCT-TYPE`,
                        INSTALLMENT_AMT_ORIGINAL = testing$INSTALLMENT_AMT_TREATED,
                        INSTALLMENT_AMT_PREDICTED = round(predict(xgb, dtest)))
evaluation$ABSOLUTE_ERROR = round(abs(evaluation[,2] - evaluation[,3]))

rm(eval_metrices)

for(prdt in as.character(unique(evaluation$ACCOUNT_TYPE))){
  eval_metricesTmp = as.data.frame(t(loss_metrices(evaluation$INSTALLMENT_AMT_ORIGINAL[evaluation$ACCOUNT_TYPE==prdt],
                                                   evaluation$INSTALLMENT_AMT_PREDICTED[evaluation$ACCOUNT_TYPE==prdt])))
  eval_metricesTmp = rbind(eval_metricesTmp, 
                           upper.trim.mean(evaluation$ABSOLUTE_ERROR[evaluation$ACCOUNT_TYPE==prdt], 0.05),
                           upper.trim.mean(evaluation$ABSOLUTE_ERROR[evaluation$ACCOUNT_TYPE==prdt], 0.1))
  if(exists("eval_metrices")==F){
    eval_metrices = data.frame(Metric=c(rownames(eval_metricesTmp)[1:21],"MAE_TRIM_TOP_5%", "MAE_TRIM_TOP_10%","TotalCount"))
  }
  eval_metrices[[prdt]] = round(c(eval_metricesTmp$V1,
                                  evaluation$INSTALLMENT_AMT_ORIGINAL[evaluation$ACCOUNT_TYPE==prdt] %>% length()),2)
  
}

eval_metricesTmp = as.data.frame(t(loss_metrices(evaluation[,2],
                                                 evaluation[,3])))
names(eval_metricesTmp) = paste(as.character(str_split(data_type_working, " ", simplify = T)), collapse = "_")

eval_metricesTmp = rbind(eval_metricesTmp, 
                         upper.trim.mean(evaluation$ABSOLUTE_ERROR, 0.05),
                         upper.trim.mean(evaluation$ABSOLUTE_ERROR, 0.1))
eval_metrices[[data_type_working]] = round(c(eval_metricesTmp[,1],
                                             evaluation$INSTALLMENT_AMT_ORIGINAL %>% length()),2)

View(eval_metrices)


# lat_long("Gandhi Engineering College, Bhubaneshwar, Orissa")
