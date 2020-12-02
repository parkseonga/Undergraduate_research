library(e1071)
library(caret)
library(NLP)
library(tm)
library(dplyr)
library(dummies)

setwd("D:\\포트폴리오\\개인\\연구\\마취\\preprocessing")

set.seed(20171490)

emr_TIVA = readRDS("emr_TIVA.rds")
head(emr_TIVA)
names(emr_TIVA)
names(emr_TIVA)[1] = "Case_ID"

summary(emr_TIVA)

TIVA_summary = readRDS("TIVA_summary.rds")
TIVA_cpt = readRDS("TIVA_cpt.rds")
TIVA_peak = readRDS("TIVA_peak.rds")
TIVA_cre = readRDS("TIVA_cre.rds")
TIVA_freq = readRDS("TIVA_freq.rds")

names(TIVA_cre)
names(TIVA_cpt)
names(TIVA_peak)
names(TIVA_freq)[1] = "Case_ID"

# AntiHBS- 0 : 검사안함 N: 검사함 네커티브 P: 검사함 파지티브브

model = function(total_data){
  
  set.seed(20171490)
  
  total_data$class = as.character(total_data$class)

  total_data$ANE.Time = NULL
  total_data$OP.Time = NULL
  
  total_data$class[total_data$class=="NORMAL"] = "TRUE"
  total_data$class[total_data$class!="TRUE"] = "FALSE"
  
  char_name = names(unlist(lapply(total_data, class)[lapply(total_data, class)%in%c("factor","character")]))
  
  total_data[is.na(total_data)] = 0
  
  # OP_Score로 인해 OP_Severity 삭제해도됨. 
  except_name = c("Case_ID","OP.Name","OP.Name_re","EKG","OP_surgery_type","OP_Severity")
  total_data2 = total_data[,!names(total_data)%in%except_name]
  
  total_data2 = total_data2 %>% mutate_if(is.character, as.factor)

  ##################################################
  #---------모든 레코드에 동일한 값 처리------------
  ##################################################
  same_name = c()
  
  # 모든레코드에 동일한 값 확인 
  for(name in names(total_data2)){
    if(length(unique(total_data2[,name]))==1){
      same_name = c(same_name,name)  
    }
  }
  

  total_data2 = total_data2[,!names(total_data2)%in%same_name]

  # facotr형으로 변경해야되는 변수들 확인(기저질환 변수)
  name_ = c()
  col_factor_feature = data.frame()
  
  for(col_ in names(total_data2)){
    df = data.frame(col_, count = length(unique(data.frame(total_data2)[,col_])))
    
    col_factor_feature = rbind(col_factor_feature,df)
  }
  
  factor_col = as.character(subset(col_factor_feature, col_factor_feature$count == 2)$col_)
  
  factor_col = factor_col[-grep("class",factor_col)]  # peak 특징은 제외 
  factor_col = factor_col[-grep("peak",factor_col)]  # peak 특징은 제외 
  
  total_data2[,factor_col] = lapply(total_data2[,factor_col], as.factor)
  
  # factor, 문자열 변수 따로 추출 
  char_name_new = names(unlist(lapply(total_data2, class)[lapply(total_data2, class)%in%c("factor","character")]))
  char_name_new = char_name_new[-grep("class",char_name_new)] # class 위치 제외 
  
  
  # 더미변수 생성
  total_data3 = total_data2[,!names(total_data2)%in%char_name_new]

  # 분산이 0에 가까운 변수 제거 
  # nearZeroVar(total_data3)
  # total_data3 = total_data3[, -nearZeroVar(total_data3)]  # 성능이 더 안좋아짐. 
  
  
  df = c()
  
  for (col_name in char_name_new){
    
    df = cbind(df, dummy(total_data2[,col_name],sep='_'))
    
  }
  
  total_data3 = cbind(total_data3, df)

  total_data3$class = as.character(total_data3$class)

  
  data_TRUE=subset(total_data3,class=="TRUE")
  data_FALSE=subset(total_data3,class=="FALSE")
  
  # 데이터셋 분리 
  inTrain=createDataPartition(1:nrow(data_TRUE),p=0.8,list=FALSE)

  
  # 특징만 추출해서 하려면 labeling으로 특징이 있는 열만 입력해야함. 
  train_x = data_TRUE[inTrain,-grep("class",names(data_TRUE))]
  
  # 모든 변수에 값은 값이 들어가면 scale할 수가 없어서 에러가 발생하기 때문에 해당 변수 삭제 
  same_name_train = c()
  
  for(name in names(train_x)){
    if(min(train_x[name])==max(train_x[name])){
      same_name_train = c(same_name_train,name)  
    }
  }
  
  train_x = train_x[,!names(train_x)%in%same_name_train]

  train_y = data_TRUE[inTrain,"class"]
  
  test_TRUE = data_TRUE[-inTrain,]
  test=rbind(test_TRUE,data_FALSE)
  test_x = test[,-grep("class",names(test))]
  # test_x = test_x[,-control_index]
  test_x = test_x[,!names(test_x)%in%same_name_train]
  test_y = test[,"class"]
  
  svm.model=svm(train_x,y=NULL,
                type='one-classification',
                nu=0.10,
                scale = TRUE,
                kernel = "radial")
  
  
  svm.predtrain=predict(svm.model,train_x)
  svm.predtest=predict(svm.model,test_x)
  
  confTrain=table(Predicted=svm.predtrain,Reference=train_y)
  confTest=table(Predicted=svm.predtest,Reference=test_y)
  
  return(confusionMatrix(confTest,positive="TRUE"))

  
}



total_data = emr_TIVA%>%inner_join(TIVA_summary,by ="Case_ID")%>%inner_join(TIVA_cpt, by = "Case_ID")%>%inner_join(TIVA_cre, by = "Case_ID")%>%inner_join(TIVA_peak, by = "Case_ID")%>%inner_join(TIVA_freq,by ="Case_ID")
head(total_data)
str(total_data)
colSums(is.na(total_data))
names(total_data)
table(total_data$class)


model(emr_TIVA%>%inner_join(TIVA_summary,by ="Case_ID"))  # 0.710
model(emr_TIVA%>%inner_join(TIVA_cpt,by ="Case_ID")) # 0.6711
model(emr_TIVA%>%inner_join(TIVA_cre,by ="Case_ID")) # 0.6566
model(emr_TIVA%>%inner_join(TIVA_freq,by ="Case_ID")) # 0.6320
model(emr_TIVA%>%inner_join(TIVA_peak,by ="Case_ID")) # 0.6836

model(emr_TIVA%>%inner_join(TIVA_summary,by ="Case_ID")%>%inner_join(TIVA_cpt, by = "Case_ID"))  # 0.6745
model(emr_TIVA%>%inner_join(TIVA_summary,by ="Case_ID")%>%inner_join(TIVA_cpt, by = "Case_ID")%>%inner_join(TIVA_cre, by = "Case_ID"))  # 0.6836
model(emr_TIVA%>%inner_join(TIVA_summary,by ="Case_ID")%>%inner_join(TIVA_cpt, by = "Case_ID")%>%inner_join(TIVA_cre, by = "Case_ID")%>%inner_join(TIVA_peak, by = "Case_ID"))  # 0.6691
model(emr_TIVA%>%inner_join(TIVA_summary,by ="Case_ID")%>%inner_join(TIVA_cpt, by = "Case_ID")%>%inner_join(TIVA_cre, by = "Case_ID")%>%inner_join(TIVA_peak, by = "Case_ID")%>%inner_join(TIVA_freq,by ="Case_ID"))  # 0.6513



# 주파수 특징을  안넣었을 때 

# Sensitivity : 0.8108 
# Specificity : 0.5333  

# 주파수 특징을 추가했을 때 
# Sensitivity : 0.8311 
# Specificity : 0.4889  


library(pROC)

roc_svm_test <- roc(response = test_y, predictor =as.numeric(svm.predtest))  # auc값이 balance accuracy를 보면 됨. 

plot(roc_svm_test, add = TRUE,col = "red", print.auc=TRUE, print.auc.x = 0.5, print.auc.y = 0.3)
legend(0.3, 0.2, legend = c("test-svm"), lty = c(1), col = c("blue"))

# 중요 feature 추출 
w = t(svm.model$coefs) %*% svm.model$SV

data = data.frame(cbind(names(train_x), as.vector(w)))

data = arrange(data, desc(X2))

feature_name = as.character(data[1:20,]$X1)

train_x = data_TRUE[inTrain,feature_name]

train_y = data_TRUE[inTrain,"class"]

test_x = test[,feature_name]

test_y = test[,"class"]

svm.model=svm(train_x,y=NULL,
              type='one-classification',
              nu=0.08,
              scale = TRUE,
              kernel = "radial")

svm.predtrain=predict(svm.model,train_x)
svm.predtest=predict(svm.model,test_x)

confTrain=table(Predicted=svm.predtrain,Reference=train_y)
confTest=table(Predicted=svm.predtest,Reference=test_y)

confusionMatrix(confTest,positive="TRUE")

print(confTrain)
print(confTest)

# Sensitivity : 0.95270
# Specificity : 0.04444

# svm 에 대한 feature selection을 하면 정상인 것만 더 잘 맞출 뿐임. 



# xgboost 모델 
library(caret)
library(xgboost)

set.seed(20171490)

train.index <- createDataPartition(y = total_data3$class, p = .6, list = F)
up_train <- upSample(x = total_data3[train.index,][,!names(total_data3[train.index,])=='class'],y = as.factor(total_data3[train.index,]$class))
test = total_data3[-train.index,]

up_train$class<-NULL

train.label = as.numeric(as.factor(up_train$Class))-1
test.label = as.numeric(as.factor(test$class))-1

new_train <- model.matrix(~.+0,data = up_train[,!names(up_train)=='Class'])
new_test <- model.matrix(~.+0,data = test[,!names(test)=='class'])

xgb_train = xgb.DMatrix(data = new_train, label = train.label)
xgb_test = xgb.DMatrix(data = new_test, label = test.label)

# params = list(
#   booster="gbtree", # 트리기반
#   eta=0.01, # 과적합 방지를 위해 단계 크기 축소 # 낮을수록 과적합 일어나지 않음
#   max_depth=6, # 나무 최대 깊이 지정 # 숫자가 커질수록 과적합 기본 =6
#   gamma=0.2, # 최소 손실 감소 지정 # 클수록 트리 깊이가 줄어든다.
#   objective="multi:softprob", # class 분류
#   eval_metric="mlogloss", # 평가 metrics
#   alpha=0.2)


params = list(
  booster="gbtree",  eta=0.01, 
  max_depth=6,
  gamma=0.2, 
  objective="multi:softprob", 
  eval_metric="mlogloss", 
  alpha=0.2)

# 검증
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

xgbcv <- xgb.cv( params = params, data = xgb_train, nrounds = 100, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)

min(xgbcv$test.error.mean)

xgb1 <- xgb.train (params = params, data = xgb_train, nrounds = 19, watchlist = list(val=xgb_test,train=xgb_train), print_every_n = 10, early_stop_round = 10, maximize = F , eval_metric = "error")

# 에러율 계산
# classification_error <- function(conf_mat) {
#   conf_mat = as.matrix(conf_mat)
#   error = 1 - sum(diag(conf_mat)) / sum(conf_mat)
#   return (error)
# }

xgbpred <- predict (xgb1,xgb_test)
xgbpred <- ifelse (xgbpred > 0.5,1,0)
confusionMatrix(as.factor(xgbpred), as.factor(test.label))


# 예측값 확인
xgb_train_preds <- data.frame(xgbcv$pred) %>% mutate(max = max.col(., ties.method = "last"), label = train.label + 1)

head(xgb_train_preds)
xgb_conf_mat_2 <- confusionMatrix(factor(xgb_train_preds$label),
                                  factor(xgb_train_preds$max),
                                  mode = "everything")
print(xgb_conf_mat_2)

# 분류된 table확인
xgb_conf_mat <- table(true = train.label + 1, pred = xgb_train_preds$max)

# 에러율 반환
cat("XGB Training Classification Error Rate:", classification_error(xgb_conf_mat), "\n")

# test data에 적용
xgb_model=xgb.train(
  params=params,
  data=xgb_train,
  nrounds=200,
  nthreads=2,
  early_stopping_rounds=50,
  watchlist=list(val1=xgb_train,val2=xgb_test),
  verbose=0,
  num_class=3
)

test_pred <- predict(xgb_model, newdata = xgb_test)
test_prediction <- matrix(test_pred, nrow = 3,
                          ncol=length(test_pred)/3) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test.label + 1,
         max_prob = max.col(., "last"))

# Confustion Matrix
confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")

xgb_val_out <- matrix(test_pred, nrow = 3, ncol = length(test_pred)/3) %>% t() %>%data.frame() %>%mutate(max = max.col(., ties.method = "last"), label = test.label + 1)
xgb_val_conf <- table(true = test.label + 1, pred = xgb_val_out$max)

cat("XGB test Classification Error Rate:", classification_error(xgb_val_conf), "\n")

mat = xgb.importance (feature_names = colnames(new_train),model = xgb_model)
gp=xgb.ggplot.importance(importance_matrix = mat,20)
print(gp)


