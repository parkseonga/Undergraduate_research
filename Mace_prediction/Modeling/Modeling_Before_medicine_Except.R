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

total_data = emr_TIVA%>%inner_join(TIVA_summary,by ="Case_ID")%>%inner_join(TIVA_cpt, by = "Case_ID")%>%inner_join(TIVA_cre, by = "Case_ID")%>%inner_join(TIVA_peak, by = "Case_ID")%>%inner_join(TIVA_freq,by ="Case_ID")

head(total_data)
str(total_data)
colSums(is.na(total_data))
names(total_data)

table(total_data$class)

total_data$class = as.character(total_data$class)
class(total_data$class)

total_data$ANE.Time = NULL
total_data$OP.Time = NULL

total_data$class[total_data$class=="NORMAL"] = "TRUE"
total_data$class[total_data$class!="TRUE"] = "FALSE"

table(total_data$class)

char_name = names(unlist(lapply(total_data, class)[lapply(total_data, class)%in%c("factor","character")]))
char_name

total_data$AntiHBs = as.numeric(total_data$AntiHBs)
total_data$AntiHIV = as.numeric(total_data$AntiHIV)

total_data[is.na(total_data)] = 0

# OP_Score로 인해 OP_Severity 삭제해도됨. 
except_name = c("Case_ID","OP.Name","OP.Name_re","EKG","OP_surgery_type","OP_Severity")
total_data2 = total_data[,!names(total_data)%in%except_name]

total_data2 = total_data2 %>% mutate_if(is.character, as.factor)
str(total_data2)
colSums(is.na(total_data2))[colSums(is.na(total_data2))>0]


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

same_name 

total_data2 = total_data2[,!names(total_data2)%in%same_name]
names(total_data2)

# facotr형으로 변경해야되는 변수들 확인(기저질환 변수)
name_ = c()
col_factor_feature = data.frame()

for(col_ in names(total_data2)){
  df = data.frame(col_, count = length(unique(data.frame(total_data2)[,col_])))
  
  col_factor_feature = rbind(col_factor_feature,df)
}

factor_col = as.character(subset(col_factor_feature, col_factor_feature$count == 2)$col_)

factor_col = factor_col[1:26]  # peak 특징은 제외 

total_data2[,factor_col] = lapply(total_data2[,factor_col], as.factor)

# factor, 문자열 변수 따로 추출 
char_name_new = names(unlist(lapply(total_data2, class)[lapply(total_data2, class)%in%c("factor","character")]))
char_name_new = char_name_new[-grep("class",char_name_new)] # class 위치 제외 


# 더미변수 생성
total_data3 = total_data2[,!names(total_data2)%in%char_name_new]
names(total_data3)

# 분산이 0에 가까운 변수 제거 
# nearZeroVar(total_data3)
# total_data3 = total_data3[, -nearZeroVar(total_data3)]  # 성능이 더 안좋아짐. 

df = c()

for (col_name in char_name_new){
  
  df = cbind(df, dummy(total_data2[,col_name],sep='_'))
  
}

total_data3 = cbind(total_data3, df)


summary(total_data3)
str(total_data3)

total_data3$class = as.character(total_data3$class)

num_name = names(unlist(lapply(total_data3, class)[lapply(total_data3, class)%in%c("numeric","integer")]))
num_name

boxplot(total_data3[,num_name[1:10]])
boxplot(total_data3[,num_name[11:20]])
boxplot(total_data3[,num_name[21:30]])
boxplot(total_data3[,num_name[31:40]])
boxplot(total_data3[,num_name[41:50]])

class(total_data3$class)
names(total_data3)

total_data4 = total_data3
total_data3 = total_data4[,c(40:64)]

data_TRUE=subset(total_data3,class=="TRUE")
data_FALSE=subset(total_data3,class=="FALSE")

# 데이터셋 분리 
inTrain=createDataPartition(1:nrow(data_TRUE),p=0.8,list=FALSE)

names(data_TRUE)

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
names(train_x)

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

confusionMatrix(confTest,positive="TRUE")

print(confTrain)
print(confTest)
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


# 더미 변수 제외 
data_TRUE=subset(total_data3,class=="TRUE")
data_FALSE=subset(total_data3,class=="FALSE")

# 데이터셋 분리 
inTrain=createDataPartition(1:nrow(data_TRUE),p=0.8,list=FALSE)

names(data_TRUE)

# 특징만 추출해서 하려면 labeling으로 특징이 있는 열만 입력해야함. 
train_x = data_TRUE[inTrain,-grep("class",names(data_TRUE))]
names(train_x)

train_x = train_x[1:189]
# 모든 변수에 값은 값이 들어가면 scale할 수가 없어서 에러가 발생하기 때문에 해당 변수 삭제 
same_name_train = c()

for(name in names(train_x)){
  if(min(train_x[name])==max(train_x[name])){
    same_name_train = c(same_name_train,name)  
  }
}

train_x = train_x[,!names(train_x)%in%same_name_train]
names(train_x)

train_y = data_TRUE[inTrain,"class"]

test_TRUE = data_TRUE[-inTrain,]
test=rbind(test_TRUE,data_FALSE)
test_x = test[,-grep("class",names(test))]
test_x = test_x[1:189]

test_x = test_x[,!names(test_x)%in%same_name_train]
test_y = test[,"class"]

svm.model=svm(train_x,y=NULL,
              type='one-classification',
              nu=0.05,
              scale = TRUE,
              kernel = "radial")


svm.predtrain=predict(svm.model,train_x)
svm.predtest=predict(svm.model,test_x)

confTrain=table(Predicted=svm.predtrain,Reference=train_y)
confTest=table(Predicted=svm.predtest,Reference=test_y)

confusionMatrix(confTest,positive="TRUE")

print(confTrain)
print(confTest)

# 더미변수를 넣은 경우의 성능의 더 좋음. 
