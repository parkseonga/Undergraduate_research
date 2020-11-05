# 환경 설정
setwd('E:\\')

# 라이브러리 불러오기 
library(stringr)
library(dlookr)
library(fBasics)
library(readxl)
library(DMwR)

library(xgboost)
library(dplyr)
library(data.table)


######################################################################################################################################
# 1. emr data preprocessing
# * 검사 결과에 대한 결측치는 검사를 하지 않아 기록이 되지 않은 것.(pre-op evaluation)
# * PFT(폐기능 검사) 3개의 검사 수치를 파생변수로 생성 - 결측치는 0으로 대체 
# * PFT, EF 검사 유무에 대한 파생 변수 생성 
# * Method에 따라 잘못 입력된 case id 수정 
# * Alb 변수에 잘못 입력된 수치라고 판단되는 값 수정(4..9-> 4.9/44 -> 4.4)
######################################################################################################################################

#데이터 불러오기
emr = read_csv("seoul_EMR_180301_200430.csv",skip=1)
str(emr)
names(emr)

# 삭제할 변수 
except_name = c('X1','X2','Patient ID','OP Name','Inotropics','Echo','Inotropics','VR','PreopDx','Cause','Cx','CPR','POD','POD_1','받기','기본','마취')
emr2 = emr[,!names(emr)%in%except_name]
str(emr2)

colSums(is.na(emr2))

emr2 = emr2[!is.na(emr2$Sex),] # 1개
# OP.TIME에 결측치가 존재하지만 VR과 확인 후 처리 
dim(emr2)

emr2 = emr2[,!names(emr2)%in%names(emr2)[find_na(emr2,rate=TRUE)==100]]   # 모든 관측치에 결측치 존재하는 열 삭제 -> 없음. 
dim(emr2)

# EF변수 결측치는 0으로 대체
emr2$EF[is.na(emr2$EF)] = 0

# Alb  잘못들어와있는 값 수정
table(emr2$Alb,useNA = 'ifany')


# PFT변수 전처리
head(emr2$PFT,15)

PFT_split = str_split(emr2$PFT,'-')

emr2$PFT_3 = sapply(PFT_split,function(x){x[3]})

emr2$PFT_2 = unlist(lapply(sapply(PFT_split,function(x){x[2]}),function(x){str_sub(x,str_locate(x,'\\(')[1]+1,str_locate(x,'\\)')[1]-1)}))

emr2$PFT_1 = unlist(lapply(sapply(PFT_split,function(x){x[1]}),function(x){str_sub(x,str_locate(x,'\\(')[1]+1,str_locate(x,'\\)')[1]-1)}))

emr2$PFT_3 = as.numeric(emr2$PFT_3)           # 자료형변환 
emr2$PFT_2 = as.numeric(emr2$PFT_2)
emr2$PFT_1 = as.numeric(emr2$PFT_1)

table(is.na(emr2$PFT_3))
table(is.na(emr2$PFT_2))
table(is.na(emr2$PFT_1))

emr2$PFT_3[is.na(emr2$PFT_3)] = 0                 # 결측치 0으로 대체
emr2$PFT_2[is.na(emr2$PFT_2)] = 0
emr2$PFT_1[is.na(emr2$PFT_1)] = 0
emr2$PFT[is.na(emr2$PFT)] = 0

emr2$EF_inspect = ifelse(emr2$EF==0,0,1)            # 검사 유무 속성 생성 
emr2$PFT_inspect = ifelse(emr2$PFT==0,0,1) 

emr2$EF_inspect = as.factor(emr2$EF_inspect)         # factor형으로 변환 
emr2$PFT_inspect = as.factor(emr2$PFT_inspect)

emr2 = emr2[,!names(emr2)%in%c('EF','PFT')]   # 불필요한 변수 삭제 
table(emr2$HbA1c,useNA = 'ifany') 

names(emr2)
str(emr2)
# voaltile에 잘못들어있는 케이스들 TIVA로 분류
Tiva_case_id = c('03_190201_1225','03_181002_1340','03_191210_1430','06_191205_0741','08_190208_0905','06_191112_0740','11_190517_0758')

table(emr2$Method)

emr_method_revise_tiva = subset(emr2, emr2$`Case ID`%in%Tiva_case_id)
emr_method_revise_tiva$Method = 'TIVA'

emr_method_revise = subset(emr2, !emr2$`Case ID`%in%Tiva_case_id)
emr_method_total = rbind(emr_method_revise, emr_method_revise_tiva)

table(emr_method_total$Method)

names(emr_method_total)[1] = 'Case_ID'

# 합병증 발생 세 달 전인 데이터만 사용 
emr_method_total_90 = subset(emr_method_total,emr_method_total$POD_2<=90|is.na(emr_method_total$POD_2))

# class 생성
emr_method_total_90$class = ifelse(emr_method_total_90$MACE==0&emr_method_total_90$Death==0,'NORMAL',ifelse(emr_method_total_90$MACE==1,'MACE','DEATH'))
table(emr_method_total_90$class) # DEATH: 38, MACE:251, NORMAL: 2339

str(emr_method_total_90)

# 전처리 후 불필요해진 필드 삭제 
emr_method_total_90$POD_MACE = NULL

emr_method_total_90$Death = NULL
emr_method_total_90$MACE = NULL

summary(emr_method_total_90)

head(emr_method_total_90)
table(emr_method_total_90$HBsAg)
table(emr_method_total_90$Alb,useNA = 'ifany')

# N, P 가 무엇을 의미하는가? P가 검사결과 음성이다라는 걸까?
emr_method_total_90$HBsAg = ifelse(is.na(emr_method_total_90$HBsAg),0,1)
emr_method_total_90$AntiHBs = ifelse(is.na(emr_method_total_90$AntiHBs),0,1)
emr_method_total_90[is.na(emr_method_total_90)] = 0

emr_method_total_90$Alb = as.numeric(emr_method_total_90$Alb)

find_na(emr_method_total_90,rate=TRUE)

summary(emr_method_total_90)

# 모든레코드에 동일한 값
emr_method_total_90$End.stage.renal.disease.ESRD. = NULL 

emr_TIVA = emr_method_total_90[emr_method_total_90$Method=='TIVA',]
emr_volatile = emr_method_total_90[emr_method_total_90$Method!='TIVA',]

emr_volatile$Method = NULL
emr_TIVA$Method = NULL

saveRDS(emr_TIVA,'preprocessing/emr_TIVA.rds')
saveRDS(emr_volatile,'preprocessing/emr_volatile.rds')

######################################################################################################################################
# 2. vital data preprocessing
# * TIVA: Propofol, Remifentanil관련 변수, AGENT 관련 변수 
# * volatile: AGENT 관련 변수, Remifentanil관련 변수 
# * 각 값에 값이 0인 것과 아닌 것은 의미가 있음
# * AGENT_ET 변수는 max가 1미만인 경우는 0, 1이상인 경우는 사용
# * AGENT관련 변수 의 결측치는 모두 0으로 처리 
# * AGENT관련 변수를 제외한 사용 변수의 결측치들은 0으로 처리, 나머지값은 그대로 사용 
# * 변수명에 VOL, RATE 들어간 경우 제외 
# * 저혈압의 경우 통계값은 mean, sd 사용 -> 참고 
######################################################################################################################################
# vital data load
file_list = list.files("seoulVR")

# vital record의 case_id 추출
file_id =  data.frame(unlist(lapply(str_split(file_list,".csv_tag"),function(x){x[1]})))
names(file_id) = 'Case_ID'
file_id$Case_ID = as.character(file_id$Case_ID)

# tiva
file_case_TIVA = inner_join(emr_TIVA,file_id,by='Case_ID')  
nrow(file_case_TIVA)  # 1007

# volatile
file_case_volatile = inner_join(emr_volatile,file_id,by='Case_ID')  
nrow(file_case_volatile) # 5022

# vitalrecord load(TIVA)
vital_TIVA = data.frame()

system.time(
  
  for (file_id in file_case_TIVA$Case_ID){
    
    df = fread(paste0('seoulVR/',file_id,'.csv_tag_filled_del.csv'))
    
    df$Case_ID = file_id
    
    vital_TIVA = bind_rows(vital_TIVA,df) 
    
  })

str(vital_TIVA)
saveRDS(vital_TIVA,'preprocessing\\vital_TIVA.rds')
length(unique(vital_TIVA$Case_ID))

vital_TIVA = readRDS('preprocessing\\vital_TIVA.rds')

# tiva preprocessing
vital_TIVA = as.data.table(vital_TIVA)
names(vital_TIVA)

# TIVA: AGENT 관련 결측치는 모두 0으로 처리
# AGENT_ET변수는 max 값이 1미만인 경우 모두0 으로 처리 
vital_TIVA$AGENT_ET[is.na(vital_TIVA$AGENT_ET)] = 0
agent_max = vital_TIVA[,max(AGENT_ET),by='Case_ID']   # data.table에서만 가능 

agent_ID = agent_max[agent_max$V1<1,Case_ID]         # max값이 1미만인 경우 추출 

vital_TIVA2 = vital_TIVA[Case_ID%in%agent_ID,] 
vital_TIVA2$AGENT_ET = 0                      # 0 대입

vital_TIVA_total = rbind(vital_TIVA[!Case_ID%in%agent_ID,],vital_TIVA2)      

names(vital_TIVA_total)
table(vital_TIVA_total[vital_TIVA_total$AGENT_ET!=0]$Case_ID, useNA = 'ifany')

# 프로포폴과 REMIFENTANIL, AGENT_ET 변수만 사용
extract_col = names(vital_TIVA_total)[str_detect(names(vital_TIVA_total), paste(c('PROPOFOL','REMIFENTANIL','AGENT'), collapse = "|"))]
extract_col = extract_col[!str_detect(extract_col, paste(c('RATE','VOL','NAME'), collapse = "|"))]
extract_col = c(extract_col,'Case_ID','Time')

vital_TIVA_total2 = select(vital_TIVA_total,extract_col)
names(vital_TIVA_total2)

vital_TIVA_total2[is.na(vital_TIVA_total2)] = 0

colSums(is.na(vital_TIVA_total2))

saveRDS(vital_TIVA_total2,'preprocessing\\vital_TIVA_NA_END.rds')

# TIVA는 레미펜타닐, 프로포폴에 값이 둘다 기록이 안된 경우 해당 케이스 자체를 제거

names(vital_TIVA_total2)

a = as.data.frame(vital_TIVA_total2)%>%select(Case_ID,REMIFENTANIL_CP,REMIFENTANIL_CE,REMIFENTANIL_CT,PROPOFOL_CP, PROPOFOL_CE, PROPOFOL_CT)%>%group_by(Case_ID)%>%summarise_all(funs(sum))

a$check = apply(a[,c(2:6)],1,sum)

case_except = a[a$check==0,]$Case_ID
length(case_except)   # 198   # 모든 case: 1007

vital_TIVA_total2 = vital_TIVA_total2[!vital_TIVA_total2$Case_ID%in%case_except,]

library(signal)
library(fBasics)
library(seewave)
library(pracma)

library(dplyr)

emr_TIVA = readRDS('preprocessing/emr_TIVA.rds')

emr_vital_tiva = vital_TIVA_total2%>%left_join(emr_TIVA,by='Case_ID')
nrow(emr_vital_tiva)
head(emr_vital_tiva)
str(emr_vital_tiva)

names(emr_vital_tiva)
table(emr_vital_tiva[emr_vital_tiva$OP.Time==0,]$Case_ID) # 1개 존재 

# 수술시간이 0인 부분 vr의 time변수를 통해 전처리 
emr_TIVA[emr_TIVA$OP.Time==0,]$OP.Time=round(difftime(emr_vital_tiva[emr_vital_tiva$OP.Time==0,]$Time[nrow(emr_vital_tiva[emr_vital_tiva$OP.Time==0,])],emr_vital_tiva[emr_vital_tiva$OP.Time==0,]$Time[1],units="mins"))
table(emr_TIVA$OP.Time)

summary(emr_TIVA)

emr_TIVA$Interstitial.lung.disease = NULL
emr_TIVA$Viral.carrier = NULL
emr_TIVA$Autoimmune = NULL
emr_TIVA$Myasthenia.gravis = NULL
emr_TIVA$DCMP = NULL

rss = function(x) rms(x)*sqrt(length(x))

col_ = names(vital_TIVA_total2)[c(1:9)]
TIVA_summary = vital_TIVA_total2 %>%group_by(Case_ID) %>%summarize_at(.vars=col_,.funs=c(mean, sd, rms, rss, IQR))

emr_vital_tiva = TIVA_summary%>%left_join(emr_TIVA,by='Case_ID')

table(emr_vital_tiva$OP.Time)
nrow(emr_vital_tiva)
length(unique(emr_vital_tiva$Case_ID))

names(emr_vital_tiva)
summary(emr_vital_tiva)

tiva_ext = emr_vital_tiva%>%ungroup()%>%select(-Case_ID)
tiva_ext$Sex = NULL
tiva_ext$EF_inspect = as.factor(tiva_ext$EF_inspect)
tiva_ext$PFT_inspect = as.factor(tiva_ext$PFT_inspect)

names(tiva_ext)
str(tiva_ext)
table(tiva_ext$class)

tiva_ext$class = as.factor(tiva_ext$class)

##############################################################################################
# 특질추출 - 변화하는 구간 파악 
##############################################################################################
library(changepoint)

v_id = unique(vital_TIVA_total2$Case_ID)

cp=list()
cp2=list()
cp3=list()

total_peak_mean = data.frame()
total_peak_var = data.frame()
total_peak_meanvar = data.frame()


for(d in v_id){
  
  f = vital_TIVA_total2[vital_TIVA_total2$Case_ID==d,]
  
  print(d)
  
  rslt = sapply(f %>% select(col_),cpt.mean) # 평균 변환
  rslt2 = sapply(f %>% select(col_),cpt.var) 
  rslt3 = sapply(f %>% select(col_),cpt.meanvar) 
  
  total_mean = c()
  total_var = c()
  total_meanvar = c()
  
  for(d_name in col_){
    
    cp[[d_name]] = cpts(rslt[[d_name]])
    cp2[[d_name]] = cpts(rslt2[[d_name]])
    cp3[[d_name]] = cpts(rslt3[[d_name]])
    
    total_mean = cbind(total_mean,length(cp[[d_name]]))
    total_var = cbind(total_var,length(cp2[[d_name]]))
    total_meanvar = cbind(total_meanvar,length(cp3[[d_name]]))
    
  }
  
  total_peak_mean = rbind(total_peak_mean, data.frame(d, total_mean))
  total_peak_var = rbind(total_peak_var, data.frame(d, total_var))
  total_peak_meanvar = rbind(total_peak_meanvar, data.frame(d, total_meanvar))
  
}

name_mean = c()
name_var = c()
name_meanvar = c()

for(d_name in col_){
  name_mean = c(name_mean, paste0("cp1_",d_name))
  name_var = c(name_var,paste0("cp2_",d_name))
  name_meanvar = c(name_meanvar, paste0("cp3_",d_name))
}

names(total_peak_mean) = c('Case_ID',name_mean)
names(total_peak_var) =  c('Case_ID',name_var)
names(total_peak_meanvar) =  c('Case_ID',name_meanvar)

total_peak = total_peak_mean%>%left_join(total_peak_var,by='Case_ID')%>%left_join(total_peak_meanvar,by='Case_ID')
total_peak

##############################################################################################
# 특질추출 - peak 추출  
##############################################################################################
library(changepoint)

p = list()
p_interval = list()
p_interval_std = list()
p_mean = list()
p_max = list()
p_min = list()
p_std = list()

peak_rslt= data.frame()

for(d in v_id){
  
  f = vital_TIVA_total2[vital_TIVA_total2$Case_ID==d,]
  
  print(d)
  
  for(d_name in col_){
    
    p[[d_name]] = ifelse(!is.null(findpeaks(f[[d_name]],threshold = 0.5)),dim(findpeaks(f[[d_name]],threshold = 0.5))[1],0)
    
    p_interval[[d_name]] = ifelse(!is.null(findpeaks(f[[d_name]],threshold = 0.5)),ifelse(dim(findpeaks(f[[d_name]],threshold = 0.5))[1]>2,mean(diff(findpeaks(f[[d_name]],threshold = 0.5)[,2])),0),0)
    
    p_interval_std[[d_name]] = ifelse(!is.null(findpeaks(f[[d_name]],threshold = 0.5)),ifelse(dim(findpeaks(f[[d_name]],threshold = 0.5))[1]>2,std(diff(findpeaks(f[[d_name]],threshold = 0.5)[,2])),0),0)
    
    p_mean[[d_name]] = ifelse(!is.null(findpeaks(f[[d_name]],threshold = 0.5)),mean(findpeaks(f[[d_name]],threshold = 0.5)[,1]),0)
    
    p_max[[d_name]] = ifelse(!is.null(findpeaks(f[[d_name]],threshold = 0.5)),max(findpeaks(f[[d_name]],threshold = 0.5)[,1]),0)
    
    p_min[[d_name]] = ifelse(!is.null(findpeaks(f[[d_name]],threshold = 0.5)),min(findpeaks(f[[d_name]],threshold = 0.5)[,1]),0)
    
    p_std[[d_name]] = ifelse(!is.null(findpeaks(f[[d_name]],threshold = 0.5)),std(findpeaks(f[[d_name]],threshold = 0.5)[,1]),0)
    
    
  }
  
  peak_rslt = rbind(peak_rslt,cbind(d, as.data.frame(p),as.data.frame(p_interval), as.data.frame(p_mean), as.data.frame(p_max),as.data.frame(p_min),as.data.frame(p_std)))
  
}

peak_name = c()

for (i in 2:length(peak_rslt)-1){
  peak_name = c(peak_name,paste0("p",i))
}

names(peak_rslt) = c('Case_ID',peak_name)

#################################################################################################3
# 특질추출-파고율  구하기 
#################################################################################################
crf = list()

crest_rslt = data.frame()

for(d in v_id){
  
  f = vital_TIVA_total2[vital_TIVA_total2$Case_ID==d,]
  
  print(d)
  
  for(d_name in col_){
    
    crf[[d_name]] = crest(f[[d_name]],1)$C
    
  }
  
  crest_rslt = rbind(crest_rslt,cbind(d,as.data.frame(crf)))
  
}

names(crest_rslt) = c('Case_ID',"cre1","cre2","cre3","cre4","cre5","cre6","cre7","cre8","cre9")

peak_final = merge(total_peak, peak_rslt, by="Case_ID")
peak_final = merge(peak_final, crest_rslt, by="Case_ID")
find_na(peak_final,rate=TRUE)

peak_final$cre1 = NULL
peak_final$cre2 = NULL
peak_final$cre3 = NULL

peak_final$Case_ID = as.character(peak_final$Case_ID)

peak_final = cbind(Case_ID=peak_final['Case_ID'],data.frame(sapply(subset(peak_final,select = -Case_ID),function(x) ifelse(is.na(x),median(x, na.rm = TRUE),x))))

total_data = peak_final[1:73]%>%left_join(emr_TIVA,by='Case_ID')

#################################################################################################
# Modeling
#################################################################################################
library(caret)
library(randomForest)

set.seed(7777)

total_data2 = total_data%>%select(-Case_ID) 

train.index <- createDataPartition(y = total_data2$class, p = .6, list = F)
up_train <- upSample(x = total_data2[train.index,][,!names(total_data2[train.index,])=='class'],y = as.factor(total_data2[train.index,]$class))
test = total_data2[-train.index,]

up_train$class<-NULL

train.label = as.numeric(as.factor(up_train$Class))-1
test.label = as.numeric(as.factor(test$class))-1

new_train <- model.matrix(~.+0,data = up_train[,!names(up_train)=='Class'])
new_test <- model.matrix(~.+0,data = test[,!names(test)=='class'])

xgb_train = xgb.DMatrix(data = new_train, label = train.label)
xgb_test = xgb.DMatrix(data = new_test, label = test.label)

params = list(
  booster="gbtree", # 트리기반
  eta=0.01, # 과적합 방지를 위해 단계 크기 축소 # 낮을수록 과적합 일어나지 않음
  max_depth=6, # 나무 최대 깊이 지정 # 숫자가 커질수록 과적합 기본 =6
  gamma=0.2, # 최소 손실 감소 지정 # 클수록 트리 깊이가 줄어든다.
  objective="multi:softprob", # class 분류
  eval_metric="mlogloss", # 평가 metric
  alpha=0.2)

# 검증
xgbcv <- xgb.cv(params = params, data = xgb_train, nrounds = 250, nfold = 10, num_class=3, showsd = TRUE, stratified = TRUE, print_every_n = 10, early_stop_round = 30, maximize = FALSE, prediction = TRUE,verbose = 0)

# 에러율 계산
classification_error <- function(conf_mat) {
  conf_mat = as.matrix(conf_mat)
  error = 1 - sum(diag(conf_mat)) / sum(conf_mat)
  return (error)
}

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
  num_class=3,
  verbose=0
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




# 확인용
# 프로포폴이 모든 CASE에 값이 존재하는가? 
# vital_TIVA_check = vital_TIVA_total2 %>%group_by(Case_ID)%>%summarise(first(PROPOFOL_CP))   # max가 1인 경우는 0으로 처리 
# vital_TIVA_check = vital_TIVA_total2 %>%group_by(Case_ID)%>%summarise(first(PROPOFOL_CE))   # max가 1인 경우는 0으로 처리 
# vital_TIVA_check = vital_TIVA_total2 %>%group_by(Case_ID)%>%summarise(first(PROPOFOL_CT))   # max가 1인 경우는 0으로 처리 
# 
# Vital_TIVA_check = vital_TIVA_total2 %>%group_by(Case_ID)%>%summarise(last(PROPOFOL_CP))   # max가 1인 경우는 0으로 처리 
# vital_TIVA_check = vital_TIVA_total2 %>%group_by(Case_ID)%>%summarise(last(PROPOFOL_CE))   # max가 1인 경우는 0으로 처리 
# vital_TIVA_check = vital_TIVA_total2 %>%group_by(Case_ID)%>%summarise(last(PROPOFOL_CT))   # max가 1인 경우는 0으로 처리 
# 
# vital_TIVA_check = vital_TIVA_total2 %>%group_by(Case_ID)%>%summarise(first(AGENT_ET))   # max가 1인 경우는 0으로 처리 

# HR 20 미만, 200초과 -> 오류 값으로 무시
# # SBP 20미만, 300 이상 -> 오류 값으로 무시 
# 
# vital_TIVA_total2 = vital_TIVA_total[(HR>=20&HR<200),]
# vital_TIVA_total2 = vital_TIVA_total2[(NIBP_SBP>=20&NIBP_SBP<300),]
# 
# names(vital_TIVA_total2)

######################################################################################################################################

# vitalrecord load(volatile)
vital_volatile = data.frame()

system.time(
  
  for (file_id in file_case_volatile$Case_ID){
    
    df = fread(paste0('seoulVR/',file_id,'.csv_tag_filled_del.csv'))
    
    df$Case_ID = file_id
    
    vital_volatile = bind_rows(vital_volatile,df) 
    
  })

str(vital_volatile)

saveRDS(vital_volatile,'preprocessing\\vital_volatile.rds')

vital_volatile = readRDS('preprocessing\\vital_volatile.rds')

# volatile preprocessing
vital_volatile = as.data.table(vital_volatile)
names(vital_volatile)

# AGENT 관련 변수 결측치 처리 
vital_volatile$AGENT_ET[is.na(vital_volatile$AGENT_ET)] = 0
agent_max = vital_volatile[,max(AGENT_ET),by='Case_ID']   # data.table에서만 가능 
agent_max

agent_ID = agent_max[agent_max$V1<1,Case_ID]         # max값이 1미만인 경우 추출 

vital_volatile2 = vital_volatile[Case_ID%in%agent_ID,] 

vital_volatile2$AGENT_ET = 0 

vital_volatile_total = rbind(vital_volatile[!Case_ID%in%agent_ID,],vital_volatile2)      

names(vital_volatile_total)

# 필요한 변수 추출  
vo_col = names(vital_volatile)[str_detect(names(vital_volatile), paste(c('REMIFENTANIL','AGENT','Time'), collapse = "|"))]

vital_volatile = as.data.frame(vital_volatile)

colSums(is.na(vital_volatile))

vital_volatile_total = vital_volatile[,names(vital_volatile)%in%vo_col]

vital_volatile_total[is.na(vital_volatile_total)] = 0

names(vital_volatile_total)

colSums(is.na(vital_volatile_total))

# RATE, VOL 관련 변수 삭제 
vital_volatile_total$REMIFENTANIL_RATE = NULL
vital_volatile_total$REMIFENTANIL_VOL = NULL
vital_volatile_total$AGENT_NAME= NULL

find_na(vital_volatile_total,rate=TRUE)

vital_volatile_total$Case_ID = vital_volatile$Case_ID 
saveRDS(vital_volatile_total,'preprocessing/vital_volatile_NA_END.rds')

vital_volatile_total = readRDS('preprocessing/vital_volatile_NA_END.rds')

names(vital_volatile_total)

# AGENT에 기록이 안된 경우 삭제 
a = as.data.frame(vital_volatile_total)%>%select(Case_ID,AGENT_ET, AGENT_FI, AGENT_MAC)%>%group_by(Case_ID)%>%summarise_all(funs(sum))

a$check = apply(a[,c(2:4)],1,sum)

case_except = a[a$check==0,]$Case_ID
length(case_except)   # 198   # 모든 case: 1007

vital_volatile_total = vital_volatile_total[!vital_volatile_total$Case_ID%in%case_except,]

library(signal)
library(fBasics)
library(seewave)
library(pracma)

library(dplyr)

emr_volatile = readRDS('preprocessing/emr_volatile.rds')

emr_vital_volatile = vital_volatile_total%>%left_join(emr_volatile,by='Case_ID')
nrow(emr_vital_volatile)
head(emr_vital_volatile)
str(emr_vital_volatile)

names(emr_vital_volatile)
table(emr_vital_volatile[emr_vital_volatile$OP.Time==0,]$Case_ID) # 없음. 

summary(emr_volatile)
emr_volatile$Morbid.obesity = NULL

rss = function(x) rms(x)*sqrt(length(x))

col_ = names(vital_volatile_total)[c(2:7)]
volatile_summary = vital_volatile_total %>%group_by(Case_ID) %>%summarize_at(.vars=col_,.funs=c(mean, sd, rms, rss, IQR))

emr_vital_volatile = volatile_summary%>%left_join(emr_volatile,by='Case_ID')

table(emr_vital_volatile$OP.Time)

names(emr_vital_volatile)
summary(emr_vital_volatile)

volatile_ext = emr_vital_volatile%>%ungroup()%>%select(-Case_ID)
volatile_ext$EF_inspect = as.factor(volatile_ext$EF_inspect)
volatile_ext$PFT_inspect = as.factor(volatile_ext$PFT_inspect)

names(volatile_ext)
str(volatile_ext)
table(volatile_ext$class)

volatile_ext$class = as.factor(volatile_ext$class)

# library(caret)
# library(randomForest)
# 
# set.seed(3569)
# 
# index = createDataPartition(tiva_ext$class, p=0.7, list=FALSE)
# 
# train = tiva_ext[index,]
# test = tiva_ext[-index,]
# 
# up_train = upSample(x = train[,!names(train)=='class'],y = as.factor(train$class))
# 
# table(up_train$Class)
# 
# rf = randomForest(formula=as.factor(Class)~.,data = up_train,importance=TRUE,confusion=TRUE,mtry=3,ntree=400,type='prob')
# 
# a = predict(rf,test)
# 
# test = cbind(test,a)
# 
# accuracy = sum(test$class==a)/length(a)
# 
# accuracy
# 
# rf
# 
# confusionMatrix(a, test$class, positive = NULL, dnn = c("Prediction", "Reference"))
# 
# library(RWeka)
# 
# RF = make_Weka_classifier("weka/classifiers/trees/RandomForest")
# 
# m = RF(as.factor(class)~.,data=tiva_ext)
# 
# m
# 
# summary(m)
# 
# e = evaluate_Weka_classifier(m, numFolds = 10, com
##############################################################################################
# 특질추출 - 변화하는 구간 파악 
##############################################################################################
library(changepoint)

v_id = unique(vital_volatile_total$Case_ID)

cp=list()
cp2=list()
cp3=list()

total_peak_mean = data.frame()
total_peak_var = data.frame()
total_peak_meanvar = data.frame()


for(d in v_id){
  
  f = vital_volatile_total[vital_volatile_total$Case_ID==d,]
  
  print(d)
  
  rslt = sapply(f %>% select(col_),cpt.mean) # 평균 변환
  rslt2 = sapply(f %>% select(col_),cpt.var) 
  rslt3 = sapply(f %>% select(col_),cpt.meanvar) 
  
  total_mean = c()
  total_var = c()
  total_meanvar = c()
  
  for(d_name in col_){
    
    cp[[d_name]] = cpts(rslt[[d_name]])
    cp2[[d_name]] = cpts(rslt2[[d_name]])
    cp3[[d_name]] = cpts(rslt3[[d_name]])
    
    total_mean = cbind(total_mean,length(cp[[d_name]]))
    total_var = cbind(total_var,length(cp2[[d_name]]))
    total_meanvar = cbind(total_meanvar,length(cp3[[d_name]]))
    
  }
  
  total_peak_mean = rbind(total_peak_mean, data.frame(d, total_mean))
  total_peak_var = rbind(total_peak_var, data.frame(d, total_var))
  total_peak_meanvar = rbind(total_peak_meanvar, data.frame(d, total_meanvar))
  
}

name_mean = c()
name_var = c()
name_meanvar = c()

for(d_name in col_){
  name_mean = c(name_mean, paste0("cp1_",d_name))
  name_var = c(name_var,paste0("cp2_",d_name))
  name_meanvar = c(name_meanvar, paste0("cp3_",d_name))
}

names(total_peak_mean) = c('Case_ID',name_mean)
names(total_peak_var) =  c('Case_ID',name_var)
names(total_peak_meanvar) =  c('Case_ID',name_meanvar)

total_peak = total_peak_mean%>%left_join(total_peak_var,by='Case_ID')%>%left_join(total_peak_meanvar,by='Case_ID')
total_peak

##############################################################################################
# 특질추출 - peak 추출  
##############################################################################################
library(changepoint)

p = list()
p_interval = list()
p_interval_std = list()
p_mean = list()
p_max = list()
p_min = list()
p_std = list()

peak_rslt= data.frame()

for(d in v_id){
  
  f = vital_volatile_total[vital_volatile_total$Case_ID==d,]
  
  print(d)
  
  for(d_name in col_){
    
    p[[d_name]] = ifelse(!is.null(findpeaks(f[[d_name]],threshold = 0.5)),dim(findpeaks(f[[d_name]],threshold = 0.5))[1],0)
    
    p_interval[[d_name]] = ifelse(!is.null(findpeaks(f[[d_name]],threshold = 0.5)),ifelse(dim(findpeaks(f[[d_name]],threshold = 0.5))[1]>2,mean(diff(findpeaks(f[[d_name]],threshold = 0.5)[,2])),0),0)
    
    p_interval_std[[d_name]] = ifelse(!is.null(findpeaks(f[[d_name]],threshold = 0.5)),ifelse(dim(findpeaks(f[[d_name]],threshold = 0.5))[1]>2,std(diff(findpeaks(f[[d_name]],threshold = 0.5)[,2])),0),0)
    
    p_mean[[d_name]] = ifelse(!is.null(findpeaks(f[[d_name]],threshold = 0.5)),mean(findpeaks(f[[d_name]],threshold = 0.5)[,1]),0)
    
    p_max[[d_name]] = ifelse(!is.null(findpeaks(f[[d_name]],threshold = 0.5)),max(findpeaks(f[[d_name]],threshold = 0.5)[,1]),0)
    
    p_min[[d_name]] = ifelse(!is.null(findpeaks(f[[d_name]],threshold = 0.5)),min(findpeaks(f[[d_name]],threshold = 0.5)[,1]),0)
    
    p_std[[d_name]] = ifelse(!is.null(findpeaks(f[[d_name]],threshold = 0.5)),std(findpeaks(f[[d_name]],threshold = 0.5)[,1]),0)
    
    
  }
  
  peak_rslt = rbind(peak_rslt,cbind(d, as.data.frame(p),as.data.frame(p_interval), as.data.frame(p_mean), as.data.frame(p_max),as.data.frame(p_min),as.data.frame(p_std)))
  
}

peak_name = c()

for (i in 2:length(peak_rslt)-1){
  peak_name = c(peak_name,paste0("p",i))
}

names(peak_rslt) = c('Case_ID',peak_name)

#################################################################################################3
# 특질추출-파고율  구하기 
#################################################################################################
crf = list()

crest_rslt = data.frame()

for(d in v_id){
  
  f = vital_volatile_total[vital_volatile_total$Case_ID==d,]
  
  print(d)
  
  for(d_name in col_){
    
    crf[[d_name]] = crest(f[[d_name]],1)$C
    
  }
  
  crest_rslt = rbind(crest_rslt,cbind(d,as.data.frame(crf)))
  
}

names(crest_rslt) = c('Case_ID',"cre1","cre2","cre3","cre4","cre5","cre6")

peak_final = merge(total_peak, peak_rslt, by="Case_ID")
peak_final = merge(peak_final, crest_rslt, by="Case_ID")
final_total = merge(as.data.frame(volatile_summary), peak_final, by = "Case_ID")
find_na(final_total,rate=TRUE)

peak_final$cre2 = NULL
peak_final$cre3 = NULL
peak_final$cre4 = NULL
peak_final$cre5 = NULL
peak_final$cre6 = NULL

peak_final$Case_ID = as.character(peak_final$Case_ID)

peak_final = cbind(Case_ID=peak_final['Case_ID'],data.frame(sapply(subset(peak_final,select = -Case_ID),function(x) ifelse(is.na(x),median(x, na.rm = TRUE),x))))

names(peak_final)
total_data = peak_final[1:56]%>%left_join(emr_volatile,by='Case_ID')

#################################################################################################
# Modeling
#################################################################################################
library(caret)
library(xgboost)

set.seed(1)

total_data2 = total_data%>%select(-Case_ID) 
str(total_data2)

total_data2$Sex = as.factor(total_data2$Sex)

train.index <- createDataPartition(y = total_data2$class, p = .6, list = F)
up_train <- upSample(x = total_data2[train.index,][,!names(total_data2[train.index,])=='class'],y = as.factor(total_data2[train.index,]$class))
test = total_data2[-train.index,]

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
xgbcv <- xgb.cv(params = params, num_class = 3, data = xgb_train, nrounds = 250, nfold = 10, showsd = TRUE, stratified = TRUE, print_every_n = 10, early_stop_round = 30, maximize = FALSE, prediction = TRUE,verbose = 0)

# 에러율 계산
classification_error <- function(conf_mat) {
  conf_mat = as.matrix(conf_mat)
  error = 1 - sum(diag(conf_mat)) / sum(conf_mat)
  return (error)
}

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


