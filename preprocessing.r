
getwd()
library(stringr)
library(dplyr)
library(readxl)
getwd()

# 파일 불러오기
fls<-dir()
fls<-fls[-1]
fls<-fls[str_detect(fls,"name_lee")]

for(f in fls){
  sheet_name<-excel_sheets(f)
  a<-data.frame()
  for(sh in sheet_name){
    a<-rbind(a,read_excel(f,sheet=sh))
  }
  assign(str_c("JIN-013_pattern"),str_sub(f,nchar(f)-11,nchar(f)-9),a)
  
}

IH_028_pattern<-a
head(IH_028_pattern)
rm(a)
str(IH_028_pattern)
table(IH_028_pattern$Date)
table(IH_028_pattern$범주)


# 범주 영어로 변경
for(i in 1:nrow(IH_028_pattern)){
  if(IH_028_pattern$범주[i]=="굽기조리"|IH_028_pattern$범주[i]=="굽기 조리"){
    IH_028_pattern$범주[i]<-"cooking"
  }
  if(IH_028_pattern$범주[i]=="실내 재실"|IH_028_pattern$범주[i]=="실내재실"){
    IH_028_pattern$범주[i]<-"indoor-house"
  }
  if(IH_028_pattern$범주[i]=="이동_걷기"){
    IH_028_pattern$범주[i]<-"walking"
  }
  if(IH_028_pattern$범주[i]=="이동_버스"){
    IH_028_pattern$범주[i]<-"bus"
  }
  if(IH_028_pattern$범주[i]=="이동_차"|IH_028_pattern$범주[i]=="이동_승용차"){
    IH_028_pattern$범주[i]<-"car"
  }
  if(IH_028_pattern$범주[i]=="일반 상업시설"|IH_028_pattern$범주[i]=="일반 상업 시설"){
    IH_028_pattern$범주[i]<-"commercial building"
  }
  if(IH_028_pattern$범주[i]=="학교 시설"|IH_028_pattern$범주[i]=="학교시설"){
    IH_028_pattern$범주[i]<-"education building"
  }
  if(IH_028_pattern$범주[i]=="실외"){
    IH_028_pattern$범주[i]<-"outdoor"
  }
  if(IH_028_pattern$범주[i]=="식당"){
    IH_028_pattern$범주[i]<-"restaurant"
  }
  if(IH_028_pattern$범주[i]=="간접흡연"|IH_028_pattern$범주[i]=="간접 흡연"){
    IH_028_pattern$범주[i]<-"passive smoking"
  }
}

table(IH_028_pattern$범주)


# NA값에 그 전 값 넣기
for(i in 1:nrow(IH_028_pattern)){
  if(is.na(IH_028_pattern$Date[i])){
    IH_028_pattern$Date[i]<-IH_028_pattern$Date[i-1]
  }
  if(is.na(IH_028_pattern$Name[i])){
    IH_028_pattern$Name[i]<-IH_028_pattern$Name[i-1]}
  if(is.na(IH_028_pattern$ID[i])){
    IH_028_pattern$ID[i]<-IH_028_pattern$ID[i-1]}
}

table(IH_028_pattern$시간)
head(IH_028_pattern)
tail(IH_028_pattern)

# 날짜 format 맞춤
IH_028_pattern$Date<-format(IH_028_pattern$Date,format = "%Y-%m-%d")
IH_028_pattern$시간<-format(IH_028_pattern$시간,format = "%H:%M")
IH_028_pattern$time_char<-paste(IH_028_pattern$Date,IH_028_pattern$시간,sep=" ")

IH_028_pattern$local_time<-as.POSIXct(IH_028_pattern$time_char)

head(IH_028_pattern)
sum(is.na(IH_028_pattern))

# RTI data 가져오고 전처리
setwd("E:\\학교활동\\연구활동\\미세먼지\\최종_20190130\\RTI")
IH_028_RTI<-read.csv("IH-028 name_lee_(SCH-005(IH))_RTI.csv",stringsAsFactor=FALSE)
head(IH_028_RTI)

# ih병원은 아래 코드 실행 X
# JIN_029_RTI<-JIN_029_RTI[-1,]

names(IH_028_RTI)<-c("Date","Time","pm2.5","Temp","RH")
head(IH_028_RTI)

IH_028_RTI$pm2.5<-as.numeric(IH_028_RTI$pm2.5)

sum(is.na(IH_028_RTI$pm2.5))
subset(IH_028_RTI$pm2.5,IH_028_RTI$pm2.5<0)
IH_028_RTI<-IH_028_RTI[!is.na(IH_028_RTI$pm2.5),]

# 분단위의 미세먼지 값 조정 전의 -값을 보정해야한다면 해야할 코드
for(i in 2:nrow(IH_028_RTI)){
  if(IH_028_RTI$pm2.5[i]<0){
    IH_028_RTI$pm2.5[i]<-IH_028_RTI$pm2.5[i-1]
  }
}

subset(IH_028_RTI$pm2.5,IH_028_RTI$pm2.5<0)

# 날짜 format 맞추기
IH_028_RTI$Date<-format(IH_028_RTI$Date,format = "%Y-%m-%d")
B<-as.POSIXct(IH_028_RTI$Time,format = "%H:%M")
IH_028_RTI$Time<-format(B,format = "%H:%M")
IH_028_RTI$time_char<-paste(IH_028_RTI$Date,IH_028_RTI$Time,sep=" ")
table(IH_028_RTI$Date)
head(IH_028_RTI)

# 공통적인 날짜만 가져오기
IH_028_RTI<-filter(IH_028_RTI,IH_028_RTI$Date %in% IH_028_pattern$Date)
#JIN_009_RTI<-filter(JIN_009_RTI,JIN_009_RTI$Date!=JIN_009_pattern$Date[1])
IH_028_pattern<-filter(IH_028_pattern,IH_028_pattern$Date %in% IH_028_RTI$Date)
table(IH_028_pattern$Date)
table(IH_028_RTI$Date)

IH_028_RTI<-IH_028_RTI[order(as.POSIXct(IH_028_RTI$time_char)),]
head(IH_028_RTI)
sum(is.na(IH_028_RTI$pm2.5))

table(IH_028_pattern$범주)

# 초단위를 분단위로 바꾸면서 미세먼지 평가(na값 때문에 결과개수 다르게 나옴)
C<-aggregate(as.numeric(pm2.5)~time_char+Date,IH_028_RTI,mean)
IH_028_RTI<-C
head(IH_028_RTI)
IH_028_RTI<-IH_028_RTI[order(as.POSIXct(IH_028_RTI$time_char)),]

table(is.na(IH_028_RTI))
class(IH_028_RTI$time_char)
table(IH_028_RTI$Date)


# close time 계산을 위해 바꿔줌
IH_028_RTI<-as_tibble(IH_028_RTI)
IH_028_pattern<-as_tibble(IH_028_pattern)
head(IH_028_RTI)
head(IH_028_pattern)


# close_time을 계산하기 위해
IH_028_RTI$local_time<-as.POSIXct(IH_028_RTI$time_char)
class(IH_028_RTI$time_char)
close_time<-c()

for(i in 1:nrow(IH_028_pattern)){
  close_time<-rbind(close_time,IH_028_RTI[which.min(abs(difftime(IH_028_RTI$local_time,IH_028_pattern$local_time[i]))),4])
}


head(close_time)
IH_028_pattern$close_time<-close_time$local_time
IH_028_pattern$diff<-abs(difftime(IH_028_pattern$local_time,IH_028_pattern$close_time))

table(IH_028_RTI$Date)


# 합치기
FULL_IH_028<-full_join(IH_028_pattern,IH_028_RTI,by="time_char")

class(FULL_IH_028$time_char)
FULL_IH_028$time_char<-as.POSIXct(FULL_IH_028$time_char)
FULL_IH_028<-FULL_IH_028[order(as.POSIXct(FULL_IH_028$time_char)),]

head(FULL_IH_028)
names(FULL_IH_028)

for(x in 2:nrow(FULL_IH_028)){
  if(is.na(FULL_IH_028$ID[x])){
    FULL_IH_028$ID[x]<-FULL_IH_028$ID[x-1]}
  if(is.na(FULL_IH_028$Name[x])){
    FULL_IH_028$Name[x]<-FULL_IH_028$Name[x-1]}
  if(is.na(FULL_IH_028$주행동[x])){
    FULL_IH_028$주행동[x]<-FULL_IH_028$주행동[x-1]}
  if(is.na(FULL_IH_028$범주[x])){
    FULL_IH_028$범주[x]<-FULL_IH_028$범주[x-1]}
  if(is.na(FULL_IH_028$비고[x])){
    FULL_IH_028$비고[x]<-FULL_IH_028$비고[x-1]}
}


# 합치면서 생긴 diff 결측치 값을 0으로 두고 한시간 이상 차이나는 것을 제외함.
FULL_IH_028$diff[is.na(FULL_IH_028$diff)]<-0
FULL_IH_028<-subset(FULL_IH_028,diff<600)

# 바로 전 값의 na 처리
f_na_2<-function(x){
  for(i in 1:eval(length(x)-1)){
    if(is.na(x[i])){
      x[i]<-x[i+1]}
    if(is.na(x[length(x)])){x[length(x)]<-x[eval(length(x)-1)]}
  }
  return(x)
}

FULL_IH_028<-as.data.frame(lapply(FULL_IH_028,f_na_2))
head(FULL_IH_028)

FULL_IH_028$Date.x<-FULL_IH_028$Date.y
names(FULL_IH_028)

# 불필요한 컬럼 삭제
FULL_IH_028<-FULL_IH_028[-c(4,9,10)]
sum(is.na(FULL_IH_028))


# 다음 날로 넘어가면 00:00:00 넣기
for(i in 1:(nrow(FULL_IH_028)-1)){
  if(difftime(FULL_IH_028$Date.x[i+1],FULL_IH_028$Date.x[i])== 1){
    FULL_IH_028$time_char[i+1]<-paste(FULL_IH_028$Date.x[i+1],"00:00:00")
  }
}

table(FULL_IH_028$Date.x)
table(FULL_IH_028$범주)

# 초기값 생성
# 실내: 각 날마다의 초기값
# 실외: 외출 한 후 5분의 중앙값
initial<-c()

ABC<-seq(as.POSIXct(FULL_IH_028$Date.x[1]),as.POSIXct(FULL_IH_028$Date.x[nrow(FULL_IH_028)]),by="day")
ABC<-as.character(ABC)

for(i in 1:(nrow(FULL_IH_028))){
  for(x in 1:length(ABC)){
    if(FULL_IH_028$주행동[i]=="실내"&FULL_IH_028$Date.x[i]==ABC[x]|FULL_IH_028$주행동[i]=="건물"&FULL_IH_028$Date.x[i]==ABC[x]){
      F_1<-subset(FULL_IH_028$as.numeric.pm2.5.,FULL_IH_028$주행동=="실내"&FULL_IH_028$Date.x==ABC[x]|FULL_IH_028$주행동=="건물"&FULL_IH_028$Date.x==ABC[x])[1]
      initial<-c(initial,F_1)} 
    
    else if(FULL_IH_028$주행동[i]=="이동"&FULL_IH_028$Date.x[i]==ABC[x]|FULL_IH_028$주행동[i]=="실외"&FULL_IH_028$Date.x[i]==ABC[x]){
      F_2<-subset(FULL_IH_028$local_time.y,FULL_IH_028$주행동=="이동"&FULL_IH_028$Date.x==ABC[x]|FULL_IH_028$주행동=="실외"&FULL_IH_028$Date.x==ABC[x])[1]
      F_3<-subset(FULL_IH_028$as.numeric.pm2.5.,(FULL_IH_028$local_time.y>=F_2&FULL_IH_028$local_time.y<=F_2+300))
      F_4<-median(F_3)
      initial<-c(initial,F_4)
    }
  }
}

table(initial)
FULL_IH_028$initial<-initial


# 초기값 - 해당 미세먼지 (변화량)
Diff<-c()
for(i in 1:nrow(FULL_IH_028)){
  Fd<-FULL_IH_028$initial[i]-FULL_IH_028$as.numeric.pm2.5.[i]
  Diff<-c(Diff,Fd)
}

FULL_IH_028$diff_PM2_5<-Diff
head(FULL_IH_028)

# 경과시간 계산
for(i in 1:eval(nrow(FULL_IH_028)-1)){
  FULL_IH_028$elapsed_time[i] <-difftime(as.POSIXct(FULL_IH_028$time_char[i+1]), as.POSIXct(FULL_IH_028$time_char[i]), units = "mins")}

# 아래 코드는 전과달리 마지막 날 시간에 굳이 1440분으로 안맞춰줘도 되서 실행하지 않음.
# FULL_JIN_029$elapsed_time[nrow(FULL_JIN_029)] <- difftime(as.POSIXct(paste(FULL_JIN_029$Date[nrow(FULL_JIN_029)],"24:00:00")), as.POSIXct(FULL_JIN_029$time_char[nrow(FULL_JIN_029)]),units="mins")

# 경과시간*미세먼지
FULL_IH_028<-cbind(FULL_IH_028,accumul=FULL_IH_028$elapsed_time*FULL_IH_028$as.numeric.pm2.5.)
head(FULL_IH_028)
table(FULL_IH_028$범주)
table(FULL_IH_028$주행동)

# 불필요한 컬럼 삭제하고 컬럼 네임 재설정
FULL_IH_028$Date.y<-NULL
FULL_IH_028$local_time.y<-NULL
names(FULL_IH_028)<-c("ID","Name","Date","주행동","비고","범주","time_char","diff","PM2_5","initial","diff_PM2_5","elapsed_time","accumul")

# 미세먼지 1000이상 확인
subset(FULL_IH_028,FULL_IH_028$PM2_5>=1000)

# 실내 실외 초기값 날짜와 함께 적어 csv파일로 내보냄.
initial_2<-c()
initial_3<-c()
ABC<-seq(as.POSIXct(FULL_IH_028$Date[1]),as.POSIXct(FULL_IH_028$Date[nrow(FULL_IH_028)]),by="day")
ABC<-as.character(ABC)

for(i in 1:(nrow(FULL_IH_028))){
  for(x in 1:length(ABC)){
    if(FULL_IH_028$주행동[i]=="실내"&FULL_IH_028$Date[i]==ABC[x]|FULL_IH_028$주행동[i]=="건물"&FULL_IH_028$Date[i]==ABC[x]){
      F_1<-paste(FULL_IH_028$Date[i],"실내",subset(FULL_IH_028$PM2_5,FULL_IH_028$주행동=="실내"&FULL_IH_028$Date==ABC[x]|FULL_IH_028$주행동=="건물"&FULL_IH_028$Date==ABC[x])[1])
      initial_2<-c(initial_2,F_1)} 
    
    else if(FULL_IH_028$주행동[i]=="이동"&FULL_IH_028$Date[i]==ABC[x]|FULL_IH_028$주행동[i]=="실외"&FULL_IH_028$Date[i]==ABC[x]){
      F_2<-subset(FULL_IH_028$time_char,FULL_IH_028$주행동=="이동"&FULL_IH_028$Date==ABC[x]|FULL_IH_028$주행동=="실외"&FULL_IH_028$Date==ABC[x])[1]
      F_3<-subset(FULL_IH_028$PM2_5,as.POSIXct(FULL_IH_028$time_char)>=F_2&as.POSIXct(FULL_IH_028$time_char)<=F_2+300)
      F_4<-paste(FULL_IH_028$Date[i],"실외",median(F_3))
      initial_2<-c(initial_2,F_4)
    }
  }
}

initial_3<-unique(unlist(initial_2))
initial_3<-as.data.frame(do.call(rbind,strsplit(initial_3," ")))
write.csv(initial_3,file="E:\\학교활동\\연구활동\\미세먼지\\R데이터\\FULL_IH_028_initial.csv")



# 주말이 있다면 주말 제외(ij병원)
# table(FULL_JIN_029$Date)
# AAA<-subset(FULL_JIN_029,Date!="2019-01-26")
# FULL_JIN_029<-subset(AAA,Date!="2019-01-27")

# 주말이 있다면 주말 제외(ih병원)
table(FULL_IH_028$Date)
AAA<-subset(FULL_IH_028,Date!="2018-10-13")
FULL_IH_028<-subset(AAA,Date!="2018-10-14")

save.image(file="E:\\학교활동\\연구활동\\미세먼지\\R데이터\\IH_028_name_lee.RData")


# Relative Frequency(%) -학교시설만
FULL_school<-subset(FULL_IH_028,FULL_IH_028$범주=="education building")

histo<-hist(FULL_school$accumul,col = "gray")
n<-length(FULL_school$accumul)
barplot(histo$counts/n,col="gray",space=0,)->bp 
axis(1,at=c(bp),labels=histo$mids)
title(ylab="Relative Frequency(%)",xlab="PM2_5",main = "학교 시설_어린이집(name_lee)")
