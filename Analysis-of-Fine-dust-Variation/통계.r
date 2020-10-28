# ig 병원 합치기 
JIN_FULL<-rbind(FULL_JIN_013,FULL_JIN_014,FULL_JIN_029)
save.image(file="E:\\학교활동\\연구활동\\미세먼지\\R데이터\\JIN_FULL.RData")

# ih 병원 합치기
IH_FULL<-rbind(FULL_IH_007,FULL_IH_008,FULL_IH_009,FULL_IH_013,FULL_IH_020,FULL_IH_028)
save.image(file="E:\\학교활동\\연구활동\\미세먼지\\R데이터\\IH_FULL.RData")

FULL_IH_JIN<-rbind(IH_FULL,JIN_FULL)
save.image(file="E:\\학교활동\\연구활동\\미세먼지\\R데이터\\IH_JIN_FULL.RData")

# 날짜별 미세먼지 총합
date_pm<-aggregate(PM2_5~Date+Name,FULL_IH_JIN,sum)
write.csv(date_pm,file="E:\\학교활동\\연구활동\\미세먼지\\R데이터\\date_pm2.5_2.csv")

# 범주별 인별 미세먼지 중앙값
분석1_1<-aggregate(PM2_5~범주+Name,FULL_IH_JIN,median)
write.csv(분석1_1,file="E:\\학교활동\\연구활동\\미세먼지\\R데이터\\분석1_1_미세먼지(범주median).csv")

# 범주별 인별 미세먼지 누적 노출량 중앙값
분석1_2<-aggregate(accumul~범주+Name,FULL_IH_JIN,median)
write.csv(분석1_2,file="E:\\학교활동\\연구활동\\미세먼지\\R데이터\\분석1_2_미세먼지 누적(범주median).csv")

# ih 미세먼지 누적 노출량 중앙값
h_분석1_4<-aggregate(accumul~범주,IH_FULL,median)
write.csv(h_분석1_4,file="E:\\학교활동\\연구활동\\미세먼지\\R데이터\\h분석1_4(범주median).csv")

# ig 병원 미세먼지 누적 노출량 중앙값
jin_분석1_4<-aggregate(accumul~범주,JIN_FULL,median)
write.csv(jin_분석1_4,file="E:\\학교활동\\연구활동\\미세먼지\\R데이터\\jin분석1_4(범주median).csv")


# ci 함수 생성
norm.interval <- function(x) {
  xbar = mean(x)
  sdx = sd(x)/sqrt(length(x))
  c(xbar - 1.96 * sdx, xbar + 1.96* sdx)
}

# 평균, 표준편차, ci 한번에 나타나도록
F<-function(x) c( mean=mean(x), sd=sd(x), ci=norm.interval(x))

FULL_accumul_T<-aggregate(accumul~Name+범주,FULL_IH_JIN,F)
FULL_PM2.5_T<-aggregate(PM2_5~Name+범주,FULL_IH_JIN,F)

write.csv(FULL_PM2.5_T,file="E:\\학교활동\\연구활동\\미세먼지\\R데이터\\FULL_t_분석1_4(범주미세먼지 통계).csv")
write.csv(FULL_accumul_T,file="E:\\학교활동\\연구활동\\미세먼지\\R데이터\\FULL_t_분석1_4(범주누적통계.csv")

# 영역 나눠서 날자별 미세먼지 분포 확인
d<-aggregate(FULL_IH_008$accumul~FULL_IH_008$범주+FULL_IH_008$Date,list(FULL_IH_008),sum)
names(d)<-c("범주","날짜","accumul")
ggplot(data=d,aes(x=날짜, y=accumul,fill=범주)) + geom_bar(stat="identity")+labs(title="일자별 미세먼지 누적 분포(name2)",x="날짜",y="PM2_5") +theme(axis.text.x = element_text(angle=45,vjust = 0.6))+facet_grid(날짜~.)


# 범주별로 그래프를 그리기 위함
# cooking
FULL_IH_JIN_cooking<-subset(FULL_IH_JIN,범주=="cooking")

table(FULL_IH_JIN_cooking$Name)
f<-FULL_IH_JIN_cooking[c(1:4),]
d<-c("name1","name2","name3","name4")
f[,2]<-d
f[,c(8:13)]<-NA
FULL_IH_JIN_cooking<-rbind(FULL_IH_JIN_cooking,f)
tail(FULL_IH_JIN_cooking)

#bus
FULL_IH_JIN_bus<-subset(FULL_IH_JIN,범주=="bus")

table(FULL_IH_JIN_bus$Name)
f<-FULL_IH_JIN_bus[c(1:5),]
d<-c("name2","name5","name6","name7","name8")
f[,2]<-d
f[,c(8:13)]<-NA
FULL_IH_JIN_bus<-rbind(FULL_IH_JIN_bus,f)
head(FULL_IH_JIN_bus)

#car
FULL_IH_JIN_car<-subset(FULL_IH_JIN,범주=="car")
table(FULL_IH_JIN_car$Name)
f<-FULL_IH_JIN_car[1,]
d<-c("name9")
f[,2]<-d
f[,c(8:13)]<-NA
FULL_IH_JIN_car<-rbind(FULL_IH_JIN_car,f)
tail(FULL_IH_JIN_car)

#commercial building
FULL_IH_JIN_commercialbuilding<-subset(FULL_IH_JIN,범주=="commercial building")
f<-FULL_IH_JIN_commercialbuilding[1,]
table(FULL_IH_JIN_commercialbuilding$Name)

d<-c("name5")
f[,2]<-d
f[,c(8:13)]<-NA
FULL_IH_JIN_commercialbuilding<-rbind(FULL_IH_JIN_commercialbuilding,f)
tail(FULL_IH_JIN_commercialbuilding)

#education building
FULL_IH_JIN_educationbuilding<-subset(FULL_IH_JIN,범주=="education building")
f<-FULL_IH_JIN_educationbuilding[1,]
table(FULL_IH_JIN_educationbuilding$Name)

d<-c("name4")
f[,2]<-d
f[,c(8:13)]<-NA
FULL_IH_JIN_educationbuilding<-rbind(FULL_IH_JIN_educationbuilding,f)
tail(FULL_IH_JIN_educationbuilding)

#walking
FULL_IH_JIN_walking<-subset(FULL_IH_JIN,범주=="walking")

table(FULL_IH_JIN_walking$Name)
f<-FULL_IH_JIN_walking[c(1:3),]
d<-c("name2","name4","name4")
f[,2]<-d
f[,c(8:13)]<-NA
FULL_IH_JIN_walking<-rbind(FULL_IH_JIN_walking,f)
tail(FULL_IH_JIN_walking)

#outdoor
FULL_IH_JIN_outdoor<-subset(FULL_IH_JIN,범주=="outdoor")

table(FULL_IH_JIN_outdoor$Name)
f<-FULL_IH_JIN_outdoor[c(1:7),]
d<-c("name1","name2","name3","name4","name5","name6","name7")
f[,2]<-d
f[,c(8:13)]<-NA
FULL_IH_JIN_outdoor<-rbind(FULL_IH_JIN_outdoor,f)
tail(FULL_IH_JIN_outdoor)

# indoor-house
FULL_IH_JIN_indoorhouse<-subset(FULL_IH_JIN,범주=="indoor-house")
table(FULL_IH_JIN_indoorhouse$Name)

#passive smoking
FULL_IH_JIN_passivesmoking<-subset(FULL_IH_JIN,범주=="passive smoking")
table(FULL_IH_JIN_passivesmoking$Name)
f<-FULL_IH_JIN_passivesmoking[c(1:8),]
d<-c("name1","name2","namae3","name4","name5","name6","name7","name8")
f[,2]<-d
f[,c(8:13)]<-NA
FULL_IH_JIN_passivesmoking<-rbind(FULL_IH_JIN_passivesmoking,f)
tail(FULL_IH_JIN_passivesmoking)

#restaurant
FULL_IH_JIN_restaurant<-subset(FULL_IH_JIN,범주=="restaurant")
table(FULL_IH_JIN_restaurant$Name)
f<-FULL_IH_JIN_restaurant[c(1:7),]
d<-c("name1","name2","name3","name4","name5","name6","name5")
f[,2]<-d
f[,c(8:13)]<-NA
FULL_IH_JIN_restaurant<-rbind(FULL_IH_JIN_restaurant,f)
tail(FULL_IH_JIN_restaurant)


# 범주별 미세먼지 변화량 그래프
library(ggplot2)
ggplot(FULL_IH_JIN_bus,aes(x=Name,y=diff_PM2_5))+geom_boxplot()+ggtitle("초기값 대비 미세먼지 변화량_bus")+theme(axis.text.x = element_text(angle=45,vjust = 0.6))
ggplot(FULL_IH_JIN_car,aes(x=Name,y=diff_PM2_5))+geom_boxplot()+ggtitle("초기값 대비 미세먼지 변화량_car")+theme(axis.text.x = element_text(angle=45,vjust = 0.6))
ggplot(FULL_IH_JIN_cooking,aes(x=Name,y=diff_PM2_5))+geom_boxplot()+ggtitle("초기값 대비 미세먼지 변화량_cooking")+theme(axis.text.x = element_text(angle=45,vjust = 0.6))
ggplot(FULL_IH_JIN_passivesmoking,aes(x=Name,y=diff_PM2_5))+geom_boxplot()+ggtitle("초기값 대비 미세먼지 변화량_passive smoking")+theme(axis.text.x = element_text(angle=45,vjust = 0.6))
ggplot(FULL_IH_JIN_indoorhouse,aes(x=Name,y=diff_PM2_5))+geom_boxplot()+ggtitle("초기값 대비 미세먼지 변화량_indoor-house")+theme(axis.text.x = element_text(angle=45,vjust = 0.6))
ggplot(FULL_IH_JIN_outdoor,aes(x=Name,y=diff_PM2_5))+geom_boxplot()+ggtitle("초기값 대비 미세먼지 변화량_outdoor")+theme(axis.text.x = element_text(angle=45,vjust = 0.6))
ggplot(FULL_IH_JIN_restaurant,aes(x=Name,y=diff_PM2_5))+geom_boxplot()+ggtitle("초기값 대비 미세먼지 변화량_restaurant")+theme(axis.text.x = element_text(angle=45,vjust = 0.6))
ggplot(FULL_IH_JIN_walking,aes(x=Name,y=diff_PM2_5))+geom_boxplot()+ggtitle("초기값 대비 미세먼지 변화량_walking")+theme(axis.text.x = element_text(angle=45,vjust = 0.6))
ggplot(FULL_IH_JIN_commercialbuilding,aes(x=Name,y=diff_PM2_5))+geom_boxplot()+ggtitle("초기값 대비 미세먼지 변화량_commercial building")+theme(axis.text.x = element_text(angle=45,vjust = 0.6))
ggplot(FULL_IH_JIN_educationbuilding,aes(x=Name,y=diff_PM2_5))+geom_boxplot()+ggtitle("초기값 대비 미세먼지 변화량_education building")+theme(axis.text.x = element_text(angle=45,vjust = 0.6))



# 구글비즈 시각화

library(googleVis)
FULL_029_30<-subset(FULL_JIN_029,FULL_JIN_029$Date=="2019-01-30")
head(FULL_029_30)

c<-strsplit(as.character(FULL_029_30$time_char)," ")
d<-sapply(c,function(l)l[[2]])
class(d)
class(FULL_029_30$Date)
FULL_029_30$time<-as.factor(d)
head(FULL_029_30)
class(FULL_029_30$time)
FULL_029_30<-as_tibble(FULL_029_30)
FULL_029_30$time<-strptime(FULL_029_30$time, format="%H:%M")
FULL_029_30$범주<-as.character(FULL_029_30$범주)
class(FULL_029_30$time)

FULL_029_30$time_char<-as.POSIXct(FULL_029_30$time_char)
FULL_029_30$time_char<-strptime(FULL_029_30$time_char, format="%Y-%m-%d %H:%M")
class(FULL_029_30$time_char)
A5 <- gvisAnnotationChart(FULL_029_30, datevar="time_char",
                          numvar="PM2_5",
                          idvar = "범주",
                          options=list(scaleType='allmaximized',width=800, height=380,displayExactValues=TRUE,displayAnnotations=TRUE)
)

A5$html$header <- gsub("charset=utf-8", "charset=euc-kr", A5$html$header)

windowsFonts(malgun = windowsFont("맑은 고딕"))
windowsFonts()

plot(A5)

setwd("E:\\학교활동\\연구활동\\미세먼지\\ig\\구글비즈")
print(A5, file="JIN-029.30DAY.html")

# 시각화
a<-aggregate(FULL_JIN_029["accumul"],list(FULL_JIN_029$범주),sum)
b<-aggregate(FULL_JIN_029["accumul"],list(FULL_JIN_029$Date),sum)

time<-aggregate(FULL_JIN_029["elapsed_time"],list(FULL_JIN_029$범주),sum)
c<-aggregate(FULL_JIN_029["elapsed_time"],list(FULL_JIN_029$Date),sum)

#미세먼지 누적량 
library(ggplot2)
ggplot(FULL_JIN_029,aes(범주,PM2_5)) +geom_boxplot()+labs(title="미세먼지 분포",x="범주",y="PM2_5") +theme(axis.text.x = element_text(angle=0,vjust = 0.6))
ggplot(data=a, aes(x=Group.1, y=accumul)) + geom_bar(stat="identity")+labs(title="미세먼지 누적 분포",x="범주",y="PM2_5") +theme(axis.text.x = element_text(angle=0,vjust = 0.6))
ggplot(data=c,aes(x=Group.1, y=elapsed_time)) + geom_bar(stat="identity")+labs(title="일자별 시간 누적 분포",x="날짜",y="elapsed_time") +theme(axis.text.x = element_text(angle=0,vjust = 0.6))

ggplot(data=time, aes(x=Group.1, y=elapsed_time)) + geom_bar(stat="identity")+labs(title="활동시간 누적 분포",x="범주",y="elapsed_time") +theme(axis.text.x = element_text(angle=0,vjust = 0.6))
ggplot(data=b, aes(x=Group.1, y=accumul)) + geom_bar(stat="identity")+labs(title="일자별 미세먼지 누적 분포",x="날짜",y="PM2_5") +theme(axis.text.x = element_text(angle=0,vjust = 0.6))


# facet_grid함수 사용

FULL_B<-c("bus","car","commercial building","cooking","education building","indoor-house","outdoor","passive smoking","restaurant","walking")

f<-FULL_IH_008[c(1:10),]
f[,6]<-FULL_B
f[,c(9:13)]<-0

FULL_IH_008_2<-rbind(FULL_IH_008,f)

F_22<-aggregate(FULL_IH_008_2$accumul~FULL_IH_008_2$범주+FULL_IH_008_2$Date,list(FULL_IH_007),sum)
names(F_22)<-c("범주","날짜","accumul")
ggplot(data=F_22,aes(x=날짜, y=accumul,fill=범주)) + geom_bar(stat="identity")+labs(title="일자별 미세먼지 누적 분포(name2)",x="날짜",y="PM2_5") +theme(axis.text.x = element_text(angle=45,vjust = 0.6))+facet_grid(날짜~.)

