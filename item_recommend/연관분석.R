# 패키지 설치 
library(readxl)
library(devtools)
library(RCurl)
library(d3Network)
library(igraph)
library(arules)

if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}

# 데이터 불러오기
pet<-read_xlsx("data.xlsx")
head(pet)

# 요약값 확인
summary(pet)

## 고객번호와 상품주무시간을 합쳐 동시에 구매하는 아이템 희소 행렬 생성
# 임시고객번호에 에러 값삭제
pet<-subset(pet,!is.na(pet$임시고객번호))
nrow(pet)

# 임시고객번호 + 상품주문시간
pet_name<- paste(pet$임시고객번호,pet$주문일시)
head(pet_name)


# 같은 고객주문별로 나눔
pet.list_category<-split(pet$카테고리명,pet_name)
pet.list_code<-split(pet$상품코드,pet_name)

# 희소행렬로 생성
pet_trans_category<-as(pet.list_category,"transactions")
pet_trans_code<-as(pet.list_code,"transactions")
summary(pet_trans_category)
summary(pet_trans_code)

## 데이터 탐색
# item 빈도 파악
itemFrequencyPlot(pet_trans_category,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
itemFrequencyPlot(pet_trans_code,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

## 알고리즘 적용
# aprior
pet_rules_category<-apriori(pet_trans_category,parameter = list(supp=0.0005, conf = 0.5, minlen=2))
pet_rules_code<-apriori(pet_trans_code,parameter = list(supp=0.0005,conf=0.9)) # 100개 규칙 생성
# pet_rules_code<-apriori(pet_trans_code,parameter = list(supp=0.005)) # 0개 규칙 생성
# pet_rules_code<-apriori(pet_trans_code,parameter = list(supp=0.0005, conf = 0.5, minlen=3))

summary(pet_rules_category)
summary(pet_rules_code)

# 규칙 및 성능 확인
inspect(pet_rules_category)
inspect(pet_rules_code)

# 결과 시각화(네트워크 분석 시각화 이용)
rules <- labels(pet_rules_code, ruleSep=" ") 
rules <- sapply(rules, strsplit, " ", USE.NAMES=F)
rulemat <- do.call("rbind", rules)

ruleg <- graph.edgelist(rulemat, directed=F)
g1<-ruleg

plot(g1,vertex.color=rainbow(52),
     vertex.label=V(g1)$name,
     vertex.label.cex=0.5,
     vertex.size=V(g1)$degree,
     edge.arrow.size=0.3,
     vertex.label.color="black",
     layout=layout.fruchterman.reingold(g1),
     vertex.label.cex=0.1,main="Layout with fr")

ceb<-cluster_edge_betweenness(g1)
plot(ceb,g1,vertex.label.cex=0.9)

deg<-c(g1)
plot(ceb,g1,vertex.label.cex=0.9)

