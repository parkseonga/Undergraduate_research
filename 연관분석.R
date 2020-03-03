library(readxl)
library(devtools)
library(RCurl)
library(d3Network)
library(igraph)
library(arules)

setwd("E:\\학교활동\\연구활동\\펫박스")

pet<-read_xlsx("190402_일부주문정보(17_19년도)_사은품제외_v1.xlsx")
head(pet)
summary(pet)

# 임시고객번호에 에러 값삭제
pet<-subset(pet,!is.na(pet$임시고객번호))
nrow(pet)

# 임시고객번호 + 상품주문시간
pet_name<- paste(pet$임시고객번호,pet$주문일시)
head(pet_name)


# 같은 고객주문별로 나눔
pet.list_category<-split(pet$카테고리명,pet_name)
pet.list_code<-split(pet$상품코드,pet_name)

# transaction형태로 변환
pet_trans_category<-as(pet.list_category,"transactions")
pet_trans_code<-as(pet.list_code,"transactions")
summary(pet_trans_category)
summary(pet_trans_code)

if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}


# item 빈도 파악
itemFrequencyPlot(pet_trans_category,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
itemFrequencyPlot(pet_trans_code,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

# aprior
pet_rules_category<-apriori(pet_trans_category,parameter = list(supp=0.0005, conf = 0.5, minlen=2))
pet_rules_code<-apriori(pet_trans_code,parameter = list(supp=0.0005,conf=0.9)) # 100개 규칙 생성
# pet_rules_code<-apriori(pet_trans_code,parameter = list(supp=0.005)) # 0개 규칙 생성
# pet_rules_code<-apriori(pet_trans_code,parameter = list(supp=0.0005, conf = 0.5, minlen=3))

summary(pet_rules_category)
summary(pet_rules_code)

inspect(pet_rules_category)
inspect(pet_rules_code)

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

