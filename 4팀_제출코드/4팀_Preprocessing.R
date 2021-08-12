####################################################
##### 1. 디렉토리, 라이브러리, 데이터 불러오기 #####
####################################################

# 디렉토리 설정 
#setwd('C:/Users/jy991/바탕 화면/2021 summer/방학세미나') #사용 경로로 바꾸기
#getwd()

# 패키지 불러오기
need_packages <- c("data.table", "tidyverse") #사용할 라이브러리로 바꾸기
options(warn = -1)
for(i in 1:length(need_packages)){
  if(require(need_packages[i], character.only = T) == 0){
    install.packages(need_packages[i])
    require(need_packages[i], character.only = T)
  }
  else{require(need_packages[i], character.only = T)}
}

# 데이터 불러오기
data <- fread('data/train.csv',data.table = FALSE)

train <- data
train = train %>% mutate(target = as.factor(train$target)) # target변수 factor로 변환
str(train)

#test셋 불러오기
test <- fread("test.csv", data.table = FALSE)


###################
####  2.EDA   #####
###################
#a. 타겟 클래스 분포 확인
ggplot(train) + geom_bar(aes(x=target),fill = '#CAD9EB') + theme_bw() # 0이 너무 많은 imbalanced data


#b. 결측치 수 확인 
sum(is.na(train))  # 결측치 없음

#c. X변수 간 상관관계 확인
a=train %>% select(-target) %>% cor()
a = a[upper.tri(a)]
sum(abs(a)>0.03) #변수 간 상관관계 0.03 넘는 경우가 없음! (매우 작음)



#####################
######3. 전처리######
#####################

# a. 차원 축소 
# PCA 시행, X 변수 189개
pca_fit <- prcomp(train[,-1], center =  TRUE, scale. = TRUE)
summary(pca_fit) #summary 보고 전체분산 95% 차지하는 변수까지 선택
train_pca <- pca_fit$x[,1:189] %>% as.data.frame 
target <- train$target
train_pca <- cbind(target, train_pca)

#test 셋 PCA 시행
test_pca <- predict(pca_fit, test) %>% as.data.frame %>% select(PC1 : PC189)


# b. 불균형 처리 등
# MWMOTE
library(imbalance)
set.seed(2021)
train_mwmote <- mwmote(train_pca, numInstances = 22382, classAttr = "target") #늘어나는 양만 반환
train_mwmote = rbind(train_pca,train_mwmote) # 원래 데이터와 늘어난 데이터를 합쳐줌
train_mwmote %>% ggplot(aes(x=factor(target))) + geom_bar() #0과 1의 크기가 같아짐


###############################
#### 4. 전처리 데이터 저장 ####
###############################

#첫번째 데이터 셋 (PCA만 적용)
train_pca %>% write.csv("train_pca.csv",row.names= FALSE) 

#두번째 데이터 셋 (PCA + MWMOTE만 적용)
train_mwmote %>% write.csv("train_mwmote.csv",row.names= FALSE) 

#test 데이터 셋 (PCA)
test_pca %>% write.csv("test_pca.csv", row.names = FALSE)

