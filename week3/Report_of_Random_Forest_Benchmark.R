---
  title: "Report of Random Forest Benchmark"
output: html_document
---
  
  # DATA from https://www.kaggle.com/benhamner/random-forest-benchmark-r/data
  
  # 載入library

library(ggplot2)
library(randomForest)


# 讀取資料，並保留字串資料型態

set.seed(1)
train <- read.csv("./all/train.csv", stringsAsFactors=FALSE)
test  <- read.csv("./all/test.csv",  stringsAsFactors=FALSE)


# 指派函數extractFeatures，輸出結果為fea。指派向量("Pclass","Age","Sex","Parch","SibSp","Fare","Embarked")為features。指派fea為變數資料中選擇features的結果。將-1指派為值為NA的fea$Age，fea$Fare的中位數指派為值為NA的fea$Fare，"S"指派為空值的fea$Embarked，將fea$Sex、fea$Embarked轉換為因子。

extractFeatures <- function(data) {
  features <- c("Pclass",
                "Age",
                "Sex",
                "Parch",
                "SibSp",
                "Fare",
                "Embarked")
  fea <- data[,features]
  fea$Age[is.na(fea$Age)] <- -1
  fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm=TRUE)
  fea$Embarked[fea$Embarked==""] = "S"
  fea$Sex      <- as.factor(fea$Sex)
  fea$Embarked <- as.factor(fea$Embarked)
  return(fea)
}


#建構隨機森林模型，extractFeatures(train)作為模型變量，將train$Survived轉換為因子，隨機森林中的決策樹數目設定為100，計算各個變量在模型中的重要性。

rf <- randomForest(extractFeatures(train), as.factor(train$Survived), ntree=100, importance=TRUE)


# 創造PassengerId的資料框submission。預測新資料submission$Survived。讀檔1_random_forest_r_submission.csv。

submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- predict(rf, extractFeatures(test))
write.csv(submission, file = "1_random_forest_r_submission.csv", row.names=FALSE)

#計算rf的重要性，計算重要性的方法為type1，並將結果指派為imp。創造資料框featureImportance，內含Feature和Importance。

imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])


#作長條圖，資料為featureImportance，x軸為Feature、Importance，y軸為Importance，對換座標軸、自訂x軸與y軸標籤，訂定標題Random Forest Feature Importance

ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

