library(dplyr)
library(randomForest)
library(corrplot)
library(MASS)
library(car)
library(caret)

setwd("D:\\Program File\\Git\\git_projects\\INDE 498\\INDE498_Project")

train <- read.csv("train.csv")

# finding the dimension of our datasets
dim(train) 

#Data cleaning for train
train$Alley <- as.character(train$Alley)
train$Alley[is.na(train$Alley)] <- "none"
train$Alley <- as.factor(train$Alley)

train$MasVnrType <- as.character(train$MasVnrType)
train$MasVnrType[is.na(train$MasVnrType)] <- "none"
train$MasVnrType <- as.factor(train$MasVnrType)

train$MasVnrArea[is.na(train$MasVnrArea)] <- 0

train$BsmtQual <- as.character(train$BsmtQual)
train$BsmtQual[is.na(train$BsmtQual)] <- "none"
train$BsmtQual <- as.factor(train$BsmtQual)

train$BsmtCond <- as.character(train$BsmtCond)
train$BsmtCond[is.na(train$BsmtCond)] <- "none"
train$BsmtCond <- as.factor(train$BsmtCond)

train$BsmtExposure <- as.character(train$BsmtExposure)
train$BsmtExposure[is.na(train$BsmtExposure)] <- "none"
train$BsmtExposure <- as.factor(train$BsmtExposure)

train$BsmtFinType1 <- as.character(train$BsmtFinType1)
train$BsmtFinType1[is.na(train$BsmtFinType1)] <- "none"
train$BsmtFinType1 <- as.factor(train$BsmtFinType1)

train$BsmtFinType2 <- as.character(train$BsmtFinType2)
train$BsmtFinType2[is.na(train$BsmtFinType2)] <- "none"
train$BsmtFinType2 <- as.factor(train$BsmtFinType2)

train$FireplaceQu <- as.character(train$FireplaceQu)
train$FireplaceQu[is.na(train$FireplaceQu)] <- "none"
train$FireplaceQu <- as.factor(train$FireplaceQu)

train$GarageType <- as.character(train$GarageType)
train$GarageType[is.na(train$GarageType)] <- "none"
train$GarageType <- as.factor(train$GarageType)

train$GarageFinish <- as.character(train$GarageFinish)
train$GarageFinish[is.na(train$GarageFinish)] <- "none"
train$GarageFinish <- as.factor(train$GarageFinish)

train$GarageQual <- as.character(train$GarageQual)
train$GarageQual[is.na(train$GarageQual)] <- "none"
train$GarageQual <- as.factor(train$GarageQual)

train$GarageCond <- as.character(train$GarageCond)
train$GarageCond[is.na(train$GarageCond)] <- "none"
train$GarageCond <- as.factor(train$GarageCond)

train$PoolQC <- as.character(train$PoolQC)
train$PoolQC[is.na(train$PoolQC)] <- "none"
train$PoolQC <- as.factor(train$PoolQC)

train$Fence <- as.character(train$Fence)
train$Fence[is.na(train$Fence)] <- "none"
train$Fence <- as.factor(train$Fence)

train$MiscFeature <- as.character(train$MiscFeature)
train$MiscFeature[is.na(train$MiscFeature)] <- "none"
train$MiscFeature <- as.factor(train$MiscFeature)

train$LotFrontage[is.na(train$LotFrontage)] <- 0

train <- train %>% filter(!is.na(Electrical) & !is.na(GarageYrBlt))

#Removing rare factors
train <- train %>% group_by(MSSubClass) %>% filter(n() >= 6) %>% 
  group_by(MSZoning) %>% filter(n() >= 6) %>%
  group_by(Alley) %>% filter(n() >= 6) %>%
  group_by(LotShape) %>% filter(n() >= 6) %>%
  group_by(LandContour) %>% filter(n() >= 6) %>%
  group_by(LotConfig) %>% filter(n() >= 6) %>%
  group_by(Neighborhood) %>% filter(n() >= 6) %>%
  group_by(Condition1) %>% filter(n() >= 6) %>%
  group_by(Condition2) %>% filter(n() >= 6) %>%
  group_by(BldgType) %>% filter(n() >= 6) %>%
  group_by(HouseStyle) %>% filter(n() >= 6) %>%
  group_by(OverallQual) %>% filter(n() >= 6) %>%
  group_by(OverallCond) %>% filter(n() >= 6) %>%
  group_by(RoofStyle) %>% filter(n() >= 6) %>%
  group_by(RoofMatl) %>% filter(n() >= 6) %>%
  group_by(Exterior1st) %>% filter(n() >= 6) %>%
  group_by(Exterior2nd) %>% filter(n() >= 6) %>%
  group_by(MasVnrType) %>% filter(n() >= 6) %>%
  group_by(ExterQual) %>% filter(n() >= 6) %>%
  group_by(ExterCond) %>% filter(n() >= 6) %>%
  group_by(Foundation) %>% filter(n() >= 6) %>%
  group_by(BsmtQual) %>% filter(n() >= 6) %>%
  group_by(BsmtCond) %>% filter(n() >= 6) %>%
  group_by(BsmtExposure) %>% filter(n() >= 6) %>%
  group_by(BsmtFinType1) %>% filter(n() >= 6) %>%
  group_by(BsmtFinType2) %>% filter(n() >= 6) %>%
  group_by(Heating) %>% filter(n() >= 6) %>%
  group_by(HeatingQC) %>% filter(n() >= 6) %>%
  group_by(CentralAir) %>% filter(n() >= 6) %>%
  group_by(KitchenQual) %>% filter(n() >= 6) %>%
  group_by(Functional) %>% filter(n() >= 6) %>%
  group_by(FireplaceQu) %>% filter(n() >= 6) %>%
  group_by(GarageType) %>% filter(n() >= 6) %>%
  group_by(GarageFinish) %>% filter(n() >= 6) %>%
  group_by(GarageQual) %>% filter(n() >= 6) %>%
  group_by(GarageCond) %>% filter(n() >= 6) %>%
  group_by(PavedDrive) %>% filter(n() >= 6) %>%
  group_by(PoolQC) %>% filter(n() >= 6) %>%
  group_by(Fence) %>% filter(n() >= 6) %>%
  group_by(SaleType) %>% filter(n() >= 6) %>%
  group_by(SaleCondition) %>% filter(n() >= 6)
  

train$MSSubClass <- factor(train$MSSubClass) 
train$MSZoning <- factor(train$MSZoning)
train$Alley <- factor(train$Alley)
train$LotShape <- factor(train$LotShape)
train$LandContour <- factor(train$LandContour)
train$LotConfig <- factor(train$LotConfig)
train$Neighborhood <- factor(train$Neighborhood)
train$Condition1 <- factor(train$Condition1)
train$Condition2 <- factor(train$Condition2)
train$BldgType <- factor(train$BldgType)
train$HouseStyle <- factor(train$HouseStyle)
train$OverallQual <- factor(train$OverallQual)
train$OverallCond <- factor(train$OverallCond)
train$RoofStyle <- factor(train$RoofStyle)
train$RoofMatl <- factor(train$RoofMatl)
train$Exterior1st <- factor(train$Exterior1st)
train$Exterior2nd <- factor(train$Exterior2nd)
train$MasVnrType <- factor(train$MasVnrType)
train$ExterQual <- factor(train$ExterQual)
train$ExterCond <- factor(train$ExterCond)
train$Foundation <- factor(train$Foundation)
train$BsmtQual <- factor(train$BsmtQual)
train$BsmtCond <- factor(train$BsmtCond)
train$BsmtExposure <- factor(train$BsmtExposure)
train$BsmtFinType1 <- factor(train$BsmtFinType1)
train$BsmtFinType2 <- factor(train$BsmtFinType2)
train$Heating <- factor(train$Heating)
train$HeatingQC <- factor(train$HeatingQC)
train$CentralAir <- factor(train$CentralAir)
train$KitchenQual <- factor(train$KitchenQual)
train$Functional <- factor(train$Functional)
train$FireplaceQu <- factor(train$FireplaceQu)
train$GarageType <- factor(train$GarageType)
train$GarageFinish <- factor(train$GarageFinish)
train$GarageQual <- factor(train$GarageQual)
train$GarageCond <- factor(train$GarageCond)
train$PavedDrive <- factor(train$PavedDrive)
train$PoolQC <- factor(train$PoolQC)
train$Fence <- factor(train$Fence)
train$SaleType <- factor(train$SaleType)
train$SaleCondition <- factor(train$SaleCondition)

#Keeping only needed columns and adding binary variables for certain features
train <- train %>% dplyr::select(MSSubClass, GrLivArea, Street, Neighborhood, HouseStyle, LotArea, YearBuilt, YearRemodAdd, Utilities, RoofMatl, Exterior1st,
                          OverallCond, Foundation, BsmtFinType1, FullBath, BsmtFullBath, HalfBath, BsmtHalfBath, Fireplaces, 
                          GarageType, GarageCars, GarageArea, CentralAir, Heating, TotalBsmtSF, WoodDeckSF, OpenPorchSF, EnclosedPorch,
                          Fence, YrSold, SalePrice) %>%
  mutate(NumFullBath = FullBath+BsmtFullBath, NumHalfBath = HalfBath+BsmtHalfBath, Deck = ifelse(WoodDeckSF > 0, 1, 0), 
        Porch = ifelse(OpenPorchSF+EnclosedPorch>0,1,0), Fence = ifelse(Fence=="none",0,1)) %>%
  dplyr::select(-FullBath, -BsmtFullBath, -HalfBath, -BsmtHalfBath, -WoodDeckSF, -OpenPorchSF, -EnclosedPorch, -Street, -Utilities)

#Coercing proper data type
train$MSSubClass <- factor(train$MSSubClass)
train$OverallCond <- factor(train$OverallCond)
train$Fence <- factor(train$Fence)
train$Deck <- factor(train$Deck)
train$Porch <- factor(train$Porch)

#Split data into 2006-2007 and 2008-2010
train.precrash <- train %>% filter(YrSold <= 2007)
train.postcrash <- train %>% filter(YrSold > 2007)

train.precrash <- train.precrash %>% dplyr::select(-YrSold)
train.postcrash <- train.postcrash %>% dplyr::select(-YrSold)

set.seed(50)
trainIndex.pre <- createDataPartition(train.precrash$Neighborhood, p=0.8, list = F, times=1) 
trainIndex.post <- createDataPartition(train.postcrash$Neighborhood, p=0.8, list = F, times=1)

test.precrash <- train.precrash[-trainIndex.pre,]
train.precrash <- train.precrash[trainIndex.pre,]

test.postcrash <- train.postcrash[-trainIndex.post,]
train.postcrash <- train.postcrash[trainIndex.post,]

#Models
#Pre
set.seed(1)
mse.pre.mtry <- c()
x.mtry <- c(1,3,5,7,9,11,13,15,17,19,21,23,25) 
for (i in x.mtry) {
  rf.realistic.pre <- randomForest(SalePrice~., data=train.precrash, ntree=120, importance=TRUE, mtry= i, nodesize= 20)
  mse.pre.mtry <- c(mse.pre.mtry, mean(rf.realistic.pre$mse))
}

set.seed(1)
mse.pre.node <- c()
x.node <- c(5,10,15,20,25,30,35,40,45,50)
for (i in x.node) {
  rf.realistic.pre <- randomForest(SalePrice~., data=train.precrash, ntree=120, importance=TRUE, mtry= 15, nodesize= i)
  mse.pre.node <- c(mse.pre.node, mean(rf.realistic.pre$mse))
}

rf.realistic.pre <- randomForest(SalePrice~., data=train.precrash, ntree=120, importance=TRUE, mtry= 15, nodesize= 15)
pred.pre <- predict(rf.realistic.pre, newdata=test.precrash)
rmse.pre <- sqrt(mean((test.precrash$SalePrice - pred.pre)^2))

#Checking for multicollinearity
#Tons of correlated variables
corrplot(cor(train[,c(2,5,6,7,13,15,16,19,22,23)]), type = "upper")

lm.realistic.pre <- lm(SalePrice~., data=train)

#Transform to log
boxcox(SalePrice~., data=train.precrash)

#Alias variables need to remove one
lm.realistic.pre <- lm(log(SalePrice)~., data=train)
train <- train %>% filter(HouseStyle != "1.5Unf")
lm.realistic.pre <- lm(log(SalePrice)~., data=train)
#Removing highly correlated variables, the rest are categorical variables
vif(lm.realistic.pre) > 10

#Remove multicolinear vars
train <- train %>% dplyr::select(-YearRemodAdd, -GarageCars)

#Heating and Fence not significant
lm.realistic.pre <- lm(log(SalePrice)~., data=train)

#Post
set.seed(1)
mse.post.mtry <- c()
x.mtry <- c(1,3,5,7,9,11,13,15,17,19,21,23,25) 
for (i in x.mtry) {
  rf.realistic.post <- randomForest(SalePrice~., data=train.postcrash, ntree=120, importance=TRUE, mtry= i, nodesize= 20)
  mse.post.mtry <- c(mse.post.mtry, mean(rf.realistic.post$mse))
}

set.seed(1)
mse.post.node <- c()
x.node <- c(5,10,15,20,25,30,35,40,45,50)
for (i in x.node) {
  rf.realistic.post <- randomForest(SalePrice~., data=train.postcrash, ntree=120, importance=TRUE, mtry= 7, nodesize= i)
  mse.post.node <- c(mse.post.node, mean(rf.realistic.post$mse))
}


rf.realistic.post <- randomForest(SalePrice~., data=train.postcrash, ntree=120, importance=TRUE, mtry= 7, nodesize= 10)
pred.post <- predict(rf.realistic.post, newdata = test.postcrash)
rmse.post <- sqrt(mean((test.postcrash$SalePrice - pred.post)^2))
#Checking for multicollinearity
corrplot(cor(train.postcrash[,c(2,5,6,7,13,15,16,19,22,23)]), type = "upper")

#Transform to log
boxcox(SalePrice~., data=train.precrash)

lm.realistic.post <- lm(log(SalePrice)~., data=train.postcrash)
train.postcrash <- train.postcrash %>% filter(HouseStyle != "1.5Unf")
lm.realistic.post <- lm(log(SalePrice)~., data=train.postcrash)
vif(lm.realistic.post) > 10

#Remove multicolinear vars
train.postcrash <- train.postcrash %>% dplyr::select(-YearRemodAdd, -GarageCars)

#Deck, Fence, GarageType, RoofMatl not sig
lm.realistic.post <- lm(log(SalePrice)~., data=train.postcrash)


