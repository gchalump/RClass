setwd("E:/OneDrive - Texas Tech University/R/R-Project/WorkSpace")

#Read Data
MyData <- read.csv(file="train.csv", header=TRUE, sep=",")
attach(MyData)
#Visualize Data
dim(MyData)
summary(MyData)

#NA Issue
na_count <-sapply(MyData, function(y) sum(length(which(is.na(y)))))
na_count[na_count>0]

#Some NA values have meaning while other are missing values

#NA Issue with LotFrontage
#Less than 25% are NA and the lest might significant
#Change value to 0
plot(LotFrontage,SalePrice)
abline(lm(SalePrice~LotFrontage), col="red")
MyData$LotFrontage[is.na(MyData$LotFrontage)]<-0

#NA has meaning of No Alley access in Alley
#Add NA as factor
MyData$Alley <- addNA(MyData$Alley)
barplot(prop.table(table(MyData$Alley)))

#NA in MasVnrType change to None
plot(MasVnrType,SalePrice)
abline(lm(SalePrice~MasVnrType), col="red")
MyData$MasVnrType[is.na(MyData$MasVnrType)] <- "None"
barplot(prop.table(table(MyData$MasVnrType)))

#NA in MasVnrArea change to 0
plot(MasVnrArea,SalePrice)
abline(lm(SalePrice~MasVnrArea), col="red")
MyData$MasVnrArea[is.na(MyData$MasVnrArea)] <- 0

#NA has meaning of No basement in  BsmtQual BsmtCond BsmtExposure BsmtFinType1 BsmtFinType2
#Add NA as factor
MyData$BsmtQual <- addNA(MyData$BsmtQual)
MyData$BsmtCond <- addNA(MyData$BsmtCond)
MyData$BsmtExposure <- addNA(MyData$BsmtExposure)
MyData$BsmtFinType1 <- addNA(MyData$BsmtFinType1)
MyData$BsmtFinType2 <- addNA(MyData$BsmtFinType2)

barplot(prop.table(table(MyData$BsmtQual)))

#NA in Electrical has only 1
#Change to Standard
MyData$Electrical[is.na(MyData$Electrical)] <- "SBrkr"


#NA has meaning of No Fire place in FireplaceQu
#Add NA as factor
MyData$FireplaceQu <- addNA(MyData$FireplaceQu)


#NA has meaning of No garage in  GarageType  GarageYrBlt GarageFinish   GarageQual   GarageCond
#Add NA as factor
MyData$GarageType <- addNA(MyData$GarageType)
MyData$GarageFinish <- addNA(MyData$GarageFinish)
MyData$GarageQual <- addNA(MyData$GarageQual)
MyData$GarageCond <- addNA(MyData$GarageCond)

#NA of GarageYrBlt chane to 0
MyData$GarageYrBlt[is.na(MyData$GarageYrBlt)] <- 0

#NA has meaning of No pool in PoolQC
#Add NA as factor
MyData$PoolQC <- addNA(MyData$PoolQC)

#NA has meaning of No Fence in Fence
#Add NA as factor
MyData$Fence <- addNA(MyData$Fence)
barplot(prop.table(table(MyData$Fence)))


#NA has meaning of No extra Miscellaneous feature in MiscFeature
#Add NA as factor
MyData$MiscFeature <- addNA(MyData$MiscFeature)
barplot(prop.table(table(MyData$MiscFeature)))

## End of NA issue


## Visualization
#install.packages("corrplot")
library(corrplot)
corrMatrix <- cor(MyData[,sapply(MyData, is.numeric)])
corrMatrix
corrplot(corrMatrix, method = "color")

for (col in colnames(MyData)){
  if(is.numeric(MyData[,col])){
    if( abs(cor(MyData[,col],MyData$SalePrice)) > 0.5){
      print(col)
      print( cor(MyData[,col],MyData$SalePrice) )
    }
  }
}

for (col in colnames(MyData)){
  if(is.numeric(MyData[,col])){
    if( abs(cor(MyData[,col],MyData$SalePrice)) <= 0.1){
      print(col)
      print( cor(MyData[,col],MyData$SalePrice) )
    }
  }
}

for (col in colnames(MyData)){
  if(is.numeric(MyData[,col])){
    plot(density(MyData[,col]), main=col)
  }
}
