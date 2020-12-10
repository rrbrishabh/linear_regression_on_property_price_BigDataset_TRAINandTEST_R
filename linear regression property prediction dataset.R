# linear regression problem   
# ----------------------------
# load the dataset

train <- read.csv(file.choose())
test <- read.csv(file.choose())

head(train)

# getting rid of the ID column
# remove the varible

test$Id <- NULL
train$Id <- NULL

View(train)

test$Sale_Price <- NA

all <- rbind(train, test)

# for col and row 
dim(all)

# structure of the dataset
str(all)

#checkoing NA value in dataset

!colSums(is.na(all))
colSums(is.na(all))

# again chacking with another method for NA values

columns_with_missing_values <- sapply(all, function(x) sum(is.na(x)))
columns_with_missing_values

# missing value treatment
all$Lane_Type<- as.character(all$Lane_Type)
# for the datatype
class(all$Lane_Type)
# filling Na with "No Acess"
all$Lane_Type[is.na(all$Lane_Type)] <- "No Access"


# basement height with "NO basement"
all$Basement_Height <- as.character(all$Basement_Height)
all$Basement_Height[is.na(all$Basement_Height)] <- "No Basement"
# Change the factor class type to character class first 
all$Basement_Condition <- as.character(all$Basement_Condition) 
# Filling NA with No Basement 
all$Basement_Condition[is.na(all$Basement_Condition)] <- "No Basement"
str(all)


# Replacing NA values with 'No Basement' for Exposure_Level variable 
# Check the class of the variable Exposure_Level 
class(all$Exposure_Level) 
# Change the factor class type to character class first 
all$Exposure_Level <- as.character(all$Exposure_Level) 
# Filling NA with No Basement 
all$Exposure_Level[is.na(all$Exposure_Level)] <- "No Basement"


# Replacing NA values with 'No Basement' for BsmtFinType1 variable 
# Check the class of the variable BsmtFinType1 
class(all$BsmtFinType1) 
# Change the factor class type to character class first 
all$BsmtFinType1 <- as.character(all$BsmtFinType1) 
# Filling NA with No Basement 
all$BsmtFinType1[is.na(all$BsmtFinType1)] <- "No Basement"


# Replacing NA values with 'No Basement' for BsmtFinType2 variable 
# Check the class of the variable BsmtFinType2 
class(all$BsmtFinType2) 
# Change the factor class type to character class first 
all$BsmtFinType2 <- as.character(all$BsmtFinType2) 
# Filling NA with No Basement 
all$BsmtFinType2[is.na(all$BsmtFinType2)] <- "No Basement"


# Replacing NA values with 'No Fireplace' for Fireplace_Quality variable 
# Check the class of the variable Fireplace_Quality 
class(all$Fireplace_Quality) 
# Change the factor class type to character class first 
all$Fireplace_Quality <- as.character(all$Fireplace_Quality) 
# Filling NA with No Fireplace 
all$Fireplace_Quality[is.na(all$Fireplace_Quality)] <- "No Fireplace"


# Replacing NA values with 'No garage' for Garage variable 
# Check the class of the variable Garage 
class(all$Garage) 
# Change the factor class type to character class first 
all$Garage <- as.character(all$Garage) 
# Filling NA with No Garage 
all$Garage[is.na(all$Garage)] <- "No Garage"


# Replacing NA values with '0' for Garage_Built_Year variable 
# Check the class of the variable Garage_Built_Year 
class(all$Garage_Built_Year) 
# Change the factor class type to character class first 
#train$Garage_Built_Year <- as.character(train$Garage_Built_Year) 
# Filling NA with 0 
all$Garage_Built_Year[is.na(all$Garage_Built_Year)] <- 0


# Replacing NA values with 'No Garage' for Garage_Finish_Year variable 
# Check the class of the variable Garage_Finish_Year 
class(all$Garage_Finish_Year) 
# Change the factor class type to character class first 
all$Garage_Finish_Year <- as.character(all$Garage_Finish_Year) 
# Filling NA with No Garage 
all$Garage_Finish_Year[is.na(all$Garage_Finish_Year)] <- "No Garage"


# Replacing NA values with 'No Garage' for Garage_Quality variable 
# Check the class of the variable Garage_Quality 
class(all$Garage_Quality) 
# Change the factor class type to character class first 
all$Garage_Quality <- as.character(all$Garage_Quality) 
# Filling NA with No Garage 
all$Garage_Quality[is.na(all$Garage_Quality)] <- "No Garage"


#Replacing NA values with 'No Garage' for Garage_Condition variable 
# Check the class of the variable Garage_Condition 
class(all$Garage_Condition) 
# Change the factor class type to character class first 
all$Garage_Condition <- as.character(all$Garage_Condition)
# Filling NA with No Garage 
all$Garage_Condition[is.na(all$Garage_Condition)] <- "No Garage"


# Replacing NA values with 'No Pool' for Pool_Quality variable 
# Check the class of the variable Pool_Quality 
class(all$Pool_Quality) 
# Change the factor class type to character class first 
all$Pool_Quality <- as.character(all$Pool_Quality) 
# Filling NA with No Pool 
all$Pool_Quality[is.na(all$Pool_Quality)] <- "No Pool"


# Replacing NA values with 'No Fence' for Fence_Quality variable 
# Check the class of the variable Fence_Quality 
class(all$Fence_Quality) 
# Change the factor class type to character class first 
all$Fence_Quality <- as.character(all$Fence_Quality) 
# Filling NA with No Fence 
all$Fence_Quality[is.na(all$Fence_Quality)] <- "No Fence"


# Replacing NA values with 'None' for Miscellaneous_Feature variable 
# Check the class of the variable Fence 
class(all$Miscellaneous_Feature) 
# Change the factor class type to character class first 
all$Miscellaneous_Feature <- as.character(all$Miscellaneous_Feature) 
# Filling NA with None 
all$Miscellaneous_Feature[is.na(all$Miscellaneous_Feature)] <- "None"


# Replacing missing values in Lot_Extent with its median value 
# Check the class of the variable Fence 
class(all$Lot_Extent) 
# Lot_Extent is a continuous variable 
# Filling missing values with median
all$Lot_Extent[is.na(all$Lot_Extent)] <- median(all$Lot_Extent, na.rm = TRUE)


# Using crosstab to generate the count of Brick_Veneer_Type by 
#type of Brick_Veneer_Area 
cross <- table(all$Brick_Veneer_Area,all$Brick_Veneer_Type) 
cross 

head(cross) 
cross_margin <- addmargins(cross)

head(cross_margin,10) 

#It can be observed that that when MasVnrArea is zero, we have 
#MasVnrType as None in the majority of cases
#impute the missing values in Brick_Veneer_Type with None and Brick_Veneer_Area 
#with zero 
all$Brick_Veneer_Area[is.na(all$Brick_Veneer_Area)] <- 0 
class(all$Brick_Veneer_Area)
all$Miscellaneous_Feature <- as.character(all$Miscellaneous_Feature) 
all$Brick_Veneer_Type[is.na(all$Brick_Veneer_Type)] <- "None"


#Let's see the distribution of the Electrical_System type by Building_Class 
cross_elec <- table(all$Electrical_System,all$Building_Class) 
cross_elec_margin <- addmargins(cross_elec) 

head(cross_elec_margin,10) 
class(all$Electrical_System) 
all$Electrical_System <- as.character(all$Electrical_System) 
all$Electrical_System[is.na(all$Electrical_System)] <- "SBrkr"
str(all$Electrical_System)
unique(all$Electrical_System)
class(all$Electrical_System)


numericVars <- which(sapply(all, is.numeric))
numericVarsnames <- names(numericVars)
numericVarsnames

install.packages("corrplot")
library(corrplot)

all_numVar <- all[, numericVars]
all_numVar
cor(all_numVar$Total_Basement_Area, all_numVar$First_Floor_Area, use = "pairwise.complete.obs")
cor_numVar <- cor(all_numVar, use = "pairwise.complete.obs") #correlations of all numeric variables 
cor_numVar


#sort on decreasing correlations with Sale_Price 
cor_sorted <- as.matrix(sort(cor_numVar[,"Sale_Price"], decreasing = T))
cor_sorted


# we can use abs for +ive all the value and highly correaltion we consider (cor more then 0.5 highly correaltion)
cor_high <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))

# we are putting this in rows and colunm
cor_numVar <- cor_numVar[cor_high, cor_high]
View(cor_numVar)


# correlation plot we create 
corrplot.mixed(cor_numVar, tl.pos = "lt", tl.col = "black")


# creating a modal

modal <- lm(Sale_Price~., all)

# check summary of the modal
summary(modal)


# predict the Sale_Price with all the X-values 
#pdct1 = predict(model1, test) 
cor(all$Sale_Price, all$Lot_Size, use = "pairwise.complete.obs") 
#length(pdct1) 
pdct1 = predict(modal) 


# table showing the difference between the actual and predicted values 
actual_Sale_Price = train$Sale_Price 
length(actual_Sale_Price) 


Actual=train$Sale_Price 
predictions=pdct1 

DF= data.frame(Actual, pdct1) 

View(DF) 

DF$Diff= DF$Actual-DF$pdct1 

DF$pct=(DF$Diff/DF$Actual)*100 

pred_Sale_Price = pdct1 
difference = actual_Sale_Price - pred_Sale_Price 
df_Sale_Price = data.frame(actual_Sale_Price, pred_Sale_Price,difference) 
View(df_Sale_Price)















