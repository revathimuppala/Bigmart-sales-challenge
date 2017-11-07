#Load Datasets
train <-read.csv("file:///C:/Users/narisp/Desktop/Revathi/Data Science/Big mart challenge/Train_UWu5bXk (1).csv")
test <-read.csv("file:///C:/Users/narisp/Desktop/Revathi/Data Science/Big mart challenge/Test_u94Q5KV (1).csv")

dim(train)
dim(test)

#Add column to test data to make 
#equal dimentions for both train  and test data

test$Item_Outlet_Sales <-0

# combine both data sets to make easy for data preparation
dim(test)

mydata <-rbind(train,test)

# change text for fat content to make correctlevels 
#levels(mydata$Item_Fat_Content)
install.packages('plyr')
install.packages('dplyr')
library(plyr)
library(dplyr)

mydata$Item_Fat_Content <- revalue(mydata$Item_Fat_Content,
                                   c("LF"="Low Fat","low fat"="Low Fat","reg"="Regular"))
levels(mydata$Outlet_Size)
#levels(mydata$Item_Fat_Content)
### Assign outlet size for missing data


# check for missing data
str(mydata)
table(is.na(mydata))
colSums(is.na(mydata))
is.na(mydata$Outlet_Size)

##Assign dummy level for blanks in Outlet size

levels(mydata$Outlet_Size)[1] <- "dummy"
levels(mydata$Outlet_Size)
colSums(is.na(mydata))

## check for dummy Outlet size with other variables
aggregate(mydata$Outlet_Size, by=list(Category=mydata$Outlet_Size), FUN=length)

aggregate(mydata$Outlet_Size, 
          by=list(Category=mydata$Outlet_Identifier, 
                  Category=mydata$Outlet_Type,
                  Category=mydata$Outlet_Size), FUN= length)
## there are 4016 dummy's in outlet size which are distributed 
##to 3 outlet_Identifier i,e OUT010,OUT017,OUT045
## in that OUT010 belongs to grocery store, So we can assign "small"
## where as OUT017 and OUT045 belongs to supermarket type 1 
##and it's most numbers are small outlet_sizes, so we consider "small" 
levels(mydata$Outlet_Size)[1] <- "Small"
levels(mydata$Outlet_Size)

## by this box plot we found OUT019 and OUT027 is not provided data,
# lets calculate mean weights by Item Identifier


weightsByItem <- as.data.frame( ddply(na.omit(mydata), 
                                      ~Item_Identifier, 
                                      summarise, 
                                      mean=mean(Item_Weight), 
                                      sd=sd(Item_Weight)))
## Replace missing data with mean weights

mydata$Item_Weight <- ifelse(is.na(mydata$Item_Weight), 
                             weightsByItem$mean[
                               match(mydata$Item_Identifier, weightsByItem$Item_Identifier)], 
                             mydata$Item_Weight)

table(is.na(mydata))

## look in to Item _ visibility,there are many zero's
# which is wrong.
#let's assign NA to zero's

mydata$Item_Visibility[mydata$Item_Visibility=='0']<-NA
table(is.na(mydata))

## Item Visibility for each Outlet Identifier must be 100,
## So we sum the Item weights of each Outlet Identifier 
## and deduct from 100, Then we distribute the rest to visibility NA's

remaining_Vis <- as.data.frame( ddply(na.omit(mydata), 
                                      ~Outlet_Identifier, 
                                      summarise, 
                                      remain=100-sum(Item_Visibility)))

table(is.na(mydata))
# check for NA'a under each Outlet Identifier

NA_vis<-subset(mydata,is.na(mydata$Item_Visibility)) 
library(plyr)
count_NAs <-count(NA_vis$Outlet_Identifier)

remaining_Vis$count <-count_NAs[,2]

#distribute that remaining percent of item visibility to the outlet wise NA's
remaining_Vis$mean <- (remaining_Vis$remain/remaining_Vis$count)

# Replace missing data with mean visbility of each outlet identifier

mydata$Item_Visibility <- ifelse(is.na(mydata$Item_Visibility), 
                                 remaining_Vis$mean[
                                   match(mydata$Outlet_Identifier, remaining_Vis$Outlet_Identifier)], 
                                 mydata$Item_Visibility)
table(is.na(mydata))
## Add item category based on Item identifier by triming the text

mydata$Item_Identifier<-strtrim(mydata$Item_Identifier,2)

str(mydata)
###

##encoding categorical variable
mydata$Item_Identifier <- factor(mydata$Item_Identifier,
                                 levels = c("FD","NC","DR"),
                                 labels = c(0,1,2))
mydata$Item_Fat_Content <- factor(mydata$Item_Fat_Content,
                                  levels = c("Low Fat","Regular"),
                                  labels = c(0,1))

mydata$Item_Type <- factor(mydata$Item_Type,
                           levels = c("Breads","Breakfast","Canned","Dairy","Frozen Foods",
                                      "Fruits and Vegetables","Hard Drinks","Health and Hygiene",
                                      "Household","Baking Goods","Meat",
                                      "Others","Seafood","Snack Foods",
                                      "Soft Drinks","Starchy Foods"),
                           labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))
mydata$Outlet_Establishment_Year <- factor(mydata$Outlet_Establishment_Year,
                                           levels = c("1985","1987","1997","1998","1999",
                                                      "2002","2004","2007","2009"),
                                           labels = c(1,2,3,4,5,6,7,8,9))
mydata$Outlet_Identifier <- factor(mydata$Outlet_Identifier,
                                   levels = c("OUT010","OUT013","OUT017","OUT018","OUT019",
                                              "OUT027","OUT035","OUT045","OUT046","OUT049"),
                                   labels = c(1,2,3,4,5,6,7,8,9,10))

mydata$Outlet_Location_Type <- factor(mydata$Outlet_Location_Type,
                                      levels = c("Tier 1","Tier 2","Tier 3"),
                                      labels = c(0,1,2))

mydata$Outlet_Type <- factor(mydata$Outlet_Type,
                             levels = c("Grocery Store","Supermarket Type1",
                                        "Supermarket Type2","Supermarket Type3"),
                             labels = c(0,1,2,3))

mydata$Outlet_Size <- factor(mydata$Outlet_Size,
                             levels = c("Small","Medium","High"),
                             labels = c(0,1,2))

# split back full dataset in to train and test

train2 <- mydata[1:8523,]
test2 <- mydata[8524:14204,]

# remove dummy outlet sales coulmn from test set

test2 <- test2[-12]

###################
## Random Forest #########

library(randomForest)
set.seed(123)
mymodel_rf = randomForest(x = train2[-12],
                          y = train2$Item_Outlet_Sales,
                          ntree = 500)
mymodel_rf
## test set pridictions
## predict the test set results
ypred_rf = predict(mymodel_rf, newdata=test2)

ypred_rf

## generate results CSV file for Random Forest model 

solution_rf <- data.frame(Item_Identifier = test$Item_Identifier, Outlet_Identifier = test$Outlet_Identifier,ypred_dt)
write.csv(solution_rf, file = "random forest.csv", row.names = FALSE)










