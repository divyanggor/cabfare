rm(list = ls())
setwd("/Users/divyanggor/Documents/Study/Online_Course/Edwisor/Project/")

# #loading Libraries
x = c("ggplot2", "corrgram", "DMwR", "usdm", "caret", "randomForest", "e1071",
      "DataCombine", "doSNOW", "inTrees", "rpart.plot", "rpart",'MASS','xgboost','stats','gdistance', 'Imap', 'car')
#load Packages
lapply(x, require, character.only = TRUE)
rm(x)

train_cab= read.csv("train_cab.csv")
test_cab= read.csv("test.csv")
test_pickup_datetime = test_cab["pickup_datetime"]
str(train_cab)
str(test_cab)
summary(train_cab)
summary(test_cab)
head(train_cab,5)
head(test_cab,5)

# Changing the data types of variables
train_cab$fare_amount = as.numeric(as.character(train_cab$fare_amount))

################################### Drop Invalid Data Entries ###############
# 1.Fare amount can't be negative or zero
nrow(train_cab[which(train_cab$fare_amount <=0 ),])
train_cab = train_cab[-which(train_cab$fare_amount <=0 ),]

#2. Passenger_count must be positive integer 1, 2, 3, 4, 5 or 6
nrow(train_cab[which(train_cab$passenger_count <1 ),])
nrow(train_cab[which(train_cab$passenger_count >6 ),])
train_cab = train_cab[-which(train_cab$passenger_count < 1 ),]
train_cab = train_cab[-which(train_cab$passenger_count > 6),]
train_cab$passenger_count=round(train_cab$passenger_count)

#3. latitude can never be more than 180
nrow(train_cab[which(train_cab$pickup_latitude > 180), ])
train_cab = train_cab[-which(train_cab$pickup_latitude > 180), ]

##################### Missing Value Analysis #########################
apply(train_cab,2,function(x){sum(is.na(x))})
missing_val = data.frame(apply(train_cab,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(train_cab)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
missing_val

train_cab[,'passenger_count'] = factor(train_cab[,'passenger_count'], labels=(1:6))
unique(train_cab$passenger_count)
apply(test_cab,2,function(x){sum(is.na(x))})

#1. Passenger_count Variable
unique(train_cab$passenger_count)
unique(test_cab$passenger_count)
#Mean Method
mean(train_cab$passenger_count, na.rm = T)
#Mode Methond
getmode = function(x) {
  uniq = unique(x)
  uniq[which.max(tabulate(match(x, uniq)))]
}
getmode(train_cab$passenger_count)
#For KNN
train_cab$passenger_count[200]
train_cab$passenger_count[200]=NA

#Mean = 1.649633
#Mode = 1
#KNN = 1

#2. Fare amount Variable
# Mean Method
mean(train_cab$fare_amount, na.rm = T)
#Median Method
median(train_cab$fare_amount, na.rm = T)
#KNN Imputation
train_cab$fare_amount[500]
train_cab$fare_amount[500]=NA
train_cab = knnImputation(train_cab, k = 3)
train_cab$fare_amount[500]
train_cab$passenger_count[200]
#Actual Value = 6
#Mean = 15.04909
#Median = 8.5
#KNN = 7

########################   Outlinear Analysis #######################
# Boxplot for fare_amount variable
pl1 = ggplot(train_cab,aes(x = factor(passenger_count),y = fare_amount))
pl1 + geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,outlier.size=1, notch=FALSE)+ylim(0,100)

# Replace all outliers with NA and impute
values = train_cab[,"fare_amount"] %in% boxplot.stats(train_cab[,"fare_amount"])$out
train_cab[which(values),"fare_amount"] = NA
#check for NA's
sum(is.na(train_cab$fare_amount))
#Imputing with KNN
train_cab = knnImputation(train_cab,k=3)
#check for NA's
sum(is.na(train_cab$fare_amount))


##################                   Feature Engineering                       ##########################
# new features derived from pickup_datetime are year,month,day_of_week,hour
train_cab$pickup_date = as.Date(as.character(train_cab$pickup_datetime))
sum(is.na(train_cab))
train_cab = na.omit(train_cab)
train_cab$day_of_week = as.factor(format(train_cab$pickup_date,"%u"))# Monday = 1
train_cab$month = as.factor(format(train_cab$pickup_date,"%m"))
train_cab$year = as.factor(format(train_cab$pickup_date,"%Y"))
pickup_time = strptime(train_cab$pickup_datetime,"%Y-%m-%d %H:%M:%S")
train_cab$hour = as.factor(format(pickup_time,"%H"))

test_cab$pickup_date = as.Date(as.character(test_cab$pickup_datetime))
test_cab$day_of_week = as.factor(format(test_cab$pickup_date,"%u"))# Monday = 1
test_cab$month = as.factor(format(test_cab$pickup_date,"%m"))
test_cab$year = as.factor(format(test_cab$pickup_date,"%Y"))
pickup_time = strptime(test_cab$pickup_datetime,"%Y-%m-%d %H:%M:%S")
test_cab$hour = as.factor(format(pickup_time,"%H"))

train_cab = subset(train_cab,select = -c(pickup_datetime,pickup_date))
test_cab = subset(test_cab,select = -c(pickup_datetime,pickup_date))
# Calculate Distance 
train_cab$geodesic = gdist(train_cab$pickup_longitude, train_cab$pickup_latitude, train_cab$dropoff_longitude, train_cab$dropoff_latitude, units = "km", a = 6378137.0, b = 6356752.3142, verbose = FALSE)
test_cab$geodesic = gdist(test_cab$pickup_longitude, test_cab$pickup_latitude, test_cab$dropoff_longitude, test_cab$dropoff_latitude, units = "km", a = 6378137.0, b = 6356752.3142, verbose = FALSE)
# Removing Outliners from geodesic
# Boxplot for fare_amount variable
pl2 = ggplot(train_cab,aes(x = factor(passenger_count),y = geodesic))
pl2 + geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,outlier.size=1, notch=FALSE)+ylim(0,100)
# Replace all outliers with NA and impute
values_1 = train_cab[,"geodesic"] %in% boxplot.stats(train_cab[,"geodesic"])$out
train_cab[which(values_1),"geodesic"] = NA
#the NA's
sum(is.na(train_cab$geodesic))
#Imputing with KNN
train_cab = knnImputation(train_cab,k=3)
# lets check the missing values
sum(is.na(train_cab$geodesic))
################  Feature selection                 ###############
numeric = sapply(train_cab,is.numeric) #selecting numeric variables
numeric_data = train_cab[,numeric]
cnames = colnames(numeric_data)
#Correlation analysis for numeric variables
cor(numeric_data)
corrgram(train_cab[,numeric],upper.panel=panel.pie, main = "Correlation Plot")

#As both numberic variables Facre_amount and geodesic are highly correlated with each other.

#Removing Categorical variables
train_cab = subset(train_cab,select = -c(pickup_longitude,pickup_latitude,dropoff_latitude, dropoff_longitude))
test_cab = subset(test_cab,select = -c(pickup_longitude,pickup_latitude,dropoff_latitude, dropoff_longitude))

#Anova Test
aov_results = aov(fare_amount ~ passenger_count + hour + day_of_week + month + year,data = train_cab)

summary(aov_results)
# pickup_weekdat has p value greater than 0.05 
train_cab = subset(train_cab,select=-day_of_week)
#remove from test set
test_cab = subset(test_cab,select=-day_of_week)


##################################             Feature Scaling         ################################################

par(mfrow=c(1,2))
qqPlot(train_cab$geodesic)                            # qqPlot, it has a x values derived from gaussian distribution, if data is distributed normally then the sorted data points should lie very close to the solid reference line 
truehist(train_cab$geodesic)                            # truehist() scales the counts to give an estimate of the probability density.
lines(density(train_cab$geodesic))  # Right skewed      # lines() and density() functions to overlay a density plot on histogram

train_cab[,'geodesic'] = (train_cab[,'geodesic'] - min(train_cab[,'geodesic']))/
  (max(train_cab[,'geodesic'] - min(train_cab[,'geodesic'])))
#################### Splitting train into train and validation subsets ###################
set.seed(1000)
tr = createDataPartition(train_cab$fare_amount,p=0.80,list = FALSE) # 80% in trainin and 25% in Validation Datasets
train_data = train_cab[tr,]
test_data = train_cab[-tr,]
###################Model Selection################
#Error metric used to select model is RMSE
#############            Linear regression               #################
lm_model = lm(fare_amount ~.,data=train_data)

summary(lm_model)
str(train_data)
plot(lm_model$fitted.values,rstandard(lm_model),main = "Residual plot",
     xlab = "Predicted values of fare_amount",
     ylab = "standardized residuals")

lm_predictions = predict(lm_model,test_data[,2:6])
qplot(x = test_data[,1], y = lm_predictions, data = test_data, color = I("blue"), geom = "point")
regr.eval(test_data[,1],lm_predictions)
#############                             Decision Tree            #####################

Dt_model = rpart(fare_amount ~ ., data = train_data, method = "anova")
summary(Dt_model)
#Predict for new test cases
predictions_DT = predict(Dt_model, test_data[,2:6])
qplot(x = test_data[,1], y = predictions_DT, data = test_data, color = I("blue"), geom = "point")
regr.eval(test_data[,1],predictions_DT)
#############                             Random forest            #####################
rf_model = randomForest(fare_amount ~.,data=train_data)
summary(rf_model)
rf_predictions = predict(rf_model,test_data[,2:6])
qplot(x = test_data[,1], y = rf_predictions, data = test_data, color = I("blue"), geom = "point")
regr.eval(test_data[,1],rf_predictions)
############          Improving Accuracy by using Ensemble technique ---- XGBOOST             ###########################
train_data_matrix = as.matrix(sapply(train_data[-1],as.numeric))
test_data_data_matrix = as.matrix(sapply(test_data[-1],as.numeric))
xgboost_model = xgboost(data = train_data_matrix,label = train_data$fare_amount,nrounds = 15,verbose = FALSE)
summary(xgboost_model)
xgb_predictions = predict(xgboost_model,test_data_data_matrix)
qplot(x = test_data[,1], y = xgb_predictions, data = test_data, color = I("blue"), geom = "point")
regr.eval(test_data[,1],xgb_predictions)
#############                         Finalizing and Saving Model for later use                         ####################
# In this step we will train our model on whole training Dataset and save that model for later use
train_data_matrix2 = as.matrix(sapply(train_cab[-1],as.numeric))
test_data_matrix2 = as.matrix(sapply(test_cab,as.numeric))

xgboost_model2 = xgboost(data = train_data_matrix2,label = train_cab$fare_amount,nrounds = 15,verbose = FALSE)

# Saving the trained model
saveRDS(xgboost_model2, "./final_Xgboost_model_using_R.rds")

# loading the saved model
super_model <- readRDS("./final_Xgboost_model_using_R.rds")
print(super_model)

# Lets now predict on test dataset
xgb = predict(super_model,test_data_matrix2)

xgb_pred = data.frame(test_pickup_datetime,"predictions" = xgb)

write.csv(xgb_pred,"xgb_predictions_R.csv",row.names = FALSE)





