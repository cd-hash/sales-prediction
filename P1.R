#### Start ####

rm(list = ls())

if (!require("data.table")) install.packages("data.table")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("caret")) install.packages("caret")
if (!require("corrplot")) install.packages("corrplot")
if (!require("xgboost")) install.packages("xgboost")
if (!require("cowplot")) install.packages("cowplot")
if (!require("forecast")) install.packages("forecast")

data.train = fread("file:///C:/Users/cxbon/Documents/R_practice/Train_bigmart.txt")
data.test = fread("file:///C:/Users/cxbon/Documents/R_practice/Test_bigmart.txt")

# using the data.table package allows us to better allocate resources on your comp. so it can run more smoothly
# models run faster overall the experience was better take note of the differences in notation when adding variables (ie. :=)
# and how you go about working with the data frames

dim(data.train); dim(data.test)

names(data.train)
names(data.test)

str(data.train)
str(data.test)

data.test[,Item_Outlet_Sales := NA] 
combi = rbind(data.train, data.test) #combining train and test datasets

dim(combi)

#### plot our variables to better understand them ####
ggplot(data.train) + geom_histogram(aes(data.train$Item_Outlet_Sales),
                               binwidth = 100, fill = "darkgreen") +  xlab("Item_Outlet_Sales")
# there is clearly right skewness in the target variable
# interestingly this shows that there are more times where stores sell less than $5000 indicating this may be a smaller store
# Now lets plot the continuous variables item weight, visibility and MRP(max retail price)
p1 = ggplot(combi) + geom_histogram(aes(Item_Weight),
                                    binwidth = 0.5, fill = "blue") 

p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility),
                                    binwidth = 0.005, fill = "blue") 

p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), 
                                    binwidth = 1, fill = "blue") 

plot_grid(p1, p2, p3, nrow = 1) # plot_grid() from cowplot package
# there is no pattern between frequency and item weight, right skewness in item visibility
# (makes sense since most products will get a proportionally smaller area),
# and most interesting is MRP which shows 4 different distributions
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +   geom_bar(aes(Item_Fat_Content, Count),
                                                                                     stat = "identity", fill = "coral1")
#LF, low fat and Low Fat are all the same thing and so is reg and Regular combine all of those into two categories like they should be

combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat" 
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + geom_bar(aes(Item_Fat_Content, Count),
                                                                                     stat = "identity", fill = "coral1")
#clearly there are more low fat items than there are regular fat content items
#maybe the demographic is more privy to low fat foods than other fat content foods
# plot for Item_Type
p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) +   geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") +  xlab("") +  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  ggtitle("Item_Type")
# plot for outlet_identifier
p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) +   geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") +  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# plot outlet size
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) +   geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))

second_row = plot_grid(p5, p6, nrow = 1)
plot_grid(p4, second_row, ncol = 1)
#outlet size is missing a label for the category with 4016  obs.
#plot for outlet establishment year
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) +   geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") +  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +  xlab("Outlet_Establishment_Year") +  theme(axis.text.x = element_text(size = 8.5))

#plot for outlet type
p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) +   geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +  theme(axis.text.x = element_text(size = 8.5))

plot_grid(p7, p8, ncol = 2)
#pretty consistent store openings year over year except for the year 1998
#supermarket type 1 is by far the most popular type

#### now we can check for any relationships we can see between the independent variables and the dependent variable ####
#first we'll start with the numerical variables
train = combi[1:nrow(data.train)] # extracting train data from the combined data

#weight vs. sales
p9 = ggplot(train) + geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +     theme(axis.title = element_text(size = 8.5))

#visibility vs. sales
p10 = ggplot(train) + geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +      theme(axis.title = element_text(size = 8.5))

# MRP vs. sales
p11 = ggplot(train) + geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +      theme(axis.title = element_text(size = 8.5))

second_row_2 = plot_grid(p10, p11, ncol = 2)
plot_grid(p9, second_row_2, nrow = 2)

# as can be expected weight doesn't seem to have a valuable relationship with sales
# 2 things about visibility: 1 there seems to be sales when visibility is at 0% which
# is impossible, 2 it looks like about 18% and below item visibility seem to have the highest
# sales
# The 4 different distributions hold true for mrp with relation to sales so this must be
# an important factor when we create our model as well we can create separate variables 
# to maybe better parse out the impact at different levels to sales

# now to look at the categorical variables 
# for this we'll use a violin plot which depicts the data better than a boxplot could
# The width of a violin plot at a particular level indicates the 
# concentration or density of data at that level.
# The height of a violin tells us about the range of the target variable values.


#type vs. sales
p12 = ggplot(train) + geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") + theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 6), axis.title = element_text(size = 8.5))

#fat content vs. sales
p13 = ggplot(train) + geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") + theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 8), axis.title = element_text(size = 8.5))

#outlet identifier vs. sales
p14 = ggplot(train) +       geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") + theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 8), axis.title = element_text(size = 8.5))

second_row_3 = plot_grid(p13, p14, ncol = 2)
plot_grid(p12, second_row_3, ncol = 1)

# sales doesn't seem to have a relationship with the different types of items
# identifier and sales seems to have the most information
# out10 and out19 are similar with eachother but significantly different from the other distributions

#size vs. sales
ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")
# small and the unidentified category seem to be similar in shape 
# so it could be inferred that they are simply mislabeled and should be in the small

# location vs. sales
p15 = ggplot(train) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta")

# type vs. sales
p16 = ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta")

plot_grid(p15, p16, ncol = 1)

# tier 1 and 3 have very similar shapes except 3 has a larger range
# grocery store has most of its obs. around the same sales levels while 
# type 1 and 3 have a wide range of sales 

#### now we will impute the missing values ####
missing_index = which(is.na(combi$Item_Weight))
for(i in missing_index)
{
  item = combi$Item_Identifier[i]
  combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = T)
}
sum(is.na(combi$Item_Weight))
head(combi$Item_Weight)

# now let's replace the 0's in item visibility
zero_index = which(combi$Item_Visibility == 0)
for(i in zero_index)
{
  item = combi$Item_Identifier[i]
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)
}
sum(is.na(combi$Item_Visibility))
head(combi$Item_Visibility)

#### Feature Engineering ####



# make a perishabel and non perishable category for the item type variable
perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")

# create our new feature
combi[,Item_Type_new := ifelse(Item_Type %in% perishable, "perishable", ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]

# compare Item_Type with the first 2 characters of Item_Identifier, i.e., ¡DR¡¦, ¡¥FD¡¦, and ¡¥NC¡¦.
# These identifiers most probably stand for drinks, food, and non-consumable.
table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))

combi[,Item_category := substr(combi$Item_Identifier, 1, 2)]
# Now we can adjust the fat content category to show when foods are not edible as the have no fat content
combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible"

#years of operation for outlets
combi[,Outlet_Years := 2013 - Outlet_Establishment_Year] 
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year)

# Price per unit weight
combi[,price_per_unit_wt := Item_MRP/Item_Weight]

# creating new independent variable - Item_MRP_clusters 
combi[,Item_MRP_clusters := ifelse(Item_MRP < 69, "1st",
                                   ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd",
                                          ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]

#### Encoding categorical variables ####
# our algorithms work best when it only has to work with numbers. So we have to do something with our categorical variables which are clearly not numbers.
# One solution would be to remove all non numerical data but that would obviously not be in our best interest.
# Instead we use one of 2 techniques
# 1. Label encoding - simply convert each category in a variable to a number (most suitable for ordinal categories or categories with an order)
# 2. One hot encoding - each category of a categorical variable is converted into a new binary column. similar to when we created the seasonal indicators in time series forecasting.

# first we'll use label encoding for outlet size and location type as these are both ordinal
combi[,Outlet_Size_num := ifelse(Outlet_Size == "Small", 0, ifelse(Outlet_Size == "Medium", 1, 2))]
# outlet size: small = 0, medium = 1, large = 2

combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0, ifelse(Outlet_Location_Type == "Tier 2", 1, 2))] 
#location type: tier 1 = 2, tier 2 = 1, tier 3 = 0

# now we remove the original variables to not confuse ourselves
combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]

# now we'll use one hot encoding for the other categorical variables
ohe = dummyVars("~.", data = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T) 

ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")]))

combi = cbind(combi[,"Item_Identifier"], ohe_df)

#### Data PreProcessing ####
# let's first deal with the skewness in the variables item visibility and price per unit weight

combi[,Item_Visibility := log(Item_Visibility + 1)] # log + 1 to avoid division by zero 
combi[,price_per_unit_wt := log(price_per_unit_wt + 1)]

# now we need to scale our numerical variables 
num_vars = which(sapply(combi, is.numeric))
num_vars_names = names(num_vars)

combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with = F]
prep_num = preProcess(combi_numeric, method=c("center", "scale"))
combi_numeric_norm = predict(prep_num, combi_numeric)

combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variables 
combi = cbind(combi, combi_numeric_norm)

train = combi[1:nrow(data.train)]
test = combi[(nrow(data.train) + 1):nrow(combi)] 

test[,Item_Outlet_Sales := NULL] # removing Item_Outlet_Sales as it contains only NA for test dataset

#### Now let's look at the correlation between independent variables ####


cor_train = cor(train[,-c("Item_Identifier")])
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)

# our plot shows there is some correlation between all of our variables but most show low amounts which is okay
# The correlation between any 2 variables is represented by a pie. A blueish pie indicates positive correlation
# while a redish pie indicates negative correlation. The magnitude of the correlation is denoted by the area covered by the
# pie.

#### Model Building ####
# linear regression
linear_reg_mod = lm(Item_Outlet_Sales ~ ., data = train[,-c("Item_Identifier")])

summary(linear_reg_mod)
accuracy(linear_reg_mod)

plot(train$Item_Outlet_Sales, 
     type= "l")
lines(linear_reg_mod$fitted.values,
      type = "l",
      col = "red")

plot(train$Item_Outlet_Sales,
     type = "l")
lines(linear_reg_mod$residuals,
      type = "l",
      col = "blue")
plot(linear_reg_mod$residuals,
     type = "l")
# how to submit answers to comp. page
submission$Item_Outlet_Sales = predict(linear_reg_mod, test[,-c("Item_Identifier")])
write.csv(submission, "Linear_Reg_submit.csv", row.names = F)

### regularized regression models
# regularized regression models can handle the correlated variables well and helps
# in overcoming overfitting

### Ridge regression
# the ridge penalty shrinks the coefficient of correlated predictors towards each other
### Lasso regression
# the lasso tends to pick one of a pair of correlated features and discard the other.


### Lasso regression
set.seed(1235)
my_control = trainControl(method="cv", number=5) 
Grid = expand.grid(alpha = 1, lambda = seq(0.1,100,by = 0.1))

lasso_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales, method='glmnet', trControl= my_control, tuneGrid = Grid)
lasso_linear_reg_mod$finalModel$lambdaOpt

lasso_linear_reg_mod$finalModel$beta[, lasso_linear_reg_mod$finalModel$lambdaOpt]
plot(lasso_linear_reg_mod)
summary(lasso_linear_reg_mod)
lasso_linear_reg_mod


### Ridge regression
set.seed(1236)
my_control = trainControl(method="cv", number=5)
Grid = expand.grid(alpha = 0, lambda = seq(0.1,100,by = 0.1))

ridge_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales, method='glmnet', trControl= my_control, tuneGrid = Grid)
#this is the optimal lambda for the ridge regression
ridge_linear_reg_mod$finalModel$lambdaOpt
# these are the coefficients for the model at the optimal lambda
ridge_linear_reg_mod$finalModel$beta[, ridge_linear_reg_mod$finalModel$lambdaOpt]

plot(ridge_linear_reg_mod)
summary(ridge_linear_reg_mod)

### Random Forest
# num.trees sets the number of to be used
#mtry is the number of predictor variables randomly sampled at each split
# min.node.size is the minimum size of the terminal node for each tree
#  -making this number large causes smaller trees and reduces overfitting

set.seed(1237)
my_control = trainControl(method="cv", number=5) # 5-fold CV
tgrid = expand.grid(
  .mtry = c(3:10),
  .splitrule = "variance",
  .min.node.size = c(10,15,20)
)

rf_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
               y = train$Item_Outlet_Sales,
               method='ranger',
               trControl= my_control,
               tuneGrid = tgrid,
               num.trees = 400,
               importance = "permutation")
rf_mod
# the best model was achieved with an mtry = 5 splitrule = variance and min.node.size = 20
# with an rmse 1087.662
# now let's visualize the scores for different tuning parameters
plot(rf_mod)

# now we can plot the variable importance 
plot(varImp(rf_mod))
# item mrp is one of the most important variables as can be expected when trying to understand why sales increase
# new features created by us like price per unit wt, outlet years, and item MRP clusters
# are among the most important in prediction. This proves the importance of feature engineering.

### XGBoost
# xgboost only works with numerical variables which is fine in our case since we already
# replaced all our categorical variables, but is something to keep in mind in future instances.
# there are many tuning parameters in xgboost which can be generalized into
# -General parameters
#  -refers to which booster we are using to do the boosting. Most commonly used are tree or linear models
# -Boosting parameters
#  -refers to which booster you have chosen
# -Learning task parameters
#  -decide on the learning scenario, for example, regression tasks may use different parameters with ranking tasks

## some of the parameters are
# eta - It is also known as the learning rate or the shrinkage factor. 
#   It actually shrinks the feature weights to make the boosting process more conservative. 
#   The range is 0 to 1. Low eta value means the model is more robust to overfitting.
# gamma: The range is 0 to ¡Û. Larger the gamma more conservative the algorithm is.
# max_depth: We can specify maximum depth of a tree using this parameter.
# subsample: It is the proportion of rows that the model will randomly select to grow trees.
# colsample_bytree: It is the ratio of variables randomly chosen to build each tree in the model.

param_list = list(objective = "reg:linear", eta=0.01, gamma = 1, max_depth=6, subsample=0.8, colsample_bytree=0.5)

dtrain = xgb.DMatrix(data = as.matrix(train[,-c("Item_Identifier", "Item_Outlet_Sales")]), label= train$Item_Outlet_Sales)

dtest = xgb.DMatrix(data = as.matrix(test[,-c("Item_Identifier")]))
# now we use the xgb.cv funtion for cross validation
# Here we are using cross validation for finding the optimal value of nrounds.

set.seed(112)
xgbcv = xgb.cv(params = param_list, data = dtrain, nrounds = 1000, nfold = 5, print_every_n = 10, early_stopping_rounds = 30, maximize = F)

# the above model will train until test_rmse hasn't improved in 30 rounds. 
# we got the best score on the 467th iteration
# so we will use 467 rounds as our nrounds for the xgboost model
xgb_model = xgb.train(data = dtrain, params = param_list, nrounds = 467)

var_imp = xgb.importance(feature_names = setdiff(names(train), c("Item_Identifier", "Item_Outlet_Sales")), model = xgb_model)
xgb.plot.importance(var_imp)
# again the features we created are among the top most important variables in the model.

#### Conclusion ####
# there are still many ways that we can improve this model
# diving deeper into the relationships held by the different independent variables 
# Based on the violin plots, we can group the categories of the categorical variables which are quite similar in shape and size.
# We can transform the target variable to reduce its skewness.
# now you can review this and continue to make improvements in your analytics skills


