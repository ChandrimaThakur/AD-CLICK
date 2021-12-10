#STEP 1:IDENTIFYING THE PROBLEM STATEMENT
##Predict the factors that will incerease click in advertisement

#STEP 2:IMPOTING THE DATA SAET AND IDENTIFYING THE TARGET VARIABLE
mydata<-read.csv(choose.files())#importing
dim(mydata)#6657rows   14columns
class(mydata)#data.frame
colnames(mydata)
#"VistID"         "Time_Spent"     "Age"            "Avg_Income"    
#"Internet_Usage" "Ad_Topic"       "Country_Name"   "City_code"     
#"Male"           "Time_Period"    "Weekday"        "Month"         
#"Year"           "Clicked"       

#OUT OF THE 14 COLUMNS,THE COLUMN CLICKED IS THE TARGET VARIABLE

str(mydata)
#VistID        : int  5183153 4023265 4708083 9771815 6451317 1475237 5578680 4037672 5792642 9377192 ...
# Time_Spent    : num  88 51.6 82.4 62.1 77.7 ...
#Age           : int  43 50 38 45 31 38 26 23 22 50 ...
#Avg_Income    : num  55901 39132 57032 48868 61608 ...
# Internet_Usage: num  185 177 211 190 205 ...
# Ad_Topic      : chr  "product_11" "product_8" "product_6" "product_19" ...
# Country_Name  : chr  "Serbia" "Turkmenistan" "Northern Mariana Islands" "South Africa" ...
# City_code     : chr  "City_5" "City_1" "City_2" "City_3" ...
# Male          : chr  "No" "No" "No" "Yes" ...
# Time_Period   : chr  "Mid-Night" "Evening" "Morning" "Morning" ...
# Weekday       : chr  "Thursday" "Saturday" "Tuesday" "Thursday" ...
# Month         : chr  "July" "June" "January" "April" ...
# Year          : int  2020 2020 2020 2020 2020 2020 2020 2020 2020 2020 ...
# Clicked       : int  0 1 0 1 0 1 1 0 0 1 ...

#VisitId 
##Is a continuous variable.It gives us the idof people eho visited website.
###It is not useful for our prediction.

#Time_Spent
## It is a continuous variable.
##It is average time spent by user on site in minutes

#Age
##It is a continuous variable.
##it give us user age in years.


#Avg_Income
#It is a continuous variable.
##Average Income of geographical area of user
##It is not useful for our prediction.

#Internet_Usage
#It is a continuous variable
##: Average minutes a day user spent on the internet


#Ad_Topics
## it is a categorical variable.
##Headline of the advertisement
#consisist of:Product_1 to Product_30
table(mydata$Ad_Topic)
#product_1 product_10 product_11 product_12 product_13 product_14 product_15 
#238        232        216        227        247        226        210 
#product_16 product_17 product_18 product_19  product_2 product_20 product_21 
#224        237        234        195        144        234        207 
#product_22 product_23 product_24 product_25 product_26 product_27 product_28 
#266        224        230        255        227        250        229 
#product_29  product_3 product_30  product_4  product_5  product_6  product_7 
#219        112        234        215        216        244        223 
#product_8  product_9 
#226        216

#Country_name
## It is categorical data
##Country of user
## It is not useful for our prediction


#City_code
## It is a categorical data
##City of user
#It consist of 9 city code:City_1 City_2 City_3 City_4 City_5 City_6 City_7 City_8 City_9
table(mydata$City_code)
##City_1 City_2 City_3 City_4 City_5 City_6 City_7 City_8 City_9 
## 2559   1675   1075    673    364    195     81     31      4

#male
#it is a categorical data
##Whether or not user was male
#it consist of :Yes, No
table(mydata$Male)
#No  Yes 
#3571 3086 
##The number of males is less than number of females.

#Time_Period
#categorical data
#Time at which consumer clicked on Ad
table(mydata$Time_Period)
#Early-Morning       Evening     Mid-Night       Morning         Night 
#       674          1266          1145          1228          1140 
#Noon 
#1204 

#Weekday 
#Categrical Variable
#Name of the day
table(mydata$Weekday)
#Friday    Monday  Saturday    Sunday  Thursday   Tuesday Wednesday 
#986       981       931       979       926       935       919 

#Month 
##Ctegorical variable
#Name of the months
table(mydata$Month)
#April February  January     July     June    March      May 
#974      944      988      930      941      930      950 

#Year
##continuous variable
##Which year the data is collected
##This is not useful for our prediction


# Removing useless columns(Visit_id,Avg_income,Country_name,Year)
mydata <-  mydata[, -c(1,4,7,13)]
head(mydata)
# Time_Spent Age Internet_Usage  Ad_Topic Country_Name City_code Male   Time_Period  Weekday Month Clicked

#conversion of categorical variables to factors
mydata$Ad_Topic<-as.factor(mydata$Ad_Topic)
mydata$City_code<-as.factor(mydata$City_code)
mydata$Male<-as.factor(mydata$Male)
mydata$Time_Period<-as.factor(mydata$Time_Period)
mydata$Weekday<-as.factor(mydata$Weekday)
mydata$Month<-as.factor(mydata$Month)

str(mydata)
#Time_Spent : num  88 51.6 82.4 62.1 77.7 ...
#Age        : int  43 50 38 45 31 38 26 23 22 50 ...
#Internet_Usage: num  185 177 211 190 205 ...
#Ad_Topic   : Factor w/ 30 levels "product_1","product_10",..: 3 29 27 11 3 2 1 29 22 10 ...
#City_code  : Factor w/ 9 levels "City_1","City_2",..: 5 1 2 3 2 2 2 4 3 4 ...
#Male       : Factor w/ 2 levels "No","Yes": 1 1 1 2 1 2 2 1 1 1 ...
#Time_Period: Factor w/ 6 levels "Early-Morning",..: 3 2 4 4 6 1 1 6 3 1 ...
#Weekday    : Factor w/ 7 levels "Friday","Monday",..: 5 3 6 5 5 3 6 4 6 1 ...
#Month      : Factor w/ 7 levels "April","February",..: 4 5 3 1 3 4 3 4 1 1 ...
#Clicked    : int  0 1 0 1 0 1 1 0 0 1 ...

##STEP 3:DATA PREPROCESSING

# Checking missing values
colSums(is.na(mydata))
#Time_Spent            Age Internet_Usage       Ad_Topic    
#0              0              0              0               
#City_code           Male    Time_Period        Weekday          Month 
#0              0              0              0              0 
#Clicked 
#0 
library(Amelia)
missmap(mydata, main="Ad-Clicking Missing Data",
        col=c("red", "black"), legend = F)

## We find that there is no missing value in the set.Missing value treatment done.

###treating the outliers###
boxplot(mydata$Time_Spent)#no outliers present
boxplot(mydata$Age)#no outliers present
boxplot(mydata$Internet_Usage)#no outliers found
boxplot(mydata$Ad_Topic)#no outliers present
boxplot(mydata$City_code)#no outliers
boxplot(mydata$Male)#no outliers
boxplot(mydata$Time_Period)#no outliers
boxplot(mydata$Weekday)#no outliers
boxplot(mydata$Month)#no outliers
boxplot(mydata$Clicked)#no outliers

##STEP 4:UNIVARIATE AND BIVARIATE ANALYSIS

##UNIVARIATE ANALYSIS

##continuous column- histogram
# Exploring MULTIPLE CONTINUOUS features
ColsForHist=c("Time_Spent","Internet_Usage","Age")
par(mfrow=c(3,1))
library(RColorBrewer)
for (contCol in ColsForHist){
  hist(mydata[,c(contCol)], main=paste('Histogram of:', contCol), 
       col=brewer.pal(8,"Paired"))
}

##categorical column- bar plot
# Exploring MULTIPLE CATEGORICAL features
ColsForBar=c("Ad_Topic","City_code","Male","Time_Period","Weekday","Month","Clicked")
par(mfrow=c(4,2))

for (ColumnName in ColsForBar){
  barplot(table(mydata[,c(ColumnName)]), main=paste('Barplot of:', ColumnName), 
          col=brewer.pal(8,"Paired"))}


##BIVARIATE ANALYSIS
# Continuous Vs Categorical --- Box Plot
#Categorical Vs Categorical -- Grouped Bar chart

# Categorical Vs Continuous Visual analysis: Boxplot

##Age: continuous, Clicked: Categorical
par(mfrow=c(1,1))
boxplot(Age~Clicked, data = mydata, col=brewer.pal(8,"Paired"))
##Time_spent: continuous, Clicked: Categorical
boxplot(Time_Spent~Clicked, data =mydata, col=brewer.pal(8,"Paired"))
##Internet_Usage: continuous, Clicked: Categorical
boxplot(Internet_Usage~Clicked, data =mydata, col=brewer.pal(8,"Paired"))

# Categorical Vs Categorical Visual analysis: Grouped Bar chart

table(mydata$Ad_Topic)
#here we get that there are total 30 products starting from product_1 to product_30
chi_cols=c("Ad_Topic")
CrossTabResult=table(mydata[,c("Clicked",chi_cols)])
CrossTabResult
barplot(CrossTabResult, beside=T, col=c('Red','Green'))

#Clicked product_1 product_10 product_11 product_12 product_13 product_14
#  0       115        133        119        142        136        119
#  1       123         99         97         85        111        107
#Clicked product_15 product_16 product_17 product_18 product_19 product_2
#   0        125        117        129        117        118       125
#   1         85        107        108        117         77        19
#Clicked product_20 product_21 product_22 product_23 product_24 product_25
#   0        131         98        126        116        135        148
#   1        103        109        140        108         95        107
#Clicked product_26 product_27 product_28 product_29 product_3 product_30
#  0        123        131        109        110        15        141
#   1        104        119        120        109        97         93
#Clicked product_4 product_5 product_6 product_7 product_8 product_9
#0       125       122       145       124       119       106
#1        90        94        99        99       107       110

table(mydata$City_code)
#here we get that there are total 9 city_code
chi_cols=c("City_code")
CrossTabResult=table(mydata[,c("Clicked",chi_cols)])
CrossTabResult
barplot(CrossTabResult, beside=T, col=c('Blue','Red'))
#Clicked City_1 City_2 City_3 City_4 City_5 City_6 City_7 City_8 City_9
# 0   1035    907    703    468    276    145     59     22      4
# 1   1524    768    372    205     88     50     22      9      0

table(mydata$Male)
#here we get that there are total no of males male and other gender
chi_cols=c("Male")
CrossTabResult=table(mydata[,c("Clicked",chi_cols)])
CrossTabResult
barplot(CrossTabResult, beside=T, col=c('Blue','Yellow'))
#Clicked   No  Yes
#      0  1896 1723
#      1   1675 1363

table(mydata$Time_Period)
#here we get that there are 4 times Early_morning,evening,mid-night,morning,night,noon
chi_cols=c("Time_Period")
CrossTabResult=table(mydata[,c("Clicked",chi_cols)])
CrossTabResult
barplot(CrossTabResult, beside=T, col=c('orange','Yellow'))
#Clicked Early-Morning Evening Mid-Night Morning Night Noon
#  0           186     690       646     728   687  682
#  1           488     576       499     500   453  522

table(mydata$Weekday)
#here we get the 7 days of week 
chi_cols=c("Weekday")
CrossTabResult=table(mydata[,c("Clicked",chi_cols)])
CrossTabResult
barplot(CrossTabResult, beside=T, col=c('grey','black'))
#Clicked Friday Monday Saturday Sunday Thursday Tuesday Wednesday
#    0    553    537      485    535      500     502       507
#    1    433    444      446    444      426     433       412

table(mydata$Month)
#here we get the months   April February  January     July     June    March      May  
chi_cols=c("Month")
CrossTabResult=table(mydata[,c("Clicked",chi_cols)])
CrossTabResult
barplot(CrossTabResult, beside=T, col=c('grey','red'))
#Clicked April February January July June March May
#    0   534      498     541  523  508   522 493
#     1   440      446     447  407  433   408 457

# Statistical Relationship between target variable (Categorical) and predictors

# Categorical Vs Continuous --- ANOVA
# Categorical Vs Categorical -- Chi-square test

# Continuous Vs Categorical relationship strength: ANOVA
# Small P-Value <5%--> Variables are correlated(H0 is rejected)
# Large P-Value--> Variables are NOT correlated (H0 is accepted)
summary(aov(Age~Clicked, data =mydata))
#              Df Sum Sq Mean Sq F value Pr(>F)    
#Clicked        1  47957   47957   421.7 <2e-16 ***

summary(aov(Time_Spent~Clicked, data =mydata))
#                Df Sum Sq Mean Sq F value Pr(>F)    
#Clicked        1 812669  812669    6860 <2e-16 ***

summary(aov(Internet_Usage~Clicked, data =mydata))
#               Df  Sum Sq Mean Sq F value Pr(>F)    
#Clicked        1 6849816 6849816    8190 <2e-16 ***
##Since The P square value is less varables are corelated


#### Categorical Vs Categorical relationship strength: Chi-Square test
# H0: Variables are NOT correlated
# Small P-Value--> Variables are correlated(H0 is rejected)
# Large P-Value--> Variables are NOT correlated (H0 is accepted)

Chisqcols=c("Ad_Topic","City_code","Male","Time_Period","Weekday","Month")
for(chi_cols in ColsForBar ){
  CrossTabResult=table(mydata[,c('Clicked',chi_cols)])
  ChiResult=chisq.test(CrossTabResult)
  print(ColsForBar)
  print(ChiResult)
}
#"Ad_Topic" p-value < 2.2e-16
#"City_code" p-value < 2.2e-16
#"Male" p-value = 0.02695
#"Time_Period"  p-value < 2.2e-16
   
## #p is very low, 
#so we reject the null and conclude that these two columns are correlated

#"Weekday"  p-value = 0.7226  
#"Month"  p-value = 0.4229 
##p value is high and w conclude that the columns are not corelated thus we discard them
str(mydata)
mydata <-  mydata[, -c(8,9)]# imputing Month and Weekday
str(mydata)
#$ Time_Spent    : num  88 51.6 82.4 62.1 77.7 ...
#$ Age           : int  43 50 38 45 31 38 26 23 22 50 ...
#$ Internet_Usage: num  185 177 211 190 205 ...
#$ Ad_Topic      : Factor w/ 30 levels "product_1","product_10",..: 3 29 27 11 3 2 1 29 22 10 ...
#$ City_code     : Factor w/ 9 levels "City_1","City_2",..: 5 1 2 3 2 2 2 4 3 4 ...
#$ Male          : Factor w/ 2 levels "No","Yes": 1 1 1 2 1 2 2 1 1 1 ...
#$ Time_Period   : Factor w/ 6 levels "Early-Morning",..: 3 2 4 4 6 1 1 6 3 1 ...
#$ Clicked       : int  0 1 0 1 0 1 1 0 0 1 ...

##STEP 5: FEATURE SCALING
TargetVariableName=c('Clicked')
BestPredictorName= c("Time_Spent","Age","Internet_Usage","Ad_Topic","City_code","Male","Time_Period")

# Extracting Target and predictor variables from data to create a generic dataset
TargetVariable=mydata[, c(TargetVariableName)]
str(TargetVariable)

# Selecting all other columns as Predictors apart from target variable
PredictorVariable=mydata[, BestPredictorName]
str(PredictorVariable)
DataForML=data.frame(TargetVariable,PredictorVariable)
head(DataForML)

##STEP 6:SPLITTING DATA-SET
library(caTools)
set.seed(1000)
split<- sample.split(mydata$Clicked,SplitRatio = 0.70)
split

table(split)
#FALSE  TRUE 
#1997  4660 
training<-subset(mydata,split==TRUE)
nrow(training) #  4660
test<-subset(mydata,split==FALSE)
nrow(test)#1997


## STEP 7: MODEL-BUILDING
lg_regression <- glm(Clicked~.,data=training,family="binomial")
lg_regression
summary(lg_regression)

#Null deviance: 6424.7  on 4659  degrees of freedom
#Residual deviance: 2046.6  on 4613  degrees of freedom
#AIC: 2140.6

## Removing Insignificant variables ad_topic and male

lg_reg1<- glm(formula = Clicked ~ Time_Spent + Age + Internet_Usage + City_code + 
                Time_Period, family = "binomial", data = training)
lg_reg1
summary(lg_reg1)
#Null deviance: 6424.7  on 4659  degrees of freedom
#Residual deviance: 2077.6  on 4643  degrees of freedom
#AIC: 2111.6
##Time_spent Internet_usage Age City_code Time_period Male Ad_Topic PREDECTOR VARIABLE
##Time_spent Internet_usage Age City_code Time_period ZRE SIGNIFICANT VARIABLE

##STEP 8:MULTICOLINEARITY CHECK
library(car)
vif(lg_reg1)
#                   GVIF Df GVIF^(1/(2*Df))
#Time_Spent     1.020888  1        1.010390
#Age            1.006481  1        1.003235
#Internet_Usage 1.027002  1        1.013411
#City_code      1.030479  8        1.001878
#Time_Period    1.027622  5        1.002728

##SINCE THE VALUES ARE LESS THAN 5.THERE IS NO MULTICOLINEARITY PRESENT



##STEP 9 AND 10:GENERATE PROBABILITIES ON TEST DATA AND ACCURACY MEASURES
#Predict and validate the final model in the test sample of your data.
pred <- predict(lg_reg1, newdata = test, type='response')
pred
pred_thre_50 <- ifelse(pred>=0.5,1,0)##threshold to be 50%
pred_thre_50
cm <- table(test$Clicked, pred_thre_50)
cm
#pred_thre_50
#  0    1
#0 1031   55
#1  100  811

library(caret)
library(e1071)
confusionMatrix(cm)
#pred_thre_50
#   0    1
#0 1031   55
#1  100  811

#Accuracy : 0.9224          
#95% CI : (0.9098, 0.9337)
#No Information Rate : 0.5663          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.8429          

#Mcnemar's Test P-Value : 0.0004091       
                                          
#            Sensitivity : 0.9116          
#            Specificity : 0.9365          
#         Pos Pred Value : 0.9494          
#         Neg Pred Value : 0.8902          
#             Prevalence : 0.5663          
#         Detection Rate : 0.5163          
#   Detection Prevalence : 0.5438          
#      Balanced Accuracy : 0.9240          
                                          
#       'Positive' Class : 0       

#### IT IA A GOOD MODEL WITH ACCURACY OF 92 PERCENT HAVING AUTO-CORELATION AND MULTICOLINIEARITY AND THRESHOLD OF 50 PERCENT,HAVING P-VALUE LESS THAN 0.05
#####THUS LOGISTIC MODEL IS BUILD

###### BUSINESS RECOMENDATIONS
##ADVERTISEMENT SHOULD BE DISPLAYED MORE DURING THE EVENING TIME AND NOON TIME
##ADVERTISEMENT SHOULD BE DISPLAYED MORE IN CITIES WITH CITY_CODE:CITY_1,CITY_2,CITY_3
##ADVERTISEMENT SHOULD BE LESS DISPLAYED IN THE CITIES WITH CITY_CODE:CITY_7,CITY_8,CITY_9
##ADVERTISEMENT TARGET THE  AUDIENCE BETWEEN AGE GROUP 20-50 THUS SHOULD BE DESIGNED ACCORDING TO THE MIDDLE YOUTH TASTE
##ADVERTISEMENT SHOULD BE DISPLAYED MORE TO PEOPLE WHO HAS MORE INTERNET USAGE 