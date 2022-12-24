#download the data 
getwd() #get current working directoy
data <- read.csv("C:/Users/sohaila/Desktop/T-test/healthcare-dataset-stroke-data-csv (1).csv",header = TRUE,sep=",")
print(data)
###############################################################
#Analyzing The csv file 
print(is.data.frame(data)) 
print(ncol(data))
print(nrow(data)) 
############################################################## 
#Gender and bmi 
gender <- data$gender 
bmi <- data$ bmi
female = c() 
male = c()  
index1 = 1
index2 = 1 
index3 = 1
for(i in gender) 
{
  if(i == "Female") 
  {
    female[index1] <- data$bmi[index3]
    index1 = index1 + 1 
    index3 = index3 + 1
  }  
  else 
  {
    male[index2] <- data$bmi[index3] 
    index2 = index2 + 1 
    index3 = index3 + 1
  }  
} 
#female 
#male 
result <- t.test(female,male) 
result
##################################################################
#Gender and Strok
gender <- data$gender 
Strok <- data$stroke
female = c() 
male = c()  
index1 = 1
index2 = 1 
index3 = 1
for(i in gender) 
{
  if(i == "Female") 
  {
    female[index1] <- data$stroke[index3]
    index1 = index1 + 1 
    index3 = index3 + 1
  }  
  else 
  {
    male[index2] <- data$stroke[index3] 
    index2 = index2 + 1 
    index3 = index3 + 1
  }  
} 
#female 
#male 
result <- t.test(female,male) 
result
#####################################################################
#Gender and hypertension
gender <- data$gender 
hyper <- data$hypertension
female = c() 
male = c()  
index1 = 1
index2 = 1 
index3 = 1
for(i in gender) 
{
  if(i == "Female") 
  {
    female[index1] <- data$hypertension[index3]
    index1 = index1 + 1 
    index3 = index3 + 1
  }  
  else 
  {
    male[index2] <- data$hypertension[index3] 
    index2 = index2 + 1 
    index3 = index3 + 1
  }  
} 
#female 
#male 
result <- t.test(female,male) 
result
###############################################################################
#Residence_type and avg_glucose_level 
Residence <- data$Residence_type
avg_OF_glucose <- data$avg_glucose_level 
Rural = c() 
Urban = c()  
index1 = 1
index2 = 1 
index3 = 1   
for(i in Residence) 
{ 
  if(i =="Rural") 
  {
    Rural[index1] <- data$avg_glucose_level[index3]
    index1 = index1 + 1 
    index3 = index3 + 1
  }  
  else 
  {
    Urban[index2] <- data$avg_glucose_level[index3] 
    index2 = index2 + 1 
    index3 = index3 + 1
  }  
} 
#Rural
#Urban
result <- t.test(Rural,Urban) 
result 
############################################################################
# printing the p-value
result$p.value
# printing the mean
result$estimate
# printing the confidence interval
result$conf.int
###############################################################################
#Residence_type and strok
Residence <- data$Residence_type
strok <- data$stroke
Rural = c() 
Urban = c()  
index1 = 1
index2 = 1 
index3 = 1   
for(i in Residence) 
{ 
  if(i =="Rural") 
  {
    Rural[index1] <- data$stroke[index3]
    index1 = index1 + 1 
    index3 = index3 + 1
  }  
  else 
  {
    Urban[index2] <- data$stroke[index3] 
    index2 = index2 + 1 
    index3 = index3 + 1
  }  
} 
#Rural
#Urban
result <- t.test(Rural,Urban) 
result 
################################################################## 
#Residence_type and hypertension
Residence <- data$Residence_type
hyper <- data$hypertension 
#Residence
Rural = c() 
Urban = c()  
index1 = 1
index2 = 1 
index3 = 1   
for(i in Residence) 
{ 
  if(i =="Rural") 
  {
    Rural[index1] <- data$hypertension[index3]
    index1 = index1 + 1 
    index3 = index3 + 1
  }  
  else 
  {
    Urban[index2] <- data$hypertension[index3] 
    index2 = index2 + 1 
    index3 = index3 + 1
  }  
} 
#Rural
#Urban
result <- t.test(Rural,Urban) 
result 