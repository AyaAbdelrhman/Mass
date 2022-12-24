install.packages("GGally")
library(tidyr) # metapackage of all tidyverse packages
library(plyr)
library(dplyr)
library(GGally)
library(dplyr)
library(modeest)
library(ggplot2)
library(cowplot)
library(mice)
library(caTools)
library(caret)
install.packages("plot_correlation")
library(plot_correlation)
install.packages("effects") 
library(effects)
#library(grid)
data <- read.csv("healthcare-dataset-stroke-data-csv.csv")
head(data)#first 6
tail(data)#last 6
dim(data)#View the dimension of the data
names(data)#name of row and column
str(data)#type of all items
anyNA(data)#if it null or not
summary(data)#explanation for data (min ,median , mean ,1q,3q ,max ,length , classType)
#Summary :
#In this data, it is known that there are 1660 male sex and 2577 female sex
#The average age is 48.57 years, with the youngest age of 7 years and the oldest age of 82 years
#From 4037 data, there were 3204 people who were married and 1033 people who were not married
#private is the most work type with 2810 data
#average glucose level : 107.08, min: 55.12, and max: 271.74
#average BMI : 30.3, min: 11.3, and max: 97.6
#From 5110 data, there were 824 people who smoked and 1798 people who didn’t smoke
age=data$age
hist(age,xlab = "age ",col="blue",breaks = 10)
avg_glucose_level=data$avg_glucose_level
hist(avg_glucose_level,xlab = " avg_glucose_level  ",col="gray",breaks=10)
hist(age,xlab = "avg_glucose_level ",col="blue",add=T)
plot(data$avg_glucose_level, data$bmi)

cat_cols = c('gender','hypertension','heart_disease','ever_married','work_type','Residence_type',
             'smoking_status','stroke')
for (col in cat_cols){
  # ggplot powerful graphics package that can be used to make very impressive data visualizations
  #take 1. reference to data 2.aesthetics - x, y 3.fill
  print(ggplot(data, aes(x=!!sym(col), y=age, fill = !!sym(col))) + geom_boxplot())#Boxplots by groups by age
}
#The ggpairs() function from the GGally package allows us to build a great scatterplot matrix.
#Scatterplots of each pair visualized in left side of the plot and Pearson correlation value
#and significance displayed on the right side.
data %>%  #my data
  select(age, avg_glucose_level, bmi) %>% #data that i want to select it
  drop_na() %>%#drops rows contains a missing value.
  ggpairs()
# The lm() function is used to fit linear models to data frames in the R Language.
#Parameter:
#fitting_formula: determines the formula for the linear model.
#dataframe: determines the name of the data frame that contains the data.
#plot effect can use this plot to compare the relative strength of the effects of various factors.it called interaction effects 
fit<-lm(avg_glucose_level~stroke+age+bmi+stroke+hypertension+heart_disease+gender,data=data)
x=data[,1:12]
data_matrix <- data.matrix(x)
#Rowv determines if and how the row dendrogram should be computed and reordered.
#NA to suppress any row dendrogram (and reordering) or by default
#centered and scaled in either the row direction or the column direction
data_heatmap <- heatmap(data_matrix, Rowv=NA, Colv=NA, scale="column")

# Plot the chart.
#formula=x,y,z numeric vector of data values to be split into groups according to the grouping variable
# data from which the variables in formula should be taken
#comparing the distribution of data across data sets by drawing boxplots for each of them.
# Give the chart file a name.
png(file = "boxplot.png")
x=c(data$age)
y=c(data$bmi) 
z=c(data$avg_glucose_level) 
boxplot(age~heart_disease,data=data_matrix,xlab="age", ylab="heart_disease",col=(c("gold","darkgreen")))
boxplot(x,y,z,data= data, horizontal=TRUE, names=c("age","bmi","avg_glucose_level"), col=c("red","yellow","blue"))
# Save the file.
dev.off()



#boxplot(age~heart_disease, data, main,notch, names)
#age~heart_disease is a vector or a formula.
#data is the data frame.
#main is used to give a title to the graph.
#notch is a logical value. Set as TRUE to draw a notch.
#names are the group labels which will be printed under each boxplot.



boxplot(age~heart_disease,data=data,
        main = "comparing the distribution of age and heart_disease",
        xlab = "age",
        ylab = "heart_disease",
        col = c("orange","darkgreen"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)


