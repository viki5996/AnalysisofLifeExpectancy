rm(list=ls())
library(data.table)
library(DBI)
library(tidyverse)
library(RSQLite)
library(plm)
library(gdata)

setwd("C:/Users/Vikram Arikath/Desktop/Applie Econ Projects")

mydata = read.csv("war_countries.csv")

pdata <- mydata %>% pdata.frame(index=c('Country','Year'))
head(pdata)

femodel1<- plm(log(Life.expectancy)~War+Alcohol+Adult.Mortality+infant.deaths+percentage.expenditure+Hepatitis.B+Measles+BMI+under.five.deaths+Polio+Total.expenditure+Diphtheria+HIV.AIDS+GDP+Population+thinness..1.19.years+thinness.5.9.years+Income.composition.of.resources+Schooling, model="within", data=pdata)
summary(femodel1)
AIC_adj1 <- function(femodel1){
  n.N   <- nrow(femodel1$model)
  u.hat <- residuals(femodel1)
  s.sq  <- log( (sum(u.hat^2)/(n.N)))
  p     <-  length(coef(femodel1)) + 1
  aic <- 2*p  +  n.N * (  log(2*pi) + s.sq  + 1 ) 
  return(aic)
}
aic1 <- AIC_adj1(femodel1)
aic1

femodel2<- plm(log(Life.expectancy)~War+Alcohol+Adult.Mortality+infant.deaths+percentage.expenditure+Hepatitis.B+Measles+BMI+Polio+Total.expenditure+Diphtheria+HIV.AIDS+GDP+Population+thinness..1.19.years+thinness.5.9.years+Income.composition.of.resources+Schooling, model="within", data=pdata)
summary(femodel2)
AIC_adj2 <- function(femodel2){
  n.N   <- nrow(femodel2$model)
  u.hat <- residuals(femodel2)
  s.sq  <- log( (sum(u.hat^2)/(n.N)))
  p     <-  length(coef(femodel2)) + 1
  aic <- 2*p  +  n.N * (  log(2*pi) + s.sq  + 1 ) 
  return(aic)
}
aic2 <- AIC_adj1(femodel2)
aic2


femodel3<- plm(log(Life.expectancy)~War+Alcohol+Adult.Mortality+infant.deaths+percentage.expenditure+Hepatitis.B+Measles+BMI+Polio+Total.expenditure+Diphtheria+HIV.AIDS+GDP+Population+thinness.5.9.years+Income.composition.of.resources+Schooling, model="within", data=pdata)
summary(femodel3)
AIC_adj3 <- function(femodel3){
  n.N   <- nrow(femodel3$model)
  u.hat <- residuals(femodel3)
  s.sq  <- log( (sum(u.hat^2)/(n.N)))
  p     <-  length(coef(femodel3)) + 1
  aic <- 2*p  +  n.N * (  log(2*pi) + s.sq  + 1 ) 
  return(aic)
}
aic3 <- AIC_adj3(femodel3)
aic3


#With imputing

rm(list=ls())
library(data.table)
library(DBI)
library(tidyverse)
library(RSQLite)
library(plm)
library(gdata)

setwd("C:/Users/Vikram Arikath/Desktop/Applie Econ Projects")

mydata = read.csv("war_countries.csv")

pdata <- mydata %>% pdata.frame(index=c('Country','Year'))
head(pdata)

sum(is.na(pdata$Life.expectancy))
sum(is.na(pdata$Status))
sum(is.na(pdata$Adult.Mortality))
sum(is.na(pdata$infant.deaths))
sum(is.na(pdata$Alcohol))
sum(is.na(pdata$percentage.expenditure))
sum(is.na(pdata$Hepatitis.B))
sum(is.na(pdata$Measles))
sum(is.na(pdata$BMI))
sum(is.na(pdata$under.five.deaths))
sum(is.na(pdata$Polio))
sum(is.na(pdata$Total.expenditure))
sum(is.na(pdata$Diphtheria))
sum(is.na(pdata$HIV.AIDS))
sum(is.na(pdata$GDP))
sum(is.na(pdata$Population))
sum(is.na(pdata$thinness..1.19.years))
sum(is.na(pdata$thinness.5.9.years))
sum(is.na(pdata$Income.composition.of.resources))
sum(is.na(pdata$Schooling))

pdata$Alcohol[is.na(pdata$Alcohol)] = mean(pdata$Alcohol, na.rm=TRUE)
pdata$Total.expenditure[is.na(pdata$Total.expenditure)] = mean(pdata$Alcohol, na.rm=TRUE)
pdata$GDP[is.na(pdata$GDP)] = mean(pdata$GDP, na.rm=TRUE)
pdata$Population[is.na(pdata$Population)] = mean(pdata$Population, na.rm=TRUE)


femodel1<- plm(log(Life.expectancy)~War+Alcohol+Adult.Mortality+infant.deaths+percentage.expenditure+Hepatitis.B+Measles+BMI+under.five.deaths+Polio+Total.expenditure+Diphtheria+HIV.AIDS+GDP+Population+thinness..1.19.years+thinness.5.9.years+Income.composition.of.resources+Schooling, model="within", data=pdata)
summary(femodel1)
AIC_adj1 <- function(femodel1){
  n.N   <- nrow(femodel1$model)
  u.hat <- residuals(femodel1)
  s.sq  <- log( (sum(u.hat^2)/(n.N)))
  p     <-  length(coef(femodel1)) + 1
  aic <- 2*p  +  n.N * (  log(2*pi) + s.sq  + 1 ) 
  return(aic)
}
aic1 <- AIC_adj1(femodel1)
aic1

femodel2<- plm(log(Life.expectancy)~War+Alcohol+Adult.Mortality+infant.deaths+percentage.expenditure+Hepatitis.B+Measles+BMI+Polio+Total.expenditure+Diphtheria+HIV.AIDS+GDP+Population+thinness..1.19.years+thinness.5.9.years+Income.composition.of.resources+Schooling, model="within", data=pdata)
summary(femodel2)
AIC_adj2 <- function(femodel2){
  n.N   <- nrow(femodel2$model)
  u.hat <- residuals(femodel2)
  s.sq  <- log( (sum(u.hat^2)/(n.N)))
  p     <-  length(coef(femodel2)) + 1
  aic <- 2*p  +  n.N * (  log(2*pi) + s.sq  + 1 ) 
  return(aic)
}
aic2 <- AIC_adj1(femodel2)
aic2


femodel3<- plm(log(Life.expectancy)~War+Alcohol+Adult.Mortality+infant.deaths+percentage.expenditure+Hepatitis.B+Measles+BMI+Polio+Total.expenditure+Diphtheria+HIV.AIDS+GDP+Population+thinness.5.9.years+Income.composition.of.resources+Schooling, model="within", data=pdata)
summary(femodel3)
AIC_adj3 <- function(femodel3){
  n.N   <- nrow(femodel3$model)
  u.hat <- residuals(femodel3)
  s.sq  <- log( (sum(u.hat^2)/(n.N)))
  p     <-  length(coef(femodel3)) + 1
  aic <- 2*p  +  n.N * (  log(2*pi) + s.sq  + 1 ) 
  return(aic)
}
aic3 <- AIC_adj3(femodel3)
aic3















num_columns <- mydata[, c(5:23)]
res <- cor(num_columns, use = "complete.obs")
round(res, 2)



pdata <- transform(pdata, pdata$Alcohol = ifelse(is.na(pdata$Alcohol), mean(pdata$Alcohol, na.rm=TRUE), pdata$Alcohol))



femodel4 <- plm(log(Life.expectancy)~War+Alcohol+Adult.Mortality+infant.deaths+percentage.expenditure+Hepatitis.B+Measles+BMI+under.five.deaths+Polio+Total.expenditure+Diphtheria+HIV.AIDS+GDP+Population+thinness..1.19.years+thinness.5.9.years+Income.composition.of.resources+Schooling, model="within", data=pdata)
summary(femodel4)

AIC(femodel4)


AIC_adj <- function(femodel4){
  # Number of observations
  n.N   <- nrow(femodel4$model)
  # Residuals vector
  u.hat <- residuals(femodel4)
  # Variance estimation
  s.sq  <- log( (sum(u.hat^2)/(n.N)))
  # Number of parameters (incl. constant) + one additional for variance estimation
  p     <-  length(coef(femodel4)) + 1
  
  # Note: minus sign cancels in log likelihood
  aic <- 2*p  +  n.N * (  log(2*pi) + s.sq  + 1 ) 
  
  return(aic)
}

aic <- AIC_adj(femodel4)
aic

AIC_adj1 <- function(femodel5){
  # Number of observations
  n.N   <- nrow(femodel5$model)
  # Residuals vector
  u.hat <- residuals(femodel5)
  # Variance estimation
  s.sq  <- log( (sum(u.hat^2)/(n.N)))
  # Number of parameters (incl. constant) + one additional for variance estimation
  p     <-  length(coef(femodel5)) + 1
  
  # Note: minus sign cancels in log likelihood
  aic <- 2*p  +  n.N * (  log(2*pi) + s.sq  + 1 ) 
  
  return(aic)
}

aic1 <- AIC_adj1(femodel5)
aic1

femodel6 <- plm(log(Life.expectancy)~War+Alcohol+Adult.Mortality+infant.deaths+percentage.expenditure+Hepatitis.B+Measles+BMI+under.five.deaths+Polio+Total.expenditure+Diphtheria+HIV.AIDS+GDP+Population+thinness.5.9.years+Income.composition.of.resources+Schooling, model="within", data=pdata)
summary(femodel6)

AIC_adj3<- function(femodel5){
  # Number of observations
  n.N   <- nrow(femodel5$model)
  # Residuals vector
  u.hat <- residuals(femodel5)
  # Variance estimation
  s.sq  <- log( (sum(u.hat^2)/(n.N)))
  # Number of parameters (incl. constant) + one additional for variance estimation
  p     <-  length(coef(femodel5)) + 1
  
  # Note: minus sign cancels in log likelihood
  aic <- 2*p  +  n.N * (  log(2*pi) + s.sq  + 1 ) 
  
  return(aic)
}

aic2 <- AIC_adj2(femodel5)
aic2
