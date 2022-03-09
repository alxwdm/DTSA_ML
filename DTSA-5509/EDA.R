# use lolcat package for analyzing the data
require(lolcat)
ro<-round.object
nqtr<-function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999)

# load data
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Data Science/MS-DS/DTSA-5509_IntroSupervisedML/Final Project")
caffeine = read.csv('caffeine.csv')
View(caffeine)

# general summary statistics
nqtr(summary.continuous(caffeine), 4)

# group data by label (type)
coffee = subset(x = caffeine, subset = caffeine$type=='Coffee')
energy.drinks = subset(x = caffeine, subset = caffeine$type=='Energy Drinks')
energy.shots = subset(x = caffeine, subset = caffeine$type=='Energy Shots')
soft.drinks = subset(x = caffeine, subset = caffeine$type=='Soft Drinks')
tea = subset(x = caffeine, subset = caffeine$type=='Tea')
water = subset(x = caffeine, subset = caffeine$type=='Water')

# look at grouped histograms
grouped.hist<-function(group){
  par(mfrow=c(1,3))
  this.type = group$type[1]
  hist(group$Volume..ml., main = "", xlab = "Volume")
  hist(group$Calories, main = paste("Histogram of ", this.type), xlab="Calories")
  hist(group$Caffeine..mg., main = "", xlab="Cafffeine")
  }
grouped.hist(coffee)
grouped.hist(energy.drinks)
grouped.hist(energy.shots)
grouped.hist(soft.drinks)
grouped.hist(tea)
grouped.hist(water)

# test for difference in means of selected group pairings
# significance level alpha = 0.05
# Bonferroni Correction to avoid inflating type I error
# 5 Tests -> alpha_corr = 0.01
paste("Coffee vs. Energy Drinks - Calories: p-value = ", 
      ro(
        (t.test.twosample.independent(
          g1=coffee$Calories,
          g2=energy.drinks$Calories)$p.value)
        ,4)
)
paste("Coffee vs. Energy Drinks - Caffeine: p-value = ", 
      ro(
        (t.test.twosample.independent(
          g1=coffee$Caffeine..mg.,
          g2=energy.drinks$Caffeine..mg.)$p.value)
        ,4)
)
paste("Coffee vs. Soft Drinks - Calories: p-value = ", 
      ro(
        (t.test.twosample.independent(
          g1=coffee$Calories,
          g2=soft.drinks$Calories)$p.value)
        ,4)
)
paste("Energy Drinks vs. Soft Drinks - Calories: p-value = ", 
      ro(
        (t.test.twosample.independent(
          g1=energy.drinks$Calories,
          g2=soft.drinks$Calories)$p.value)
        ,4)
)
paste("Coffee vs. Energy Shots - Volume: p-value = ", 
      ro(
        (t.test.twosample.independent(
          g1=coffee$Volume..ml.,
          g2=energy.shots$Volume..ml.)$p.value)
        ,4)
)
