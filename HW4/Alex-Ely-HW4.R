#Alex Ely
#Homework 4

#1___________________________________________________________________________
#a)
#calculating point estimate
realVal = c(334,369,191,63,22,12,9,0,0,0,0)
pEst = sum(realVal*0:10)/10000

#b)
#calculating expected value
expVal = dbinom(0:10,10,pEst)*1000

#c)
#counting expected values less than 5
count = 0
for(i in 1:11) {
  if(expVal[i] < 5)
    count = count + expVal[i]
}

#d)
#computing chisquared
chiSq <- sum((realVal - expVal)^2/expVal)

#e)
#computing p value
pVal = 1-pchisq(chiSq,10)


#2___________________________________________________________________________
#a)
#H0: 2 variables, independent for education and location)
#Ha: 2 variables, independent for education and location

#b)
#creating observed matrix
obsMtx <- matrix(c(15,8,6,12,15,8,8,9,7),3,3)

#c)
#generating expected matrix
expMtx <- chisq.test(obsMtx)$expected

#d)
#statistic value of chisquare test
stat <- chisq.test(obsMtx)$statistic

#e)
qchisq(1-0.05,4)

#f)
p <- chisq.test(obsMtx)$p.value

#g)
#We did not reject the null hypothesis


#3___________________________________________________________________________
#a)
#setting working directory
setwd("/Users/alexely/Downloads")

#b)
#setting Subjects to the csv file
Subjects <- read.csv("SOCR-HeightWeight-1.csv")

#c)
#showing first few data values
head(Subjects)

#d)
#storing height and weight
Height <- Subjects$Height
Weight <- Subjects$Weight

#e)
#finding 95% conf interval for Height
t.test(Height, conf.level = 0.95)
#finding 99% conf interval for Weight
t.test(Weight, conf.level = 0.99)


