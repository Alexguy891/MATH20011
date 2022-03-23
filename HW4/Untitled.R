#Alex Ely
#Homework 4

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


