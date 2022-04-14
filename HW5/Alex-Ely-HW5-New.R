#Alex Ely
#HW5

install.packages("car")
#1)________________________________________________________
# importing csv dataset
my_data = read.csv(file.choose())
# setting one.way to the result of an anova on my_data, with comprison of tko_win_ratio and stance
one.way <- aov(stance ~ tko_win_ratio, data = my_data)
# summarizes results of anova
summary(one.way)
# Pr(>F) which is the p-value of 0.489 which indicates very little to no correlation between stance and tko rate 
#and we fail to reject the null hypothesis

#2)________________________________________________________
library(car)
#a) importing csv dataset
diet_data = read.csv(file.choose())
#b) summarizing data
summary(diet_data)
#c) creating weight loss column
weightLoss = diet_data['weight'] - diet_data['weight6weeks']
#inputting weightLoss column into data
diet_data['weightloss'] = weightLoss
#d) running anova test on weightloss data
model = aov(weightloss ~ Diet * gender, data = diet_data)
summary(model)
#e) levenne test
leveneTest(weightloss ~ Diet * gender, data = diet_data)
#f) TurkeyHSD test
TukeyHSD(model, conf.level = 0.95)
#g)
#ANOVA shows significant correlation with wieghtloss depending on diet (reject null hypothesis), 
#little to no correlation with weightloss depending on genders (fail to reject) 
#and little correlation with weightloss depending on diet and gender together (fail to reject)
#Levene Test shows we failed to reject null hypothesis, p-value > 0.05, differences equal
#Tukey HSD shows C:F-A:F, C:F-B:F, B:M-C:F are signficantly different at 95 conf interval