#1__________________________________________
#a________
#of patients
patPop = 1000
#probability of side effects
prob = 0.1
#vec of 1-10000
vec = seq(1:1000)
#generate probability dbinom
probs = dbinom(vec,1000,0.1)
#calculate expected value
expVal = sum(vec*probs)

#b________
#calculate variance
var = sum((vec-expVal)^2*probs)
#calculate standard deviation
sdExpVal = sqrt(var)

#c________
#probabilty of 100 patients 
probOf100 = pbinom(100,patPop,prob)

#d________
#probability of 100 patients with normal approx
normOf100 = pnorm(100.5,expVal,sdExpVal)

#2__________________________________________
#a________
#number of green
gNum = 8
#number of red
rNum = 12
#total
totalNum = gNum + rNum
#calculating probability
redProb = (rNum/totalNum)^5

#b______
#calculating expected values
redExpVal = 1/redProb

#c______
#calculating standard deviation
redSD = sqrt(1-redProb)/redProb

#3__________________________________________
#a________
#given mean
meanFor = 100
#given standard deviation
sdFor = 15
#calculating probability of less than 120 IQ
pNormOne = pnorm(120,meanFor,sdFor)

#b________
#calculating probability of less than 130 and greater than 110
pNormTwo = pnorm(130,meanFor,sdFor)-pnorm(110,meanFor,sdFor)

#c________
#calculating probability of greater than 140 IQ
pNormThree = (1-(pnorm(140,meanFor,sdFor)))*100

#4__________________________________________
#randomly generating geometric 
#k value
k = 5
#p value
p = 0.6
#vector of random geometric variables
randomGeo = rgeom(k, p)
#sum of all values in vector
sumGeo = sum(randomGeo)

#a_________
#count variable
count = 0;
#index variable
i = 0;
#loop to calculate # of sums of randomGeo < 8 
while(i < 100000) {
  randomGeo <- rgeom(k, p)
  count = count + (sum(randomGeo) < 8)
  i = i + 1;
}

#b__________
#calculate ratio of sums < 8 vs total size
ratioNum = count / 100000

#c__________
#calculate P(X <= 7) for neg binom
negBin = pnbinom(7,5,0.6)

#d__________
#A negative binomial distribution with r = 1 is a geometric distribution
#and the ratio is approx 1 and the pnbinom is approx 1

#5__________________________________________
#a______ 
#creating vector of 32% Graduate and 68% not graduate
vecPop <- c(rep("Graduate",320000),rep("Not Graduate",680000))

#b______
#size of sample
n = 1000
#generating sample
samPop = sample(vecPop, n)

#c______
#count for # of graduates
countB = 0
#calculating count
for(val in samPop) {
  countB = countB + (val == "Graduate")
}
#point estimate calc
pHat = countB / n

#d______
#z value
z = qnorm((1-0.01/2),0,1)

#calculating 99% confidence interval
#upper side of interval
confHigh = pHat + z * sqrt((pHat*(1-pHat))/n)
#lower side of interval
confLow = pHat - z * sqrt((pHat*(1-pHat))/n)

#e______
#loading epitools package
install.packages("epitools")
library("epitools")

#f______
#calculating conf interval using binom.approx 
confInt = binom.approx(countB, n, 0.99)