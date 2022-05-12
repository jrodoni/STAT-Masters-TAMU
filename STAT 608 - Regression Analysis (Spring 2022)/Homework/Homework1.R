# 1.) 
playbill <- read.csv("C:/Users/jackr/OneDrive/Desktop/Graduate School Courses/STAT 608 - Regression Analysis/Data/playbill.csv")

lm1 = lm(playbill$CurrentWeek~playbill$LastWeek)

modelsum = summary(lm1)


# 1.a)
confint(lm1, level = 0.95)
# Rs calculation

lm1$coefficients[2] - qt(.975,16)*modelsum$coefficients[2,2]
lm1$coefficients[2] + qt(.975,16)*modelsum$coefficients[2,2]
## by hand calculaiton


# Yes, 1 is a reasonable value for B1 as 1 is contained in our confidence interval

# 1.b)

teststat = (lm1$coefficients[1] - 10000)/modelsum$coefficients[1,2]
teststat
qt(.975,16)


# teststat < 2.11 => fail to reject null

2*(1-(pt(abs(teststat),16)))
# also, pvalue (SEE STAT 641 H.O. 12, pg 24

# 1.c)


v = c(2,3,4,5,6,7)
u = c(3,5,7,8,9,1)

sum(v*u)
sum(u)*sum(v)



