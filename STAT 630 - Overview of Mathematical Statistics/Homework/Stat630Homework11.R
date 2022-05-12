# 4.)
2*pnorm((4.88-5)/(sqrt(.5/10)))
4.88+qnorm(.975)*sqrt(.5/10)


data = c(4.7, 5.5, 4.4, 3.3, 4.6, 5.3, 5.2, 4.8, 5.7, 5.3)
var(data)
sd(data)

2*pt((4.88-5)/(sqrt(var(data)/10)), df = length(data)-1)

mean(data) + qt(.975,df=length(data)-1)*(sd(data)/sqrt(length(data)))

###### 5
# wald
z = (.62-.65)/sqrt((.62*(1-.62))/250)
pnorm(z)

# score
num = 250*((0.62 - 0.65)^2)
denom = (0.65*(1-0.65))
z = sqrt(num/denom)
1-pnorm(z)

# 6
2*(1 - pnorm(60,50,5))

pnorm((39.5-50)/sqrt(100*(0.5*0.5)))


# b
n = 100
theta_not = .5
theta_hat = seq(0,1,.01)
alpha = .05
power = pnorm((sqrt(n)*(theta_hat - theta_not)/sqrt(theta_hat*(1-theta_hat))) - qnorm(1-alpha/2)) +
        (1 - pnorm((sqrt(n)*(theta_hat - theta_not)/sqrt(theta_hat*(1-theta_hat))) + qnorm(1-alpha/2)))

plot(theta_hat,power, type = "l")
