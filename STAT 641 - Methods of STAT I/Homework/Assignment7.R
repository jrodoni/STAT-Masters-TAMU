


pnorm(-1.96+(sqrt(29)*(20-20.05)/.43))+1-pnorm(1.96+(sqrt(29)*(20-20.05)/.43))

#1.2
delta <- (sqrt(29)*(20.05-20))/.43
pt(-qt(.975,28),28,delta)+1-pt(qt(.975,28),28,delta)
x <- c(19.9,19.95,19.99,20,20.05,20.1,20.15,20.2,20.25,20.3,20.4,20.5)
y <- c(.227,.093,.052,.05,.093,.227,.442,.667,.856,.952,.998,.99998)

# make sure to attach pdf of this plot to submission
plot(x,y,type = "l", main = "Problem 1.2: Sketch of Power Curve")

#1.3
power.t.test(n=,delta=.15,sd=.43,sig.level = .05,power = .8,
             type = c("one.sample"),alternative = c("one.sided"))


#2.1

pt((sqrt(15)*(8.7-10))/2,14)

#2.2
delta = (sqrt(15)*(8.5-10))/2
pt(-qt(.99,14),14,delta)


#3.1
x <- c(125,123,117,123,115,112,128,118,124,111,116,109,125,120,113,123,112,118,
       121,118,122,115,105,118,131)
shapiro.test(x)

ts <-((length(x)-1)*var(x))/10^2
pchisq(ts,length(x)-1)

#3.2
sds = c(5,6,7,8,9,10)
ys = 1-pchisq((100/(sds^2))*qchisq(.1,length(x)-1),length(x)-1)

cbind(sds,ys)

#3.3
sqrt(((length(x)-1)*var(x))/qchisq(.05,length(x)-1))

#4.1
median(x)
S_plus=length(which(x>120))
pbinom(S_plus,length(x),.5)
qbinom(.05,length(x),.5)

#4.2
z = x - 120
nstar = z[-which(z==0)]
nstar = abs(nstar)
nstar = sort(nstar,decreasing = FALSE)


wp <- 1+4+8.5+8.5+8.5+11.5+14.5+14.5+19+22.5
wn <- 4+4+4+4+8.5+11.5+14.5+14.5+17+19+19+21+22.5+24

qsignrank(.05,length(x),TRUE)
psignrank(wp,length(x),TRUE)


#4.3
118.4+(1.711*(sd(x)/sqrt(length(x))))


#5.1

ci = c(.92-(1.96*sqrt((.92*.08)/100)),.92+(1.96*sqrt((.92*.08)/100)))
ci

#5.3

ps = seq(.75,.95,by=.05)
ys = 1-pbinom(qbinom(ps,100,.8)-1,100,ps)
cbind(ps, ys)

#5.4

qnorm(.95)
qnorm(.80)
(((qnorm(.95)*sqrt(.8*.2))+(qnorm(.80)*sqrt(.9*.1)))/.1)^2




1-(1 - pchisq((23.8^2)/(47.9^2)*qchisq(.95,9),9))
