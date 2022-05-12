circulation<-read.table("circulation.txt", header=TRUE, sep="\t")
attach(circulation)

par(mfrow=c(1,1), cex=1.3, mar=c(5,5,1,1))
plot(log(Weekday),log(Sunday),xlab="log(Weekday Circulation)",ylab="log(Sunday Circulation)",
pch=Tabloid.with.a.Serious.Competitor+1,col=Tabloid.with.a.Serious.Competitor+1)
legend(11.6, 14.1,legend=c("0","1"),pch=1:2,col=1:2,title="Tabloid dummy variable")

m1 <- lm(log(Sunday) ~ log(Weekday) + Tabloid.with.a.Serious.Competitor)
par(mfrow=c(2,2))
StanRes1 <- rstandard(m1)
plot(log(Weekday),StanRes1,ylab="Standardized Residuals",xlab="log(Sunday Circulation)")
plot(Tabloid.with.a.Serious.Competitor,StanRes1,ylab="Standardized Residuals")
plot(m1$fitted.values,StanRes1,ylab="Standardized Residuals",xlab="Fitted Values")



# Let's make that plot again, this time with the new model:
par(mfrow=c(1,1), cex=1.3, mar=c(5,5,1,1))
plot(log(Weekday),log(Sunday),xlab="log(Weekday Circulation)",ylab="log(Sunday Circulation)",
     pch=Tabloid.with.a.Serious.Competitor+1,col=Tabloid.with.a.Serious.Competitor+1)
abline(a=-0.44730, b= 1.06133, lwd=1.5)
abline(a=(-0.44730-0.53137), b= 1.06133, lwd=1.5, col="red", lty=2)
legend(11.6, 14.1,legend=c("0","1"),
       pch=1:2, col=1:2, lty=1:2, title="Tabloid dummy variable")


par(mfrow=c(1,1))
plot(m1$fitted.values,log(Sunday),xlab="Fitted Values",ylab="log(Sunday Circulation)")
abline(lsfit(m1$fitted.values,log(Sunday)))

par(mfrow=c(2,2))
plot(m1)
abline(v=2*3/89,lty=2)
# from Matthew Landowski
# add_conservative_cooks_line
# add a line to the Residuals vs Leverage plot for Cook's distance
# uses 4/df to define the line's cutoff
add_conservative_cooks_line <- function(model){
    dc <- 4/model$df.residual 
    h <- seq(0.01, max(hatvalues(model))*1.1, by = 0.01) 
    r <- sqrt(length(model$coefficients) * dc * (1-h)/h)   
    lines(h, r, col ='purple', lty='dotdash') 
    lines(h, -r, col ='purple', lty='dotdash') 
   #mtext(side = 4,las=1,at = c(max(hatvalues(model)), max(r))+1, cex=0.7, text = bquote(.(dc))) 
   #mtext(side = 4,las=1,at = c(max(hatvalues(model)), max(r))-1, cex=0.7, text = bquote(.(dc)))
}
add_conservative_cooks_line(m1)

summary(m1)
