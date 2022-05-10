ms2580 <- c(70.3,465.7,359.0)
ms25160 <- c(70.7,333.0,252.3)
ms3580 <- c(408,274.7,243)
ms35160 <- c(331,311.7,230.7)
ms <- cbind(ms2580,ms25160,ms3580,ms35160)
s <- c(10,25,40)
postscript("u:/meth2/psfiles/Shrimp3wayprofile.ps",horizontal=FALSE)
matplot(s,ms,type="b",xlab="salinity Level %",ylab="Weight Gain (mg)",
        main="Profile Plots of Shrimp Culture Experiment",cex=.75,col="black",
        ylim=c(0,700),lab=c(3,16,5),pch=c("#@$+"),xaxt="n")
axis(side=1,at=c(10,25,40),labels=c(10,25,40))
legend(10,700,pch=c("#@$+"),legend=c("T25D80","T25D160","T35D80","T35D160"))
graphics.off()

