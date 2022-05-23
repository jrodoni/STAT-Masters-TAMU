
run = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14,
15,15,16,16,17,17,18,18)
Res = c(
        136.75, 138.75,
        137.75, 139.75,
        136.25, 138.75,
        146.75, 148.75,
        149.25, 145.25,
        146.75, 148.25,
        147.25, 149.25,
        150.75, 148.75,
        146.25, 148.75,
        136.65, 138.85,
        137.55, 139.55,
        140.45, 137.95,
        166.75, 168.25,
        157.25, 161.25,
        162.25, 165.25,
        122.75, 124.75,
        124.25, 127.25,
        118.75, 120.25)
spec = seq(1,6)

# lower and upper bounds for the rectangles
lb_rec_1 = min(Res[1:6]) - 2
ub_rec_1 = max(Res[1:6]) + 2

lb_rec_2 = min(Res[7:12]) - 2
ub_rec_2 = max(Res[7:12]) + 2

lb_rec_3 = min(Res[13:18]) - 2
ub_rec_3 = max(Res[13:18]) + 2

lb_rec_4 = min(Res[19:24]) - 2
ub_rec_4 = max(Res[19:24]) + 2

lb_rec_5 = min(Res[25:30]) - 2
ub_rec_5 = max(Res[25:30]) + 2

lb_rec_6 = min(Res[31:36]) - 2
ub_rec_6 = max(Res[31:36]) + 2


plot(run,Res,type="p",xlab="Operators",ylab="Chemical Level",
        main="Figure 3: Chemical Variation Plot ",cex=.99,
        ylim=c(80,200),xaxt="n")
rect(0.75,lb_rec_1,3.25,ub_rec_1)
segments(1,min(Res[1:2]),1,max(Res[1:2]))
segments(2,min(Res[3:4]),2,max(Res[3:4]))
segments(3,min(Res[5:6]),3,max(Res[5:6]))
text(1,lb_rec_1-2,"R1",cex=.55)
text(2,lb_rec_1-2,"R2",cex=.55)
text(3,lb_rec_1-2,"R3",cex=.55)
text(2,lb_rec_1-6,"SPEC1",cex=.75)

rect(3.75,lb_rec_2,6.25,ub_rec_2)
segments(4,min(Res[7:8]),4,max(Res[7:8]))
segments(5,min(Res[9:10]),5,max(Res[9:10]))
segments(6,min(Res[11:12]),6,max(Res[11:12]))
text(4,lb_rec_2-2,"R4",cex=.55)
text(5,lb_rec_2-2,"R5",cex=.55)
text(6,lb_rec_2-2,"R6",cex=.55)
text(5,lb_rec_2-6,"SPEC2",cex=.75)

rect(6.75,lb_rec_3,9.25,ub_rec_3)
segments(7,min(Res[13:14]),7,max(Res[13:14]))
segments(8,min(Res[15:16]),8,max(Res[15:16]))
segments(9,min(Res[17:18]),9,max(Res[17:18]))
text(7,lb_rec_3-2,"R7",cex=.55)
text(8,lb_rec_3-2,"R8",cex=.55)
text(9,lb_rec_3-2,"R9",cex=.55)
text(8,lb_rec_3-6,"SPEC3",cex=.75)

rect(9.75,lb_rec_4,12.25,ub_rec_4)
segments(10,min(Res[19:20]),10,max(Res[19:20]))
segments(11,min(Res[21:22]),11,max(Res[21:22]))
segments(12,min(Res[23:24]),12,max(Res[23:24]))
text(10,lb_rec_4-2,"R10",cex=.55)
text(11,lb_rec_4-2,"R11",cex=.55)
text(12,lb_rec_4-2,"R12",cex=.55)
text(11,lb_rec_4-6,"SPEC4",cex=.75)

rect(12.75,lb_rec_5,15.25,ub_rec_5)
segments(13,min(Res[25:26]),13,max(Res[25:26]))
segments(14,min(Res[27:28]),14,max(Res[27:28]))
segments(15,min(Res[29:30]),15,max(Res[29:30]))
text(13,lb_rec_5-2,"R13",cex=.55)
text(14,lb_rec_5-2,"R14",cex=.55)
text(15,lb_rec_5-2,"R15",cex=.55)
text(14,lb_rec_5-6,"SPEC5",cex=.75)

rect(15.75,lb_rec_6,18.25,ub_rec_6)
segments(16,min(Res[31:32]),16,max(Res[31:32]))
segments(17,min(Res[33:34]),17,max(Res[33:34]))
segments(18,min(Res[35:36]),18,max(Res[35:36]))
text(16,lb_rec_6-2,"R16",cex=.55)
text(17,lb_rec_6-2,"R17",cex=.55)
text(18,lb_rec_6-2,"R18",cex=.55)
text(17,lb_rec_6-6,"SPEC6",cex=.75)


axis(side=1,at=c(3.5,9.5,15.5),labels=c("Operator 1","Operator 2","Operator 3"))
segments(0,mean(Res),19,mean(Res))
text(12,mean(Res)+2.5,"mean of 36 chemical analyses")



