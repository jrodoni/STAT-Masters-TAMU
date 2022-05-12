############################# STAT 608 Assignment 4 ############################
# 7.) 
# (a)
A = matrix(c(.5,.5,-1))
x = matrix(c(rep(c(1,0),times=c(4,8)),
             rep(c(0,1,0),times = c(4,4,4)),
             rep(c(0,1),times = c(8,4))),nrow = 12,ncol = 3,byrow = FALSE)


xtxInv = solve(t(x)%*%x)
A_xtxInv_At_Inv = solve(t(A)%*%xtxInv%*%A)
A_xtxInv_At_Inv

# (b)
B = matrix(c(5.6,7.9,6.1))

AB = t(A)%*%B

(AB^2)*A_xtxInv_At_Inv*9/12.8
