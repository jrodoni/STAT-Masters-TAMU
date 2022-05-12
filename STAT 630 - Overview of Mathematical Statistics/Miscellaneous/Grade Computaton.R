# 630 grade computation

stat630 <- function(homework, exam1 = 0, exam2 = 0, final = 0) {
  hw <- homework*2
  hw <- sum(hw)/(length(hw)*100)
  hw <- hw*100*0.20
  ex1 <- exam1*100*0.225
  ex2 <- exam2*100*0.225
  ex3 <- final*100*0.35
  if ((exam1 == 0) && (exam2 == 0) && (final == 0)) {
    score <- hw/20
  }
  else if ((exam1 != 0) && (exam2 == 0) && (final == 0)) {
    score <- (hw+ex1)/(20+22.5)
  }
  else if ((exam1 != 0) && (exam2 != 0) && (final == 0)) {
    score <- (hw+ex1+ex2)/(20+22.5+22.5)
  }
  else if ((exam1 != 0) && (exam2 != 0) && (final != 0)) {
    if (final > exam1){
      ex1 = final*100*0.225
      score = (hw+ex1+ex2+ex3)/100
    }else{
      score <- (hw+ex1+ex2+ex3)/100
    }
  
  }
  score <- score*100
  if (score >= 85) {
    grade <- "A"
  }
  else if ((score >= 70) && (score < 85)) {
    grade <- "B"
  }
  else if ((score >= 60) && (score < 70)) {
    grade <- "C"
  }
  else if ((score >= 50) && (score < 60)) {
    grade <- "D"
  }
  else {
    grade <- "F"
  }
  cat(score, " (", grade, ")", sep = "")
}


hw = c(49,50,48,50,48,45,50,48,49,50,40)

exam1 = 47/60
exam2= 41/50
final = 15/100

stat630(homework=hw,exam1=exam1,exam2 = exam2, final)












