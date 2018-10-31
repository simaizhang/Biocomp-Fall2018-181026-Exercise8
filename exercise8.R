# Simai Zhang / Exercise8
#1 
setwd("~/bio/exercise/Biocomp-Fall2018-181026-Exercise8/")
result=read.table("UWvMSU_1-22-13.txt", header = T, stringsAsFactors = F)

# UW score
UW=result[result$team=="UW",]
oldUW=UW[,3] # select the score results of UW
cumUW=c(1:length(oldUW)) # creat a new vector to contain the cumlative score of UW
# count the cumlative score
for (i in 1:length(oldUW)) {
  cumUW[i]=0
  for (n in 1:i) {
    cumUW[i]=cumUW[i]+oldMSU[n]
  }
  i = i + 1
}
cumUW # test the cumlative scores
# MSU score
MSU=result[result$team=="MSU",]
oldMSU=MSU[,3] # select the score results of MSU
cumMSU=c(1:length(oldMSU)) # creat a new vector to contain the cumlative score of MSU
# count the cumlative score 
for (i in 1:length(oldMSU)) {
  cumMSU[i]=0
  for (n in 1:i) {
    cumMSU[i]=cumMSU[i]+oldMSU[n]
  }
  i = i + 1
}
cumMSU # test the cumlative scores

timeUW=UW[,1] 
timeMSU=MSU[,1]

# draw two lines in one plot
plot(timeUW, cumUW,type = "l",xlab="time", ylab="score", main="UW vs MSU Cumlative Scores over Time")
legend("topleft", c("UW","MSU"), pch=16, col=c("black","blue"))
par(new=TRUE)
plot(timeMSU, cumMSU, type = "l", axes = FALSE, xlab="",ylab="",col="blue")
lines(timeMSU, cumMSU,col="blue")
--------------------------------------------------------------------------

#2 
game = function(){ # define the function, with no argument
  print("I'm thinking of a number 1-100...")
  n=sample(1:100)[1] # get the first random number from 1 to 100
  guess=readline(prompt = "Guess: " ) # get user's input
# guess <- as.integer(guess)
  while (guess!=n) {
    if (guess<n)  # if the guess was lower
      {print("Lower")
      guess=readline(prompt = "Guess: ")
      guess <- as.integer(guess)}
    else if (guess>n)   # if the guess was higher
      {print("Higher")
      guess=readline(prompt = "Guess: ")
      guess <- as.integer(guess)}
    if (guess==n)   # if the guess was correct
    {print("Correct!")}
  }
}
game() # test the code




