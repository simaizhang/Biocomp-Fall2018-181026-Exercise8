#1 
setwd("~/bio/exercise/Biocomp-Fall2018-181026-Exercise8/")
result=read.table("UWvMSU_1-22-13.txt", header = T, stringsAsFactors = F)

# UW score
UW=result[result$team=="UW",]
oldUW=UW[,3] # select the score results of UW
accumUW=c(1:length(oldUW)) # creat a new vector to contain the accmulative score of UW
# count the accumlative score
for (i in 1:length(oldUW)) {
  accumUW[i]=0
  for (n in 1:i) {
    accumUW[i]=acccumUW[i]+accum[n]
  }
  i = i + 1
}

# MSU score
MSU=result[result$team=="MSU",]
oldMSU=MSU[,3] # select the score results of MSU
accumMSU=c(1:length(oldMSU)) # creat a new vector to contain the accumlative score of MSU
# count the accumlative score 
for (i in 1:length(oldMSU)) {
  accumMSU[i]=0
  for (n in 1:i) {
    accumMSU[i]=accumMSU[i]+oldMSU[n]
  }
  i = i + 1
}
timeUW=UW[,1] 
timeMSU=MSU[,1]

# draw two lines in one plot
plot(timeUW, accumUW,type = "l",xlab="time", ylab="score")
par(new=TRUE)
plot(timeMSU, accumMSU, type = "l", axes = FALSE, xlab="",ylab="",col="blue")

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




