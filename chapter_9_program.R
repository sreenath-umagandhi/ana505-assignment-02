# Game-day Simulator for Baseball (R)
library(lattice)  # graphics package for probability matrix visual
simulator <- function(home_mean,away_mean,niterations) { ## home game mean, away game mean and number of iterations are passed as arguments to this function.
     # input runs scored means, output probability of winning for home team
     set.seed(1234)  # set to obtain reproducible results ## this means that the random numbers generated for seed 1234 remains the same no matter where this code is executed.
     away_game_score <- numeric(niterations) ## away_game_score is set to be numeric
     home.game.score <- numeric(niterations) ## home.game.score is set to be numeric
     home_win <- numeric(niterations) ## home_win is set to be numeric
     i <- 1 ## i is initialized to be 1
     while (i < niterations + 1) { ## running while loop until the condition i less than number of iterations + 1 times is true
         away_game_score[i] <- rnbinom(1,mu=away_mean, size = 4) ## rnbinom function is used to calculate random density for negative binomial distribution
         home.game.score[i] <- rnbinom(1,mu=home_mean, size = 4)
         if(away_game_score[i] > home.game.score[i]) home_win[i] <- 1 ## if the i-th iteration of away_game_Score is greater than home.game.score then set i-th iteration of home_win to be 1.
         if(away_game_score[i] > home.game.score[i] || 
         away_game_score[i] < home.game.score[i]) i <- i + 1 ## if the i-th iteration of away_game_Score is greater than home.game.score or if i-th iteration of away_game_Score is lesser than home.game.score then increment i by 1
         }
     n_home_win <- sum(home_win) ## sum of home_win is set to n_home_win
     n_home_win/niterations  # return probability of away team winning 
     } 

niterations <- 1000  # use smaller number for testing ## number of iterations is set to 1000
# probability matrix for results... home team is rows, away team is columns
probmat <- matrix(data = NA, nrow = 9, ncol = 9,  ## number of rows and columns set to 9
  dimnames = list(c(as.character(1:9)), c(as.character(1:9)))) ## naming the rows and columns
for (index_home in 1:9) ## for loop with index_home between 1 to 9
for (index_away in 1:9) ## for loop with index_away between 1 to 9
if (index_home != index_away) {
     probmat[index_home,index_away] <- 
        simulator(index_home, index_away, niterations) 
     } ## if index_home is not equal to index_away, then set the probability matrix with simulator values for row and column
pdf(file = "fig_sports_analytics_prob_matrix.pdf", width = 8.5, height = 8.5) ## export the matrix to this file with height and width as 8.5
x <- rep(1:nrow(probmat),times=ncol(probmat)) ## replicate the number of rows and number of columns as assign it to x
y <- NULL 
for (i in 1:ncol(probmat)) y <- c(y,rep(i,times=nrow(probmat))) ## for i between 1 to number of columns in probability matrix, assign the i-th iteration value of number of rows of probability matrix for y.
probtext <- sprintf("%0.3f", as.numeric(probmat))  # fixed format 0.XXX
text_data_frame <- data.frame(x, y, probtext) ## assign text_data_frame with values of x,y and probability text
text_data_frame$probtext <- as.character(text_data_frame$probtext) # returns the string of 1's and 0's or a character vector of features
text_data_frame$probtext <- ifelse((text_data_frame$probtext == "NA"), 
    NA,text_data_frame$probtext)  # define diagonal cells as missing ## if text_data_frame$probtext is NA, then NA, else text_data_frame$probtext
text_data_frame <- na.omit(text_data_frame)  # diagonal cells
print(levelplot(probmat, cuts = 25, tick.number = 9,
    col.regions=colorRampPalette(c("violet", "white", "light blue")),
    xlab = "Visiting Team Runs Expected", 
    ylab = "Home Team Runs Expected",
    panel = function(...) {
        panel.levelplot(...)  
        panel.text(text_data_frame$x, text_data_frame$y, 
        labels = text_data_frame$probtext)
        })) ## plot the matrix using levelplot function
dev.off()        
# Suggestion for the student: Develop simulators for football or basketball.    

