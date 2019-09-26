library("readxl")
library("dplyr")
# Function which reads in and cleans the data
read_clean_data <- function(name){
  data <- read_excel(name)
  # remove suspended and removed rows, because the rows
  filt_data <- data[data$runnerStatus != "REMOVED",]
  # filt_data$callTime_GMT <- as.numeric(format(filt_data$callTime_GMT, "%H:%M:%S"))
  filt_data$callTime_GMT <- as.numeric(as.POSIXct(filt_data$callTime_GMT))
  filt_data$callTime_GMT <- filt_data$callTime_GMT - filt_data$callTime_GMT[1]
  return(filt_data)
}


get_winner <- function(df){
  # Filter the winner_ID
  # https://stackoverflow.com/questions/7381455/filtering-a-data-frame-by-values-in-a-column
  winner <- df[df$runnerStatus == "WINNER",]
  winner_ID <- unique(winner["selectionId"])$selectionId
  return(winner_ID)
}


pick_three <- function(winner_ID, df){
  runners <- df[df$selectionId != toString(winner_ID),]
  filt_runner <- unique(runners["selectionId"])
  filt <- filt_runner$selectionId
  return(sample(filt, 3, replace=TRUE))
  # This is a pipe %>%
}

filter_to_three <- function(df, vector){
  return(df[df$selectionId %in% vector,])
}


plot_odds <- function(df){
  # Give the chart file a name.
  png(file = paste("Back_and_lay_odds", toString(unique(df$selectionId)),".jpg"))
  # Plot the bar chart.
  plot(df$callTime_GMT, df$bckPrc1,type = "o",col = "red", xlab = "Time", ylab = "Odds",
     main = paste("Back and Lay Odds For Competitor ", toString(unique(df$selectionId))))
  lines(df$callTime_GMT, df$layPrc1, type = "o", col = "blue")
  # Save the file.
  dev.off()
}

plot_volumes <- function(df){
    # Give the chart file a name.
    png(file = paste("Back_and_lay_volumes", toString(unique(df$selectionId)),".jpg"))
    # Plot the bar chart.
    plot(df$callTime_GMT, df$bckSz1,type = "o",col = "red", xlab = "Time", ylab = "Volumes",
       main = paste("Back and Lay Volumes For Competitor ", toString(unique(df$selectionId))))
    lines(df$callTime_GMT, df$laySz1, type = "o", col = "blue")
    # Save the file.
    dev.off()
}

mean_variance <- function(df){
  mean_lay <- mean(df$layPrc1)
  mean_back <- mean(df$bckPrc1)
  var_lay <- var(df$laySz1)
  var_back <- var(df$bckSz1)
  print(c(mean_lay, mean_back, var_lay, var_back))
  return(c(mean_lay, mean_back, var_lay, var_back))
}

plot_distribution <- function(df){
  png(file = paste("Backing Distribution for time period up to 5 minutes before", toString(unique(df$selectionId)),".jpg"))
  hist(df$bckPrc1)
  dev.off()
}

filtered_data <- read_clean_data("Ascot 4pm 20th June 2018 1 mile handicap.xlsx")

start_time <- as.numeric(filtered_data[min(which(filtered_data$inplay == TRUE)), "callTime_GMT"])

winner_ID <- get_winner(filtered_data)
three_runners <- pick_three(winner_ID, filtered_data)
all_runners <- c(winner_ID, three_runners)
runners_data <- filter_to_three(filtered_data, all_runners)
# Ask about Type Coercion
runners_data$layPrc1 <- as.numeric(runners_data$layPrc1)
runners_data$laySz1 <- as.numeric(runners_data$laySz1)
runners_data$bckPrc1 <- as.numeric(runners_data$bckPrc1)
runners_data$bckSz1 <- as.numeric(runners_data$bckSz1)

for (i in (1:4)){
  print("--------------------")
  print(paste("Competitor ", toString(all_runners[i])))
  print("--------------------")
  # !±!!!!!!!!!!!!!!!!!!!!!!! UNCOMMENT
  # plot_odds(filter(runners_data, selectionId == all_runners[i] & inplay == "FALSE"))
  # plot_volumes(filter(runners_data, selectionId == all_runners[i] & inplay == "FALSE"))

  up_to_fifteen = mean_variance(filter(runners_data, marketStatus != "SUSPENDED" & selectionId == all_runners[i] & callTime_GMT < (start_time - 15*60)))
  fifteen_to_five = mean_variance(filter(runners_data, marketStatus != "SUSPENDED" &  selectionId == all_runners[i] & callTime_GMT >= (start_time - 15*60) & callTime_GMT < (start_time - 5*60)))
  five_to_start = mean_variance(filter(runners_data, marketStatus != "SUSPENDED" &  selectionId == all_runners[i] & callTime_GMT >= (start_time - 5*60) & callTime_GMT < start_time ))

  mean_variance_df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("Mean_Lay_Odds", "Mean_Backing_odds", "Variance_Lay_Odds", "Variance_Backing_Odds"))
  mean_variance_df[nrow(mean_variance_df) + 1,] = up_to_fifteen
  mean_variance_df[nrow(mean_variance_df) + 1,] = fifteen_to_five
  mean_variance_df[nrow(mean_variance_df) + 1,] = five_to_start

  print(mean_variance_df)

  plot_distribution(filter(runners_data, marketStatus != "SUSPENDED" &  selectionId == all_runners[i] & callTime_GMT >= (start_time - 5*60) & callTime_GMT < start_time))
}
