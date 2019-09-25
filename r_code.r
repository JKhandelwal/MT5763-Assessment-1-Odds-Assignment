library("readxl")
library("dplyr")
# Function which reads in and cleans the data
read_clean_data <- function(name){
  data <- read_excel(name)
  filt_data <- data[data$runnerStatus != "REMOVED",]
  # x <- head(as.numeric(as.POSIXct(filt_data$callTime_GMT)))
  # print(typeof(x))
  # print(head(x))
  # filt_data$callTime_GMT <- as.numeric(as.POSIXct(filt_data$callTime_GMT))
  # print(head(data))
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
  plot_odds <- function(df){
    # Give the chart file a name.
    png(file = paste("Back_and_lay_volumes", toString(unique(df$selectionId)),".jpg"))
    # Plot the bar chart.
    plot(df$callTime_GMT, df$bckSz1,type = "o",col = "red", xlab = "Time", ylab = "Volumes",
       main = paste("Back and Lay Odds For Competitor ", toString(unique(df$selectionId))))

    lines(df$callTime_GMT, df$laySz1, type = "o", col = "blue")
    # Save the file.
    dev.off()
  }
}

filtered_data <- read_clean_data("Ascot 4pm 20th June 2018 1 mile handicap.xlsx")

winner_ID <- get_winner(filtered_data)
three_runners <- pick_three(winner_ID, filtered_data)
all_runners <- c(winner_ID, three_runners)
runners_data <- filter_to_three(filtered_data, all_runners)

for (i in (1:4)){
  plot_odds(filter(runners_data, selectionId == all_runners[i] & inplay == "FALSE"))
  plot_volumes(filter(runners_data, selectionId == all_runners[i] & inplay == "FALSE"))
}
