library("readxl")
library("writexl")
library("dplyr")
library("reshape2")
library("tidyr")

# Function which read and cleans the data in Excel, however,
# this wrecked havoc on the whole system, but has been preserved for Posterity
read_clean_data <- function(name){
  data <- read_excel(name)
  filt <- filter(data, selectionId == 11473056)
  # remove removed rows
  filt_data <- data[data$runnerStatus != "REMOVED",]
  filt_data$seconds_time <- as.integer(as.POSIXct(filt_data$callTime_GMT))
  filt_data$seconds_time <- filt_data$seconds_time - filt_data$seconds_time[1]
  return(filt_data)
}

# Proper function which reads in data in the proper format
read_clean_csv <- function(name){
  data <- read.csv(name, header=TRUE, sep=",",)

  filt <- filter(data, selectionId == 11473056)

  filt_data <- data[data$runnerStatus != "REMOVED",]
  filt_data <- distinct(filt_data)

  filt_data <- numerify(filt_data)

  filt_data <- filt_data %>% group_by(callTime_GMT, marketStatus, inplay, selectionId, runnerStatus) %>% summarize(layPrc1 = min(layPrc1), bckPrc1 = max(bckPrc1), laySz1 = mean(laySz1), bckSz1 = mean(bckSz1)) %>% arrange()

  # Write the Data back out, so that the same filtered data can be used in SAS
  write.csv(filt_data, file="grouped_data.csv", row.names=FALSE, na="")
  filt_data <- read.csv("grouped_data.csv", header=TRUE, sep=",",)

  filt_data$seconds_time <- as.integer(as.POSIXct(filt_data$callTime_GMT, format="%d/%m/%Y %H:%M:%S"))
  filt_data$seconds_time <- filt_data$seconds_time - filt_data$seconds_time[1]
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
  # runners <- df[df$selectionId != toString(winner_ID),]
  # filt_runner <- unique(runners["selectionId"])
  # filt <- filt_runner$selectionId
  # return(sample(filt, 3, replace=TRUE))

  return(c(8528919, 12452848, 11295113))
  # This is a pipe %>%
}

filter_to_three <- function(df, vector){
  return(df[df$selectionId %in% vector,])
}

plot_odds <- function(df){
  # Give the chart file a name.
  png(file = paste("Back_and_lay_odds", toString(unique(df$selectionId)),".jpg"))
  # Plot the bar chart.
  plot(df$seconds_time, df$bckPrc1,type = "o",col = "red", xlab = "Time", ylab = "Odds",
     main = paste("Back and Lay Odds For Competitor ", toString(unique(df$selectionId))))
  lines(df$seconds_time, df$layPrc1, type = "o", col = "blue")
  # Save the file.
  dev.off()
}

plot_volumes <- function(df){
    # Give the chart file a name.
    png(file = paste("Back_and_lay_volumes", toString(unique(df$selectionId)),".jpg"))
    # Plot the bar chart.
    plot(df$seconds_time, df$bckSz1,type = "o",col = "red", xlab = "Time", ylab = "Volumes",
       main = paste("Back and Lay Volumes For Competitor ", toString(unique(df$selectionId))))
    lines(df$seconds_time, df$laySz1, type = "o", col = "blue")
    # Save the file.
    dev.off()
}

mean_variance <- function(df){
  mean_lay <- mean(df$layPrc1)
  mean_back <- mean(df$bckPrc1)
  var_lay <- var(df$layPrc1)
  var_back <- var(df$bckPrc1)
  return(c(mean_back, mean_lay, var_back, var_lay))
}

plot_distribution <- function(df){
  png(file = paste("Backing Distribution from 5 minutes before to start", toString(unique(df$selectionId)),".jpg"))
  hist(df$bckPrc1, main = paste("Backing Distribution from 5 minutes before to start", toString(unique(df$selectionId))))
  dev.off()
}

build_and_merge <-function(runners, winner_id, all_runner_data){
  df <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("selectionId", "Win/Loss"))
  for (i in (1:4)){
    if (runners[i] == winner_ID){
      df[nrow(df) + 1,] <- c(runners[i], "WINNER")
    } else {
      df[nrow(df) + 1,] <- c(runners[i], "LOSER")
    }
  }

  print("Winner Loser Table")
  print(df)

  merged <- merge(all_runner_data, df,by="selectionId")
  return(merged)
}

numerify <- function(filt_runner_data){
  filt_runner_data$layPrc1 <- as.numeric(filt_runner_data$layPrc1)
  filt_runner_data$laySz1 <- as.numeric(filt_runner_data$laySz1)
  filt_runner_data$bckPrc1 <- as.numeric(filt_runner_data$bckPrc1)
  filt_runner_data$bckSz1 <- as.numeric(filt_runner_data$bckSz1)

  return(filt_runner_data)
}

reshape <- function(df){
  bck_prc_spread <- df %>%  select(selectionId, callTime_GMT, bckPrc1) %>%
              spread(key = selectionId, value = bckPrc1)

  lay_prc_spread <- df %>%  select(selectionId, callTime_GMT, layPrc1) %>%
              spread(key = selectionId, value = layPrc1)

  bck_sz_spread <- df %>%  select(selectionId, callTime_GMT, bckSz1) %>%
              spread(key = selectionId, value = bckSz1)

  lay_sz_spread <- df %>%  select(selectionId, callTime_GMT, laySz1) %>%
              spread(key = selectionId, value = laySz1)

  sheets <- list("bckprc" = bck_prc_spread, "layprc" = lay_prc_spread, "bcksz" = bck_sz_spread, "laysz" = lay_sz_spread)
  write_xlsx(sheets, "ascot_wide_data.xlsx")
}

get_start_time <- function(filtered_data){
  as.numeric(filtered_data[min(which(filtered_data$inplay == TRUE)), "seconds_time"])
}

arbitrage <- function(df){
  max_profit <- 0
  bcktime <- NULL
  laytime <- NULL
  bck_row <- 0
  lay_row <- 0
  final_volume <- 0
  for (i in 1:nrow(df)){
    if (!is.null(df$bckSz1[i]) && !is.na(df$bckSz1[i])){
      for (j in 1:nrow(df)){
        if (!is.null(df$laySz1[j]) && !is.na(df$laySz1[j])){
          volume <- min(df$bckSz1[i],df$laySz1[j])
           prof <- ((df$bckPrc1[i] - 1) * volume) - ((df$layPrc1[j] - 1) * volume)
            if (prof > max_profit){
              bcktime <- df$callTime_GMT[i]
              laytime <- df$callTime_GMT[j]
              bck_row <- i
              lay_row <- j
              max_profit <- prof
              final_volume <- volume
            }
        }
      }
    }
  }
  #
  # print(df[bck_row, c(1,6,7)])
  # print(df[lay_row, c(1,8,9)])
  return(paste(max_profit," ",bcktime, " ", laytime, " ", df[bck_row, 6] , " ", df[lay_row, 8], " ", final_volume))
}

filtered_data <- read_clean_csv("Ascot_csv.csv")

# filtered_data <- read_clean_data("Ascot 4pm 20th June 2018 1 mile handicap.xlsx")
start_time <- get_start_time(filtered_data)

winner_ID <- get_winner(filtered_data)
three_runners <- pick_three(winner_ID, filtered_data)

filt_runner_list <- c(winner_ID, three_runners)
filt_runner_data <- filter_to_three(filtered_data, filt_runner_list)
filt_runner_data <- numerify(filt_runner_data)

for (i in (1:4)){
  print("--------------------")
  print(paste("Competitor ", toString(filt_runner_list[i])))
  print("--------------------")
#   # !±!!!!!!!!!!!!!!!!!!!!!!! TODO UNCOMMENT ALL PLOTS
  plot_odds(filter(filt_runner_data, selectionId == filt_runner_list[i] & inplay == "FALSE" & marketStatus != "SUSPENDED"))
  plot_volumes(filter(filt_runner_data, selectionId == filt_runner_list[i] & inplay == "FALSE" & marketStatus != "SUSPENDED"))
#
  up_to_fifteen <- mean_variance(filter(filt_runner_data, marketStatus != "SUSPENDED" & selectionId == filt_runner_list[i] & seconds_time < (start_time - 15*60)))
  fifteen_to_five <- mean_variance(filter(filt_runner_data, marketStatus != "SUSPENDED" &  selectionId == filt_runner_list[i] & seconds_time >= (start_time - 15*60) & seconds_time < (start_time - 5*60)))
  five_to_start <- mean_variance(filter(filt_runner_data, marketStatus != "SUSPENDED" &  selectionId == filt_runner_list[i] & seconds_time >= (start_time - 5*60) & seconds_time < start_time ))
#
  mean_variance_df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("Mean_Backing","Mean_Lay", "Variance_Backing", "Variance_Lay"))
  mean_variance_df[nrow(mean_variance_df) + 1,] <- up_to_fifteen
  mean_variance_df[nrow(mean_variance_df) + 1,] <- fifteen_to_five
  mean_variance_df[nrow(mean_variance_df) + 1,] <- five_to_start

  # Print out the Mean_Variance
  print(mean_variance_df)

  plot_distribution(filter(filt_runner_data, marketStatus != "SUSPENDED" &  selectionId == filt_runner_list[i] & seconds_time >= (start_time - 5*60) & seconds_time < start_time))
}

merged <- build_and_merge(filt_runner_list, winner_ID, filt_runner_data)
print("Merged Data")
print(head(merged))

# Arbitrage
print("Arbitrage Position")
print(arbitrage(filter(filt_runner_data, selectionId == winner_ID & inplay == FALSE)))

# Excel Long Wide
print("Reshaping Data")
reshape(filter(filt_runner_data, selectionId %in% filt_runner_list & marketStatus != "SUSPENDED"))
