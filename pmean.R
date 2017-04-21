pmean <- function(directory, pollutant, id=1:332){
  # Create a list of filenames with the specified id's
  filenames <- sprintf("%03d.csv", id)
  # Create a list of valid path's to the files with data
  filenames <- file.path(directory, filenames)
  # Read the data from every file, creating a list of data frames
  ldf <- lapply(filenames, read.csv)
  # Merge all data frames from the list into one big_frame
  big_frame <- do.call("rbind", ldf)
  # Find the mean for specified pollutant
  mean(big_frame[[pollutant]], na.rm = TRUE)
}

complete <- function(directory, id){
  # Create a list of filenames with the specified id's
  filenames <- sprintf("%03d.csv", id)
  # Create a list of valid path's to the files with data
  filenames <- file.path(directory, filenames)
  # Read the data from every file, creating a list of data frames
  ldf <- lapply(filenames, read.csv)
  # Find complete cases for every file
  cc <- lapply(ldf, complete.cases)
  # Find the sum of complete cases in every file
  cc_sum <- lapply(cc, sum)
  # Create resulting data frame
  data.frame(id = id, nobs = I(cc_sum))
}

corr <- function(directory, threshold=0){
  # Get path for every *.csv file in specified directory
  filenames <- list.files(directory, pattern="*.csv", full.names=TRUE)
  # Read the data from every file, creating a list of data frames
  ldf <- lapply(filenames, read.csv)
  # Initialize an empty vector 
  res <- c()
  # For every dataframe in ldf
  for (df in ldf){
    # If the number of complete cases in the df is > threshold
    if (sum(complete.cases(df)) > threshold){
      # Append the sulfate/nitrate corelation to the resulting vector
      res<-c(res, cor(df[['sulfate']], df[['nitrate']], use='pairwise.complete.obs'))
    }
  }
  return(res)
}