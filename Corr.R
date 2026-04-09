source('complete.R')

corr <- function(directory, threshold=0){
      
      # Initializing the correlation values
      correlation <- numeric()
      
      # Calculating id's that meet the threshold
      total_pm_values <- complete(directory)
      nobs <- total_pm_values['nobs']
      total_monitors <- 1:332
      id <- total_monitors[nobs > threshold]
     
      # If no monitors meet the threshold
      if (length(id) == 0){
            return(correlation)
      }
      
      for (i in id){
            
            # Determining the file name
            if (i < 10){
                  preceeding_zeros <- "00"
            } else if (i < 100){
                  preceeding_zeros <- "0"
            } else {
                  preceeding_zeros <-""
            }
            filename <- paste(directory, "/", preceeding_zeros, i, '.csv', sep="")
            
            # Reading the data from the file
            current_data <-read.csv(filename)
            filtered_data <- current_data[complete.cases(current_data),]
            
            # Calculating the correlation
            test <- cor(filtered_data[,'sulfate'], filtered_data[,'nitrate'])
            
            correlation <- c(correlation, test)
      }
      return(correlation)
}

