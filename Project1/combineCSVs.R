library(dplyr)

# Function to read and combine two CSV files
combine_csv_files <- function(file1, file2, output_file) {
  # Read the first CSV file
  data1 <- read.csv(file1, stringsAsFactors = FALSE)
  
  # Read the second CSV file
  data2 <- read.csv(file2, stringsAsFactors = FALSE)
  
  # Check if both data frames have the same columns
  if (!identical(colnames(data1), colnames(data2))) {
    stop("The CSV files have different attributes/columns.")
  }
  
  # Combine the data frames by appending rows of the second data frame to the first
  combined_data <- bind_rows(data1, data2)
  
  # Write the combined data frame to a new CSV file
  write.csv(combined_data, output_file, row.names = FALSE)
  
  # Message to indicate completion
  cat("CSV files have been combined and saved to:", output_file)
}

# Example usage
# Make sure to replace "path/to/your/first.csv" and "path/to/your/second.csv" with the actual file paths.
# The "combined.csv" will be created in the current working directory or specify a different path for it.
combine_csv_files("path/to/your/first.csv", "path/to/your/second.csv", "combined.csv")
