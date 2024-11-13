# Load necessary libraries
library(dplyr)  # for data manipulation
library(readr)  # for reading CSV files
library(data.table)

# Define the main directory path
main_dir <- "/lstr/sahara/mdep/luzhangstat/Risks/data/vxx"

# List all subfolders
subfolders <- list.dirs(main_dir, recursive = FALSE)

# Loop through each subfolder, read the CSV without headers, and combine them
combined_data <- subfolders %>%
  lapply(function(subfolder) {
    # Extract the date from the subfolder name (assuming date is at the start)
    date <- sub("_.*", "", basename(subfolder))
    
    # Look for a CSV file in the subfolder that starts with the date
    csv_file <- list.files(subfolder, pattern = paste0("^", date, "_.*\\.csv$"), full.names = TRUE)
    
    # Check if the file exists
    if (length(csv_file) == 1) {  # Proceed only if exactly one matching file is found
      # Read CSV without headers
      data <- read_csv(csv_file, col_names = FALSE)  # Read data without headers
      
      # Add the extracted date as a new column
      data <- data %>% mutate(date = date)
      
      return(data)
    } else {
      NULL  # Return NULL if the file doesn't exist or multiple files match
    }
  }) %>%
  bind_rows()  # Combine all data frames

# View or save the combined data
dim(combined_data)

fwrite(combined_data, "/lstr/sahara/mdep/luzhangstat/Risks/data/vxx.csv")
