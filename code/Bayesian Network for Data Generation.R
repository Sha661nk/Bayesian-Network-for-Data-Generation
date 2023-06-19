# Install and load the required packages
library(dplyr)  # Package for data manipulation and transformation
library(readr)  # Package for reading and parsing data
library(bnlearn)  # Package for Bayesian network learning and inference

# Function for label encoding categorical variables
label_encoder <- function(df) {
  le_dict <- list()  # Dictionary to store label encodings
  encoded_df <- df  # Create a copy of the input dataframe
  
  for (col in names(df)) {
    if (is.character(df[[col]])) {  # Check if the column is of character type
      levels <- unique(df[[col]])  # Get unique levels/categories in the column
      encoded_df[[col]] <- as.integer(factor(df[[col]], levels = levels))  # Convert the column to integer codes
      le_dict[[col]] <- levels  # Store the mapping of levels for decoding
    }
  }
  
  return(list(encoded_df, le_dict))  # Return the encoded dataframe and the label encoding dictionary
}

# Function for label decoding encoded variables
label_decoder <- function(encoded_df, le_dict) {
  decoded_df <- encoded_df  # Create a copy of the encoded dataframe
  
  for (col in names(encoded_df)) {
    if (col %in% names(le_dict)) {  # Check if the column has label encoding mapping
      levels <- le_dict[[col]]  # Get the levels for decoding
      decoded_df[[col]] <- levels[encoded_df[[col]]]  # Decode the column using the mapping
    }
  }
  
  return(decoded_df)  # Return the decoded dataframe
}

# Function to convert integer to factor
convert_integer_to_factor <- function(x) {
  if (is.integer(x) && length(unique(x)) > 1) {  # Check if the input is an integer and has more than one unique value
    return(as.factor(x))  # Convert to factor if the conditions are met
  } else {
    return(as.numeric(x))  # Otherwise, convert to numeric
  }
}

#-------------------------------  Hott Gauss 3  ----------------------------------------------------

# Define input & output directory paths
output_path = '--- output path ----'
path = '--- Input Path ----'

# File Name Vector
file_names <- c("sub0", "sub1", "sub2", "sub3", "sub4", "sub5", "sub6", "sub7", "sub8", "sub9")
output_dir_path = output_path

# Read training data in a loop and write data in the output directory
for (file_name in file_names) {
  file_path <- paste(path, file_name, '.csv', sep="")  # Create the file path
  data <- read.csv(file_path)  # Read the CSV file
  
  data <- subset(data, select=-ind)  # Remove the 'ind' column from the data
  
  bn <- hc(data)  # Learn the Bayesian network structure from the data
  fitted_bn <- bn.fit(bn, data)  # Fit the Bayesian network to the data
  synthetic_data <- rbn(fitted_bn, n = nrow(data))  # Generate synthetic data using the fitted Bayesian network
  
  write.csv(synthetic_data, paste0(output_dir_path, file_name, ".csv"), row.names = FALSE)  # Write the synthetic data to a CSV file
}

# -------------------------------  Merge Gauss 3 ----------------------------------------------------

output_dir_path = '--- output path ----'
path = '--- Input Path ----'

file_names <- c("combined")

input_dir_path = paste0(path, 'merge/')
file_path <- paste(merge_dir_path, filename, '.csv', sep="")
data <- read.csv(file_path)

data$X6 <- as.factor(data$X6)  # Convert a specific column (X6) to a factor

bn <- hc(data)  # Learn the Bayesian network structure from the data
fitted_bn <- bn.fit(bn, data)  # Fit the Bayesian network to the data
synthetic_data <- rbn(fitted_bn, n = nrow(data))  # Generate synthetic data using the fitted Bayesian network

write.csv(synthetic_data, paste0(output_dir_path, file_name, ".csv"), row.names = FALSE)  # Write the synthetic data to a CSV file

