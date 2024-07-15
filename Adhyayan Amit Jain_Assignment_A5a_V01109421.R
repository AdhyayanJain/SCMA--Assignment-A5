# Set the working directory and verify it
setwd('D:\\#YPR\\VCU\\Summer Courses\\SCMA\\Assignments\\A5')
cat("Current Working Directory: ", getwd(), "\n")

# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA", "sf", "geojsonio")
lapply(libraries, install_and_load)

# Reading the dataset
cat("Reading the dataset 'NSSO68.csv'\n")
data <- read.csv("NSSO68.csv")

# Filtering the dataset for Madhya Pradesh (MP)
cat("Filtering the dataset for Madhya Pradesh (MP)\n")
df <- data %>%
  filter(state_1 == "MP")

# Display dataset information
cat("Dataset Information:\n")
cat("Column Names:\n", paste(names(df), collapse = ", "), "\n")
cat("First few rows of the dataset:\n")
print(head(df))
cat("Dimensions of the dataset: ", dim(df), "\n")

# Finding missing values
cat("Finding missing values in the dataset\n")
missing_info <- colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)

# Subsetting the data for analysis
cat("Subsetting the data for analysis\n")
mpnew <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Function to impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}

# Impute missing values for 'Meals_At_Home'
cat("Imputing missing values for 'Meals_At_Home'\n")
mpnew$Meals_At_Home <- impute_with_mean(mpnew$Meals_At_Home)

# Function to remove outliers from a dataset column
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

# Remove outliers from specific columns
cat("Removing outliers from 'ricepds_v' and 'chicken_q'\n")
outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  mpnew <- remove_outliers(mpnew, col)
}

# Summarize total consumption
cat("Summarizing total consumption\n")
mpnew$total_consumption <- rowSums(mpnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

# Function to summarize and display top consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- mpnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

# Summarize consumption by district and region
cat("Summarizing consumption by district and region\n")
district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

# Display top consuming districts and region consumption summary
cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)

# Mapping district and sector codes to their names
cat("Mapping district and sector codes to their names\n")
district_mapping <- c(
  "1" = "Sheopur", "2" = "Morena", "3" = "Bhind", "4" = "Gwalior",
  "5" = "Datia", "6" = "Shivpuri", "7" = "Guna", "8" = "Tikamgarh",
  "9" = "Chhatarpur", "10" = "Panna", "11" = "Sagar", "12" = "Damoh",
  "13" = "Satna", "14" = "Rewa", "15" = "Umaria", "16" = "Shahdol",
  "17" = "Sidhi", "18" = "Neemuch", "19" = "Mandsaur", "20" = "Ratlam",
  "21" = "Ujjain", "22" = "Shajapur", "23" = "Dewas", "24" = "Jhabua",
  "25" = "Dhar", "26" = "Indore", "27" = "West Nimar", "28" = "Barwani",
  "29" = "East Nimar", "30" = "Rajgarh", "31" = "Vidisha", "32" = "Bhopal",
  "33" = "Sehore", "34" = "Raisen", "35" = "Betul", "36" = "Harda",
  "37" = "Hoshangabad", "38" = "Katni", "39" = "Jabalpur", "40" = "Narsimhapur",
  "41" = "Dindori", "42" = "Mandla", "43" = "Chhindwara", "44" = "Seoni",
  "45" = "Balaghat", "46" = "Ashoknagar", "47" = "Anuppur", "48" = "Burhanpur",
  "49" = "Alirajpur", "50" = "Singrauli"
)
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

# Apply mappings to the dataset
cat("Applying mappings to the dataset\n")
mpnew$District <- as.character(mpnew$District)
mpnew$Sector <- as.character(mpnew$Sector)
mpnew$District <- ifelse(mpnew$District %in% names(district_mapping), district_mapping[mpnew$District], mpnew$District)
mpnew$Sector <- ifelse(mpnew$Sector %in% names(sector_mapping), sector_mapping[mpnew$Sector], mpnew$Sector)

# View the modified dataset
cat("Viewing the modified dataset\n")
View(mpnew)

# Plot histogram of total consumption
cat("Plotting histogram of total consumption\n")
hist(mpnew$total_consumption, breaks = 10, col = 'blue', border = 'black', 
     xlab = "Consumption", ylab = "Frequency", main = "Consumption Distribution in Madhya Pradesh State")

# Aggregate total consumption by district
cat("Aggregating total consumption by district\n")
mp_consumption <- aggregate(total_consumption ~ District, data = mpnew, sum)
View(mp_consumption)

# Plot total consumption by district using a barplot
cat("Plotting total consumption by district using a barplot\n")
barplot(mp_consumption$total_consumption, 
        names.arg = mp_consumption$District, 
        las = 2, # Makes the district names vertical
        col = 'blue', 
        border = 'black', 
        xlab = "District", 
        ylab = "Total Consumption", 
        main = "Total Consumption per District",
        cex.names = 0.7) # Adjust the size of district names if needed

# Plot total consumption on the Madhya Pradesh state map
cat("Plotting total consumption on the Madhya Pradesh state map\n")
data_map <- st_read("MADHYA PRADESH_DISTRICTS.geojson")
data_map <- data_map %>% rename(District = dtname)
data_map_data <- merge(mp_consumption, data_map, by = "District")

ggplot(data_map_data) + 
  geom_sf(aes(fill = total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption by District") +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black")
