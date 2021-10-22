
# setting the working directory
getwd()
setwd("C:/Users/robles/OneDrive/Desktop/First-Mini-Proj")
getwd()

# Pollutant Mean #######
pollutantmean <- function (directory, pollutant, id = 1:332){
  
  # listing all the files for the directory 
  directory <- list.files(".")
  
  # creating a blank data frame for row binding all the rows of the csv files
  binded_csvfile <- data.frame()
   
  for(i in id){
    
    # reading the csv files
    read_file <- read.csv(directory[i], header = TRUE)
    
    # binding the rows of each specified csv file 
    binded_csvfile <- rbind(read_file, binded_csvfile)
    
  }
 
  # getting the mean of the pollutants of the specified csv files
  mean_pollutant <- mean(binded_csvfile[, pollutant], na.rm = TRUE)
  
  # returning the calculated mean of the specified csv files
  return(mean_pollutant)
  
}

# Sample code for Pollutant Mean #######
  
  # pollutantmean("specdata", "sulfate", 1:10)
  # [1] 4.064128

  # pollutantmean("specdata", "nitrate", 70:72)
  # 1.706047

  # pollutantmean("specdata", "nitrate", 23)
  # 1.280833



# Number of Completely Observed Cases #######

complete <- function(directory, id = 1:332){
  
  # listing all the files for the directory 
  directory <- list.files(".")
  
  # creates a blank data frame for the cases
  comp_cases <- data.frame()
  
  for(i in id){
    
    # reading the csv files
    read_file <- read.csv(directory[i], header = TRUE)
    
    # finding the complete cases in each csv file
    cases_csvfile <- complete.cases(read_file)
    
    # calculating the total number of observations for the specified  csv files
    nobs <- sum(cases_csvfile)
    
    # creating a data frame for the index and the number of observations
    index_nobs <- data.frame(i, nobs)
    
    # binding the all the specified index with their number of observations
    comp_cases <- rbind(comp_cases, index_nobs)
  }
  
  # creating column names for the index and number of observations
  colnames(comp_cases) <- c("id", "nobs")
 
   return(comp_cases)
}

# Sample code for Number of Completely Observed Cases #######
 
  # complete("specdata", 1)
  #    id nobs
  # 1  1  117

  # complete("specdata", c(2, 4, 8, 10, 12))
  #   id nobs
  # 1  2 1041
  # 2  4  474
  # 3  8  192
  # 4 10  148
  # 5 12   96

  # complete("specdata", 30:25)
  #   id nobs
  # 1 30  932
  # 2 29  711
  # 3 28  475
  # 4 27  338
  # 5 26  586
  # 6 25  463

  # complete("specdata", 3)
  #   id nobs
  # 1  3  243


# Correlation between Sulfate and Nitrate #######

corr <- function (directory, threshold = 0){
  
  # listing all the files for the directory 
  directory <- list.files(".")
  
  # creating an empty vector for the correlation results
  cor_result <- c()
  
  for(i in 1:332){
    
    # reading the csv files
    read_file <- read.csv(directory[i])
    
    # finding the  complete cases in each csv file
    cases_csvfile <- complete.cases(read_file)
    
    # contains the complete cases of the csv files 
    cor_cases <- read_file[cases_csvfile, ]
    
    # calculates the correlation if the number of completely observed cases is greater than the threshold
    if(nrow(cor_cases) > threshold){
      
      # calculates the correlation between sulfate and nitrate
      cor_sul_nit <- cor(cor_cases[, 2], cor_cases[, 3])
      
      # concatenates the correlation results
      cor_result <- c(cor_result, cor_sul_nit)
      
    }
    
    else {
      # returns zero if it does not meet the threshold requirement
      0
    }
 
  }
  
  # returns a vector of correlation results 
  return(cor_result)
}

# Sample Code for Correlation between Sulfate and Nitrate #######

  # cr <- corr("specdata", 150)
  # head(cr)
  # summary(cr)
  # [1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
  #     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
  # -0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313

  # cr <- corr("specdata", 400)
  # head(cr)
  # summary(cr)
  # [1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860
  #     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
  # -0.17623 -0.03109  0.10021  0.13969  0.26849  0.76313 

   #cr <- corr("specdata", 5000)
  # head(cr)
  # summary(cr)
  # length(cr)
  # NULL
  # Length  Class   Mode 
  #     0   NULL   NULL 
  # [1] 0

  # cr <- corr("specdata")
  # head(cr)
  # summary(cr)
  # length(cr)
  # [1] -0.22255256 -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667
  #     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
  # -1.00000 -0.05282  0.10718  0.13684  0.27831  1.00000 
  # [1] 323



# Hospital Data #######

# setting the work directory
getwd()
setwd("C:/Users/robles/OneDrive/Desktop/First-Mini-Proj")
getwd()

# reading the csv file
outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")

# finding the number of columns
ncol(outcome)

# finding the number of rows
nrow(outcome)

# finding the names of the columns
names(outcome)

# histogram of the 30-day death rates from heart attack
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11],
  main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack", # title of the histogram
  xlab = "Deaths", # label for the x axis
  col = "light blue") # color of the histogram
