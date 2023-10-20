
# Skeleton file 2 for Assignment 1 in BAN400. 
# -------------------------------------------
setwd("/Users/jameshaugen/Documents/git_lesson/git-jamhau")
# More detailed steps to complete Problem 1.

library(tidyverse)    # Contains most of what we need.

# Read the entire data file into memory using the readLines()-function. Use the
# URL direcly or read the data from the local file that is in the repository.

# The readLines()-function puts each line into a separate element in a character
# vector. Looking at ?readLines it seems that we only need to provide one
# argument: the "connection", or in other words the location of the text file.
# Replace the "?" below with the location of the file. We do get a warning that
# the file does not end with an "end of line"-character (EOL). This does not
# seem to pose a problem later, and it seems that we can silece the warning by
# switchin off the "warn"-argument. Do that if you wish.
raw_file <- readLines(con = "suites_dw_Table1.txt")

# Identify the line number L of the separator line between the column names and
# the rest of the data table.

# Now every line in the file is in its separate element in the character vector
# "raw_file". The next key step is to identify which line contains the separator
# line between the column names and the rest of the table. We have to come up
# with a rule that defines this line. Let us for example say that "L is the
# first line in the data file that starts with '--'". We can extract the first
# two letters of each of the elements in the "raw_data" vector using the
# substr()-function.

# What do you need to replace the two question marks with in order to extract
# the first two letters?
substr(x = raw_file, start = 1, stop = 2)

# The next step is then to find out *which* line starts with "--", and pick out
# the first one. This can be done in a nice little pipe, where you have to fill
# out the question marks and the missing function names:
L <- 
  (substr(x = raw_file, start = 1, stop = 2) == "--") %>% 
  which() %>% 
  min()


# Save the variable descriptions (i.e. the information in lines 1:(L-2)) in a
# text-file for future reference using the cat()-function. The first argument is
# the information that we want to print out. In order to get each element in the
# "raw_file"-vector on a separate line we also provide the sep-argument, where
# we put the "end-of-line"-character "\n". We also need to come up with a file
# name. Replace the question marks:
cat(raw_file[1:(L-2)], sep = "\n", file = "variable_desc.txt")


# Extract the variable names (i.e. line (L-1)), store the names in a vector.

# This is a little bit dirty. We want to *split* the string in raw_data[L-1]
# *by* the character "|", and then we want to *trim* away all the leading and
# trailing white spaces. The first step can be accomplished using the
# str_split()-function in the stringr-package (this is already loaded through
# tidyverse), but there is a delicate detail here. The "|"-character has special
# meaning in R ("or"), so it must be *escaped*, meaning that we tell R that it
# should be interpreted as a normal character. We do that by adding two forward
# slashes in front of it. This function returns a list, with one element for
# each input element. We only send one string in, and hence get only one list
# element out (check that!). We just unlist it to get out the vector. Then we
# apply the str_trim()-function (also in the stringr-package) to get rid of all
# the empty space. Replace the question mark below:
variable_names <- 
  str_split(string = raw_file[L-1], pattern = "\\|") %>% 
  unlist() %>% 
  str_trim()

# Read the data. One way to do this is to rewrite the data to a new .csv-file
# with comma-separators for instance using cat() again, with the variable names
# from the step above on the first line (see for instance paste() for collapsing
# that vector to a single string with commas as separators).

# Let us try the approach described above. It is quite transparent, but could
# probably be done quicker. We take the elements in "raw_file" containing data,
# replace all "|" with "," and remove all empty space. The gsub-function is
# super for this kind of search-and-replace. Replace the question mark below.

comma_separated_values <- 
  raw_file[(L+1):length(raw_file)] %>% 
  gsub("\\|", ",", .) %>% 
  gsub(" ", "", .)


# We then just add the variable names (separated with commas) on top, and
# cat()-the whole ting to a .csv-file in the same way as we did with the
# variable descriptions above.

comma_separated_values_with_names <- 
  c(paste(variable_names, collapse = ","),
    comma_separated_values)    

# Replace the question mark and come up with a file name
cat(comma_separated_values_with_names, sep = "\n", file = "cleaned_data.csv")

# Read the file back in as a normal csv-file. The readr-package is part of
# tidyverse, so it is already loaded.
galaxies <- read_csv("cleaned_data.csv")


# You should now have a nice, clean data frame with galaxies and their
# characteristics in memory. As of March 2022 it should contain 796
# observations.


#--------Problem 3 ----------

library(ggplot2)
galaxies$log_mhi <- as.numeric(as.character(galaxies$log_mhi))

# Filtering out NA values for log_mhi
filtered_galaxies <- galaxies[!is.na(galaxies$log_mhi),]


# Plotting the histogram
ggplot(filtered_galaxies, aes(x=log_mhi)) +
  geom_histogram(binwidth=0.2, fill="blue", color="black", alpha=0.7) +
  theme_minimal() +
  labs(title="Distribution of log_mhi values", 
       x="log_mhi", 
       y="Number of galaxies")

#Comment: 
#The histogram shows a majority of galaxies within a specific "log_mhi" range.
#The left side indicates fewer galaxies with lower hydrogen masses,
#suggesting possible under-representation of smaller galaxies in the sample, 
#using hydrogen mass as a size proxy.

# -----Problem 4-------


# Read the data 
raw_file2 <- readLines(con = "UCNG_Table4.txt")


substr(x = raw_file2, start = 1, stop = 2)
L <- 
  (substr(x = raw_file2, start = 1, stop = 2) == "--") %>% 
  which() %>% 
  min()

cat(raw_file2[1:(L-2)], sep = "\n", file = "variable_desc2.txt")

# Extract the variable names
variable_names2 <- 
  str_split(string = raw_file2[L-1], pattern = "\\|") %>% 
  unlist() %>% 
  str_trim()


comma_separated_values2 <- 
  raw_file2[(L+1):length(raw_file2)] %>% 
  gsub("\\|", ",", .) %>% 
  gsub(" ", "", .)

comma_separated_values_with_names2 <- 
  c(paste(variable_names2, collapse = ","),
    comma_separated_values2)    

cat(comma_separated_values_with_names2, sep = "\n", file = "cleaned_data2.csv")


galaxies2 <- read_csv("cleaned_data2.csv")




