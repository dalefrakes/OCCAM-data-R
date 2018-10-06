# This file contains a function to take a data-frame of categorical variables and output
# an OCCAM compliant data file.
# 
# The end of this file shows several examples of calling the function, along with an example
# of writing the data out to a file.
# 
#  Author:  Dale Frakes
#  Date:    Mar 20, 2018
#  Version: 0.1

make_OCCAM_data <- function(df, DV=NULL, ignore_cols=c(), test_rows=c()) {
  # takes a dataframe of factors and returns a vector of strings that can be
  # written to a text file
  # DV represents a column number to be set as a Dependent Variable
  # ignore_cols is a vector of column numbers to be listed to be "ignored"
  # by OCCAM
  # Caveats/TODO:
  #   * can only handle data up to 26 variables (larger number of variables are usually labeled AA, AB, ..., ZZ)
  #   * currently only produces a "no frequency" variant; the same data can be represented with cases and a count (makes a smaller file)
  #   * doesn't label a dependent varaible (DV) as "Z" in the abbreviations (as is the norm in OCCAM files/DMM literature)
  #   * doesn't check for non-factor variables and may behave unpredictably with these (e.g. cardinality = number of cases!)
  #   * use existing R tools to bin continuous data or re-bin factors to lower cardinality
  #   * could be nicer if it aligned the header section on commas for readability

  if (length(df) > 26){
    stop("make_OCCAM_data can only handle data-frames with 26 or fewer columns")

  }
  # first "drop levels" to ensure cardinality is as low as possible
  df <- droplevels(df)

  # default OCCAM variable type is "independent" = 1 (2=DV, 0=Ignore)
  var_types <- rep(1, length(df))

  if (!is.null(DV)) {
    var_types[DV] <- 2
  }

  # now handle "ignore" columns
  for (i in ignore_cols) {
    var_types[i] <- 0
  }

  # make a new dataframe where the factor variables are saved as numbers
  df_num <- data.frame(as.numeric(df[, 1]))
  names(df_num)[1] <- names(df)[1]

  for (i in 2:length(df)){
    df_num <- cbind(df_num, as.numeric(df[, i]))
    names(df_num)[i] <- names(df)[i]
  }


  # now let's build a dataframe to hold the variable definitions for OCCAM
  # :nominal
  # alpha, 2,1,a
  # beta, 2,1,b
  # gamma, 2,2,c

  # build a dataframe to hold the variable characteristics
  var_header <- data.frame(VarName = character(),
                           Cardinality = numeric(),
                           VarType = numeric(),
                           VarLabel = character(),
                           stringsAsFactors = FALSE)


  for (i in 1:length(df_num)){
    cardinality <- length(levels(as.factor(df_num[, i])))
    var_header <- rbind(var_header, c(NA, cardinality, NA, NA))
  }

  var_header[, 1] <- names(df_num)
  var_header[, 3] <- var_types
  var_header[, 4] <- letters[1:length(df_num)] # OCCAM lettering, A...Z
  
  names(var_header) <- c("VarName", "Cardinality", "VarType", "VarLabel")
  
  if (!is.null(DV)) {
    var_header[var_header$VarType == 2, ]$VarLabel = "z"
  }

  # prepare a vector with the data to output
  out_vec <- c("# OCCAM DATA FILE")
  out_vec <- c(out_vec, "")
  out_vec <- c(out_vec, ":nominal")

  for (i in 1:nrow(var_header)) {
    datarow <- paste(as.character(var_header[i, ]), collapse = ", ")
    out_vec <- c(out_vec, datarow)
  }

  out_vec <- c(out_vec, "")
  out_vec <- c(out_vec, ":no-frequency")
  out_vec <- c(out_vec, "")

  if (length(test_rows) == 0) {
     # since there are no test rows, fill up the test_row vector with FALSE to simplify "data" section
     test_rows <- rep(FALSE, nrow(df_num))
  }
    

  out_vec <- c(out_vec, ":data")
  
  for (i in 1:nrow(df_num)) {
    if (test_rows[i] == FALSE) {
      datarow <- paste(as.character(df_num[i, ]), collapse = " ")
      out_vec <- c(out_vec, datarow)
    }
  }
  
  
  if (length(test_rows) > 0) {
    # test rows have been designated in the dataset
    out_vec <- c(out_vec, "")
    out_vec <- c(out_vec, ":test")
    
    for (i in 1:nrow(df_num)) {
      if (test_rows[i] == TRUE) {
        datarow <- paste(as.character(df_num[i, ]), collapse = " ")
        out_vec <- c(out_vec, datarow)
      }
    }
    out_vec <- c(out_vec, "")
  } 
  
  
  out_vec <- c(out_vec, "# End of OCCAM data file")

  return(out_vec)
}

# -----------------------------------------------------------------------------------
# the following provides a simple demonstration of using the function

# build a small random data frame with some factors in it
NumRows <- 15

shirts <- data.frame(sample(x = c("Red", "Orange", "Yellow", "Green", "Blue",
                                  "Indigo", "Violet"),
                            size = NumRows, replace = TRUE))

shirts <- cbind(shirts, sample(x = c("Girl", "Boy"),
                               size = NumRows, replace = TRUE))

shirts <- cbind(shirts, sample(x = c("S", "M", "L", "XL"),
                               size = NumRows, replace = TRUE))

colnames(shirts) <- c("Color", "Gender", "Size")

# Look at the structure (str) of our data
str(shirts)

# run function so all variables are IV and store in shirts_data vector
shirts_data <- make_OCCAM_data(shirts)

# cat with "\n" to force 1 element to a line and prevent interleaving of results
cat(shirts_data, sep = "\n")

# run function so 2nd variable is a DV
shirts_data <- make_OCCAM_data(shirts, DV = 2)
cat(shirts_data, sep = "\n")


# run funciton telling OCCAM to ignore variables 1 and 3
shirts_data <- make_OCCAM_data(shirts, ignore_cols = c(1, 3))
cat(shirts_data, sep = "\n")

# run funciton telling OCCAM to ignore variable 3, with variable 2 as a DV
shirts_data <- make_OCCAM_data(shirts, DV = 2, ignore_cols = c(1))
cat(shirts_data, sep = "\n")

# add designation of test rows
test_prob <- 0.2     # 20% probability of a row being designated "test"

test_data <- sample(x = c(TRUE, FALSE), 
                    size = nrow(shirts), 
                    replace = TRUE, 
                    prob = c(test_prob, 1 - test_prob))

shirts_data <- make_OCCAM_data(shirts, test_rows = test_data)
cat(shirts_data, sep = "\n")

# store the result in a variable and write to a file
occam_data <- make_OCCAM_data(shirts)

filename <- "shirtsdata.txt"
fileConn <- file(filename)
writeLines(occam_data, fileConn)
close(fileConn)
