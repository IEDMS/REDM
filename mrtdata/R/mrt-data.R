##########################################################################################################
# Copyright (C) 2018
# Tuan Hoang - TEL Junior Researcher
#
# Email: tuanhcmup@gmail.com, tuan.hmt@outlook.com
# University of Science, HCM City, Vietnam
# Education Data Mining -> ITS Data Analyzing Tools. 
# Based on REDM project (http://www.educationaldatamining.org/). Power by R language.
##########################################################################################################

#' mrt.kdd.data preprocessing function
#'
#' @param file path to KDD data file
#' @param cols retained columns index list
#' @param kc.oppr.cols knowledge and opportunity column index
#' @param stu.obs.cols student and observation column index
#' @param is.train is train or test data, default is train data
#' @param sep
#'
#' @return mrt.data tidy table data
#' @export
#'
#' @examples
mrt.kdd.data <- function(file, cols, kc.oppr.cols, stu.obs.cols, is.train = TRUE, sep = "~~") {
  # load data from KDD data txt
  cat("[1] Loading ... \n")
  mrt.data = fread(file, sep = "\t", header = TRUE, na.strings = "")
  cat("File is loaded success !! ... \n")
  Sys.sleep(0.01)
  
  # just keep some columns
  cat("[2] Selecting columns ... \n")
  mrt.data = mrt.data[, ..cols]
  Sys.sleep(0.01)
  
  # remove NA value for main columns
  cat("[3] Cleaning rows with na value ... \n")
  mrt.data = na.omit(mrt.data, cols = kc.oppr.cols)
  if (is.train == TRUE)
    mrt.data = na.omit(mrt.data, cols = stu.obs.cols)
  else
    mrt.data = na.omit(mrt.data, cols = stu.obs.cols[1])
  Sys.sleep(0.01)
  
  # Separate the multiple values KC & Opprs columns
  cat("[4] Separating rows ... ")
  mrt.data = separate_rows(mrt.data, kc.oppr.cols, sep = sep)
  Sys.sleep(0.01)
  
  # Rename and order mains columns
  # cat("[5] Renaming columns ... ")
  names(mrt.data)[stu.obs.cols[1]] <- "stu"
  names(mrt.data)[stu.obs.cols[2]] <- "obs"
  names(mrt.data)[kc.oppr.cols[1]] <- "kc"
  names(mrt.data)[kc.oppr.cols[2]] <- "oppr"
  
  # convert oppr column to numeric
  mrt.data = transform(mrt.data, oppr = as.numeric(oppr))
  mrt.data = mrt.data[order(kc, stu, oppr)]
  
  # Return tidy data
  cat("Done !!! ... \n")
  return(mrt.data)
}