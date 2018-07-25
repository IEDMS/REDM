##########################################################################################################
# ITS Data Preprocessing Tool. Copyright (C) 2018
# Based on REDM project (http://www.educationaldatamining.org/). Power by R language.
# 
# Tuan Hoang - Junior Researcher
# Email: tuanhcmup@gmail.com, tuan.hmt@outlook.com
# University of Science, HCM City, Vietnam
##########################################################################################################

#' as.its.data preprocessing function
#'
#' @param file path to its data file
#' @param cols retained columns index list
#' @param kc.oppr.cols knowledge and opportunity column index
#' @param stu.obs.cols student and observation column index
#' @param is.train is train or test data, default is train data
#' @param sep
#' @param first 
#'
#' @return mrt.data tidy table data
#' @export
#'
#' @examples
as.its.data <- function(file, cols, kc.oppr.cols, stu.obs.cols, is.train = TRUE, sep = "~~", first = 'kc') {
  # load data from KDD data txt
  cat("[1] Loading ...\n")
  mrt.data = fread(file, sep = "\t", header = TRUE, na.strings = "")
  cat("File is loaded success !! ...\n")
  Sys.sleep(0.01)
  
  # just keep some columns
  cat("[2] Selecting columns ...\n")
  mrt.data = mrt.data[, ..cols]
  Sys.sleep(0.01)
  
  # remove NA value for main columns
  cat("[3] Cleaning rows with na value ...\n")
  mrt.data = na.omit(mrt.data, cols = kc.oppr.cols)
  if (is.train == TRUE)
    mrt.data = na.omit(mrt.data, cols = stu.obs.cols)
  else
    mrt.data = na.omit(mrt.data, cols = stu.obs.cols[1])
  Sys.sleep(0.01)
  
  # Separate the multiple values KC & Opprs columns
  cat("[4] Separating rows ...\n")
  mrt.data = separate_rows(mrt.data, kc.oppr.cols, sep = sep)
  Sys.sleep(0.01)
  
  # Rename and order mains columns
  cat("[5] Renaming columns ...\n")
  names(mrt.data)[stu.obs.cols[1]] <- "stu"
  names(mrt.data)[stu.obs.cols[2]] <- "obs"
  names(mrt.data)[kc.oppr.cols[1]] <- "kc"
  names(mrt.data)[kc.oppr.cols[2]] <- "oppr"
  Sys.sleep(0.01)
  
  # convert obs column to numeric
  cat("[5] Transforming obs to numeric ...\n")
  mrt.data = transform(mrt.data, oppr = as.numeric(oppr))
  Sys.sleep(0.01)
  
  # ordering obs by kc, stu & oppr
  cat("[6] Ordering columns ...")
  if (first == 'kc')
    mrt.data = mrt.data[order(kc, stu, oppr)]
  else
    mrt.data = setorderv(mrt.data, c(first, 'kc', 'oppr'))
  Sys.sleep(0.01)
  
  # Return tidy data
  cat("Done !!! ... \n")
  return(mrt.data)
}

#' Clean data by remove and rename some student's behavior cols
#'
#' @param tha.cols 
#' @param tha.cols.names 
#' @param first 
#' @param its.data see more at as.its.data function
#'
#' @return semi-BKT data
#' @export
#'
#' @examples
as.semi.bkt.data <- function(its.data, tha.cols = vector('integer'), tha.cols.names = vector('character'), first = 'kc') {
  if (length(tha.cols) == length(tha.cols.names)) {
    for (i in 1:length(tha.cols)) {
      names(its.data)[tha.cols[i]] <- tha.cols.names[i]
    }
  }
  if (first == 'kc')
    cols = c("kc", "stu", "oppr", tha.cols.names, "obs")
  else
    cols = c(first, "kc", "oppr", tha.cols.names, "obs")
  
  return(its.data[, ..cols])
}

#' as.redm.data.kc convert data to REDM format
#'
#' @param data data returned from as.its.data
#' @param minStu skills have number of student lower than this will be removed
#' @param minOppr students have number of opprs associate with a skill lower than this will be removed
#' @param first 
#'
#' @return perfect data
#' @export
#'
#' @examples
as.bkt.data <- function(data, minStu = 10, minOppr = 10, first = 'kc') {
  # convert data to list skills
  if (first == 'kc') {
    mrt.data = split(data, by = c('kc', 'stu'), flatten = FALSE, keep.by = FALSE)
    # remove skills which have number of students is lower than minStu
    mrt.data = Filter(function(x) length(x) > minStu, mrt.data)
    # remove students that have number of opprs is lower than minOppr
    mrt.data = lapply(mrt.data, function(x) Filter(function(y) length(y$obs) > minOppr, x))
    # remove skills again
    mrt.data = Filter(function(x) length(x) > minStu, mrt.data)
  } else {
    mrt.data = split(data, by = c(first, 'kc'), flatten = FALSE, keep.by = FALSE) 
  }
  
  # just keep only obs col
  mrt.data = lapply(mrt.data, function(x) lapply(x, function(y) y$obs))
  mrt.data = lapply(mrt.data, function(x) lapply(x, t))
  mrt.data = lapply(mrt.data, function(x) lapply(x, as.data.table))
  mrt.data = lapply(mrt.data, function(x) list(rbindlist(x, fill = TRUE), names(x)))
  mrt.data = lapply(mrt.data, function(x) {tmp = as.data.frame(x[[1]]); row.names(tmp) = x[[2]]; tmp})
  # mrt.data = lapply(mrt.data, function(x) lapply(names(x), as.data.table))
  
  return(mrt.data)
}
