# ----------------------- Fri Jan 11 17:37:44 2019 ------------------------#
#' go
#'
#' Tests if a value or object does not exist, is length 0, NULL, or NA and returns FALSE if it is, otherwise returns TRUE. Useful for conditional evaluation of a valid value for an object for further calculations or functions.
#' @param x \code{(string)} An object or the name of an object to test for it's non-NuLL, non-NA, non-length(0) existence
#' @return \code{(boolean)} A TRUE indicates the object exists, is not NULL, NA or length 0. Otherwise false.
#' @examples
#' x <- {4/0}
#' go("x")
#' # Infinite is an acceptable value
#' x <- NaN
#' go("x")
#' x <- NA
#' go("x")
#' x <- NULL
#' go("x")
#' x <- numeric(0)
#' go("x")
#' x <- list()
#' x$go <- 2
#' go("x$go")
#' go("x[['go']]")
#' go(x[['go']])
#' @export
go <- function(x, env = parent.frame()) {
  if (!exists("debug", mode = "logical", envir = .GlobalEnv)) debug <- F else {
    debug <- get0("debug", envir = .GlobalEnv)
    message("Debug: " ,debug)
    }
  lgl <- list()
  lgl$is_str <- tryCatch(grepl("^\\\"|^\\'", x), error = function(cond) {
    return(F)
  })
  lgl$is_filename <- list()
  suppressWarnings({
  lgl$is_filename[[1]] <- try(load(file = x), silent = T)
  lgl$is_filename[[2]] <- try(read.csv(file = x), silent = T)
  lgl$is_filename[[3]] <- try(readLines(x), silent = T)
  })
  if(any(sapply(lgl$is_filename, class) != "try-error")) {
    if (debug) message("Processing as filename")
    return(T)
  }
  lgl$is_ind <- tryCatch(grepl("\\$|\\[", x) & lgl$is_str, error = function(cond) {
    return(F)
  })
  lgl$exists <- tryCatch({

    x_nm <- stringr::str_extract(deparse(substitute(x)), "[[:alnum:]\\.\\_\\%\\-]+")
    ex <- any(purrr::map_lgl(unique(sys.parents()), ~ any(stringr::str_detect(ls(sys.frame(.x), all.names = T), stringr::fixed(x_nm)))))

    if (debug) message(paste0("Exists: ", ex))
    ex
    }, error = function(cond) {
    return(F)
  })
  if(!lgl$exists) return(F)
  if (any(lgl$is_str, lgl$is_ind)) {
    if (debug) message("Processing as string...")
    it <- stringr::str_extract(x, "[[:alnum:]\\.\\_\\%\\-]+")
    # Get the initial object
    object <- get0(it, envir = sys.parent(), inherits = F)
    #print(ls())
    lgl$ind_exists <- try(eval(parse(text = deparse(substitute(x)))))
    if (class(lgl$ind_exists) == "try-error") {
      return(F)
      }
    if (lgl$is_ind) {
      message("Processing as string of indexes")
      accessors <- as.list(unlist(stringr::str_split(stringr::str_replace_all(x, "\\]\\]|\\'",""),"\\[\\[|\\$")[[1]][-1]))
    out <- purrr::pluck(.x = object, !!!accessors)
    } else {
      out <- object
    }
    if (debug) message(paste0("out:",out))
  } else {
    if (debug) message("Processing as object...")
    is_obj <- try(eval(x, envir = .GlobalEnv), silent = T)
    if (any(class(is_obj) == "try-error")) return(F)
    if (length(x) == 0) return(F) else if (is.null(x)) return(F) else if (is.na(x)) return(F) else return(T)
  }
  if (length(out) == 0) F else if (is.null(out)) F else if (is.na(out)) F else T
}

# ----------------------- Fri Jan 11 18:00:33 2019 ------------------------#
#' startPkgs
#'
#' Loads all packages supplied by the character vector silently and quickly. Useful for invisibly starting all library requirements with less typing in the setup chunk rather than calling each library independently.
#' @param pkgs \code{(character)} Vector of package names to be loaded in the R environment
#' @examples
#' req.pkgs <- c("tidyverse","magrittr","dplyr")
#' system.time({library(tidyverse);library(magrittr);library(dplyr)})
#' system.time(startPkgs(req.pkgs))
#' @export
startPkgs <- function(pkgs) {
  init <- Vectorize(FUN = function(pkg){suppressPackageStartupMessages(library(pkg,character.only = TRUE))})
  invisible(init(pkgs))
}

# ----------------------- Tue Dec 18 16:50:33 2018 ------------------------#
#' unloadPkgs
#'
#' Silently unloads all packages in the supplied character vector. Large packages loaded into the R environment can slow down computation. Unload those packages easily and silently with unloadPkgs
#' @param pkgs \code{(character)} Vector of package names to be unloaded from the R environment
#' @examples startPkgs(c("tidyverse","magrittr"))
#' system.time({detach("package:tidyverse", character.only = TRUE)
#' detach("package:magrittr", character.only = TRUE)})
#' startPkgs(c("tidyverse","magrittr"))
#' system.time(unloadPkgs(c("tidyverse","magrittr")))
#' @export

unloadPkgs <- function(pkgs){
  ul <- Vectorize(FUN = function(pkg){pkg <- gsub("^","package:",pkg)
    detach(pkg,character.only = TRUE, force = TRUE)})
  invisible(ul(pkgs))
  }

# ----------------------- Tue Dec 18 17:48:35 2018 ------------------------#
#' unloadAllPackages
#'
#' unloads all but the base packages.
#' @export
unloadAllPackages <- function() {

  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")

  package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1, TRUE, FALSE)]

  package.list <- setdiff(package.list, basic.packages)

  if (length(package.list) > 0)  for (package in package.list) invisible(detach(package, character.only = TRUE))

}

# ----------------------- Tue Dec 18 17:48:58 2018 ------------------------#
#' Mode
#'
#' Computes the mode of a numeric vector. R does not have a built in function for computing the mode (the most frequent value) in a vector of numeric values. This function (\emph{Note: Mode with a capital M}) returns the mode.
#' @param v \code{(numeric)} a numeric vector
#' @return \code{(numeric)} The mode as a numeric
#' @examples
#' x <- c(rep(1:3, each = 2),rep(4,3))
#' Mode(x)
#' dat <- data.frame(y = rnorm(15,0,1),x = {rnorm(15,0,1) + rnorm(15,0,.02)})
#' apply(dat, 2, Mode)
#' @export
Mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# ----------------------- Tue Dec 18 16:48:05 2018 ------------------------#
#' findna
#'
#' Report summary of NA values and their Indices. Iterates over the columns of a data.frame or matrix and returns descriptive summary of NA values
#' @param dat \code{(data.frame, matrix)} data.frame, vector or matrix object
#' @return \code{(list)} object with the following values:
#' \itemize{
#'  \item{`NA`}{ A vector}
#'    \enumerate{
#'      \item{\code{No.}}{The Number of NA}
#'      \item{\code{P}}{ The percentage as a decimal: the number of NA values / the total number of values for each column.}
#'    }
#'  \item{\code{Indices}}{ The indices of the row numbers that contain NA in that column, useful for locating NA and examining adjacent data.}
#' }
#' @examples
#' findna(c(1, NA, 2))
#' dat <- data.frame(y = rnorm(15,0,1),x = {rnorm(15,0,1) + rnorm(15,0,.02)})
#' dat[sample(1:nrow(dat), size = 3), "x"] <- NA
#' findna(dat)
#' @export
findna <- function(dat){
  if (!is.null(dim(dat))) {
  	nalist <- apply(dat, 2, function(x){
      nas <- sum(is.na(x))
      nap <- {nas / length(x)}
      nalist <- list("NA" = c("No." = nas, "P" = nap), Indices = which(is.na(x), arr.ind = T))
      return(nalist)
    })
	} else {
    nas <- sum(is.na(dat))
    nap <- {nas / length(dat)}
    nalist <- list("NA" = c("No." = nas, "P" = nap), Indices = which(is.na(dat), arr.ind = T))
  }
  nalist <- nalist[!sapply(nalist, is.null)]
  return(nalist)
}

# ----------------------- Tue Nov 20 08:29:21 2018 ------------------------#
#' find_peaks
#'
#' Find Peaks in a timeseries. Credit: \url{https://github.com/stas-g}
#' @param x \code{(numeric)} A vector object
#' @param m \code{(numeric)} A steepness of slope numeric as a threshold for calculating peaks. The larger the number the fewer the peaks meeting the criteria.
#' @return \item{(numeric)}{ A numeric vector of exact indices of peaks.}
#' \emph{Note:} this differs from \code{\link[quantmod:findPeaks]{quantmod::findPeaks}} in that is supplies the indices of the exact peaks, whereas quantmod supplies the vector of indices lagged by one time unit - useful when selling a stock but not if you want to know the exact peak locations.
#' @examples
#' dat <- data.frame(y = rnorm(15,0,1),x = {rnorm(15,0,1) + rnorm(15,0,.02)})
#' apply(dat, 2, find_peaks)
#' @export
find_peaks <- function(x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if (all(x[c(z:i, (i + 2):w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}

# ----------------------- Mon Apr 08 16:49:54 2019 ------------------------#
#' rleIndex
#'
#' Given an \code{\link[rle]{rle}} this function will return a dataframe of starts, ends, and indexes thereof of the run lengths.
#' Credit: \url{https://stackoverflow.com/questions/43875716/find-start-and-end-positions-indices-of-runs-consecutive-values}
#' @param x \code{(rle)} An rle object
#' @return \item{(data.frame)}{ A data.frame with length, values, start and end indices.}
#' @examples
#' dat <- data.frame(y = rnorm(15,0,1),x = {rnorm(15,0,1) + rnorm(15,0,.02)})
#' rleIndex(rle(abs(dat$x) > .5))
#' @export
rleIndex <- function(input_rle) {
 out <- input_rle %>%
    unclass() %>%
    as.data.frame() %>%
    dplyr::mutate(end = cumsum(lengths),
           start = c(1, dplyr::lag(end)[-1] + 1)) %>%
    dplyr::select(c(1,2,4,3))
 return(out)
}

# ----------------------- Wed Jul 03 08:11:57 2019 ------------------------#
#' toNum
#'
#' A simple function to convert financials retrieved from googlesheets as characters to numeric
#'
#' @param x \code{(character)} A character vector in the form "$12.23"
#' @return \code{(numeric)} A numeric vector
#' @export
toNum <- function(x){
  as.numeric(stringr::str_replace_all(x, "\\$|\\,",""))
}
