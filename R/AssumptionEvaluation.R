# ----------------------- Tue Dec 18 17:49:02 2018 ------------------------#
#' visEDA
#' 
#' A function for visual exploratory data analysis.
#' This function provides three common plots useful for visually exploring data and testing assumptions prior to statistical analysis.
#' @param dat \code{(data.frame)} A data.frame for which variables are to be explored.
#' @return A \code{gridExtra::grid.arrange} grob object with a Density plot, Normal QQ plot and Histogram for each numeric variable in the data.frame. Each \code{grid.arrange} grob contains the following graphs:
#'  /describe{
#'    \item{\code{a}}{ The first graph is a density plot with the mean and standard deviations marked. The caption contains the results of the Shapiro-Wilks test of Normality.}
#'    \item{\code{qq}}{ The second graph is a QQ plot to evaluate normality of the distribution.}
#'    \item{\code{b}}{ The third graph is a histogram with density curve plotted. The caption contains the results of the two-sided Kolmogorov-Smirnov test of Normality}
#' }
#' @examples
#' dat <- data.frame(y = rnorm(15,0,1),x = {rnorm(15,0,1) + rnorm(15,0,.02)})
#' visEDA(dat)
#' @seealso \code{\link[stats:shapiro.test]{shapiro.test}}, \code{\link[stats:ks.test]{ks.test}}, \code{\link[gridExtra:grid.arrange]{grid.arrange}}, \code{\link[ggpubr:ggqqplot]{ggqqplot}}
#' @export
visEDA <- function(dat){
  requireNamespace("tidyverse", quietly = TRUE)
  requireNamespace("ggplot2", quietly = TRUE)
  dat <- dat[, unlist(lapply(dat, is.numeric))]
  out <- purrr::map(.x = seq_along(dat), BP = dat, .f = function(x, BP){
    st <- stats::shapiro.test(BP[, x, drop = T])
    mn <- mean(BP[, x, drop = T], na.rm = T)
    s <- stats::sd(BP[, x, drop = T], na.rm = T)
    ks <- stats::ks.test(BP[, x, drop = T], y = "pnorm", mean = mn, sd = s, alternative =  "two.sided")
    dense <- stats::density(BP[, x, drop = T])
    annolabs <- paste0(c(-3,-2,-1,0,1,2,3), " SD")
    annolabs[4] <- "mean"
    lr <- data.frame(xint = s * c(-3,-2,-1,0,1,2,3) + mn) %>% cbind(ymax = dense[["y"]][sapply(.[["xint"]], function(x) which(abs(dense[["x"]] - x) %in% min(abs(dense[["x"]] - x))))], ymin = rep(min(dense[["y"]]), nrow(.)))
    clr <- RColorBrewer::brewer.pal(n = 3, "Pastel2")[x]
    g <- list()
    k <- abs(diff(range(BP[, x, drop = T]))) / 10
    .e <- environment()
    g$a <- BP %>%
      ggplot2::ggplot(data = ., mapping = ggplot2::aes_string(x = names(BP)[x])) +
      ggplot2::geom_linerange(data = lr, ggplot2::aes(x = xint, ymin = ymin, ymax = ymax),color = clr,alpha = 1) +
      ggplot2::stat_density(fill = clr,alpha = .5) +
      ggplot2::annotate("text", x = lr$xint, y = mean(dense[["y"]]), label = annolabs, angle = 90, size = 2) +
      ggplot2::labs(title = paste0("Density of ",names(BP)[x]),
           subtitle = paste0("mu=",mn %>% round(3)," ","sd=",s %>% round(3)," ","n=", length(BP[, x, drop = T])),
           caption = paste0(st$method,": ",st$p.value %>% p.txt," ","\n",ifelse(st$p.value < .1,"Not Normally Distributed","Normally Distributed")),
           x = "",y = "") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = .5,size = 10),plot.subtitle = ggplot2::element_text(hjust = .5,size = 8),plot.caption = ggplot2::element_text(size = 6))
    g$qq <- ggpubr::ggqqplot(data = BP,x = names(BP)[x],ylab = names(BP)[x],main = paste0("QQplot of ",names(BP)[x])) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = .5, size = 10),
            plot.subtitle = ggplot2::element_text(hjust = .5, size = 8),
            plot.caption = ggplot2::element_text(size = 6))
    g$b <- BP %>% ggplot2::ggplot(data = ., mapping = ggplot2::aes_string(x = names(BP)[x]), ggplot2::aes(y = .e$k * ..count..), environment = .e) +
      ggplot2::geom_histogram(binwidth = .e$k) +
      ggplot2::geom_density() +
      ggplot2::labs(title = paste0("Histogram of ",names(BP)[x]),
           subtitle = "",
           caption = paste0(ks$method,": ",ks$p.value %>% p.txt," ","\n",ifelse(ks$p.value < .1,"Not Normally Distributed","Normally Distributed")),
           x = "",y = "") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = .5, size = 10),
            plot.subtitle = ggplot2::element_text(hjust = .5, size = 8),
            plot.caption = ggplot2::element_text(size = 6))
    (out <- gridExtra::grid.arrange(grobs = g, nrow = 2))
  })
  return(out)
}
# ----------------------- Tue Dec 18 17:49:11 2018 ------------------------#
#' homoVariance
#' 
#' Performs the three most common tests of homogeneity of variance.
#' This test performs three of the most common homogeneity of variance tests:
#' \itemize{
#'      \item{\code{\link[car:leveneTest]{leveneTest}}}
#'      \item{\code{\link[stats:bartlett.test]{bartlett.test}}}
#'      \item{\code{\link[stats:fligner.test]{fligner.test}}}
#' }
#' @param form \code{(formula)} A formula in the form amenable to the lm function for which homogeneity of variance between predictors will be tested.
#' @param dat \code{(data.frame)} The data in data.frame format
#' @return A list with the output of each test
#'    \describe{
#'      \item{Levene}{Levene's test for homogeneity of variance across groups, see \link[car]{leveneTest}}
#'      \item{Bartlett}{Bartlett's test for homogeneity of variance, see \link{bartlett.test}}
#'      \item{Fligner}{Fligner-Killeen test for homogeneity of variance, see \link{fligner.test}}
#'    }
#' @examples
#' dat <- data.frame(y = rnorm(15,0,1),x = {rnorm(15,0,1) + rnorm(15,0,.02)})
#' homoVariance(y ~ x, dat)
#' @export
homoVariance <- function(form, dat){
  form <- stats::as.formula(form)
  out <- list()
   out$Levene <- tryCatch(
    {
      out$Levene <- car::leveneTest(form, data = dat)
      },
    error = function(cond) {
      return(cond)
      },
      warning = function(cond) {
      return(cond)
      },
      finally = {
     }
    )
   out$Bartlett <- tryCatch(
     {
       out$Bartlett <- stats::bartlett.test(form, data = dat)
     },
     error = function(cond) {
       # Choose a return value in case of error
       return(cond)
     },
     warning = function(cond) {
       # Choose a return value in case of warning
       return(cond)
     },
     finally = {
     }
   )
   out$Fligner <- tryCatch(
     {
       out$Fligner <- stats::fligner.test(form, data = dat)
     },
     error = function(cond) {
       # Choose a return value in case of error
       return(cond)
     },
     warning = function(cond) {
       # Choose a return value in case of warning
       return(cond)
     },
     finally = {
     }
   )


       return(out)
}

# ----------------------- Fri Dec 21 12:22:51 2018 ------------------------#
#' testTrans
#' 
#' Test mathematical transformations (exp, log, square, cube) of predictor variables.
#' For a set of dependent variables and independent variables testTrans will test the logarithmic, exponential, quadratic, and cubic relationships of the DV between each DV & IV and provide visual feedback to allow determination of the best fitting mathematical tranformation of each dependent variable for a given independent variable.
#' @param dvs \code{(character)} The names of the dependent variables as a character vector
#' @param ivs \code{(character)} The name of the indpendent variables as a character vector
#' @param dat \code{(data.frame)} The data.frame that contains the variables
#' @return \item{list}{ of ggplot2 grobs showing the relationship of each variable to each other variable}
#' Of note, the declaration of the best-fitting transformations for each relationship can be accessed via \code{lapply(result, purrr::pluck,"labels","caption")}
#' @examples
#' dat <- data.frame(y = rnorm(15,0,1),x = {rnorm(15,0,1) + rnorm(15,0,.02)})
#' testTrans("y", "x", dat)
#' @export
testTrans <- function(dvs, ivs, dat){
  requireNamespace("tidyverse", quietly = TRUE)
  requireNamespace("ggplot2", quietly = TRUE)
  if (any(!c(dvs,ivs) %in% names(dat))) stop("A named variable does not exist in the data, check variable spelling.")
  ivdv <- expand.grid(.y = dvs, .x = ivs, stringsAsFactors = F)
trans <- apply(ivdv, 1, dat = dat, function(x, dat){
  # ----------------------- Fri Dec 21 12:58:32 2018 ------------------------#
  # Check Valid Relationships
  testdf <- data.frame(none = dat[[x[[".x"]]]], log = log(dat[[x[[".x"]]]]), exp = exp(dat[[x[[".x"]]]]), sq = I(dat[[x[[".x"]]]]^2), cu = I(dat[[x[[".x"]]]]^3))
  clms <- lapply(testdf, function(clm){
    any(purrr::map_lgl(clm, .f = is.null), purrr::map_lgl(clm, .f = is.na), purrr::map_lgl(clm, .f = is.nan), purrr::map_lgl(clm, .f = is.na), purrr::map_lgl(clm, .f = is.infinite))
  })
  if (any(unlist(clms)) == T) {
    warning(paste0("The ", names(clms)[unlist(clms)], " transformation produces non-numeric values for ",paste0(x,collapse = ", "),". Omitting."), call. = F)
    nms <- names(clms)[!unlist(clms)]
    lms <- vector("list", length = length(nms))
    names(lms) <- nms
    lms$none$form <- stats::as.formula(paste0(x[[".y"]],"~",x[[".x"]]))
    if (any(grepl("log", nms))) {
      lms$log$form <- stats::as.formula(paste0(x[[".y"]],"~log(",x[[".x"]],")"))
    } else {
        lms$log$form <- NULL
        }
    if (any(grepl("exp", nms))) {
      lms$exp$form <- stats::as.formula(paste0(x[[".y"]],"~exp(",x[[".x"]],")"))
    } else {
      lms$exp$form <- NULL
      }
    if (any(grepl("sq", nms))) {
      lms$sq$form <- stats::as.formula(paste0(x[[".y"]],"~I(",x[[".x"]],"^2)"))
    } else {
        lms$sq$form <- NULL
        }
    if (any(grepl("cu", nms))) {
      lms$cu$form <- stats::as.formula(paste0(x[[".y"]],"~I(",x[[".x"]],"^3)"))
    } else {
        lms$cu$form <- NULL
        }
  } else {
    lms <- vector("list",length = 5)
    names(lms) <- c("none","log","exp","sq","cu")
    lms$none$form <- stats::as.formula(paste0(x[[".y"]],"~",x[[".x"]]))
    lms$log$form <- stats::as.formula(paste0(x[[".y"]],"~log(",x[[".x"]],")"))
    lms$exp$form <- stats::as.formula(paste0(x[[".y"]],"~exp(",x[[".x"]],")"))
    lms$sq$form <- stats::as.formula(paste0(x[[".y"]],"~I(",x[[".x"]],"^2)"))
    lms$cu$form <- stats::as.formula(paste0(x[[".y"]],"~I(",x[[".x"]],"^3)"))
  }
# ----------------------- Fri Dec 21 13:30:58 2018 ------------------------#
# Create Linear Models

  lms <- lapply(lms, dat = dat,function(l, dat){
    l$lm <- stats::lm(l$`form`, data = dat)
    l$p <- l$lm %>% summary %>% utils::capture.output() %>% stringr::str_extract("(?<=p.value\\:[\\s\\<]{1,3})[\\d\\.]+") %>% .[!is.na(.)] %>% as.numeric() %>% p.txt
    l$r2 <- l$lm %>% summary %>% .[["adj.r.squared"]] %>% round(3)
    return(l)
  })
  r2s <- unlist(lapply(lms, purrr::pluck, "r2"))
  cap <- vector()
  cap[1] <- paste("R2: ","Best = ",paste(names(which.max(r2s)),r2s[which.max(r2s)]),paste(lapply(lms,purrr::pluck,"r2") %>% paste0(names(.)," ",.),collapse=" "))
  cap[2] <- paste("p:", paste(lapply(lms, purrr::pluck, "p") %>% paste0(names(.), " ", .), collapse=" "))
  g <- dat %>% ggplot2::ggplot(data = ., mapping = ggplot2::aes_string(x = x[[".x"]], y = x[[".y"]])) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", se = F) +
    ggplot2::geom_smooth(method = "loess", se = F) +
    ggplot2::labs(title = paste0("Scatter of ",x[[".y"]]," v ",x[[".x"]]),
         caption = paste0(cap,collapse = "\n"),
         x = "",y = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = .5,size = ggplot2::unit(10,"mm"),
          margin = ggplot2::unit(rep(0,4),"mm"),vjust = 0),
          plot.subtitle = ggplot2::element_text(hjust = .5,margin = ggplot2::unit(rep(0,4),"mm")),
          plot.caption = ggplot2::element_text(size = ggplot2::unit(7,"mm"),vjust = 0,margin = ggplot2::unit(rep(0,4),"mm")),
          plot.margin = ggplot2::unit(rep(0,4),"mm"),axis.title.x = ggplot2::element_text(margin = ggplot2::unit(rep(0,4),"mm")),
          axis.text.x =  ggplot2::element_text(margin = ggplot2::unit(rep(0,4),"mm")))

  for (i in seq_along(lms)) {
    g <- g + ggplot2::geom_line(ggplot2::aes(x = x, y = y), color = RColorBrewer::brewer.pal(8, "Accent")[i], data = data.frame(y = stats::predict(lm(lms[[i]][["form"]], data = dat), newdata = dat), x = dat[[x[[".x"]]]]))
  }
  return(g)
})
purrr::map(split(trans, rep(x = 1:length(dvs), each = length(ivs))), function(g) gridExtra::grid.arrange(grobs = g,ncol = 2))
names(trans) <- apply(ivdv, 1, paste, collapse = " ~ ")
return(trans)
}
