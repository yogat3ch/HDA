# ----------------------- Tue Dec 18 16:52:08 2018 ------------------------#
#' p.txt formats a p-value as an APA style character vector.
#'
#' A p-value of .07 will return 'p<.1', a p-value of .04: P<.05 etc.
#'
#' If a p-value is not significant (p>.1) the p-value will be reported to 2 decimal places.
#' @param p.val \code{(numeric)} A numeric p-value (or vector thereof)
#' @return \item{p text}{The p-value as an APA style character vector}
#' Useful for inline r calls within the text of an Rmd.
#' See \href{https://rmarkdown.rstudio.com/lesson-4.html}{knitr documentation} for info on inline r code.
#' @examples p.txt(.07)
#' # "p<.1"
#' p.txt(.04)
#' # "p<.05"
#' @export
p.txt <- function(p.val){
  p.txt <- ifelse(p.val < .001, "p<.001",
                  ifelse(p.val < .01, "p<.01",
                         ifelse(p.val < .05, "p<.05",
                                ifelse(p.val < .1, "p<.1",
                                       paste0("p=",round(p.val, 2))))))
  return(p.txt)
}


# ----------------------- Mon Oct 08 09:26:58 2018 ------------------------#
#' apa: Turn statistical test output into APA style citations.
#' This takes a number of test types and outputs an APA style output for the test.
#' IE \eqn{Statistic Name_(Degrees of Freedom)=.xx CI[.xx, .xx], p<.xx} \cr
#' If latex is selected, the inline statement must be wrapped in \code{$} or \code{$$} to generate latex properly.
#' @param tout The output list from a statistical test. See \bold{Details} for supported test types.
#' @param type \code{(character)} The type of APA style output required, html or latex, defaults to html.
#' @return Either an html widget or latex with the APA style output.
#' This function is useful when writing up results from statistical analyses to provide inline APA style declaration of the
#' results given the resulting output of the function.
#' See \href{https://rmarkdown.rstudio.com/lesson-4.html}{knitr documentation} for info on inline r code.
#' Currently compatible test types are:
#' \describe{
#'  \item{\code{kruskal.test}}{Kruskal-Wallis rank sum test}
#'  \item{\code{wilcox.text}}{Wilcoxon Rank-sum (Mann-Whitney) test}
#'  \item{\code{t.test}}{Difference in means test}
#'  \item{\code{anova | aov}}{Analysis of Variance. \emph{Note:} Multiple variables will be return as a named list. The APA output for the specific variable can be returned by calling the index by the name of the variable}
#'  \item{\code{effsize::cohen.d}}{Cohen's D and Hedge's G effect sizes}
#'  \item{\code{shapiro.test}}{Shapiro-Wilk test of normality}
#'  \item{\code{car::leveneTest}}{Levene's test for homogeneity of variance across groups}
#'  \item{\code{bartlett.test}}{Bartlett Test of Homogeneity of Variances}
#'  \item{\code{fligner.test}}{Fligner-Killeen Test of Homogeneity of Variances}
#'  \item{\code{cor(method="pearson")}}{Pearson's (r) product-moment test of correlation}
#' }
#' @examples
#' dat <- data.frame(y = rnorm(15,0,1),x = {rnorm(15,0,1) + rnorm(15,0,.02)})
#' apa(t.test(y = dat$y, x = dat$x))
#' apa(t.test(y = dat$y, x = dat$x), type = "latex")
#' @importFrom magrittr %>%
#' @export
apa <- function(tout, type = "html"){
  type <- tolower(type)
  requireNamespace("magrittr")
  test <- utils::capture.output(print(tout))
  if (length(test) < 1) test <- tout[["call"]][[1]]
  if (length(test) < 1) test <- tout[["method"]] else NA
  ttype <- ifelse(any(grepl("Kruskal",test)), "Kruskal",
            ifelse(any(grepl("Wilcox",test)), "Wilcox",
             ifelse(any(grepl("t-test",test)), "t-test",
              ifelse(any(grepl("Analysis of Variance",test)), "anova",
               ifelse(any(grepl("aov",test)), "aov",
                ifelse(class(tout) == "effsize", "Cohen",
                 ifelse(any(grepl("Shapiro",test)), "Shapiro",
                  ifelse(any(grepl("Levene",test)), "Levene",
                   ifelse(any(grepl("Bartlett",test)), "Bartlett",
                    ifelse(any(grepl("Fligner",test)), "Fligner",
                     ifelse(any(grepl("(?:Pearson)|(?:Kendall)|(?:Spearman)",test, perl = T)), "Pearson",NA)))))))))))
  if(ttype == "t-test") {
    stat.lbl <-  names(tout$statistic)
    st <- tout[["statistic"]][[stat.lbl]]
    deg.fr <- tout %>% .[["parameter"]] %>% .[["df"]] %>% round()
    p.val <- tout[["p.value"]]
    ci <- tout[["conf.int"]]
  }else if (ttype == "Kruskal") {
    stat.lbl <-  names(tout$statistic)
    st <- tout[["statistic"]][[stat.lbl]]
    deg.fr <- tout %>% .[["parameter"]] %>% .[["df"]] %>% round()
    p.val <- tout[["p.value"]]
    ci <- tout[["conf.int"]]
  }else if (ttype == "Wilcox"){
    stat.lbl <- names(tout$statistic)
    st <- tout[["statistic"]][[stat.lbl]]
    deg.fr <- NA
    p.val <- tout[["p.value"]]
    ci <- NA
  }else if (ttype == "Cohen") {
    stat.lbl <- "d"
    st <- tout[["estimate"]][[1]]
    deg.fr <- NA
    p.val <- NA
    ci <- tout[["conf.int"]]
    eff.size <- paste0(". This effect size is considered ",tout[["magnitude"]],".")
  }else if (ttype == "aov") {
    sum.table <- tout %>% summary() %>% .[[1]] %>% .[-nrow(.),]
    aov.out <- apply(sum.table, 1, function(rr){
      out <- data.frame("stat.lbl" = "F", "st" = rr[['F value']] %>% round(3) %>% .[!is.na(.)], "deg.fr" = rr[["Df"]]  %>% round %>% .[!is.na(.)], "p.val" = rr[['Pr(>F)']] %>% round(3) %>% .[!is.na(.)])
      return(out)
    }) %>% do.call("rbind",.)
  }else if (any(ttype == "anova")) {
    aov.out <- apply(tout[-nrow(tout), ], 1, function(rr){
      out <- data.frame("stat.lbl" = "F", "st" = rr[['F value']] %>% round(3) %>% .[!is.na(.)], "deg.fr" = rr[["Df"]]  %>% round %>% .[!is.na(.)], "p.val" = rr[['Pr(>F)']] %>% round(3) %>% .[!is.na(.)])
      return(out)
    }) %>% do.call("rbind",.)
  }else if (any(ttype == "Shapiro")) {
    stat.lbl <- "W"
    st <- tout[["statistic"]][["W"]] %>% round(3) %>% .[!is.na(.)]
    deg.fr <- NA
    p.val <- tout[['p.value']]
    ci <- NA
  }else if (any(ttype == "Levene")) {
    stat.lbl <- "F"
    st <- tout[["F value"]] %>% round(3) %>% .[!is.na(.)]
    deg.fr <- tout[["Df"]] %>% .[!is.na(.)]
    p.val <- tout[["Pr(>F)"]] %>% .[!is.na(.)]
    ci <- NA
  }else if (any(ttype == "Bartlett")) {
    stat.lbl <- ifelse(type == "html", "K<sup>2</sup>", "K^2")
    st <- tout[["statistic"]][["Bartlett's K-squared"]] %>% round(3) %>% .[!is.na(.)]
    deg.fr <- tout[["parameter"]][["df"]] %>% .[!is.na(.)]
    p.val <- tout[["p.value"]] %>% .[!is.na(.)]
    ci <- NA
  }else if (any(ttype == "Fligner")) {
    stat.lbl <- ifelse(type == "html","&chi;<sup>2</sup>", "x^2")
    st <- tout[["statistic"]][["Fligner-Killeen:med chi-squared"]] %>% round(3) %>% .[!is.na(.)]
    deg.fr <- tout[["parameter"]][["df"]] %>% .[!is.na(.)]
    p.val <- tout[["p.value"]] %>% .[!is.na(.)]
    ci <- NA
  }else if (any(ttype == "Pearson")) {
    stat.lbl <- tout[["estimate"]] %>% names()
    st <- tout[["estimate"]][[stat.lbl]] %>% round(3) %>% .[!is.na(.)]
    ref <- data.frame(cor = c(html = "r",latex = "r"), tau = c(html = "&tau;",latex = "\\tau"), rho = c(html = "&rho;", latex = "\\rho"))
    stat.lbl <- as.character(ref[type, stat.lbl])
    deg.fr <- ifelse(length(tout[["parameter"]][["df"]] %>% .[!is.na(.)]) > 0, tout[["parameter"]][["df"]] %>% .[!is.na(.)], NA)
    p.val <- tout[["p.value"]] %>% .[!is.na(.)] %>% round(3)
    ci <- tout[["conf.int"]]
  }




  if (type == "html") {
# ----------------------- Wed Nov 14 11:02:13 2018 ------------------------# HTML ouput
# ----------------------- Wed Dec 26 09:28:06 2018 ------------------------# Case when input is aov
    if (ttype == "aov" | ttype == "anova") {
      out <- apply(aov.out, 1, function(o){
        paste0(o[["stat.lbl"]],"<sub>",paste("(",paste0(o[["deg.fr"]],collapse = ","),")",sep = ""),"</sub>","=",as.numeric(o[["st"]]),paste0(", ",p.txt(as.numeric(o[["p.val"]]))))
      })
      return(htmltools::HTML(out))
    }

    # P Value Text
    if (p.val %n% T) {p.txt <- paste0(", ",p.txt(p.val))}else {p.txt <- ""}
    # DEgree Freedom Text
    if (deg.fr %n% T) {
      df.txt <- htmltools::HTML(paste0("<sub>",paste("(",paste0(deg.fr,collapse = ","),")",sep = ""),"</sub>"))}else {df.txt <- ""}
    # CI Text
    if (ci %n% T) {
      if (ttype == "Cohen") {
        ci.txt <- paste0(", CI","<sub>(&alpha;=",{1 - tout[["conf.level"]]},")</sub>","[",paste0(tout[["conf.int"]] %>% unlist %>% sapply(round,2),collapse = ","),"]")
      } else {
        ci.txt <- paste0(", CI","<sub>(&alpha;=",{1 - attr(ci,"conf.level")},")</sub>","[",paste0(tout[["conf.int"]] %>% unlist %>% sapply(round,2),collapse = ","),"]")
      }
      } else {ci.txt <- ""}
    # Out Text
    out <- htmltools::HTML(paste(stat.lbl,df.txt,"=",st %>% round(2),ci.txt,p.txt,sep = ""))
    if (ttype == "Cohen") {
      out <- htmltools::HTML(paste(stat.lbl, df.txt,"=",st %>% round(2),ci.txt,p.txt,sep = ""),eff.size)}
    }else if (type == "latex") {
# ----------------------- Wed Nov 14 11:02:04 2018 ------------------------# Latex output
      if (ttype == "aov" | ttype == "anova") {
        out <- apply(aov.out, 1, function(o){
          paste0(o[["stat.lbl"]],"_",paste("{(",paste0(o[["deg.fr"]],collapse = ","),")}",sep = ""),"=",as.numeric(o[["st"]]),paste0(", ",p.txt(as.numeric(o[["p.val"]]))))
        })
        return(out)
      }
      # P Value Text
      if (p.val %n% T) {
        p.txt <- paste0(", ",p.txt(p.val))}else {p.txt <- ""}
      # DEgree Freedom Text
      if (deg.fr %n% T) {
        df.txt <- paste0("_{",paste("(",paste0(deg.fr,collapse = ","),")",sep = ""),"}")
        }else {df.txt <- ""}
      # CI Text
      if (ci %n% T) {
        ci.txt <- paste(", CI[",paste0(tout[["conf.int"]] %>% unlist %>% sapply(round,2),collapse = ","),"]",sep = "")
        }else {ci.txt <- ""}
      # Out Text
      out <- paste(stat.lbl,df.txt,"=",st %>% round(2),ci.txt,p.txt,sep = "")
      if (ttype == "Cohen") {
        out <- paste(stat.lbl,df.txt,"=",st %>% round(2),ci.txt,p.txt,eff.size,sep = "")}
    }
  return(out)
}
