# ----------------------- Tue Jan 29 18:27:53 2019 ------------------------#
#' extractHighlights
#'
#' Easily extract highlights made with a \href{http://www.docear.org/support/user-manual/#pdf_editors}{compatible PDF reader} from PDF documents, combine multi-page highlights, and apply formatting based on specific markup.
#' @param document \code{(character)} The path to and the name of the filename ie. "my directory/myfile.pdf"
#' @param cit \code{(boolean or charachter)} If a journal article set this to TRUE and each of the highlights will be accompanied by an APA style citation using the pdf metadata. If citation is not formatted properly, this can be set as the character string of the APA style citation which will be appended to each highlight. Defaults to FALSE.
#' @return Parsed highlights, formatted correctly when knitted to an HTML document and chunk options are set to display results='asis'
#' @details Highlighting markup is as follows:
#' \itemize{
#'  \item{\code{\*Text\*}}{Will italicize alphanumeric text as per RMarkdown markup - also works when knitting to PDF}
#'  \item{\code{i\{text\}}}{Will remove brackets and italicize and make light blue all alphanumeric text including only the following symbols: ,.&-:_ Works when knitting to all formats}
#'  \item{\code{\*\*Text\*\*}}{Will bold all alphanumeric text as per RMarkdown markup - also works when knitting to all formats}
#'  \item{\code{b\{text\}}}{Will remove brackets and bold all alphanumeric text including only the following symbols: ,.&-:_ Works when knitting to HTML only}
#'  \item{\code{#Keyword}}{Will make text bold retaining the hash for easy identification. Allows tagging of highlights. Any bracketed text that immediately follows a keyword with no space (\{Text\}) will be italicized and made light blue. Allows for description of the highlight or rationale for the keyword.}
#'  \item{\code{color\{\}}}{Will make text the \href{https://www.w3schools.com/colors/colors_names.asp}{named HTML color}. Can be combined with asterisk-based italicization and bolding.}
#' }
#' @export
#' @importFrom magrittr %<>%


extractHighlights <- function(document, cit = F){
  # ----------------------- Tue Jan 29 18:24:10 2019 ------------------------#
  # Extract Highlights
  highlights <- rpdfclown::extractPDF(document)[1] %>% gsub("[^[:alnum:][:blank:]?&/\\-\\:\\.\\,\\'\\|\\#\\{\\}\\*\\(\\)]", " ", .) %>% gsub("\\s(?=Page.?.?\\d)","\\\n",.,perl = T) %>% gsub("(?<=Page.\\d)\\:",": ",.,perl = T) %>% stringr::str_split(pattern = "\\n") %>% .[1] %>% lapply(function(.)gsub("\\s{2,}", " ", .))  %>% unlist
# ----------------------- Tue Jan 29 18:24:01 2019 ------------------------#
# Combine Highlights
  comb <- grepl("\\#[Cc]ombine", highlights)
  if (length(comb) > 0) {
  comb.rle <- rle(comb)
  comb.ind <- split(which(comb), rep(seq_along(comb.rle$lengths[comb.rle$values]),times = comb.rle$lengths[comb.rle$values]))
  e <- new.env(parent = emptyenv())
  e$highlights <- highlights
  sapply(comb.ind, comments = e$highlights, function(i, comments){
    coms <- gsub("\\#[Cc]ombine\\s?\\d{1,2}", "", comments[i])
    out <- paste(coms, collapse = " ")
    e$highlights[i[1]] <- out
  })
  highlights <- e$highlights[-unlist(sapply(comb.ind, `[`, -1))]
  }
  # ----------------------- Tue Jan 29 18:27:35 2019 ------------------------#
  # Add HTML
highlights %<>% gsub("(\\#[0-9A-Za-z\\.]+)","\\<strong\\>\\1\\<\\/strong\\>", ., perl = T) %>%
gsub("(?<=\\>)\\{([^\\}]+)\\}", " \\<span style\\=\\'color\\:#4054e9\\'\\>\\1\\<\\/span\\>", ., perl = T) %>%
gsub("i\\{([^\\}]+)\\}", "\\<em>\\1\\<\\/em\\>", ., perl = T) %>%
gsub("b\\{([^\\}]+)\\}", "\\<strong>\\1\\<\\/strong\\>", ., perl = T) %>%
gsub("([A-Za-z]+)\\{(.*)\\}", "\\<span style\\=\\'color\\:\\1\\;\\'\\>\\2\\<\\/span\\>", ., perl = T) %>%
gsub("([a-z])([A-Z])", "\\1 \\2", .)

if (cit == T) {
  md <- pdftools::pdf_info(document)
  if (any(grepl("[Aa]uthor", names(md$keys)))) {
  if (grepl("\\,", md$keys$Author)) {
    author <- stringr::str_extract(md$keys$Author, "^([A-Za-z\\-]+)")
  }else {
    author <- stringr::str_extract(md$keys$Author,"([A-Za-z\\-]+)$")
  }
  }else {
    author <- NA
    message("Message: No author detected")
  }

  year <- substr(md$created, 1, 4)
  if (author %n% F) {
    cit <- paste0("(",author, " et al., ",year,")")
  }else {cit <- paste0("(",year,")")}

}
if (cit == T | is.character(cit)) highlights <- gsub("$", cit, highlights)
highlights %<>% gsub("(.*)\\s{0,2}\\|([A-Za-z]+)\\s(.*)", "\\<div style\\=\\'background\\-color\\:whitesmoke\\; border\\-left\\:groove \\2;padding\\:5px 5px 5px 10px\\;\\-webkit\\-box\\-shadow\\: 1px 1px 2px 1px rgba\\(0\\,0\\,0\\,0\\.75\\)\\;\\-moz\\-box\\-shadow\\: 1px 1px 3px 1px rgba\\(0\\,0\\,0\\,0\\.75\\)\\;box\\-shadow\\: 1px 1px 3px 1px rgba\\(0\\,0\\,0\\,0\\.75\\)\\;\\'\\>\\1\\3\\<\\/div\\>", .)

  return(highlights)
}
