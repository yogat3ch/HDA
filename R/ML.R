tuneGrids <- function(caretmod) {.bestTunes <- caretmod$results %>% dplyr::arrange(dplyr::desc(RMSE)) %>% .[1:3, ] %>% dplyr::select(!!.param_nms)
out <- purrr::imap(.bestTunes, function(.x, .y){
  if (all(purrr::map_lgl(.x, ~ {.x %% 1} == 0))) {
    .param_lims <- range(.x)
    .int <- ifelse( {diff(.param_lims) %/% 10} > 0, {diff(.param_lims) %/% 10}, 1)
    print(.int)
    if(HDA::go(.int)) {
      out <- seq(from = .param_lims[1], to = .param_lims[2], by = .int)
    } else {
      out <- .param_lims[1]
    }
  } else if (is.numeric(.x)) {
    .param_lims <- range(.x)
    .int <- try({diff(.param_lims) / 10})
    if(HDA::go(.int)) {
    out <- seq(from = .param_lims[1], to = .param_lims[2], by = .int)
    } else {
      out <- .param_lims[1]
      }
  } else {
      out <- unique(.x)
  }
}) %>% expand.grid()
return(out)
}
