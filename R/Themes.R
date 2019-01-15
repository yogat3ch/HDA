# ----------------------- Tue Dec 25 14:31:58 2018 ------------------------#
#' theme_blue A dark blue theme for ggplot2.
#'
#' This theme sometimes exhibits abberant behavior when using bar graphs with multiple captions and facets.
#' Please document any issues you encounter such that they can be addressed. Or if you happen to discover what is doing this, feel free
#' to make a pull request! url("github.com/yogat3ch/HDA/issues")
#' @param base_size \code{(integer)} Base font size to be used on graphs
#' @param base_family \code{(character)} Base font style name to be used on graphs
#' @export
theme_blue <- function(base_size = 11, base_family = "") {
requireNamespace("ggplot2", quietly = TRUE)
  theme_grey(base_size = base_size, base_family = base_family) %+replace%

    theme(line  = element_line(colour = "white", size = I(1.5), linetype = "solid",
                               lineend = "round", arrow = NULL, inherit.blank = FALSE),
          rect  = element_rect(fill = "#00010F", colour = "#00010F", size = 0, linetype = "solid", inherit.blank = FALSE),
          text  = element_text(family = "Helvetica", face = "plain", colour = "white", size = base_size,hjust = I(.5), vjust = I(.5), angle = 0, lineheight = base_size,color = NULL,margin = margin(I(.2),I(.2),I(.2),I(.2),unit="pt"),debug=F, inherit.blank = FALSE),
          title  = element_text(family = "AvantGarde", face = "plain", colour = "white", size = I(base_size),hjust = I(.5), vjust = I(1), angle = 0, lineheight = I(base_size),color = NULL,margin = margin(I(1),I(1),I(1),I(1),unit="pt"), inherit.blank = FALSE),
          axis.line  = element_line(colour = "#3A42B0", size = 1, linetype = "solid",
                                    lineend = "round", inherit.blank = FALSE),
          axis.text.x = element_text(size = base_size*0.6, color = "white", lineheight = 0.6,hjust = I(1), vjust = I(1),angle=60),
          axis.text.y = element_text(size = base_size*0.6, color = "white", lineheight = 0.9,hjust = I(.5), vjust = I(1),angle=60),
          axis.ticks = element_line(colour = "#3A42B0", size  =  0.2),
          axis.title  = element_text(family = "Helvetica", face = "italic", colour = "white", size = I(base_size*1),hjust = I(.5), vjust = I(.5), lineheight = I(base_size*1), inherit.blank = FALSE),
          axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, .5, 0, 0)),
          axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, .5, 0, 0)),
          axis.ticks.length = unit(0.3, "lines"),
          legend.background = element_rect(color = NA, fill = "#060713"),
          legend.key = element_rect(color = "white",  fill = "black"),
          legend.key.size = unit(1.2, "lines"),
          legend.key.height = NULL,
          legend.key.width = NULL,
          legend.text = element_text(size = base_size*0.8, color = "white"),
          legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),
          legend.position = "right",
          legend.text.align = NULL,
          legend.title.align = NULL,
          legend.direction = "vertical",
          legend.box = NULL,
          panel.background  = element_rect(fill = "#060713",color="#060713", inherit.blank = FALSE),
          panel.border = element_rect(color = "#3A42B0", size = 1, linetype = "solid",inherit.blank = FALSE),
          panel.grid.major  = element_line(colour = "#34415B", size = I(.1), linetype = "solid",lineend = "butt", inherit.blank = FALSE),
          panel.grid.minor  = element_line(colour = "#1C2D52", size = I(.1), linetype = "dotted",lineend = "butt", inherit.blank = FALSE),
          panel.spacing = margin(unit(0,unit="pt")),
          panel.spacing.x  = unit(0,"pt"),
          panel.spacing.y  = unit(0,"pt"),
          strip.background = element_rect(fill = "#060713", color = "#060713"),
          strip.text  = element_text(family = "Helvetica", face = "plain", colour = "white", size = I(base_size*.8),hjust = .5,lineheight = I(base_size*1),margin = margin(rep(1,4), "pt"),inherit.blank = FALSE),
          strip.text.x = element_text(color = "white"),
          strip.text.y = element_text(color = "white",angle = -90),
          plot.background  = element_rect(fill = "#00010F",colour = "#00010F", linetype = "solid", inherit.blank = FALSE),
          plot.title = element_text(family=base_family,size = base_size, color = "white", lineheight = I(1.2),vjust=1),
          plot.subtitle  = element_text( size = I(base_size*.8),hjust = I(.5),vjust=1, lineheight = I(1.2), inherit.blank = FALSE),
          plot.margin = margin(.2,.2,.2,.2,unit="cm"),
          strip.switch.pad.grid  = unit(0, "cm"),
          complete = T
    )

}

# ----------------------- Tue Dec 25 14:35:57 2018 ------------------------#
#' ggColor: R's default color wheel
#'
#' Credit: \url{https://stackoverflow.com/users/412342/john-colby}
#' @param n \code{(integer)} The number of colors from the default color wheel you would like to return
#' @return \code{(character)} Character vector of hexadecimal color values
#' @examples
#' temp <- data.frame(num = c(5, 7, 6, 4, 8), fac = factor(c("a", "a", "b", "b", "b")))
#' ggplot2::qplot(data = temp, x = num, fill = fac)
#' par(mfrow=c(1,2))
#' cls <- ggColor(2)
#' barplot(temp$num, col = cls[1])
#' barplot(temp$num, col = cls[2])
#' @export
ggColor <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}



