## Plot themes

#' A ggplot2 theme with no gridlines
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' ggplot(aes(x = 1:5, y = 2:6)) + geom_line() + theme_norse_bw()
theme_norse_bw <- function(){
  # font <- "Georgia"   #assign font family up front

  theme_bw() %+replace%    #replace elements we want to change

    theme(

      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines

    )
}
