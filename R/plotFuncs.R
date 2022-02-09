### plotting functions

#' Plot histogram of NORSE items.
#'
#' \code{item_plot} plots histogram of item responses from a dataframe.
#'
#' @param item The name of the item to plot.
#' @param data A dataframe.
#' @return Not much, just a plot.
#' @examples
#' item_plot("Q71", data.joined)
#' item_plot(Q71, data.joined)  # error
#'
#' # Can be done in lapply:
#' needs.names <- c("Q71", "Q72", "Q74", "Q152", "Q153")
#' lapply(needs.names, item_plot)
item_plot <- function(item, ..., data = data.joined) {

  suppressWarnings(ggplot(data = data %>%
                            ungroup(),
                          aes_string(x = item),
                          environment = environment(),
                          na.rm = TRUE) +  # trick is aes_string()
                     geom_histogram(stat = "count",
                                    binwidth = 1,
                                    na.rm = TRUE) +
                     theme_bw() +
                     ggtitle(paste0(item, " responses")))
}

#' Plot an annotated histogram for NF scales
#'
#' @param data The data set on which to compute.
#' @param item.names The names of items to include, in a character vector.
#' @param title_obj Title of the plot.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' scale_plot(hb.scored.2019, cog.names, "Cognitive Problems")
scale_plot <- function(data, item.names, title_obj){
  yout <- data %>%
    select(all_of(item.names)) %>%
    summarise(y.mean = rowMeans(., na.rm = TRUE))
  ysd <- sd(yout$y.mean, na.rm = TRUE)

  hist.y <- ggplot(data = yout, aes(x = y.mean)) +
    geom_histogram(binwidth = 1/length(item.names), aes(x = y.mean), fill = "gray") +
    theme_bw() +
    geom_vline(xintercept = mean(yout$y.mean, na.rm = TRUE),
               color = "red",
               show.legend = TRUE) +
    geom_vline(xintercept = c(mean(yout$y.mean, na.rm = TRUE) - ysd,
                              mean(yout$y.mean, na.rm = TRUE) + ysd),
               color = "red",
               linetype = "dashed",
               show.legend = TRUE) +
    ggtitle(title_obj) +
    xlab("Scale score") +
    ylab("Number of responses") +
    labs(caption = "Solid red line is the mean, dashed lines are 1 SD") +
    norse_theme_bw

  hist.y
}

#' Plotting trigger item predicted distributions.
#' NEEDS options to allow mirt objects with name,
#' picking from list of SA objects, or new computation from data.
#' Also to assign a new trigger item. Right now it does the first item.
#'
#' @param scaleName A quoted simple scale name, e.g., \code{"somAnx"}.
#' Suitable for use with \code{scale_names}.
#' @param data A data set on which to test triggers.
#' @param lim Interval for plotting, as a vector of two values.
#'
#' @importFrom  ggplot2 ggplot aes stat_function ggtitle
#'
#' @return ggplot object, printed by default.
#' @export
#'
#' @examples
#' TK
trigger_plot <- function(scaleName,
                         mirt_obj,
                         lim = c(-3,3)){

  nitems <- item_descriptions %>%
    filter(scale == scaleName) %>%
    nrow()

  niceName <- item_descriptions %>%
    filter(scale == scaleName) %>%
    select(nicerScale) %>%
    slice(1) %>%
    pull()

  if(!"SingleGroupClass" %in% class(mirt_obj)){
    mirt_obj <- cogGP_sa_mirt[[scaleName]]$mirt
  }

  n.trig.resp <- ncol(coef(mirt_obj)[[1]])

  # create response pattern
  resp_pattern <- data.frame(cbind(1:n.trig.resp,
                                   matrix(rep(rep(NA,
                                                  n.trig.resp),
                                              nitems - 1),
                                          ncol = nitems - 1)))

  # compute the fscores
  trigOnly <- data.frame(fscores(mirt_obj,
                                 response.pattern = resp_pattern))

  # this is the ggplot call
  ggplot(data = data.frame(x = lim),
         aes(x)) +
    stat_function(fun = dnorm,
                  n = 101,
                  args = list(mean = trigOnly$F1[1],
                              sd =   trigOnly$SE_F1[1]),
                  color = "firebrick") +
    stat_function(fun = dnorm,
                  n = 101,
                  args = list(mean = trigOnly$F1[2],
                              sd =   trigOnly$SE_F1[2]),
                  color = "darkorange") +
    stat_function(fun = dnorm,
                  n = 101,
                  args = list(mean = trigOnly$F1[3],
                              sd =   trigOnly$SE_F1[3]),
                  color = "gold2") +
    stat_function(fun = dnorm,
                  n = 101,
                  args = list(mean = trigOnly$F1[4],
                              sd =   trigOnly$SE_F1[4]),
                  color = "darkgreen") +
    stat_function(fun = dnorm,
                  n = 101,
                  args = list(mean = trigOnly$F1[5],
                              sd =   trigOnly$SE_F1[5]),
                  color = "dodgerblue") +
    stat_function(fun = dnorm,
                  n = 101,
                  args = list(mean = trigOnly$F1[6],
                              sd =   trigOnly$SE_F1[6]),
                  color = "darkorchid") +
    stat_function(fun = dnorm,
                  n = 101,
                  args = list(mean = trigOnly$F1[7],
                              sd =   trigOnly$SE_F1[7]),
                  color = "deeppink") +
    ggtitle(paste(niceName, "trigger responses")) +
    labs(caption = "generated with NORSEpkg::trigger_plot()") +
    theme_bw_norse
}

# trigger_plot("cog", cogGP_mirt)
