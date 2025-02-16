### plotting functions

#' Plot histogram of NORSE items.
#'
#' \code{item_plot} plots histogram of item responses from a dataframe.
#'
#' @param item The name of the item to plot.
#' @param data A dataframe.
#' @return Not much, just a plot.
#'
#' @export
#'
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


#' Generate Item Characteristic Curves (ICCs) from a mirt Object
#'
#' This function computes and plots item characteristic curves (ICCs) for each item in a
#' unidimensional IRT model fitted using the \pkg{mirt} package. It extracts the probability
#' traces for each item using \code{extract.item()} and \code{probtrace()}, reshapes the resulting
#' matrix into long format, and then builds a \pkg{ggplot2} object. Separate lines are plotted for
#' each response category.
#'
#' @param fit An object of class \code{SingleGroupClass} from \pkg{mirt} representing a
#' unidimensional IRT model.
#' @param Theta A numeric vector of theta values at which to compute the item probabilities.
#'   Default is \code{seq(-6, 6, length.out = 200)}.
#' @param facet_items Logical. If \code{TRUE} (the default), the resulting plot will be faceted by item.
#'
#' @return A \pkg{ggplot2} object displaying the item characteristic curves (ICCs). Each item
#' is optionally faceted, and within each item, separate curves represent the probability curves
#' for each response category.
#'
#' @details
#' For each item in the model, the function performs the following steps:
#' \enumerate{
#'   \item Determines the number of items using \code{extract.mirt(fit, 'nitems')}.
#'   \item Creates a unidimensional theta grid from the supplied \code{Theta} vector.
#'   \item For each item, extracts the item object with \code{extract.item()} and computes its
#'         probability trace using \code{probtrace()}. This returns a matrix with rows corresponding
#'         to theta values and columns corresponding to response categories.
#'   \item If the probability matrix does not have column names, they are assigned as "Category 1",
#'         "Category 2", etc.
#'   \item The wide-format probability matrix is reshaped into long format using \code{tidyr::pivot_longer()},
#'         with separate rows for each theta value and response category.
#'   \item A \pkg{ggplot2} line plot is constructed, showing the probability (y-axis) as a function
#'         of theta (x-axis), with different colors for each response category.
#'   \item If \code{facet_items} is \code{TRUE}, the plot is faceted by item.
#' }
#'
#' @seealso \code{\link[mirt]{extract.item}}, \code{\link[mirt]{probtrace}},
#'   \code{\link[tidyr]{pivot_longer}}, \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_line}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   library(mirt)
#'   library(ggplot2)
#'   library(tidyr)
#'
#'   # Fit a unidimensional IRT model using mirt.
#'   data <- expand.grid(matrix(sample(1:5, 100 * 5, replace = TRUE), ncol = 5))
#'   names(data) <- paste0("Item", 1:5)
#'   fit_m <- mirt(data, 1, itemtype = "graded")
#'
#'   # Generate the ICC plot.
#'   icc_plot <- ggplot_icc_plot(fit_m)
#'   print(icc_plot)
#' }
ggplot_icc_plot <- function(fit, Theta = seq(-6, 6, length.out = 200), facet_items = TRUE) {
  # Determine the total number of items.
  n_items <- extract.mirt(fit, 'nitems')
  which.items <- 1:n_items

  # For a unidimensional model, use the provided Theta vector as the grid.
  ThetaFull <- Theta

  # Extract probability traces for each item.
  P_list <- lapply(which.items, function(i) {
    # Extract the i-th item from the model.
    item_obj <- extract.item(fit, i)
    # Compute the probability trace over the theta grid.
    tmp <- probtrace(item_obj, ThetaFull)
    ncat <- ncol(tmp)
    # If the matrix has no column names, assign them as "Category 1", "Category 2", etc.
    if (is.null(colnames(tmp))) {
      colnames(tmp) <- paste0("Category ", 1:ncat)
    } else {
      # Optionally, you can force the names to follow this convention:
      colnames(tmp) <- paste0("Category ", seq_len(ncat))
    }
    # Create a data frame with Theta and the probability columns.
    df <- data.frame(Theta = ThetaFull, tmp, check.names = FALSE)
    # Reshape the data from wide to long format.
    df_long <- tidyr::pivot_longer(df, cols = -Theta,
                                   names_to = "category",
                                   values_to = "P")
    # Ensure the category column is a factor (so distinct lines can be drawn).
    df_long$category <- factor(df_long$category,
                               levels = paste0("Category ", seq_len(ncat)))
    # Add an identifier for the item.
    df_long$item <- paste0("Item ", i)
    return(df_long)
  })

  # Combine the data for all items.
  plotobj <- do.call(rbind, P_list)

  # Build the ggplot object.
  p <- ggplot(plotobj, aes(x = Theta, y = P, color = category)) +
    geom_line(linewidth = 1) +
    labs(title = "Item Characteristic Curves",
         x = expression(theta),
         y = "Probability",
         color = "Response Category") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Optionally facet the plot by item.
  if (facet_items) {
    p <- p + facet_wrap(~ item)
  }

  return(p)
}


#' Generate Test or Item Information Plots from a mirt Object
#'
#' This function computes and plots information functions for a unidimensional IRT model
#' fitted using the \pkg{mirt} package. By setting the \code{plot_type} argument, the user
#' can choose to return either the overall test information function (a single plot) or
#' faceted individual item information curves.
#'
#' @param fit An object of class \code{SingleGroupClass} from \pkg{mirt} representing a
#'   unidimensional IRT model.
#' @param Theta A numeric vector of theta values at which to compute information. Default is
#'   \code{seq(-6, 6, length.out = 200)}.
#' @param plot_type A character string indicating which plot to return. Must be either
#'   \code{"test"} to return the overall test information function or \code{"items"} to return
#'   faceted individual item information curves. Default is \code{"test"}.
#'
#' @return A \pkg{ggplot2} object displaying either the overall test information function (if
#'   \code{plot_type = "test"}) or the individual item information functions (if
#'   \code{plot_type = "items"}).
#'
#' @details
#' The function works by first creating a unidimensional theta grid based on the provided
#' \code{Theta} vector. If \code{plot_type = "test"}, it computes the overall test information
#' using \code{mirt::testinfo()} and returns a single line plot. If \code{plot_type = "items"},
#' it computes the information function for each item using \code{mirt::iteminfo()} on each
#' extracted item (via \code{extract.item()}) and then combines the curves into a data frame that
#' is plotted with \pkg{ggplot2} and faceted by item.
#'
#' @seealso \code{\link[mirt]{testinfo}}, \code{\link[mirt]{iteminfo}},
#'   \code{\link[mirt]{extract.item}}, \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_line}},
#'   \code{\link[ggplot2]{facet_wrap}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   library(mirt)
#'   library(ggplot2)
#'
#'   # Fit a unidimensional IRT model using mirt.
#'   data <- expand.grid(matrix(sample(1:5, 100 * 5, replace = TRUE), ncol = 5))
#'   names(data) <- paste0("Item", 1:5)
#'   fit_m <- mirt(data, 1, itemtype = "graded")
#'
#'   # Get the overall test information function.
#'   test_plot <- ggplot_information_plot(fit_m, plot_type = "test")
#'   print(test_plot)
#'
#'   # Get faceted individual item information curves.
#'   item_plot <- ggplot_information_plot(fit_m, plot_type = "items")
#'   print(item_plot)
#' }
ggplot_information_plot <- function(fit, Theta = seq(-6, 6, length.out = 200),
                                    plot_type = c("test", "items")) {
  plot_type <- match.arg(plot_type)

  # For a unidimensional model, we use the provided Theta vector directly.
  ThetaFull <- Theta

  if (plot_type == "test") {
    # Compute the overall test information function.
    TIF <- mirt::testinfo(fit, ThetaFull)
    df <- data.frame(Theta = ThetaFull, Information = TIF)

    p <- ggplot(df, aes(x = Theta, y = Information)) +
      geom_line(color = "blue", size = 1) +
      labs(title = "Test Information Function",
           x = expression(theta),
           y = "Information") +
      theme_minimal()

  } else if (plot_type == "items") {
    # Determine the total number of items.
    n_items <- extract.mirt(fit, 'nitems')
    which.items <- 1:n_items

    # Compute individual item information functions.
    item_info_list <- lapply(which.items, function(i) {
      item_obj <- extract.item(fit, i)
      info_i <- mirt::iteminfo(item_obj, ThetaFull)
      data.frame(Theta = ThetaFull, Information = info_i, Item = paste("Item", i))
    })
    df <- do.call(rbind, item_info_list)

    p <- ggplot(df, aes(x = Theta, y = Information, color = Item)) +
      geom_line(size = 1) +
      labs(title = "Item Information Functions",
           x = expression(theta),
           y = "Information",
           color = "Item") +
      theme_minimal() +
      facet_wrap(~ Item, ncol = 2, scales = "free_y")
  }

  return(p)
}
