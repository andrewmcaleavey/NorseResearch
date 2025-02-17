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
#' Suitable for use with \code{scale.names}.
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


#' Generate Item Characteristic Curves (ICCs) from a mirt Object with Flexible Layout
#'
#' This function computes and plots item characteristic curves (ICCs) for each item in a
#' unidimensional IRT model fitted using the \pkg{mirt} package. It extracts the probability
#' traces for each item using \code{extract.item()} and \code{probtrace()}, reshapes the resulting
#' matrix into long format, and then builds a \pkg{ggplot2} object. Separate lines are plotted for
#' each response category. The plot titles incorporate a user-specified \code{scale.name} and the actual
#' variable names from the mirt object are used instead of generic labels. When generating ICC plots,
#' the user can choose to have a single faceted plot or a list of separate plots.
#'
#' @param fit An object of class \code{SingleGroupClass} from \pkg{mirt} representing a
#'   unidimensional IRT model.
#' @param Theta A numeric vector of theta values at which to compute the item probabilities.
#'   Default is \code{seq(-4, 4, length.out = 200)}.
#' @param facet_items Logical. If \code{TRUE} (the default), the resulting plot will be faceted by item.
#'   If \code{FALSE}, the function returns a list of separate plots (one per item).
#' @param scale.name A character string specifying the scale name (e.g., "Sad Affect") that will be
#'   incorporated into the plot titles.
#'
#' @return If \code{facet_items = TRUE}, a single \pkg{ggplot2} object displaying the item characteristic
#'   curves (ICCs) faceted by item. If \code{facet_items = FALSE}, a list of \pkg{ggplot2} objects (one per item)
#'   is returned, with each plot's title including the scale name and the corresponding variable name.
#'
#' @details
#' The function works as follows:
#' \enumerate{
#'   \item Determines the number of items using \code{extract.mirt(fit, 'nitems')}.
#'   \item Extracts the variable names from the mirt object via \code{colnames(mirt::extract.mirt(fit, "data"))}.
#'   \item Creates a unidimensional theta grid from the supplied \code{Theta} vector.
#'   \item For each item, extracts the item object with \code{extract.item()} and computes its probability trace
#'         using \code{probtrace()}. This returns a matrix with rows corresponding to theta values and columns corresponding
#'         to response categories.
#'   \item If the probability matrix lacks column names, they are assigned as "Category 1", "Category 2", etc.
#'   \item The wide-format probability matrix is reshaped into long format using \code{tidyr::pivot_longer()},
#'         with separate rows for each theta value and response category.
#'   \item A \pkg{ggplot2} line plot is constructed showing probability (y-axis) as a function of theta (x-axis),
#'         with different colors for each response category.
#'   \item The plot title is generated to include \code{scale.name}. When returning separate plots, each plot's title
#'         also includes the actual variable name.
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
#'   # Generate a faceted ICC plot with scale name.
#'   icc_plot_faceted <- ggplot_icc_plot(fit_m, scale.name = "Sad Affect", facet_items = TRUE)
#'   print(icc_plot_faceted)
#'
#'   # Generate separate ICC plots for each item.
#'   icc_plot_list <- ggplot_icc_plot(fit_m, scale.name = "Sad Affect", facet_items = FALSE)
#'   lapply(icc_plot_list, print)
#' }
ggplot_icc_plot <- function(fit,
                            Theta = seq(-4, 4, length.out = 200),
                            facet_items = TRUE,
                            scale.name = "") {
  # Determine the total number of items.
  n_items <- mirt::extract.mirt(fit, 'nitems')
  which.items <- 1:n_items

  # Extract variable names from the mirt object.
  item_names <- colnames(mirt::extract.mirt(fit, "data"))

  # For a unidimensional model, use the provided Theta vector as the grid.
  ThetaFull <- Theta

  # Extract probability traces for each item.
  P_list <- lapply(which.items, function(i) {
    # Extract the i-th item from the model.
    item_obj <- mirt::extract.item(fit, i)
    # Compute the probability trace over the theta grid.
    tmp <- mirt::probtrace(item_obj, ThetaFull)
    ncat <- ncol(tmp)
    # If the matrix has no column names, assign them as "Category 1", "Category 2", etc.
    if (is.null(colnames(tmp))) {
      colnames(tmp) <- paste0("Category ", 1:ncat)
    } else {
      # Force names to follow the convention.
      colnames(tmp) <- paste0("Category ", seq_len(ncat))
    }
    # Create a data frame with Theta and the probability columns.
    df <- data.frame(Theta = ThetaFull, tmp, check.names = FALSE)
    # Reshape the data from wide to long format.
    df_long <- tidyr::pivot_longer(df, cols = -Theta,
                                   names_to = "category",
                                   values_to = "P")
    # Ensure the category column is a factor.
    df_long$category <- factor(df_long$category,
                               levels = paste0("Category ", seq_len(ncat)))
    # Add an identifier for the item using the actual variable name.
    df_long$item <- item_names[i]
    return(df_long)
  })

  # Combine the data for all items.
  plotobj <- do.call(rbind, P_list)

  if (facet_items) {
    # Build a single faceted ggplot.
    main_title <- "Item Characteristic Curves"
    if (scale.name != "") {
      main_title <- paste0(main_title, ": ", scale.name)
    }
    p <- ggplot(plotobj, aes(x = Theta, y = P, color = category)) +
      geom_line(linewidth = 1) +
      labs(title = main_title,
           x = expression(theta),
           y = "Probability",
           color = "Response Category") +
      scale_y_continuous(limits = c(0, 1)) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      facet_wrap(~ item)
    return(p)
  } else {
    # Build a list of separate plots, one per item.
    plot_list <- lapply(split(plotobj, plotobj$item), function(df_item) {
      item_name <- unique(df_item$item)
      item_title <- "Item Characteristic Curve"
      if (scale.name != "") {
        item_title <- paste0(item_title, ": ", scale.name, " - ", item_name)
      } else {
        item_title <- paste0(item_title, " - ", item_name)
      }
      p_item <- ggplot(df_item, aes(x = Theta, y = P, color = category)) +
        geom_line(linewidth = 1) +
        labs(title = item_title,
             x = expression(theta),
             y = "Probability",
             color = "Response Category") +
        scale_y_continuous(limits = c(0, 1)) +
        theme_minimal() +
        theme(legend.position = "bottom")
      return(p_item)
    })
    return(plot_list)
  }
}


#' Generate Test or Item Information Plots from a mirt Object
#'
#' This function computes and plots information functions for a unidimensional IRT model
#' fitted using the \pkg{mirt} package. By setting the \code{plot_type} argument, the user
#' can choose to return either the overall test information function (a single plot) or
#' individual item information curves. The \code{scale.name} parameter is incorporated into the
#' plot titles, and when plotting items, the actual variable names from the mirt object are used.
#' Additionally, the user can choose whether to facet the individual item curves (default)
#' or display them all on one plot.
#'
#' @param fit An object of class \code{SingleGroupClass} from \pkg{mirt} representing a
#'   unidimensional IRT model.
#' @param Theta A numeric vector of theta values at which to compute information. Default is
#'   \code{seq(-4, 4, length.out = 200)}.
#' @param plot_type A character string indicating which plot to return. Must be either
#'   \code{"test"} to return the overall test information function or \code{"items"} to return
#'   individual item information curves. Default is \code{"test"}.
#' @param scale.name A character string specifying the name of the scale (e.g., "Sad Affect"). This
#'   is appended to the plot title.
#' @param facet Logical. When \code{plot_type = "items"}, if \code{TRUE} (the default), the individual
#'   item curves will be displayed in separate facets; if \code{FALSE}, all curves will be shown
#'   in a single plot.
#'
#' @return A \pkg{ggplot2} object displaying either the overall test information function (if
#'   \code{plot_type = "test"}) or the individual item information functions (if
#'   \code{plot_type = "items"}).
#'
#' @details
#' The function works by first creating a unidimensional theta grid based on the provided
#' \code{Theta} vector. If \code{plot_type = "test"}, it computes the overall test information
#' using \code{mirt::testinfo()} and returns a single line plot with a title that includes
#' \code{scale.name} (if provided). If \code{plot_type = "items"}, it computes the information function
#' for each item using \code{mirt::iteminfo()} on each extracted item (via \code{extract.item()}),
#' uses the actual variable names from the mirt fit object, and then combines the curves into a data frame
#' that is plotted with \pkg{ggplot2}. When \code{facet = TRUE}, the curves are faceted by item.
#'
#' @seealso \code{\link[mirt]{testinfo}}, \code{\link[mirt]{iteminfo}},
#'   \code{\link[mirt]{extract.item}}, \code{\link[mirt]{extract.mirt}},
#'   \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_line}},
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
#'   names(data) <- paste0("Question", 1:5)
#'   fit_m <- mirt(data, 1, itemtype = "graded")
#'
#'   # Get the overall test information function with a scale name.
#'   test_plot <- ggplot_information_plot(fit_m, plot_type = "test", scale.name = "Sad Affect")
#'   print(test_plot)
#'
#'   # Get individual item information curves on separate facets.
#'   item_plot_facet <- ggplot_information_plot(fit_m, plot_type = "items", scale.name = "Sad Affect")
#'   print(item_plot_facet)
#'
#'   # Get individual item information curves in a single plot.
#'   item_plot_all <- ggplot_information_plot(fit_m, plot_type = "items", scale.name = "Sad Affect", facet = FALSE)
#'   print(item_plot_all)
#' }
ggplot_information_plot <- function(fit, Theta = seq(-4, 4, length.out = 200),
                                    plot_type = c("test", "items"),
                                    scale.name = "",
                                    facet = TRUE) {
  plot_type <- match.arg(plot_type)

  # Use the provided Theta vector as the grid (suitable for unidimensional models).
  ThetaFull <- Theta

  # Get the variable names from the mirt fit object.
  item_names <- colnames(mirt::extract.mirt(fit, "data"))

  if (plot_type == "test") {
    # Compute the overall test information function.
    TIF <- mirt::testinfo(fit, ThetaFull)
    df <- data.frame(Theta = ThetaFull, Information = TIF)

    # Create the plot title using the scale name if provided.
    main_title <- "Test Information Function"
    if (scale.name != "") {
      main_title <- paste0(main_title, ": ", scale.name)
    }

    p <- ggplot(df, aes(x = Theta, y = Information)) +
      geom_line(color = "blue", linewidth = 1) +
      labs(title = main_title,
           x = expression(theta),
           y = "Information") +
      theme_minimal()

  } else if (plot_type == "items") {
    # Determine the total number of items.
    n_items <- mirt::extract.mirt(fit, 'nitems')
    which.items <- 1:n_items

    # Compute individual item information functions.
    item_info_list <- lapply(which.items, function(i) {
      item_obj <- mirt::extract.item(fit, i)
      info_i <- mirt::iteminfo(item_obj, ThetaFull)
      data.frame(Theta = ThetaFull,
                 Information = info_i,
                 Item = item_names[i])
    })
    df <- do.call(rbind, item_info_list)

    main_title <- "Item Information Functions"
    if (scale.name != "") {
      main_title <- paste0(main_title, ": ", scale.name)
    }

    p <- ggplot(df, aes(x = Theta, y = Information, color = Item)) +
      geom_line(linewidth = 1) +
      labs(title = main_title,
           x = expression(theta),
           y = "Information",
           color = "Item") +
      theme_minimal()

    # Apply faceting if requested.
    if (facet) {
      p <- p + facet_wrap(~ Item, ncol = 2, scales = "fixed")
    }
  }

  return(p)
}
