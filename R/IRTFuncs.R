# IRT FUNCTIONS
# some helpful IRT functions for scale analysis



#' Marginal information per item. Only works with {ltm} objects.
#'
#' \code{info_ltm} provides a quick summary of item performance in terms of marginal
#' information over some range of theta.
#'
#' @param fit an IRT fitted model, built for those derived from package \code{ltm}.
#' @param z a vector, upper and lower limits on theta of interest. Default is c(-6,6)
#' @param n.items the number of items in the scale, defaults to the full scale from \code{fit}.
#' @param printAuto Should the info table be printed? Defaults to TRUE.
#'
#' @return a table with per-item marginal information over the range supplied by \code{z}.
#'
#' @examples
#' TK.
info_ltm <- function(fit,
                 z = c(-6,6),
                 n.items = length(names(fit$coefficients)),
                 printAuto = TRUE){
  total <- information(fit, range = z)  # defines total information within the range
  y <- matrix(nrow = n.items, ncol = 3,
              dimnames = list(NULL, c("item", "info", "PctTot")))
  # creating the output matrix
  for(i in 1:n.items){  # loop through items
    temp.fit <- information(fit, range = z, items = i)
    row <- c(names(fit$coefficients)[i], round(temp.fit$InfoRange, 2),
             round(temp.fit$InfoRange*100/total$InfoRange, 2))
    y[i, ] <- row  # take that row into matrix
  }
  if(printAuto) {
    cat("The average contribution is: ", round(100/n.items, 2), "% per item. \n", sep = "")
  }
  # this is the benchmark for equal info per item
  y.df <- as.data.frame(y)  # making DF for better output and presentation
  if(printAuto) print(y.df)  # this prints to screen, but it also returns y.df as output
  return(y.df)
}


#' Marginal information per item. Only works with {mirt} objects.
#'
#' \code{info_mirt} provides a quick summary of item performance in terms of marginal
#' information over some range of theta.
#'
#' @param fit an IRT fitted model, built for those derived from package \code{mirt}.
#' @param z a vector, upper and lower limits on theta of interest. Default is c(-6,6)
#' @param n.items the number of items in the scale, defaults to the full scale from \code{fit}.
#' @param printAuto Should the info table be printed? Defaults to TRUE for direct calls.
#'
#' @return a table with per-item marginal information over the range supplied by \code{z}.
#'
#' @examples
#' info_mirt(cogGP_mirt)
info_mirt <- function(fit,
                      z = c(-6,6),
                      n.items = length(dimnames(fit@Data$data)[[2]]),
                      printAuto = TRUE){

  y <- data.frame(t(matrix(sapply(1:n.items,
                                  function(x) mirt::areainfo(fit,
                                                             c(-6, 6),
                                                             which.items = x)),
                           nrow = 6))) %>%
    dplyr::select(3) %>%
    rename(Info = X3) %>%
    mutate(Info = as.numeric(Info)) %>%
    # group_by(info) %>%
    mutate(PctTot = round(100 * .data$Info / sum(.data$Info), 2),
           Item = dimnames(fit@Data$data)[[2]]) %>%
    select(Item, Info, PctTot)

  if(printAuto) {
    cat("The average contribution is: ", round(100/n.items, 2), "% per item. \n", sep = "")
  }
  # this is the benchmark for equal info per item

  # if(printAuto) print(y)  # this prints to screen, but it also returns y as output
  (y)
}

#' Summary analysis of scales using IRT
#'
#' \code{scale_analysis} is a summary function, especially useful in report writing.
#' Given some limited input, it returns a list of output objects which can
#' later be printed in reports or extracted in various ways.
#'
#' @param scale.name String. The name of the scale of interest,
#' e.g., \code{"Suicide Risk"}.
#' @param item.names Character vector. The names of the items that comprise the scale.
#' @param data Data frame. The data set in which to find items for analysis.
#' @param print.now Logical. Should the item curves print on running, or
#' be stored for later use ONLY? Defaults to \code{FALSE}, which will not print on run.
#' @param irt.package Which underlying package to use for estimation of GRM? Defaults
#' to \code{'ltm'}, for backwards compatibility. May also be \code{'mirt'}.
#'
#' @note This function will give an error if the \code{grm()} call doesn't work, which may
#' depend on having all observed levels of all items. Use \code{irt.package = 'mirt'}
#' in that case.
#'
#' @return a list with the following named values:
#'   \itemize{
#'     \item{\code{new_data}}{: The data set used for analysis}
#'     \item{\code{histogram}} {Item response histogram}
#'     \item{\code{min}} {Observed minimum item response mean}
#'     \item{\code{max}} {Observed maximum item response mean}
#'     \item{\code{zmin}} {Standardized obsered minimum item response mean}
#'     \item{\code{zmax}} {Standardized obsered maximum item response mean}
#'     \item{\code{floor}} {Percent of individuals at absolute minimum}
#'     \item{\code{ceiling}} {Percent of individuals at absolute maximum}
#'     \item{\code{grm}} {An object of type \code{grm} or \code{mirt} for scale analysis}
#'     \item{\code{ICC}} {Item Characteristic Curves. One per item, faceted if \code{irt.package = 'mirt'}.
#'     Either a plot object or the plot information,
#'     depending on the value of \code{print.now}}
#'     \item{\code{TIF}} {Either a plot object or the plot information,
#'     depending on the value of \code{print.now}}
#'     \item{\code{IIC}} {Either a plot object or the plot information,
#'     depending on the value of \code{print.now}}
#'     \item{\code{printed}} {logial; Were the IRT plots printed on run?}
#'     \item{\code{info}} {Relative marginal information contribution table. Currently not working
#'     for \code{irt.package = 'mirt'} (2020 AUG 25).}
#'     \item{\code{tables}} {A list of tables if item response frequency, of length k items}
#'     \item{\code{cor}} {A polychoric correlation table of item responses}
#'   }
#'
#' @examples
#' internal.out <- scale_analysis("Internal Avoidance", internal.names, data = data2017_18)
#'
#'
scale_analysis <- function(scale.name,
                           item.names,
                           data,
                           print.now = FALSE,
                           irt.package = 'ltm'){  # give it a names vector
  # item.names is a character vector of item names in quotes
  # scale.name is character string for naming purposes

  ind <- match(item.names, names(data))  # find the indices
  data$mi <- rowSums(is.na(data[, ind]))  # counts missing items on this scale

  # data$y.sum <- rowSums(data[, ind], na.rm = T)  # calculates by sum
  data$y.mean <- rowMeans(data[, ind], na.rm = TRUE)  # by mean

  # data$scale.name[which(data$mi > length(item.names)/2)] <- NA
  # data$scale.name.mean[which(data$mi > length(item.names)/2)] <- NA

  y_m <- mean(data$y.mean, na.rm = TRUE)
  y_sd <- sd(data$y.mean, na.rm = TRUE)
  y_min <- min(data$y.mean, na.rm = TRUE)
  y_max <- max(data$y.mean, na.rm = TRUE)
  y_zmin <- (y_min - y_m)/y_sd
  y_zmax <- (y_max - y_m)/y_sd

  # what percent are at the floor?
  y_pct_floor <- data %>%
    filter(y.mean == y_min) %>%   # just rows equal to min
    nrow()/nrow(data[!is.na(data$y.mean), ])  # set denominator to rows with actual values
  # what percent are at the celing?
  y_pct_ceiling <- data %>%
    filter(y.mean == y_max) %>%   # just rows equal to min
    nrow()/nrow(data[!is.na(data$y.mean), ])   # set denominator to rows with actual values
  # histogram
  title_obj <- paste0(scale.name, " raw score distribution")
  hist.y <- ggplot(data = data, aes(x = y.mean)) +
    geom_histogram(binwidth = 1/length(item.names), aes(x = y.mean), fill = "gray") +
    theme_bw() +
    geom_vline(xintercept = mean(y_m, na.rm = T),
               color = "red",
               show.legend = TRUE) +
    geom_vline(xintercept = y_m - y_sd,
               color = "red",
               linetype = "dashed",
               show.legend = TRUE) +
    ggtitle(title_obj) +
    xlab("Subscale score") +
    ylab("Number of responses")

  # data trimming for the scale
  y.data <- ungroup(data) %>%
    dplyr::select(ind)

  # Alpha and unidimensionality
  alpha.data <- psych::alpha(y.data)

  mirt.y <- NA
  grm.y <- NA
  itemScore.y <- NA

  # IRT
  if(irt.package == 'ltm'){
    grm.y <- ltm::grm(y.data)
    summary(grm.y)
  } else if(irt.package == 'mirt'){
    mirt.y <- mirt::mirt(data = y.data,
                        model = 1,
                        itemtype = "graded",
                        SE = TRUE,
                        verbose = FALSE,
                        technical = list(removeEmptyRows=TRUE))
    # print(summary(mirt.y))
  }

  if(print.now == TRUE ){
    if(irt.package == 'ltm'){
      ICC.y <- plot(grm.y, ask = FALSE)
      TIF.y <- plot(grm.y, type = "IIC", items = 0, zrange = c(-4, 4), ask = FALSE)  # Test information
      #
      IIC.y <- plot(grm.y, type = "IIC", zrange = c(-4, 4), ask = FALSE)  # Test information by item,
    }
    # summary(grm.y)
    if(irt.package == 'mirt'){ ## this doesn't work, maybe because S4 methods not available
      # in the package environment? I am tired of trying to solve that problem.
      # Making a plot function does work, however, so I define funcitons
      # for that below.
      #
      # print(pryr::method_from_call(plot(mirt.y, type = 'trace')))
      ICC.y <- plot(mirt.y, type = 'trace',
                    facet_items = TRUE)
      TIF.y <- plot(mirt.y, type = 'info',
                    facet_items = FALSE)
      IIC.y <- plot(mirt.y, type = 'infotrace',
                    facet_items = FALSE)
      itemScore.y <- plot(mirt.y, type = 'itemscore',
                         facet_items = FALSE)
      ICC.y
      TIF.y
      IIC.y
      itemScore.y
      # ICC.y <- "Run mirt.icc() instead"
      # TIF.y <- "Run mirt.tif() instead"
      # IIC.y <- "Run mirt.iic() instead"
    }
  }
  if(print.now == FALSE) {  # print.now is FALSE
    if(irt.package == 'ltm'){
      ICC.y <- plot(grm.y, plot = FALSE)
      TIF.y <- plot(grm.y, type = "IIC", items = 0, zrange = c(-4, 4), plot = FALSE)  # Test information
      #
      IIC.y <- plot(grm.y, type = "IIC", zrange = c(-4, 4), plot = FALSE)  # Test information by item,
    }
    if(irt.package == 'mirt'){
      invisible(ICC.y <- plot(mirt.y, type = 'trace',
                              facet_items = TRUE))
      invisible(TIF.y <- plot(mirt.y, type = 'info',
                              facet_items = FALSE))
      invisible(IIC.y <- plot(mirt.y, type = 'infotrace',
                              facet_items = FALSE))
      invisible(itemScore.y <- plot(mirt.y, type = 'itemscore',
                              facet_items = FALSE))
      # ICC.y <- "Run icc.mirt() instead"
      # TIF.y <- "Run tif.mirt() instead"
      # IIC.y <- "Run iic.mirt() instead"
    }
  }
  if(irt.package == 'ltm'){
    y.info <- info_ltm(grm.y, printAuto = FALSE)
  } else {
    y.info <- info_mirt(mirt.y, printAuto = FALSE)
  }

  item_freq_tables <- lapply(y.data, table)

  item_cor <- tryCatch({  # look for errors here
    unname(psych::polychoric(y.data))[[1]]  # do this preferably
  },
  error=function(e){ # if error then do this
    cat("\n", conditionMessage(e), "\n",
        "Error while computing polychoric correlation in",
        scale.name, ".\n",
        "Returning Pearson correlations.\n")
    return(cor(y.data))
  },
  warning=function(w){
    cat("\n", conditionMessage(w), "\n",
        "Warning while computing polychoric correlation in",
        scale.name, ".\n",
        "Returning Pearson correlations.\n")
    return(cor(y.data))
  }
  )

  return(list(
    new_data = y.data,
    histogram = hist.y,
    min = y_min,
    max = y_max,
    zmin = y_zmin,
    zmax = y_zmax,
    floor = y_pct_floor,
    ceiling = y_pct_ceiling,
    alpha = alpha.data,
    irt.package = irt.package,
    grm = grm.y,
    mirt = mirt.y,
    ICC = ICC.y,
    TIF = TIF.y,
    IIC = IIC.y,
    itemScore = itemScore.y,
    printed = if_else(print.now == TRUE, TRUE, FALSE),
    info = y.info,
    tables = item_freq_tables,
    cor = round(item_cor, 2)
  )
  )
}

#' Plotting Test Information Functions in {ltm}
#'
#' \code{tif.grm} takes a fitted ltm model and plots its Test Information Function
#'
#' @param x fitted object.
#' @param lim limits for x-axis. Defaults to c(-4, 4).
tif.grm <- function(x, lim = c(-4, 4)) {
  # x is a grm object
  plot(x, type = "IIC", items = 0, zrange = lim)
}

#' Plotting Item Information Curves
#'
#' \code{icc.grm} takes a fitted ltm model and plots its Item information curves
#'
#' @param x fitted object.
#' @param lim limits for x-axis. Defaults to c(-4, 4).
iic.grm <- function(x, lim = c(-4, 4)) {
  # x is a grm object
  plot(x, type = "IIC", zrange = lim)
}

# MAKE THIS WORK
icc.mirt <- function(x, lim = c(-4, 4)) {
  plot(x$mirt,
       type = "trace",
       facet_items = TRUE,
       theta_lim = lim)
}

iic.mirt <- function(x, lim = c(-4, 4)) {
  plot(x$mirt,
       type = "infotrace",
       facet_items = FALSE,
       theta_lim = lim)
}

tif.mirt <- function(x, lim = c(-4, 4)) {
  plot(x$mirt,
       type = "info",
       theta_lim = lim)
}

#' Plotting scale_analysis objects
#'
#' \code{plot.scale_analysis} takes an object from \code{scale_analysis()} and
#' provides several printed plots, for fast visual analysis.
#'
#' @param x fitted scale analysis object (named list).
#' @param lim limits for x-axis. Defaults to c(-4, 4).
plot.scale_analysis <- function(x){
  # x is a scale_analysis output
  print(x$histogram)
  if(x$irt.package == "ltm"){
    if(x$printed == TRUE){
      plot(x$grm)
    } else {
      replot.grm(x$ICC)
    }
    tif.grm(x$grm)
    iic.grm(x$grm)
  }
  if(x$irt.package == "mirt"){
    print(x$ICC)
    print(x$TIF)
    print(x$IIC)
    print(x$itemScore)
  }

}

#' Summarizing scale_analysis objects
#'
#' \code{summary.scale_analysis} takes an object from \code{scale_analysis()} and
#' provides several summaries, but no plots.
#'
#' @param x fitted object.
#' @param lim limits for x-axis. Defaults to c(-4, 4).
summary.scale_analysis <- function(x){
  print(paste(
    "Min: ", x$min), quote = FALSE
  )
  print(paste(
    "Max: ", x$max), quote = FALSE
  )
  print(paste(
    "Z-min: ", round(x$zmin, 2)), quote = FALSE
  )
  print(paste(
    "Z-max: ", round(x$zmax, 2)), quote = FALSE
  )
  print("", quote = FALSE)

  print(paste(
    "Percent at floor: ", round(x$floor, 2)), quote = FALSE
  )
  print(paste(
    "Percent at ceiling: ", round(x$ceiling, 2)), quote = FALSE
  )
  print("", quote = FALSE)

  print("Information share by item", quote = FALSE)
  print(x$info)
  print("", quote = FALSE)

  print("Item response distributions", quote = FALSE)
  print(x$tables)

  print("Item correlations", quote = FALSE)
  print(x$cor)
}

#' Plotting ICCs from grm()-derived plot information
#'
#' \code{replot.grm} takes plot information derived from
#' \code{ltm::plot.grm(plot = FALSE)}, and returns a \code{ggplot}-based series of plots
replot.grm <- function(fit, type = "ICC"){
  if(class(fit) == "grm"){
    if(type == "ICC"){
      nplots <- length(fit[[2]])
      for(i in 1:nplots){
        testdat <- cbind(fit[[1]], matrix(fit[[2]][[i]], ncol = 7)) %>%
          as_tibble() %>%
          set_names(nm = as.character(c("theta", 1, 2, 3, 4, 5, 6, 7))) %>%
          gather(key = "response", value = "value", -theta)

        print(ggplot(data = testdat, aes(x = theta, y = value)) +
                geom_line(aes(color = response)) +
                theme_bw() +
                ggtitle(paste0(names(fit[[2]])[i], " Item Information Curve")))
      }
    }
  }
   # ends type == ICC
}
