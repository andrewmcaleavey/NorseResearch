#' Retrieving the (English) item text from an item number
#'
#' \code{get_item_text} simply returns the English text, given a
#' character item number in the format "Q11".
#'
#' @param target the character string to look up, e.g., "Q26".
#' @param table the lookup table, defaults to \code{item_descriptions}.
#' Can be expanded to provide Norwegian or other lookup tables.
#'
#'
#' @return a character string with the English-language item text.
get_item_text <- function(target,
                          table = item_descriptions){
  .Deprecated("lookup_item")
  as.character(table[table$item == target, "item_text"])
}

#' Look up information about an item
#'
#' @param target Item quoted as character (e.g., "Q61")
#' @param version Norse version to use. Changes the lookup table from 'item_descriptions'
#' (used for v. 2.0) to 'nf2.1.item.descriptions' (used for 2.1).
#' @param verbose Logical. Provide more than just item text?
#'
#' @return If \code{verbose = FALSE}, a string of item text.
#' If \code{verbose = TRUE}, a single row data frame.
#'
#' @export
#'
#' @examples
#' lookup_item("Q145")
#' # no difference:
#' lookup_item("Q145", version = 2)
#'
#' # version can be any of the following: 2, 2.0, "2", 2.1, or "2.1".
#'
#' # compare v 2 to 2.1 using verbose = TRUE
#' lookup_item("Q61", version = 2, verbose = TRUE)    # It was the trigger in v2.0
#' lookup_item("Q61", version = 2.1, verbose = TRUE)  # but not in v2.1
#' @seealso \code{\link{get_item_text}}, a deprecated version but necessary for re-running analyses.
lookup_item <- function(target,
                      version = "2.1",
                      verbose = FALSE){
  # version 2.0 is mostly a copy of get_item_text()
  if (version == 2 | version == "2" | version == "2.0") {
    text_ret <- as.character(item_descriptions[item_descriptions$item == target, "item_text"])
    verb_out <- item_descriptions[item_descriptions$item == target, ]
  }

  # version 2.1 is similar
  if (version == 2.1 | version == "2.1") {
    text_ret <- as.character(nf2.1.item.descriptions[match(target, nf2.1.item.descriptions$item), "item_text_e"])
    verb_out <- nf2.1.item.descriptions[nf2.1.item.descriptions$item == target, ]
  }

  if (version == 3 | version == "3" | version == "3.1" | version == 3.1) {
    text_ret <- as.character(NF3.1_items[match(target, NF3.1_items$item), "item_text_e"])
    verb_out <- NF3.1_items[NF3.1_items$item == target, ]
  }

  # This is the return() block
  if (verbose) return(verb_out)
  else return(text_ret)
}

#' Look up the trigger item given a scale name in either programming or presentation format.
#'
#' @param scaleName A (partial) scale name or simple name. "Somatic Anxiety",
#' "somAnx", and "Somatic" should all work for version = 2.1.
#' @param version Version of the NF to use. Changes the lookup table from 'item_descriptions'
#' (used for v. 2.0) to 'nf2.1.logic' (used for 2.1).
#'
#' @return A character item name of the trigger.
#' @export
#'
#' @examples
#' lookup_trigger("hopeless")
#' # "Q115"
#' lookup_trigger("hopeless", version = 2)
#' # "Q61"
#'
#' # works with nicer names in v. 2.1 ONLY:
#' lookup_trigger("Hopeless")
#' # "Q115"
#'
#' # when lookup fails:
#' \dontrun{
#' lookup_trigger("Hopless")  # easy misspelling
#' }
#' # "scaleName not recognized"
#' # NULL
#'
lookup_trigger <- function(scaleName,
                           version = 2.1){

  # version 2.0 is copy of find_trigger()
  if(version == 2 | version == "2" | version =="2.0"){
    output <- item_descriptions %>%
      dplyr::filter(scale == scaleName) %>%
      dplyr::filter(trigger == TRUE) %>%
      dplyr::select(item) %>%
      pull()
  }

  else if(version == 2.1 | version == "2.1"){
    # determine whether a simple or full name is supplied.

    if(!is.na(pmatch(scaleName, nf2.1.logic$scale_e))){  # if this is missing,
      # it is not recognized as a unique full scale name.
      # so this will be evaluated if it can find one
      rowN <- pmatch(scaleName, nf2.1.logic$scale_e)
    }

    else if(!is.na(pmatch(scaleName, nf2.1.logic$simple_scale))){
      # This will only be evaluated if it finds a match in the simple scale
      rowN <- pmatch(scaleName, nf2.1.logic$simple_scale)
    }

    else {print("scaleName not recognized")
      return()}

    # this makes the output from v2.1
    output <- nf2.1.logic %>%
      dplyr::slice(rowN) %>%
      dplyr::select(trigger_item) %>%
      pull()
  }

  return(output)
}

#' Look up trigger item among a list of item names
#'
#' @param items A vector or list of quoted item names,
#' e.g., somAnx.names
#' @param version Version of the NF to use. Changes the lookup table from 'item_descriptions'
#' (used for v. 2.0) to 'nf2.1.logic' (used for 2.1).
#'
#' @return The item name that is identified as a trigger in the appropriate version.
#' If none of the items are identified as a trigger, returns \code{NA} with a warning.
#' When multiple trigger items are detected, returns a character vector with a message.
#'
#' @export
#'
#' @examples
#' lookup_trigger_among(somAnx.names)
#'
#' # sends a message:
#' lookup_trigger_among(append(somAnx.names, sad.names))
#'
#' # returns NA with warning:
#' lookup_trigger_among(subRecov.names)
lookup_trigger_among <- function(items,
                                 version = 2.1) {
  # output <- NA

  # version 2.0 is copy of find_trigger_among()
  ifelse(version %in% list(2, "2", "2.0"),
                    {
                      output <- item_descriptions %>%
                        dplyr::filter(item %in% items) %>%
                        dplyr::filter(trigger == TRUE) %>%
                        dplyr::select(item) %>%
                        pull()
                    },
                   # if version is not 2, check if 2.1
                    ifelse(version %in% list(2.1, "2.1"),
                            {
                              # Version 2.1 is basically the same, new item description table
                              output <- nf2.1.item.descriptions %>%
                                dplyr::filter(item %in% items) %>%
                                dplyr::filter(trigger == TRUE) %>%
                                dplyr::select(item) %>%
                                pull()
                            },
                           # Otherwise, keep it NA
                            {
                              output <- NA
                            }))

  # If multiple  triggers returned - send message to alert
  if(length(output) > 1){
    message(paste(length(output), "trigger items were found. \n Was this intentional?\n"))
  }
  # A warning might be helpful if no trigger items were found
  # But not an error.
  else if(is.na(output)){
    warning(paste("None of these items are triggers in NF v.", version))
  }
  output
}

#' Compute Item and Test Information for IRT Models
#'
#' This function calculates item-level and total test information for a fitted
#' graded response model (GRM) from either the `{ltm}` or `{mirt}` package.
#'
#' @param fit A fitted IRT model object from either `{ltm}` (`grm()`) or `{mirt}` (`mirt()`).
#' @param z A numeric vector of length 2 specifying the theta range for computing information.
#'   Default is `c(-6, 6)`.
#' @param n.items Optional. The number of items in the model. If `NULL`, the function
#'   determines this automatically.
#' @param printAuto Logical. If `TRUE`, the function prints the item information table to the console.
#'   Default is `TRUE`.
#'
#' @return A data frame containing:
#'   \item{item}{The item names.}
#'   \item{info}{The computed item information over the specified theta range.}
#'   \item{PctTot}{The percentage of total test information contributed by each item.}
#'
#' @details
#' - When a model from `{ltm}` is provided, the function uses `ltm::information()`.
#' - When a model from `{mirt}` is provided, the function computes item information using `mirt::iteminfo()`
#'   and total test information using `mirt::testinfo()`.
#'
#' @examples
#' \dontrun{
#' library(ltm)
#' data(Science)
#' fit_ltm <- grm(Science)
#' info(fit_ltm)
#'
#' library(mirt)
#' data <- data.frame(matrix(sample(1:5, 100 * 5, replace = TRUE), ncol = 5))
#' names(data) <- paste0("Q", 1:5)
#' fit_mirt <- mirt::mirt(data, 1, itemtype = "graded")
#' info(fit_mirt)
#' }
#' @export
info <- function(fit, z = c(-6, 6), n.items = NULL, printAuto = TRUE) {
  # Detect if the model is from {mirt} or {ltm}
  is_mirt <- inherits(fit, "SingleGroupClass")  # {mirt} models have this class
  theta_range <- seq(z[1], z[2], length.out = 100)  # Standard theta range

  if (is_mirt) {
    # {mirt} computation
    total_info <- sum(mirt::testinfo(fit, Theta = theta_range))  # Total test info
    item_names <- colnames(mirt::extract.mirt(fit, "data"))
    n.items <- length(item_names)

    # Extract item-level information correctly
    item_info_values <- sapply(1:n.items, function(i) {
      item_obj <- mirt::extract.item(fit, i)  # Extract item separately
      sum(mirt::iteminfo(item_obj, Theta = theta_range))  # Compute information
    })

    # Store results in a dataframe
    y.df <- data.frame(
      item = item_names,
      info = round(item_info_values, 2),
      PctTot = round(item_info_values * 100 / total_info, 2),
      stringsAsFactors = FALSE
    )

  } else {
    # {ltm} computation
    total <- ltm::information(fit, range = z)  # Total test info
    item_names <- names(fit$coefficients)
    n.items <- length(item_names)

    y <- matrix(nrow = n.items, ncol = 3, dimnames = list(NULL, c("item", "info", "PctTot")))

    for (i in seq_len(n.items)) {
      temp.fit <- ltm::information(fit, range = z, items = i)
      y[i, ] <- c(item_names[i], round(temp.fit$InfoRange, 2), round(temp.fit$InfoRange * 100 / total$InfoRange, 2))
    }

    # Convert to dataframe
    y.df <- as.data.frame(y, stringsAsFactors = FALSE)
    y.df$info <- as.numeric(y.df$info)
    y.df$PctTot <- as.numeric(y.df$PctTot)
  }

  # Print results if requested
  if (printAuto) {
    cat("The average contribution is: ", round(100 / n.items, 2), "% per item. \n", sep = "")
    print(y.df)
  }

  return(y.df)
}



#' Conduct Scale Analysis with Classical and IRT Methods
#'
#' This function performs scale analysis by computing descriptive statistics, reliability,
#' and item response theory (IRT) models using either the `{ltm}` or `{mirt}` package.
#'
#' @param scale.name A character string specifying the name of the scale for labeling.
#' @param item.names A character vector of item names (column names in `data`).
#' @param data A data frame containing the item responses.
#' @param IRTpackage A character string specifying which IRT package to use.
#'   Options: `"ltm"` or `"mirt"` (default).
#' @param print.now Logical. If `TRUE`, plots for item characteristic curves (ICC) and
#'   test information function (TIF) are displayed immediately. Default is `FALSE`.
#'   @param version character string specifying the version of the NF to use
#'   when looking up item text
#'
#' @return An S3 object of type `scale_analysis2` containing:
#'   \item{new_data}{Subset of `data` containing only the selected items.}
#'   \item{histogram}{A ggplot histogram of the scale score distribution.}
#'   \item{min, max}{Observed minimum and maximum scale scores.}
#'   \item{zmin, zmax}{Standardized minimum and maximum scores.}
#'   \item{floor, ceiling}{Proportion of responses at the minimum and maximum score.}
#'   \item{alpha}{Cronbach's alpha and other reliability statistics.}
#'   \item{reliability}{More reliability statistics.}
#'   \item{grm}{The fitted graded response model (`ltm::grm` or `mirt::mirt`).}
#'   \item{ICC, TIF, IIC}{Item characteristic curves, test information, and item information.}
#'   \item{printed}{Logical indicating if plots were printed (`TRUE`/`FALSE`).}
#'   \item{info}{Item information statistics from the IRT model.}
#'   \item{tables}{Frequency tables for each item.}
#'   \item{cor}{Polychoric correlation matrix of the items.}
#'   \item{scale_name}{The name of the scale.}
#'   \item{items}{The item names.}
#'   \item{item_text}{English text.}
#'   \item{pct_open_scale}{The percent of people without missing data on non-trigger items.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#' Q1 = sample(1:5, 100, replace = TRUE),
#' Q2 = sample(1:5, 100, replace = TRUE),
#' Q3 = sample(1:5, 100, replace = TRUE)
#' )
#' result <- scale_analysis("Example Scale", c("Q1", "Q2", "Q3"),
#'                          df, IRTpackage = "mirt")
#' print(result$histogram)
#' }
scale_analysis2 <- function(scale.name,
                            item.names,
                            data,
                            IRTpackage = "mirt", # can be "ltm" or "mirt"
                            print.now = FALSE,
                            version = "3.1") {

  # Identify indices of selected items
  ind <- match(item.names, names(data))

  # Count missing responses per participant
  data$mi <- rowSums(is.na(data[, ind]))

  # Compute mean-based scale score (ignoring missing values)
  data$y.mean <- rowMeans(data[, ind], na.rm = TRUE)

  # Summary statistics
  y_m <- mean(data$y.mean, na.rm = TRUE)
  y_sd <- sd(data$y.mean, na.rm = TRUE)
  y_min <- min(data$y.mean, na.rm = TRUE)
  y_max <- max(data$y.mean, na.rm = TRUE)
  y_zmin <- (y_min - y_m) / y_sd
  y_zmax <- (y_max - y_m) / y_sd

  # Floor and ceiling effects
  y_pct_floor <- mean(data$y.mean == y_min, na.rm = TRUE)
  y_pct_ceiling <- mean(data$y.mean == y_max, na.rm = TRUE)

  # Histogram
  title_obj <- paste0(scale.name, " raw score distribution")
  hist.y <- ggplot(data, aes(x = y.mean)) +
    geom_histogram(binwidth = 1 / length(item.names), fill = "gray") +
    theme_bw() +
    geom_vline(xintercept = y_m, color = "red") +
    geom_vline(xintercept = y_m - y_sd, color = "red", linetype = "dashed") +
    ggtitle(title_obj) +
    xlab("Subscale score") +
    ylab("Number of responses")

  # Extract item response data
  y.data <- data[, ind]

  # Compute Cronbach's alpha
  alpha.data <- psych::alpha(y.data)

  # Compute more reliability statistics
  reliability.data <- psych::reliability(y.data)

  # Fit IRT model
  if (IRTpackage == "ltm") {
    grm.y <- ltm::grm(y.data)
    ICC.y <- plot(grm.y, ask = FALSE)
    TIF.y <- plot(grm.y, type = "IIC", items = 0, zrange = c(-4, 4), ask = FALSE)
    IIC.y <- plot(grm.y, type = "IIC", zrange = c(-4, 4), ask = FALSE)
    y.info <- NorseResearch::info(grm.y, printAuto = FALSE)
  } else if (IRTpackage == "mirt") {
    grm.y <- mirt::mirt(y.data, 1, itemtype = "graded", verbose = FALSE)
    # message("Checking model fit structure:")
    # print(class(grm.y))  # Should be "SingleGroupClass"
    # print(isS4(grm.y))   # Should return TRUE
    # str(grm.y)           # Check internal structure
    # flush.console()

    ICC.y <- ggplot_icc_plot(grm.y,
                             scale.name = scale.name)
    TIF.y <- ggplot_information_plot(grm.y,
                                     plot_type = "test",
                                     scale.name = scale.name)
    IIC.y <- ggplot_information_plot(grm.y,
                                     plot_type = "item",
                                     scale.name = scale.name)
    if (print.now) {
      print(ICC.y)
      print(TIF.y)
      print(IIC.y)
      # message("2")
    }
    # message("3")

    y.info <- NorseResearch::info(grm.y, printAuto = FALSE)  # Extract item information
    # message("4")

  } else {
    stop("Invalid IRT package specified. Use 'ltm' or 'mirt'.")
  }

  # Compute frequency tables and polychoric correlations
  item_freq_tables <- lapply(y.data, table)
  item_cor <- psych::polychoric(y.data)$rho

  # create a table for the item text
  item_text <- lookup_item(item.names, version = version)

  # can provide the percent of people who have missing data on the non-trigger items
  pct_open_scale <- 1- max(alpha.data$response.freq %>%
                             data.frame() %>%
                             pull(miss))

  # Return results
  result <- list(
    new_data    = y.data,
    histogram   = hist.y,
    min         = y_min,
    max         = y_max,
    zmin        = y_zmin,
    zmax        = y_zmax,
    floor       = y_pct_floor,
    ceiling     = y_pct_ceiling,
    alpha       = alpha.data,
    reliability = reliability.data,
    grm         = grm.y,
    ICC         = ICC.y,
    TIF         = TIF.y,
    IIC         = IIC.y,
    printed     = print.now,
    info        = y.info,
    tables      = item_freq_tables,
    cor         = round(item_cor, 2),
    scale_name  = scale.name,  # optionally, store the scale name as part of the object
    items       = item.names,  # optionally, store the item names as part of the object
    item_text   = item_text,
    pct_open_scale = pct_open_scale  # percent of people with missing data on non-trigger items
  )
  class(result) <- "scale_analysis2"  # assign custom class
  return(result)
}


#' Print Method for scale_analysis2 Objects
#'
#' This function provides a concise textual summary for objects produced by
#' \code{scale_analysis2()}. It displays key descriptive statistics, reliability
#' measures, polychoric correlations, and frequency tables. It does not attempt
#' to print the graphical elements.
#'
#' @param x An object of class \code{scale_analysis2} returned by \code{scale_analysis2()}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns the object \code{x}.
#'
#' @examples
#' \dontrun{
#'   result <- scale_analysis2("Example Scale", c("Q1", "Q2", "Q3"), data, IRTpackage = "mirt")
#'   print(result)
#' }
#'
#' @export
#' @method print scale_analysis2
print.scale_analysis2 <- function(x, ...) {
  cat("Scale Analysis Results\n")
  cat("----------------------\n")
  cat("Scale Name: ", x$scale_name, "\n\n")

  # make a table with item name and English text
  item_table <- data.frame(item = x$items,
                           item_text = x$item_text)
  # print the item names and text
  print(item_table)

  cat("\nDescriptive Statistics:\n")
  cat("  Min: ", x$min, "\n")
  cat("  Max: ", x$max, "\n")
  cat("  Standardized Min: ", x$zmin, "\n")
  cat("  Standardized Max: ", x$zmax, "\n")
  cat("  Floor effect: ", x$floor, "\n")
  cat("  Ceiling effect: ", x$ceiling, "\n")
  cat("  Percent of people opening scale (estimated): ", x$pct_open_scale, "\n\n")

  cat("Reliability Statistics:\n")
  # print(x$alpha)
  print(x$reliability)

  cat("\nPolychoric Correlation Matrix:\n")
  print(x$cor)

  cat("\nMarginal Information Share:\n")
  print(x$info)

  cat("\nItem Frequency Tables:\n")
  print(x$tables)

  # Do not attempt to print the plots here.
  invisible(x)
}

#' Plot Method for scale_analysis2 Objects
#'
#' This method displays the graphical outputs contained in an object of class
#' \code{scale_analysis2} returned by \code{scale_analysis2()}. It prints the histogram,
#' item characteristic curves (ICC), test information function (TIF), and item information curves (IIC).
#'
#' @param x An object of class \code{scale_analysis2} returned by \code{scale_analysis2()}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns the object \code{x}.
#'
#' @examples
#' \dontrun{
#'   result <- scale_analysis2("Example Scale", c("Q1", "Q2", "Q3"), data, IRTpackage = "mirt")
#'   plot(result)
#' }
#'
#' @export
#' @method plot scale_analysis2
plot.scale_analysis2 <- function(x, ...) {
  # Print the histogram
  if (!is.null(x$histogram)) {
    print(x$histogram)
  }
  # Print the ICC plot
  if (!is.null(x$ICC)) {
    print(x$ICC)
  }
  # Print the test information function plot
  if (!is.null(x$TIF)) {
    print(x$TIF)
  }
  # Print the item information curves plot
  if (!is.null(x$IIC)) {
    print(x$IIC)
  }
  invisible(x)
}
