# fake data simulation

# need a way to make new fake data like (but not really like) actual NF data.

# could model this as totally random item responses for now, say




#' Item response RNG
#'
#' @return A single number, 1:7
#' @export
#'
#' @examples
#' random_item_response()
random_item_response <- function(){sample(1:7, 1)}

#' Random item responses as a vector
#'
#' @param num Numeric. How many items?
#' @param range Vector. Defaults to 1:7
#' @param replace Logical. Should observations be repeatable? Defaults to TRUE.
#'
#' @return a vector of length `num`
#' @export
#'
#' @examples
#' random_item_vector(20)
random_item_vector <- function(num = 1,
                               range = 1:7,
                               replace = TRUE){
  sample(range,
         size = num,
         replace = replace)
}
# random_item_vector(5)


### modeling a whole data set
# need a model for item responses (done with random)
# patient-level variables we need:
# A model for patient length of treatment/number of observations
# A model for time between observations (interval)
# A model for other variables (e.g., gender/age)

#' Generate a set of random patient variables of arbitrary length
#'
#' @param num Integer. Number of people.
#' @param mean_obs Numeric. Defines a typical number of observations per person. Is randomly altered.
#' @param gender Character. Defaults to NULL, and is replaced.
#' @param birthyear Numeric. Defaults to NULL, and is replaced
#' @param tx_focus Character. Defaults to NULL, and is replaced
#' @param in_or_out Character. Defaults to NULL, and is replaced
#' @param ... Additional parameters
#'
#' @return Something, I am not sure what
#' @export
#'
#' @examples
#' random_person_generator(100)
random_person_generator <- function(num = 2,  # num people
                                    mean_obs = 5.79,  # grand mean number of observations per person
                                    gender = NULL,
                                    birthyear = NULL,
                                    tx_focus = NULL,
                                    in_or_out = NULL,
                                    ...){
  # replace null defaults with random ones
  # makes a new vector for each variable.
  if(is.null(gender)){gender <- sample(c("male", "female"),
                                       size = num, replace = TRUE)
  }
  if(is.null(birthyear)){birthyear <- sample(1936:2003,
                                             size = num,
                                             replace = TRUE)
  }
  if(is.null(tx_focus)){tx_focus <- sample(c("Sub", "MH"),
                                             size = num,
                                             replace = TRUE)
  }
  if(is.null(in_or_out)){in_or_out <- sample(c("Inpatient", "Outpatient"),
                                           size = num,
                                           replace = TRUE)
  }
  # note: if gender or birthyear are supplied, they  should  be either
  # size 1 or the same size as num. Otherwise will not work. Spits error.

  tibble::tibble(anon_id = 1:num,
                 pt_first_date = sample(seq(as.Date("2017-11-13 11:30:51 UTC"),
                                            as.Date('2021-05-10 08:19:11 UTC'),
                                            by="day"),
                                        num,
                                        replace = TRUE),
                 pt_total_obs = rpois(num, mean_obs - 1) + 1,
                 tx_focus = tx_focus,
                 in_or_out = in_or_out,
                 gender = gender,
                 birthyear = birthyear)
}

# random_person_generator(100)

# observation level variables:
# item responses for all NF items
# A model for missing data (tough!)
# Date variable: after the first per person

# still needs to properly treat the treatment needs variables.


#' Generate random NF-like data
#'
#' @param date_df A data.frame if provided. Defaults to NULL, and then is rewritten.
#' @param num_dates Optional. Number of assessments to produce.
#' @param version Character. NF version (`"2"` or `"3"`) to generate.
#' @param include_98 Logical. Include `-98` (not relevant) sentinel responses.
#' @param include_99 Logical. Include `-99` (prefer not to answer) sentinel responses.
#' @param sentinel_probability Numeric. Probability that an administered item is
#'   replaced by a requested sentinel value.
#' @param trigger_scales Logical. When `TRUE`, non-trigger items are `NA` when
#'   their scale's trigger response does not meet its opening threshold.
#' @param nf3_trigger_threshold Numeric NF3 opening threshold. NF2 uses the
#'   thresholds in `nf2.1.logic`.
#' @param ... Additional parameters
#'
#' @return You have to find out
#' @export
#'
#' @examples
#' random_assessment_generator()
#' random_assessment_generator(Sys.Date())
#' random_assessment_generator(date_df = tibble("date" = c(Sys.Date(), Sys.Date() +1)))
#' random_assessment_generator(num_dates = 3)
random_assessment_generator <- function(date_df = NULL,
                                        num_dates = NULL,
                                        version = "2",
                                        include_98 = FALSE,
                                        include_99 = FALSE,
                                        sentinel_probability = 0.05,
                                        trigger_scales = FALSE,
                                        nf3_trigger_threshold = 4.5,
                                        ...){
  # if no value given, use today to have something
  if(is.null(date_df)){date_df <- data.frame("date" = Sys.Date(),
                                             "anon_id" = 1)}
  #else if only a single value provided, convert that to an appropriate df
  else if(is.atomic(date_df)){
    date_val <- as.Date(date_df)
    date_df <- data.frame("date" = date_val,
                          "anon_id" = 1)}
  else if(is.data.frame(date_df)){
    if(!"anon_id" %in% names(date_df)){
      date_df$anon_id <- 1
    }
  }

  if(is.null(num_dates)){num_dates <- nrow(date_df)}
  version <- .validate_random_nf_versions(version)
  if (length(version) != 1) {
    stop("`random_assessment_generator()` creates one version at a time; use `random_norse_data()` for mixed versions.", call. = FALSE)
  }
  item_df <- .random_nf_items(
    versions = rep(version, num_dates),
    include_98 = include_98,
    include_99 = include_99,
    sentinel_probability = sentinel_probability,
    trigger_scales = trigger_scales,
    nf3_trigger_threshold = nf3_trigger_threshold
  )

  bind_cols(date_df, item_df) %>%
    as_tibble() %>%
    # set_names(var_names) %>%
    dplyr::select(anon_id, date, everything())
}

# Validate version input used by the synthetic-data generators.
.validate_random_nf_versions <- function(versions) {
  versions <- unique(as.character(versions))
  if (!length(versions) || !all(versions %in% c("2", "3"))) {
    stop("`versions` must contain one or both of '2' and '3'.", call. = FALSE)
  }
  sort(versions)
}

# Generate the item portion of an assessment.  Columns from inactive versions
# remain NA, which mirrors a combined raw export.
.random_nf_items <- function(versions,
                             include_98,
                             include_99,
                             sentinel_probability,
                             trigger_scales,
                             nf3_trigger_threshold,
                             available_versions = unique(versions)) {
  if (!is.numeric(sentinel_probability) || length(sentinel_probability) != 1 ||
      is.na(sentinel_probability) || sentinel_probability < 0 || sentinel_probability > 1) {
    stop("`sentinel_probability` must be one number between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(nf3_trigger_threshold) || length(nf3_trigger_threshold) != 1 ||
      is.na(nf3_trigger_threshold) || nf3_trigger_threshold < 1 || nf3_trigger_threshold > 7) {
    stop("`nf3_trigger_threshold` must be one number between 1 and 7.", call. = FALSE)
  }

  available_versions <- .validate_random_nf_versions(available_versions)
  metadata <- list(
    "2" = nf2.1.item.descriptions,
    "3" = NF3.1_items
  )
  all_items <- unique(unlist(lapply(metadata[available_versions], `[[`, "item"), use.names = FALSE))
  out <- as.data.frame(matrix(NA_real_, nrow = length(versions), ncol = length(all_items)))
  names(out) <- all_items

  for (row in seq_along(versions)) {
    item_info <- metadata[[versions[[row]]]]
    active_items <- item_info$item
    responses <- stats::runif(length(active_items))
    out[row, active_items] <- floor(responses * 7) + 1

    if (trigger_scales) {
      trigger_scales_here <- unique(item_info$simple_scale[which(item_info$trigger %in% TRUE)])
      for (scale in trigger_scales_here) {
        scale_rows <- which(!is.na(item_info$simple_scale) & item_info$simple_scale == scale)
        scale_items <- item_info$item[scale_rows]
        trigger_item <- item_info$item[scale_rows[item_info$trigger[scale_rows] %in% TRUE]]
        if (length(trigger_item) != 1) next

        threshold <- nf3_trigger_threshold
        if (identical(versions[[row]], "2")) {
          threshold_match <- match(trigger_item, nf2.1.logic$trigger_item)
          if (!is.na(threshold_match)) threshold <- nf2.1.logic$trigger_val[[threshold_match]]
        }
        if (out[row, trigger_item] <= threshold) {
          out[row, setdiff(scale_items, trigger_item)] <- NA_real_
        }
      }
    }

    observed <- active_items[!is.na(out[row, active_items])]
    if (length(observed) && (include_98 || include_99)) {
      sentinel_draw <- stats::runif(length(observed))
      sentinel_values <- c(if (include_98) -98, if (include_99) -99)
      selected <- observed[sentinel_draw < sentinel_probability]
      if (length(selected)) {
        out[row, selected] <- sample(sentinel_values, length(selected), replace = TRUE)
      }
    }
  }

  tibble::as_tibble(out)
}
# # tests:
# random_assessment_generator()
# random_assessment_generator(Sys.Date())
# # doesn't assign an id variable, gets one back
# random_assessment_generator(date_df = tibble("date" = c(Sys.Date(), Sys.Date() +1)))
# random_assessment_generator(num_dates = 3)

# this  just takes a date and a length of treatment and returns a series of ordered
# dates as a tibble column including the first date as the first row.

#' Generate an entire treatment course, possibly given several specific parameters
#'
#' @param num_obs Vector. Number of observations per person. Can be a vector.
#' @param num_pts Integer. Number of patients.
#' @param first_date Vector of dates with length = num_pts.
#' @param identifiers Vector. Optional names for individuals.
#' @param tx_days Numeric. Defaults to NULL and is rewritten. Defines typical length of treatment.
#' @param ... Additional parameters.
#'
#' @return A data frame probably
#' @export
#'
#' @examples
#' random_tx_generator(4)
#' random_tx_generator(num_obs = c(3, 5))
#' random_tx_generator(num_obs = 5, num_pts = 3)
#' random_tx_generator(1:3, first_date = c(as.Date("2017-11-20"),
#'                                       as.Date("2017-03-20"),
#'                                       as.Date("2017-12-20")))
#' random_tx_generator(num_obs = c(1, 4, 2),
#'                     first_date = c(as.Date("2017-11-20"),
#'                                         as.Date("2017-03-20"),
#'                                         as.Date("2017-12-20")),
#'                     num_pts = 3)
random_tx_generator <- function(num_obs,
                                num_pts = length(num_obs),
                                first_date = rep(as.Date("2017-11-13 11:30:51 UTC"), num_pts),
                                identifiers = 1:num_pts,
                                tx_days = NULL,
                                ...){ # a typical sd of tx_length in weeks
  # assume num_obs could be any length vector
  # works if equal to the number of patients or length 1 (repeated value)

  # generate length of treatment
  if(is.null(tx_days)) {
    tx_days <- rpois(num_pts, 21*7) - sample(0:6,
                                             num_pts,
                                             replace = TRUE)  # add a random draw from a "typical" max tx length distribution
    # subtract 0-6 days at random
  }
  # setting maximum date is a little weird
  max_date <- as.Date(first_date + tx_days)

  id_var <- rep(identifiers, times = num_obs)

  date_seq <- c(first_date)

  if(num_pts != length(num_obs)){
    if(length(num_obs == 1)){
      num_obs <- rep(num_obs, num_pts)
    }
  }

  for(i in 1:num_pts){
    date_seq_i <- append(first_date[i],
                       sort(sample(seq(first_date[i] + 1,
                                       max_date[i],
                                       by="day"),
                                   num_obs[i] - 1,
                                   replace = FALSE)))
    date_seq <- append(date_seq, date_seq_i)
  }
  date_seq <- date_seq[(num_pts + 1):length(date_seq)]

  # generate data
  # export a data frame
  tibble("anon_id" = id_var,
    "date" = date_seq) %>%
    arrange(anon_id, date)
}
# random_tx_generator(4)
# random_tx_generator(num_obs = c(3, 5))
# # need this to work seamlessly:
# random_tx_generator(num_obs = 5, num_pts = 3)
# random_tx_generator(1:3, first_date = c(as.Date("2017-11-20"),
#                                       as.Date("2017-03-20"),
#                                       as.Date("2017-12-20")))
# random_tx_generator(num_obs = c(1, 4, 2),
#                     first_date = c(as.Date("2017-11-20"),
#                                         as.Date("2017-03-20"),
#                                         as.Date("2017-12-20")),
#                     num_pts = 3)

# what happens when given a series of patients?
# model for treatment numbers (tough!)

# combining them
# random_assessment_generator(random_tx_generator(10))
#
# testdata_assess <- random_assessment_generator(random_tx_generator(num_obs = c(1, 4, 2),
#                     first_date = c(as.Date("2017-11-20"),
#                                    as.Date("2017-03-20"),
#                                    as.Date("2017-12-20")),
#                     num_pts = 3))

# testdata_pt <- random_person_generator(3)
# random_tx_generator(num_obs = testdata_pt$pt_total_obs[1], first_date = testdata_pt$pt_first_date[1])
#
# random_assessment_generator(random_tx_generator(num_obs = testdata_pt$pt_total_obs[1],
#                                                 first_date = testdata_pt$pt_first_date[1]))
#
# random_tx_generator(num_obs = 1:3)

# join patient data and assessment data
# left_join(testdata_pt, testdata_assess, by = "anon_id")

# simplest possible wrapper>
# want to require the least possible information at the time of calling


#' Create a synthetic data set of NF-like data
#'
#' @param num_ppl Numeric. Number of patients.
#' @param versions Character vector containing `"2"`, `"3"`, or both. With
#'   both versions, each patient's NF2 assessments precede their NF3 assessments.
#' @param include_98 Logical. Include `-98` (not relevant) sentinel responses.
#' @param include_99 Logical. Include `-99` (prefer not to answer) sentinel responses.
#' @param sentinel_probability Numeric. Probability that an administered item is
#'   replaced by a requested sentinel value.
#' @param trigger_scales Logical. When `TRUE` (the default), non-trigger items
#'   are `NA` when their scale's trigger response does not meet its opening threshold.
#' @param nf3_trigger_threshold Numeric NF3 opening threshold. NF2 uses the
#'   thresholds in `nf2.1.logic`.
#' @param version_variable Character. Name of the generated version column.
#' @param num_obs Optional positive integer (or one per patient) giving the
#'   number of observations. By default it is generated per patient.
#' @param ... Additional parameters
#'
#' @seealso \code{\link{random_assessment_generator}}, \code{\link{random_person_generator}},
#' \code{\link{random_tx_generator}}
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' random_norse_data(2, num_obs = c(3, 2))
#' random_norse_data(2, num_obs = 5)
#' random_norse_data(2)
#' random_norse_data(20, versions = c("2", "3"), include_98 = TRUE, include_99 = TRUE)
random_norse_data <- function(num_ppl,
                              versions = "2",
                              include_98 = FALSE,
                              include_99 = FALSE,
                              sentinel_probability = 0.05,
                              trigger_scales = TRUE,
                              nf3_trigger_threshold = 4.5,
                              version_variable = "Ver_10",
                              num_obs = NULL,
                              ...){
  versions <- .validate_random_nf_versions(versions)
  if (!is.character(version_variable) || length(version_variable) != 1 ||
      is.na(version_variable) || !nzchar(version_variable)) {
    stop("`version_variable` must be one non-empty column name.", call. = FALSE)
  }

  pt_data <- random_person_generator(num = num_ppl, ...)
  if (!is.null(num_obs)) {
    if (!is.numeric(num_obs) || any(is.na(num_obs)) || any(num_obs < 1) ||
        any(num_obs != as.integer(num_obs)) || !(length(num_obs) %in% c(1, num_ppl))) {
      stop("`num_obs` must be one positive integer or one per patient.", call. = FALSE)
    }
    pt_data$pt_total_obs <- rep(as.integer(num_obs), length.out = num_ppl)
  }
  assess_data <- random_tx_generator(num_obs = pt_data$pt_total_obs,
                                     first_date = pt_data %>%
                                       group_by(anon_id) %>%
                                       slice(1) %>%
                                       pull(pt_first_date),
                                     num_pts = num_ppl)

  # A mixed export has a single rollout: within every patient, all NF2 rows
  # precede all NF3 rows. A patient may consequently have only one version.
  assess_data[[version_variable]] <- unlist(lapply(
    split(seq_len(nrow(assess_data)), assess_data$anon_id),
    function(rows) {
      if (length(versions) == 1) return(rep(versions, length(rows)))
      n_nf2 <- sample.int(length(rows) + 1, 1) - 1
      c(rep("2", n_nf2), rep("3", length(rows) - n_nf2))
    }
  ), use.names = FALSE)

  item_data <- .random_nf_items(
    versions = assess_data[[version_variable]],
    include_98 = include_98,
    include_99 = include_99,
    sentinel_probability = sentinel_probability,
    trigger_scales = trigger_scales,
    nf3_trigger_threshold = nf3_trigger_threshold,
    available_versions = versions
  )
  assess_data <- dplyr::bind_cols(assess_data, item_data)

  left_join(pt_data, assess_data,
            by = "anon_id") %>%
    select(anon_id,
           date,
           everything())
}
#
# random_norse_data(2, num_obs = c(3, 2))
# random_norse_data(2, num_obs = 5)
# random_norse_data(2)
