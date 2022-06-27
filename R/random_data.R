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
  item_names <- item_names_nf2
  item_values <- random_item_vector(length(item_names) * num_dates)
  item_values_mat <- matrix(item_values,
                            nrow = num_dates)

  item_df <- suppressMessages(data.frame(item_values_mat)) %>%
    setNames(item_names)

  bind_cols(date_df, item_df) %>%
    as_tibble() %>%
    # set_names(var_names) %>%
    dplyr::select(anon_id, date, everything())
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
random_norse_data <- function(num_ppl,
                              ...){
  pt_data <- random_person_generator(num = num_ppl)
  assess_data <- random_tx_generator(num_obs = pt_data$pt_total_obs,
                                     first_date = pt_data %>%
                                       group_by(anon_id) %>%
                                       slice(1) %>%
                                       pull(pt_first_date),
                                     num_pts = num_ppl) %>%
    random_assessment_generator()

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

