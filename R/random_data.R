# fake data simulation

# need a way to make new fake data like (but not really like) actual NF data.

# could model this as totally random item responses for now, say


item_names_nf2 <- names(NORSEpkg::hf.scored.2019)[grepl("Q", names(NORSEpkg::hf.scored.2019))]


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
#' @param num
#' @param range
#' @param replace
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
random_item_vector(5)


### modeling a whole data set
# need a model for item responses (done with random)
# patient-level variables we need:
# A model for patient length of treatment/number of observations
# A model for time between observations (interval)
# A model for other variables (e.g., gender/age)
random_person_generator <- function(num = 2,  # num people
                                    mean_obs = 5.79,  # grand mean number of observations per person
                                    gender = NULL,
                                    birthyear = NULL,
                                    tx_focus = NULL,
                                    in_or_out = NULL){
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

random_person_generator(100)

# observation level variables:
# item responses for all NF items
# A model for missing data (tough!)
# Date variable: after the first per person


random_assessment_generator <- function(date_df = NULL,
                                        num_dates = NULL){
  # if no value given, use today to have something
  if(is.null(date_df)){date_df <- data.frame("date" = Sys.Date())}
  #else if only a single value provided, convert that to an appropriate df
  else if(is.atomic(date_df)){
    date_val <- as.Date(date_df)
    date_df <- data.frame("date" = date_val)}

  if(is.null(num_dates)){num_dates <- nrow(date_df)}
  item_names <- item_names_nf2
  item_values <- random_item_vector(length(item_names) * num_dates)
  item_values_mat <- matrix(item_values,
                            nrow = num_dates)

  item_df <- suppressMessages(data.frame(item_values_mat)) %>%
    setNames(item_names)

  bind_cols(date_df, item_df) %>%
    # set_names(var_names) %>%
    dplyr::select(date, everything())
}
# tests:
random_assessment_generator()
random_assessment_generator(Sys.Date())
random_assessment_generator(date = tibble("date" = c(Sys.Date(), Sys.Date() +1)))
random_assessment_generator(num_dates = 3)

# this  just takes a date and a length of treatment and returns a series of ordered
# dates as a tibble column including the first date as the first row.
random_tx_generator <- function(num_obs = 1,
                                num_pts = length(num_obs),
                                first_date = rep(as.Date("2017-11-13 11:30:51 UTC"), num_pts),
                                identifiers = 1:num_pts,
                                tx_days = NULL){ # a typical sd of tx_length in weeks
  # define behavior for the case when more than 1 patient is provided.
  # assume num_obs could be any length vector

  # generate length of treatment
  if(is.null(tx_days)) {
    tx_days <- rpois(num_pts,
                     21*7 - sample(0:6, num_pts))  # add a random draw from a "typical" max tx length distribution
    # subtract 0-6 days at random
  }
  # print("first_date is")
  # print(first_date)
  # print(identifiers)
  # setting maximum date is a little weird
  max_date <- as.Date(first_date + tx_days)

  # print(max_date)

  id_var <- rep(identifiers, times = num_obs)

  # print(id_var)

  date_seq <- c(first_date)


  for(i in 1:num_pts){
    date_seq_i <- append(first_date[i],
                       sort(sample(seq(first_date[i] + 1,
                                       max_date[i],
                                       by="day"),
                                   num_obs[i] - 1,
                                   replace = FALSE)))
    # print(date_seq_i)
    date_seq <- append(date_seq, date_seq_i)
  }
  # print(date_seq)
  date_seq <- date_seq[(num_pts + 1):length(date_seq)]
  # print(date_seq)
  # date_seq <- append(first_date,
  #                    sort(sample(seq(first_date+1,
  #                             max_date,
  #                             by="day"),
  #                         num_obs-1,
  #                         replace = FALSE)))

  # generate data
  # export a data frame
  tibble("id" = id_var,
    "date" = date_seq)
}
random_tx_generator(4)
random_tx_generator(num_obs = c(3, 5))
random_tx_generator(1:3, first_date = c(as.Date("2017-11-20"),
                                      as.Date("2017-03-20"),
                                      as.Date("2017-12-20")))
random_tx_generator(num_obs = c(1, 4, 2),
                    first_date = c(as.Date("2017-11-20"),
                                        as.Date("2017-03-20"),
                                        as.Date("2017-12-20")),
                    num_pts = 3)

# what happens when given a series of patients?
# model for treatment numbers (tough!)


random_assessment_generator(random_tx_generator(10))

testdata_pt <- random_person_generator(3)
random_tx_generator(num_obs = testdata_pt$pt_total_obs[1], first_date = testdata_pt$pt_first_date[1])

random_assessment_generator(random_tx_generator(num_obs = testdata_pt$pt_total_obs[1],
                                                first_date = testdata_pt$pt_first_date[1]))

random_tx_generator(num_obs = 1:3)
