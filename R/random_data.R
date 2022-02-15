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



### modeling a whole data set
# need a model for item responses (done with random)
# patient-level variables we need:
# A model for patient length of treatment/number of observations
# A model for time between observations (interval)
# A model for other variables (e.g., gender/age)
random_person_generator <- function(num = 2,  # num people
                                    mean_obs = 5.79,  # grand mean number of observations per person
                                    mean_interval, # grand mean time distance bw obs
                                    sd_obs,  # SD of number of observations to use
                                    sd_interval,  # sd of intervals to use
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
                 pt_first_data = sample(seq(as.Date("2017-11-13 11:30:51 UTC"),
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
# A model for missing data (tough!)

# model for treatment numbers (tough!)
