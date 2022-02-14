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
