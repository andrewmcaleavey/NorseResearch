# Scoring functions
#
# These functions help with scoring the NORSE, and I don't want to keep copying them.

# convert -98s to 1
# experimental. How do the scales work if we take all the -98s,
# which are "not relevant for me" and make them 1?


# REVERSE SCORING
# The first step on raw data, if there are no missing.
#

#' Reverse score NORSE items (1-7).
#'
#' \code{rev_score} takes a vector and returns its 1-7 revers
#'
#' @param x A vector of item responses.
#' @return A reverse-coded vector of \code{x}.
#' @examples
#' data <- data_frame(Q27 = c(1, 2, 2, 1), Q28 = c(7, 6, 6, 7))
#' mutate(data, Q27 = rev_score(Q27))
rev_score <- function(x){
  x * -1 + 8
}

# REVERSING NORSE 2.0 Items
rev_score_NORSE2 <- function(data) {mutate(data,  # note: rev_score() is defined above
                  Q27 = rev_score(Q27),   Q131 = rev_score(Q131),
                  Q140 = rev_score(Q140), Q40 = rev_score(Q40),
                  Q15 = rev_score(Q15),   Q132 = rev_score(Q132),
                  Q10 = rev_score(Q10),   Q62 = rev_score(Q62),
                  Q135 = rev_score(Q135), Q50 = rev_score(Q50),
                  Q134 = rev_score(Q134), Q109 = rev_score(Q109),
                  Q133 = rev_score(Q133), Q154 = rev_score(Q154),
                  Q80 = rev_score(Q80),   Q108 = rev_score(Q108),
                  Q136 = rev_score(Q136), Q155 = rev_score(Q155),
                  Q138 = rev_score(Q138), Q84 = rev_score(Q84),
                  Q139 = rev_score(Q139), Q82 = rev_score(Q82),
                  Q67 = rev_score(Q67),   Q43 = rev_score(Q43),
                  # alliance items should be reversed too (8 Sept 2022)
                  Q11  = rev_score(Q11),
                  Q12  = rev_score(Q12),
                  Q13  = rev_score(Q13),
                  Q14  = rev_score(Q14))
}


# SCORING FUNCTIONS  -----------------

# This function scores by taking mean of all items, and for cases with
# missing values will still return the mean. That includes cases with the Trigger only.

#' Scoring NORSE scales without losing subthreshold scores to NA
#'
#' \code{score_NORSE_trigger} provides scores for NORSE subscales without
#' trashing missing values when trigger not met
#'
#' @param dat a data_frame.
#' @param vars variables that make the scale, defaults to all in \code{dat}.
#' Can use with e.g., "somAnx.names".
#' @param trigger the trigger item. Default uses \code{lookup_trigger_among} with
#' \code{vars} but this can be overwritten. This will use the NF v2.1 Triggers
#' by default.
#'
#' Inconsequential in present implementation.
#'
#' @examples
#' test_data <- data.frame(Q1 = c(1, 2, 3, 5, 3, 2, NA, NA),
#'                         Q2 = c(3, 4, 2, 5, 2, NA, 1, NA),
#'                         Q3 = c(4, 3, 5, 2, NA, NA, NA, NA))
#'
#' score_NORSE_trigger(dat = test_data, vars = c("Q1", "Q2", "Q3"))
#'
#' # NOTE use of (.) in pipes
#' test_data %>% mutate(subscale_score = score_NORSE_trigger(.))
#' test_data %>% mutate(subscale_score = score_NORSE_trigger(., trigger = "Q3"))
#'
#' # Can score other versions of NF using options directly in trigger parameter:
#' score_NORSE_trigger(dat = test_data, vars =)
score_NORSE_trigger <- function(dat,
                                vars = names(dat),
                                trigger = lookup_trigger_among(vars)){
  # dat is a data.frame
  # vars is a vector of variable names (e.g., somAnx.names)
  # trigger is the trigger item, defaults to use the identified trigger in
  # `item_descriptions` through convenience function `find_trigger_among()`.
  dat <- dplyr::select(dat, vars) %>%
    transmute(score = rowMeans(., na.rm = TRUE)) %>%
    mutate(score = ifelse(is.nan(score), NA, score))
  return(dat$score)
}

#' Identify the trigger item for a named scale
#'
#' @param scale The name of the scale to look up. Must be one of the NF2 coding names
#'
#' @return item name of the trigger item, e.g., "Q51"
#'
#' @examples
#' find_trigger("somAnx")  # "Q51"
#'
#' # Can be used in concert with get_item_text():
#' get_item_text(find_trigger("somAnx"))
#' # "I have a sense of restlessness and unease in me most of the time"
find_trigger <- function(scaleName){
  .Deprecated("lookup_trigger")
  item_descriptions %>%
    dplyr::filter(scale == scaleName) %>%
    dplyr::filter(trigger == TRUE) %>%
    dplyr::select(item) %>%
    pull()
}

#' Identify which of the items is a trigger item
#'
#' @param items A vector of quoted item names, e.g., somAnx.names
#'
#' @return The item name that is identified as a trigger in `item_descriptions`.
#'
#' @examples
#' find_trigger_among(somAnx.names)
find_trigger_among <- function(items){
  item_descriptions %>%
    dplyr::filter(item %in% items) %>%
    dplyr::filter(trigger == TRUE) %>%
    dplyr::select(item) %>%
    pull()
}

#### SCORING ALL SUBSCALES
# Hard- coding a way to score all subscales at once


#' Scoring All NORSE scales without losing subthreshold scores to NA
#'
#' \code{score_all_NORSE2} provides scores for ALL NORSE subscales without
#' trashing missing values when trigger not met.
#'
#' @param dat a data_frame.
#' @param process_vars Logical. Should process variables (Alliance and
#' Expressed Needs) be computed. Defaults to TRUE.
#'
#' @return a data_frame with new subscale values
#'
#' @export
#'
#' @details
#'
#' Provides the following scale scores:
#' - Cognitive problems: \code{cog}
#' - Control: \code{control}
#' - etc.
#'
#' @examples
#' test_out <- score_all_NORSE2(data2017_18)
score_all_NORSE2 <- function(dat, process_vars = TRUE){
  if(!check_rev(dat)){
    warning("DATA may not be properly scored, check reversing and NA values!!!")
  }

  if(process_vars){
    dat <- ungroup(dat) %>%
      mutate(dat, cog = score_NORSE_trigger(dat, cog.names),
                  control = score_NORSE_trigger(dat, control.names),
                  eating = score_NORSE_trigger(dat, eating.names),
                  genFunc = score_NORSE_trigger(dat, genFunc.names),
                  hopeless = score_NORSE_trigger(dat, hopeless.names),
                  internal = score_NORSE_trigger(dat, internal.names),
                  irritable = score_NORSE_trigger(dat, irritable.names),
                  ready = score_NORSE_trigger(dat, ready.names),
                  recovEnv = score_NORSE_trigger(dat, recovEnv.names),
                  sad = score_NORSE_trigger(dat, sad.names),
                  selfCrit = score_NORSE_trigger(dat, selfCrit.names),
                  avoidSit = score_NORSE_trigger(dat, avoidSit.names),
                  avoidSoc = score_NORSE_trigger(dat, avoidSoc.names),
                  socialSafety = score_NORSE_trigger(dat, socialSafety.names),
                  somAnx = score_NORSE_trigger(dat, somAnx.names),
                  subRecov = score_NORSE_trigger(dat, subRecov.names),
                  subUse = score_NORSE_trigger(dat, subUse.names),
                  suicide = score_NORSE_trigger(dat, suicide.names),
                  trauma = score_NORSE_trigger(dat, trauma.names),
                  worry = score_NORSE_trigger(dat, worry.names),
                  alliance = score_NORSE_trigger(dat, alliance.names),
                  needs = score_NORSE_trigger(dat, needs.names)
    )
  } else {
    dat <- mutate(dat,
                  cog          = score_NORSE_trigger(dat, cog.names),
                  control      = score_NORSE_trigger(dat, control.names),
                  eating       = score_NORSE_trigger(dat, eating.names),
                  genFunc      = score_NORSE_trigger(dat, genFunc.names),
                  hopeless     = score_NORSE_trigger(dat, hopeless.names),
                  internal     = score_NORSE_trigger(dat, internal.names),
                  irritable    = score_NORSE_trigger(dat, irritable.names),
                  ready        = score_NORSE_trigger(dat, ready.names),
                  recovEnv     = score_NORSE_trigger(dat, recovEnv.names),
                  sad          = score_NORSE_trigger(dat, sad.names),
                  selfCrit     = score_NORSE_trigger(dat, selfCrit.names),
                  avoidSit     = score_NORSE_trigger(dat, avoidSit.names),
                  avoidSoc     = score_NORSE_trigger(dat, avoidSoc.names),
                  socialSafety = score_NORSE_trigger(dat, socialSafety.names),
                  somAnx       = score_NORSE_trigger(dat, somAnx.names),
                  subRecov     = score_NORSE_trigger(dat, subRecov.names),
                  subUse       = score_NORSE_trigger(dat, subUse.names),
                  suicide      = score_NORSE_trigger(dat, suicide.names),
                  trauma       = score_NORSE_trigger(dat, trauma.names),
                  worry        = score_NORSE_trigger(dat, worry.names)
    )
  }
  # should insert some checks here to see if range is appropriate.
  if(any(c(dat$cog          < 1,
           dat$control      < 1,
           dat$eating       < 1,
           dat$genFunc      < 1,
           dat$hopeless     < 1,
           dat$internal     < 1,
           dat$irritable    < 1,
           dat$ready        < 1,
           dat$recovEnv     < 1,
           dat$sad          < 1,
           dat$selfCrit     < 1,
           dat$avoidSit     < 1,
           dat$avoidSoc     < 1,
           dat$socialSafety < 1,
           dat$somAnx       < 1,
           dat$subRecov     < 1,
           dat$subUse       < 1,
           dat$suicide      < 1,
           dat$trauma       < 1,
           dat$worry        < 1),
         na.rm = TRUE)){
    warning("Some values below 1. Scale scores not valid. Check item data.")
  }
  if(any(c(dat$cog          > 7,
           dat$control      > 7,
           dat$eating       > 7,
           dat$genFunc      > 7,
           dat$hopeless     > 7,
           dat$internal     > 7,
           dat$irritable    > 7,
           dat$ready        > 7,
           dat$recovEnv     > 7,
           dat$sad          > 7,
           dat$selfCrit     > 7,
           dat$avoidSit     > 7,
           dat$avoidSoc     > 7,
           dat$socialSafety > 7,
           dat$somAnx       > 7,
           dat$subRecov     > 7,
           dat$subUse       > 7,
           dat$suicide      > 7,
           dat$trauma       > 7,
           dat$worry        > 7),
         na.rm = TRUE)){
    warning("Some values above 7. Scale scores not valid. Check item data.")
  }
  dat
}


#' Scoring Over-Under style
#'
#' \code{score_NORSE_overunder} is a general method for scoring
#' over-under closing and opening thresholds.
#'
#' @param dat a data_frame.
#' @param vars the variables to score, defaults to all variables in \code{dat}.
#' @param trigger the trigger item, defaults to the first item in \code{dat}.
#' @param closing.thresh the closing threshold value. Default is 0.
#' @param opening.thresh the opening threshold value. Default is 0.
#'
#' @return a vector of new subscale scores
#'
#' @details
#' The principle of this scoring is that WHILE THE SCALE IS OPEN,
#' scores should be centered on the closing threshold, and
#' WHILE THE SCALE IS CLOSED, scores should be centered on the
#' opening threshold.
#'
#' Pseudocode scoring:
#' IF ONLY the trigger is completed
#'   Score is the trigger score MINUS the opening threshold
#' ELSE
#'   Score is the mean score as computed standardly,
#'   MINUS the closing threshold
#'
#' This will mean that there will be some NEGATIVE scores for open scales
#'
#' @export
#' @examples
#' test_data <- data.frame(Q1 = c(1, 2, 3, 5, 3, 2, NA, NA),
#'                         Q2 = c(3, 4, 2, 5, 2, NA, 1, NA),
#'                         Q3 = c(4, 3, 5, 2, NA, NA, NA, NA))
#'
#' score_NORSE_overunder(dat = test_data, vars = c("Q1", "Q2", "Q3"))
#'
#' # NOTE use of (.) in pipes
#' test_data %>% mutate(subscale_score = score_NORSE_trigger(.))
#' test_data %>% mutate(subscale_score = score_NORSE_trigger(., trigger = "Q3"))
score_NORSE_overunder <- function(dat,
                                  vars = names(dat),
                                  trigger = vars[1],
                                  closing.thresh = 0,
                                  opening.thresh = 0){
  # dat is a data.frame
  # vars is a vector of variable names
  # trigger is the trigger item, defaults to the first value of vars
  # closing.thresh is the closing threshold, default to 2
  # opening thresh is the opening threshold, default to 2
  dat <- dplyr::select(dat, vars)

  # values of the trigger items
  trigger_vals <- dat[, names(dat) %in% trigger][[1]]

  # raw mean scale scores
  rawscores <- rowMeans(dat, na.rm = TRUE)

  # is the trigger item the only item present?
  test_condition <- if_else(!is.na(trigger_vals) & rowSums(!is.na(dat)) == 1, 1, 0)

  # put together into data
  new_dat <- tibble(trigger_vals = trigger_vals,
                           rawscores = rawscores,
                           test_condition = test_condition)

  # These are the new scores
  new_dat <- mutate(new_dat,
                   new_scores = if_else(test_condition == 1,
                   trigger_vals - opening.thresh,
                   rawscores - closing.thresh)) %>%
    mutate(score = ifelse(is.nan(new_scores), NA, new_scores))  # replaces Nan with NA

  # returns just the score vector, don't need this format
  return(new_dat$score)
}





#' Scoring All NORSE scales OVER-UNDER
#'
#' \code{score_all_NORSE2_ou} provides scores for ALL NORSE subscales
#' in the Over-Under style.
#'
#' @param dat a data_frame.
#'
#' @return a data_frame with new subscale values in subscales
#' with names ending in "_ou".
#'
#' @details
#'
#' Provides the following scale scores:
#' - Cognitive problems: \code{cog_ou}
#' - Control: \code{control_ou}
#' - etc.
#'
#' @export
#' @examples
#' test_out <- score_all_NORSE2_ou(data2017_18)
#'
score_all_NORSE2_ou <- function(dat){
  mutate(dat,
         cog_ou = score_NORSE_overunder(dat, cog.names,
                                        opening.thresh = 4,
                                        closing.thresh = 3),
         control_ou = score_NORSE_overunder(dat, control.names,
                                            opening.thresh = 5,
                                            closing.thresh = 3),
         eating_ou = score_NORSE_overunder(dat, eating.names,
                                           opening.thresh = 3,
                                           closing.thresh = 2),
         genFunc_ou = score_NORSE_overunder(dat, genFunc.names,
                                            opening.thresh = 5,
                                            closing.thresh = 3),
         hopeless_ou = score_NORSE_overunder(dat, hopeless.names,
                                             opening.thresh = 4,
                                             closing.thresh = 3),
         internal_ou = score_NORSE_overunder(dat, internal.names,
                                             opening.thresh = 4,
                                             closing.thresh = 4),
         irritable_ou = score_NORSE_overunder(dat, irritable.names,
                                              opening.thresh = 5,
                                              closing.thresh = 3),
         ready_ou = score_NORSE_overunder(dat, ready.names,
                                          opening.thresh = 4,
                                          closing.thresh = 3),
         recovEnv_ou = score_NORSE_overunder(dat, recovEnv.names,
                                             opening.thresh = 4,
                                             closing.thresh = 3),
         sad_ou = score_NORSE_overunder(dat, sad.names,
                                        opening.thresh = 6,
                                        closing.thresh = 4),
         selfCrit_ou = score_NORSE_overunder(dat, selfCrit.names,
                                             opening.thresh = 5,
                                             closing.thresh = 3),
         avoidSit_ou = score_NORSE_overunder(dat, avoidSit.names,
                                             opening.thresh = 5,
                                             closing.thresh = 4),
         avoidSoc_ou = score_NORSE_overunder(dat, avoidSoc.names,
                                             opening.thresh = 5,
                                             closing.thresh = 4),
         socialSafety_ou = score_NORSE_overunder(dat, socialSafety.names,
                                                 opening.thresh = 6,
                                                 closing.thresh = 4),
         somAnx_ou = score_NORSE_overunder(dat, somAnx.names,
                                           opening.thresh = 5,
                                           closing.thresh = 4),
         subRecov_ou = score_NORSE_overunder(dat, subRecov.names,
                                             opening.thresh = 2,
                                             closing.thresh = 3),
         subUse_ou = score_NORSE_overunder(dat, subUse.names,
                                           opening.thresh = 2,
                                           closing.thresh = 2),
         suicide_ou = score_NORSE_overunder(dat, suicide.names,
                                            opening.thresh = 2,
                                            closing.thresh = 2),
         trauma_ou = score_NORSE_overunder(dat, trauma.names,
                                           opening.thresh = 4,
                                           closing.thresh = 3),
         worry_ou = score_NORSE_overunder(dat, worry.names,
                                          opening.thresh = 6,
                                          closing.thresh = 4),
         alliance_ou = score_NORSE_overunder(dat,
                                             alliance.names,
                                             opening.thresh = 0,
                                             closing.thresh = 0),
         needs_ou = score_NORSE_overunder(dat, needs.names,
                                          opening.thresh = 0,
                                          closing.thresh = 0)
  )
}

#########
#            NORMING
#########


# basic norm function
compute_normed <- function(x, m_bar, sd){
  (x - m_bar) / sd
}

#' Norm NF data
#'
#' @param dat A data.frame. Data set on which to add normed variables.
#' @param scale String. Which scale should be normed. Does one scale at a time.
#' @param normTable Which norm table should be used. Defaults to the 2019
#' MH outpatient first visit norms ("summary_norms_MH_out").
#' A custom normTable can be supplied, or
#' several options within the package could be developed.
#' @param inputFormat What is the scale/format of the data to be normed?
#' Defaults to "raw", which indicates scale scores will range from 1-7.
#' Alternatives may include previously normed data on Z, T, or other scales
#' if in use.
#' @param outputFormat What is the intended normalized scale? Defaults
#' to "Z", indicating mean = 0 and SD = 1. Alternatives could include T or
#' other
#'
#' @return A tibble. the input data.frame with additional variable(s) for
#' normed scores. Normed scale score variables take the name format
#' scaleName_normed_normTable_outputFormat, such as "cog_normed_MHout_Z".
#' @export
#'
#' @examples
#' testout <- score_normed_NF(hf.scored.2019, scale = "cog")
score_normed_NF <- function(dat,
                            scale,
                            normTable = summary_norms_MH_out,
                            inputFormat = "raw",
                            outputFormat = "Z"){
  # dat <- ensym(dat)
  scale <- ensym(scale)

  # short table name
  shortTableName <- ifelse(identical(normTable, summary_norms_MH_out),
                           "MHout",
                           "NONORMTABLE")

  # # create variable name
  # var_name <- paste0(scale,
  #                    "_normed_",
  #                    shortTableName,
  #                    "_",
  #                    outputFormat)

  if(length(scale) > 1) warning("Too many scales")

  normMean <- normTable[{{scale}}][1, ][[1]]
  normSD <- normTable[{{scale}}][2, ][[1]]


  dat <- dat %>%
    mutate(across({{ scale }},
                  ~ compute_normed(x = .data[[{{scale}}]],
                                   m_bar = normMean,
                                   sd = normSD),
                  .names = "{.col}_normed_{shortTableName}_{outputFormat}"))
#
  # names(dat)[names(dat) %in% "tempname"] <- "var_name"
}
# testout <- score_normed_NF(hf.scored.2019, scale = "cog")
# right now this works for one scale at a time, and doesn't
# have a lot of flexibility in the information contained in the variable name.

# but it does work, and that is good.
# it doesn't work for more than one scale at a time, however.

# testout <- score_normed_NF(hf.scored.2019, scale = scale_names)


# a function to norm the item responses themselves, then generate averaged scale
# scores
item_norm <- function(item,
                      value,
                      norm_data){
  # need to set norm_data to NorseResearch::HF_research_data_2021_fscores
  # item <- dplyr::ensym(item)
  M_item <- mean(norm_data[{{item}}], na.rm = TRUE)
  SD_item <- sd(norm_data[{{item}}], na.rm = TRUE)

  (value - M_item) / SD_item
}
# item_norm("Q142", value = 2, norm_data = NORSEpkg)


#' Score the NF3 scales
#'
#' @param dat Data
#' @param process_vars logical. Should the process variables be included?
#'
#' @return A data frame with additional variables
#' @export
#'
#' @examples
#' /dontrun{
#' score_all_nf3(HF_research_data_2021)
#' }
score_all_nf3 <- function(dat, process_vars = TRUE){
  dat <- ungroup(dat) %>%
    mutate(anger = score_NORSE_trigger(dat, anger.names.nf3),
           cog = score_NORSE_trigger(dat, cog.names.nf3),
           eating = score_NORSE_trigger(dat, eating.names.nf3),
           genFunc = score_NORSE_trigger(dat, genFunc.names.nf3),
           hopeless = score_NORSE_trigger(dat, hopeless.names.nf3),
           impulsivity = score_NORSE_trigger(dat, impulsivity.names.nf3),
           intAvoid = score_NORSE_trigger(dat, intAvoid.names.nf3),
           intMem = score_NORSE_trigger(dat, intMem.names.nf3),
           pain = score_NORSE_trigger(dat, pain.names.nf3),
           physAnx = score_NORSE_trigger(dat, physAnx.names.nf3),
           ready = score_NORSE_trigger(dat, ready.names.nf3),
           sad = score_NORSE_trigger(dat, sad.names.nf3),
           selfComp = score_NORSE_trigger(dat, selfComp.names.nf3),
           selfContempt = score_NORSE_trigger(dat, selfContempt.names.nf3),
           socAvoid = score_NORSE_trigger(dat, socAvoid.names.nf3),
           socSup = score_NORSE_trigger(dat, socSup.names.nf3),
           subUse = score_NORSE_trigger(dat, subUse.names.nf3),
           suicide = score_NORSE_trigger(dat, suicide.names.nf3),
           worry = score_NORSE_trigger(dat, worry.names.nf3),
           QOL = Q226)
}

#' Check which version(s) of the NF are present in a data set.
#'
#' Should not require the data to already have clean names. But needs to work when that is the case.
#'
#' @param dat a data.frame
#'
#' @return a character vector containing one or more of the following: "2", "3".
#' If no matching items are found, will return "No NF items found."
#' @export
#'
#' @examples
#' /dontrun{
#' check_version_nf(dataGoesHere)
#' }
check_version_nf <- function(dat){
  if(any(grepl("Q1[0-9]{2}", names(dat))) &
     any(grepl("Q2[0-9]{2}", names(dat)))){
    return(c("2", "3"))
  } else if(any(grepl("Q1[0-9]{2}", names(dat)))){
    return(c("2"))
  } else if(any(grepl("Q2[0-9]{2}", names(dat)))){
    return(c("3"))
  } else {
    return("No NF items found.")
  }
}

#####
# come back to this when score_all_nf3() functions
# need to make it work using those features, so that it only calls those
# functions when the items are present.
# NF 3


#' Score all versions of the NF3 in raw scale form
#'
#' This function AUTOMATICALLY treats data with -98 and -99 values. The assumption is that
#' -98 means "not relevant to me" and -99 means "Prefer not to answer". For all scores,
#' -99s are considered NA when calculating scores. For all scales EXCEPT PROCESS VARIABLES -98s
#' mean 1, the lowest possible severity. For Alliance and Preference scales, -98 means NA.
#'
#' @param dat A dataset. Should have clean names that have been simplified to include
#'   only item indicators. For example, `M53_Q142_1` should instead be `Q142`.
#' @param process_vars Logical. Should the process variables be included?
#' @param versions Which versions of the NF do you want to score? Includes both
#'   2.x and 3.x by default.
#' @param version_variable Character name of the variable encoding the version
#'   to use for each row.
#'
#' @return A data.frame/tibble with new variables. The original item responses remain unchanged.
#' @export
#'
#' @examples
#' \dontrun{
#' score_all(dataGoesHere)
#' }
score_all <- function(dat,
                      process_vars = TRUE,
                      versions = c("2", "3"),
                      version_variable = "Ver_10") {

  # Helper function for non-process items: -98 becomes 1; -99 becomes NA.
  prepare_items <- function(dat, items) {
    dat_temp <- dat
    for (col in items) {
      if (col %in% names(dat_temp)) {
        dat_temp[[col]] <- ifelse(dat_temp[[col]] == -98, 1,
                                  ifelse(dat_temp[[col]] == -99, NA, dat_temp[[col]]))
      }
    }
    dat_temp
  }

  # Helper function for process variables: both -98 and -99 become NA.
  prepare_items_process <- function(dat, items) {
    dat_temp <- dat
    for (col in items) {
      if (col %in% names(dat_temp)) {
        dat_temp[[col]] <- ifelse(dat_temp[[col]] == -98, NA,
                                  ifelse(dat_temp[[col]] == -99, NA, dat_temp[[col]]))
      }
    }
    dat_temp
  }

  if (!any(names(dat) == version_variable)) {
    stop("The version_variable does not exist in the dataframe.\n")
  }

  dat <- dplyr::ungroup(dat) %>%
    dplyr::mutate(
      anger = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, anger.names.nf3), anger.names.nf3),
        grepl("2", .data[[version_variable]]) ~ NA,
        TRUE ~ NA
      ),
      cog = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, cog.names.nf3), cog.names.nf3),
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, cog.names), cog.names),
        TRUE ~ NA
      ),
      eating = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, eating.names.nf3), eating.names.nf3),
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, eating.names), eating.names),
        TRUE ~ NA
      ),
      genFunc = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, genFunc.names.nf3), genFunc.names.nf3),
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, genFunc.names), genFunc.names),
        TRUE ~ NA
      ),
      hopeless = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, hopeless.names.nf3), hopeless.names.nf3),
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, hopeless.names), hopeless.names),
        TRUE ~ NA
      ),
      impulsivity = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, impulsivity.names.nf3), impulsivity.names.nf3),
        grepl("2", .data[[version_variable]]) ~ NA,
        TRUE ~ NA
      ),
      intAvoid = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, intAvoid.names.nf3), intAvoid.names.nf3),
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, internal.names), internal.names),
        TRUE ~ NA
      ),
      intMem = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, intMem.names.nf3), intMem.names.nf3),
        grepl("2", .data[[version_variable]]) ~ NA,
        TRUE ~ NA
      ),
      pain = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, pain.names.nf3), pain.names.nf3),
        grepl("2", .data[[version_variable]]) ~ NA,
        TRUE ~ NA
      ),
      physAnx = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, physAnx.names.nf3), physAnx.names.nf3),
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, somAnx.names), somAnx.names),
        TRUE ~ NA
      ),
      ready = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, ready.names.nf3), ready.names.nf3),
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, ready.names), ready.names),
        TRUE ~ NA
      ),
      sad = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, sad.names.nf3), sad.names.nf3),
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, sad.names), sad.names),
        TRUE ~ NA
      ),
      selfComp = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, selfComp.names.nf3), selfComp.names.nf3),
        grepl("2", .data[[version_variable]]) ~ NA,
        TRUE ~ NA
      ),
      selfContempt = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, selfContempt.names.nf3), selfContempt.names.nf3),
        grepl("2", .data[[version_variable]]) ~ NA,
        TRUE ~ NA
      ),
      socAvoid = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, socAvoid.names.nf3), socAvoid.names.nf3),
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, avoidSoc.names), avoidSoc.names),
        TRUE ~ NA
      ),
      socSup = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, socSup.names.nf3), socSup.names.nf3),
        grepl("2", .data[[version_variable]]) ~ NA,
        TRUE ~ NA
      ),
      subUse = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, subUse.names.nf3), subUse.names.nf3),
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, subUse.names), subUse.names),
        TRUE ~ NA
      ),
      suicide = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, suicide.names.nf3), suicide.names.nf3),
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, suicide.names), suicide.names),
        TRUE ~ NA
      ),
      worry = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, worry.names.nf3), worry.names.nf3),
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, worry.names), worry.names),
        TRUE ~ NA
      ),
      QOL = Q226,
      # Scales only on NF2
      trauma = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ NA,
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, trauma.names), trauma.names),
        TRUE ~ NA
      ),
      selfCrit = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ NA,
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, selfCrit.names), selfCrit.names),
        TRUE ~ NA
      ),
      socialSafety = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ NA,
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, socialSafety.names), socialSafety.names),
        TRUE ~ NA
      ),
      control = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ NA,
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, control.names), control.names),
        TRUE ~ NA
      ),
      internal = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ NA,
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, internal.names), internal.names),
        TRUE ~ NA
      ),
      irritable = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ NA,
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, irritable.names), irritable.names),
        TRUE ~ NA
      ),
      recovEnv = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ NA,
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, recovEnv.names), recovEnv.names),
        TRUE ~ NA
      ),
      avoidSit = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ NA,
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, avoidSit.names), avoidSit.names),
        TRUE ~ NA
      ),
      subRecov = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ NA,
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, subRecov.names), subRecov.names),
        TRUE ~ NA
      ),
      somAnx = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ NA,
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, somAnx.names), somAnx.names),
        TRUE ~ NA
      ),
      avoidSoc = dplyr::case_when(
        grepl("3", .data[[version_variable]]) ~ NA,
        grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items(dat, avoidSoc.names), avoidSoc.names),
        TRUE ~ NA
      )
    )

  # Process variables: for these, treat both -98 and -99 as missing.
  if (process_vars) {
    dat <- dplyr::mutate(dat,
                         alliance = dplyr::case_when(
                           grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items_process(dat, alliance.names.nf3), alliance.names.nf3),
                           grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items_process(dat, alliance.names), alliance.names),
                           TRUE ~ NA
                         ),
                         pref = dplyr::case_when(
                           grepl("3", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items_process(dat, pref.names.nf3), pref.names.nf3),
                           grepl("2", .data[[version_variable]]) ~ score_NORSE_trigger(prepare_items_process(dat, needs.names), needs.names),
                           TRUE ~ NA
                         ))
  }

  # Check if scale scores are in valid range.
  if (dat %>% dplyr::select(scale_names_nf3) %>% all(. < -1 & !is.na(.))) {
    warning("Some values below 1. Scale scores not valid. Check item data.")
  }
  if (dat %>% dplyr::select(scale_names_nf3) %>% all(. > 7 & !is.na(.))) {
    warning("Some values above 7. Scale scores not valid. Check item data.")
  }

  dat
}


# score_all(HF_research_data_2021_fscores)


# Need a function to combine all values from variables like Q140 and Q140_1, when one of them is NA, then
# delete the item Q140
#' Combine Suffix Variables with the Same Base Name
#'
#' This function merges columns that share the same base name but have different suffixes
#' (e.g., `Q140`, `Q140_1`, `Q140_2`). If a row contains `NA` in one column but a value in another,
#' the function combines them, keeping the non-NA value. The function only considers variable names
#' that start with a single letter followed by numbers.
#'
#' @param dat A data frame containing the variables to be merged.
#' @param sep A character string specifying the separator between the base name and the suffix.
#'   Default is `"_"`
#'
#' @return A data frame with merged variables, where redundant suffix columns are removed.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   Q140 = c(NA, 2, 3, NA),
#'   Q140_1 = c(1, NA, NA, 4),
#'   Q140_2 = c(NA, NA, 5, NA),
#'   Q141 = c(10, 11, 12, 13),  # Should remain unchanged
#'   stringsAsFactors = FALSE
#' )
#'
#' df_combined <- combine_suffix_variables(df)
#' print(df_combined)
combine_suffix_variables <- function(dat, sep = "_") {
  # Identify relevant variable names
  var_names <- names(dat)

  # Extract base names (e.g., "Q140" from "Q140", "Q140_1", "Q140_2")
  base_names <- unique(str_extract(var_names, "^[A-Za-z]\\d+"))
  base_names <- base_names[!is.na(base_names)]  # Remove NA values

  # Iterate over each base name and combine values
  for (base in base_names) {
    # Find all columns that start with the base name
    matching_cols <- var_names[str_detect(var_names, paste0("^", base, "(_\\d+)?$"))]

    if (length(matching_cols) > 1) {
      # Use coalesce to merge values row-wise
      dat[[base]] <- do.call(coalesce, dat[matching_cols])

      # Remove the suffix columns after merging using base R
      dat <- dat[, !names(dat) %in% setdiff(matching_cols, base)]
    }
  }

  return(dat)
}
