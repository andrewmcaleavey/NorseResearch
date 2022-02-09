#' Retrieving the (English) item text from an item number
#'
#' \code{get_item_text} simply returns the English text, given a
#' character item number in the format "Q11".
#'
#' @param target the character string to look up, e.g., "Q26".
#' @param table the lookup table, defaults to \code{item_descriptions}.
#' Can be expanded to provide Norwegian or other lookup tables.
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
  if(version == 2 | version == "2" | version =="2.0"){
    text_ret <- as.character(item_descriptions[item_descriptions$item == target, "item_text"])
    verb_out <- item_descriptions[item_descriptions$item == target, ]
  }

  # version 2.1 is similar
  if(version == 2.1 | version == "2.1"){
    text_ret <- as.character(nf2.1.item.descriptions[match(target, nf2.1.item.descriptions$item), "item_text_e"])
    verb_out <- nf2.1.item.descriptions[nf2.1.item.descriptions$item == target, ]
  }

  # This is the return() block
  if(verbose) return(verb_out)
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


