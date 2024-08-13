#######################################
## Function for Determination of     ##
## Consensus Sequence                ##
## James Hunter                      ##
## 03/11/18                          ##
## inputs: seq - XStringSet Object   ##
##         minpct - real num         ##
## minpct = to appear in consensus   ##
##          must appear in >= minpct ##
##          sequences                ##
## seq can be aligned or not as you  ##
##    wish                           ##
## output: vector with consensus     ##
##         letter                    ##
##         for each position or "X"  ##
##         if no consensus reached   ##
## Algorithm: most frequent letter   ##
##    subject to minpct and to a     ##
##    minimum of 2 instances         ##
#######################################

consensus_seq <- function(seq, minpct = .5) {
  pacman::p_load(tidyverse, Biostrings, DECIPHER)
  positions <- max(width(seq))

  ## set up output vector
  con_seq <- character(positions) # based on widest seq

  ## Bring strings into a tibble for ease of processing
  process_db <- tibble()
  var_names <- paste0("P", 1:positions)
  process_db <- setNames(data.frame(matrix(ncol = positions,
                                           nrow = length(seq))),
                         c(var_names))

  ## split the strings into individual characters in a vector (row)
  ## and append it to dataframe
  for (i in 1:length(seq)) {
    process_db[i,] <- unlist(strsplit(as.character(seq[[i]]),
                                      split = ""))
  }

  ## loop to process each position for each sequence

  for (j in 1:positions) {
    ## initialize df to hold counts for calculation

    temp <- tibble(lett = AA_ALPHABET,
                   count = 0)
    ## initialize table/df for each position

    calc <- as.data.frame(table(process_db[, j]), stringsAsFactors = F)

    for (k in 1:nrow(calc)) {
      temp$count[temp$lett == calc$Var1[k]] <-
        temp$count[temp$lett == calc$Var1[k]] +
        calc$Freq[k]
    }
    ## find consensus character and test to see if > minpct and greater than 1
    mf <- temp$lett[temp$count == max(temp$count)]
    # check for ties; accept just the first in the case of ties
    mf <- ifelse(length(mf) > 1, mf[1], mf)
    con_count <- temp$count[temp$lett == mf]
    app_factor <- minpct * nrow(process_db)
    con_seq[j] <- ifelse(con_count > 1 & con_count > app_factor, mf, "X")
  }
  return(con_seq)
} #end function
