
load_packages <- function(packages_list) {
  # install required packages if they are not installed yet:
  new_packages <- packages_list[
    !(packages_list %in% installed.packages()[, "Package"])]
  if (length(new_packages)) install.packages(new_packages)
  # load all required packages:
  invisible(lapply(packages_list, library, character.only = TRUE))
}

save_datatable <- function(dt, path) {
  write.table(dt, file = path, sep = ",", row.names = FALSE)
}

detect_cores <- function() {
  # get the maximum number of available cores rstudio has access to:
  # We will get the maximum number of computing cores available.
  # This information could be used later to parallelize execution when needed.
  num_cores <- parallel::detectCores(all.tests = TRUE, logical = TRUE)
  sprintf("Number of available computing cores: %d", num_cores)
  # use all available cores for processing in r
  doMC::registerDoMC(cores = num_cores)
  return(num_cores)
}

round_pvalues <- function(pvalue) {
  # This function can be used to round p-values.
  pvalue_rounded <- vector()
  for (p in seq_len(length(pvalue))) {
    pvalue_rounded[p] <- format.pval(
      pv = pvalue[p], digits = 1, eps = 0.001, nsmall = 2, scientific = FALSE)
    if (pvalue_rounded[p] == "<0.001") {
      pvalue_rounded[p] <- gsub("<", "p < ", pvalue_rounded[p])
    } else {
      pvalue_rounded[p] <- paste0("p = ", pvalue_rounded[p])
    }
    pvalue_rounded[p] <- stringr::str_replace(pvalue_rounded[p], ".0", " ")
  }
  return(pvalue_rounded)
}

label_fill <- function(original, offset = 0, mod = 2, fill = "") {
  # This function can be used to generate axis labels that omit e.g.,
  # every second label. Solution was taken from [here](https://bit.ly/2VycSy0).
  ii <- as.logical((seq_len(length(original)) - 1 + offset) %% mod)
  original[ii] <- fill
  return(original)
}

extract_number <- function(mixed_string) {
  # this function can be used to extract a number from a mixed string.
  number <- regmatches(mixed_string, gregexpr("[[:digit:]]+", mixed_string))
  number <- as.numeric(unlist(number))
}

get_labeller <- function(array, suffix = " ms") {
  facet_labels_new <- unique(paste0(as.numeric(array) * 1000, suffix))
  facet_labels_old <- as.character(unique(array))
  names(facet_labels_new) <- facet_labels_old
  labeller <- as_labeller(facet_labels_new)
  return(labeller)
}

# function to get sequential position and switch to next sequential stimulus:
get_pos <- function(data, events) {
  # get the matching subject id:
  sub_id <- events$subject == unique(data$id)
  # get the matching sequence trial number (trial)
  trial <- events$trial == unique(data$trial)
  # get the sequence of the current trial:
  seq <- events$stim_label[sub_id & trial]
  # get only unique elements of the sequence while maintaining their order:
  seq_items <- unique(seq)
  # get the trial number of the switch to the second item within the sequence:
  change <- min(which((seq == seq_items[2]) == TRUE))
  # get the sequential position of the current label:
  position <- which(seq_items == unique(data$class))
  # repeat the sequential position as needed (depending on length of the data):
  position <- rep(position, nrow(data))
  # repeat the change trial as needed (depending on length of the data):
  change <- rep(change, nrow(data))
  # get the target cue of the current trial:
  trial_cue <- rep(unique(events$trial_cue[sub_id & trial]), nrow(data))
  # get the target cue position of the current trial:
  tmp_target <- events$target[sub_id & trial]
  tmp_position <- events$serial_position[sub_id & trial]
  trial_cue_position <- rep(tmp_position[tmp_target == 1], nrow(data))
  # get the accuracy of the current trial:
  accuracy <- rep(unique(events$trial_accuracy[sub_id & trial]), nrow(data))
  # return the position and change indices as result of the function:
  return(list(position = position, change = change,
              trial_cue = trial_cue, accuracy = accuracy,
              trial_cue_position = trial_cue_position))
}

prepare_data <- function(dt) {
  library(data.table)
  dt <- setDT(dt)
  # specify whether one-vs-rest or multi-class classifiers have been used:
  dt[, classification := ifelse(classifier == "log_reg", "multi", "ovr")]
  # add column to specify the task condition:
  dt[grepl("odd", test_set, fixed = TRUE), condition := "oddball"]
  dt[grepl("seq", test_set, fixed = TRUE), condition := "sequence"]
  dt[grepl("rep", test_set, fixed = TRUE), condition := "repetition"]
  dt[grepl("rest", test_set, fixed = TRUE), condition := "rest"]
  # display warning if there is any empty row in the task condition column:
  if ( any(is.na(dt$condition)) ) warning("missing condition assignment")
  # add a within speed condition trial counter across all runs (max should be 15):
  dt[, by = .(id, classifier, condition, tITI, class, seq_tr), ":=" (trial_tITI = 1:.N)]
  # check if the maximum within speed condition trial counter does not exceed 15:
  #if( max(subset(dt, condition == "sequence")$trial_tITI) != 15 )
  #  warning('max within speed counter does not equal 15!')
  #if( max(subset(dt, condition == "repetition")$trial_tITI) != 45 )
  #  warning('max within speed counter does not equal 45!')
  # probabilities are normalized for each class within a trial to sum up to 1
  dt[, by = .(mask, id, condition, classification, classifier, test_set, session, run_study, tITI, trial, class), ":=" (
    probability_norm = probability / sum(probability),
    probability_zscore = (probability - mean(probability))/sd(probability),
    probability_cum = cumsum(probability) / max(cumsum(probability)))] %>%
    # check if the number of TRs match:
    verify(.[, by = .(mask, id, condition, classification, classifier, test_set, session, run_study, tITI, trial, class), .(
      num_trs = .N
    )]$num_trs %in% c(1, 5, 7, 13, 233))
  # order sequence trial data by participant, classifier, trial and serial TR:
  dt = setorder(dt, mask, id, condition, classification, classifier, tITI, trial, class, seq_tr) %>% setDT(.)
  # return the prepared data table:
  return(dt)
}

data_summary <- function(x) {
  # Function to produce summary statistics (mean and +/- sem)
  m <- mean(x)
  sem_lower <- m - (sd(x) / sqrt(length(x)))
  sem_upper <- m + (sd(x) / sqrt(length(x)))
  return(c(y = m, ymin = sem_lower, ymax = sem_upper))
}

round_updown <- function(numbers, base) {
  numbers_round <- rep(NA, length(numbers))
  for (i in seq_len(length(numbers))) {
    if (numbers[i] < 0) {
      numbers_round[i] <- -base
    } else if (numbers[i] > 0) {
      numbers_round[i] <- base
    }
  }
  return(numbers_round)
}
