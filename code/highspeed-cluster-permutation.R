# cluster permutation test in R
# function to find clusters of time-points above a specified threshold with the
# clustering being performed separately for samples with a positive and negative
# value in the time-series.

# references:
# Maris, E., & Oostenveld, R. (2007). Nonparametric statistical testing of EEG- and MEG-data.
# Journal of Neuroscience Methods, 164(1), 177–190. https://doi.org/10.1016/j.jneumeth.2007.03.024



cluster_indices <- function(timeseries, threshold) {
  # step 1: clustering
  # we cluster adjacent time-samples that all exhibit a similar difference
  # (in sign and magnitude)
  
  # initialize an empty cluster index vector depending on the length of the timeseries:
  cluster_id = replicate(length(timeseries), 0)
  # get positions of cluster with t-values above or below the treshhold
  timepoints_thresh = which(abs(timeseries) > threshold)
  # set all positions of negative t-values to a negative sign
  timepoints = timepoints_thresh * sign(timeseries[timepoints_thresh])
  # split the position indices into clusters of consecutive numbers:
  clusters = split(abs(timepoints), cumsum(c(1, abs(diff(timepoints)) != 1)))
  # write cluster indices into the cluster index vector:
  for (i in seq(1,length(clusters))) {
    cluster_id[unlist(clusters[i])] = i
  }
  # return the cluster indices:
  return(cluster_id)
}

shuffle_timeseries <- function(timeseries) {
  # function to randomly shuffle (i.e., invert a timeseries) through
  # multiplication by -1 with a 0.5 probability:
  # participant-specific timesources are flipped (multiplied by -1) randomly:
  
  timeseries_shuffled = timeseries * sample(x = c(-1, 1), size = 1)
  return(timeseries_shuffled)
}

cluster_stats = function(data, cfg, shuffle = TRUE) {
  
  data_clustered = data %>%
    
    # shuffle time series for each participant, classification and speed
    # if shuffle = TRUE. If shuffle = FALSE, the data is just copied.
    # check if number of timepoints matches the true number of TRs (here, 13):
    .[, by = c(cfg$grouping, "id"), ":=" (
      n_trs = .N,
      variable_data = if(shuffle) {shuffle_timeseries(get(cfg$variable))} else {get(cfg$variable)}
    )] %>% verify(n_trs == cfg$n_trs) %>%
    
    # run a two-sided one-sample t-test against 0 at every TR to get a
    # timeseries of t-values (by taking out the participant factor):
    # check if there were as many data points as there were participants:
    .[, by = c(cfg$grouping, "seq_tr"), .(
      num_subs = .N,
      tvalue = t.test(variable_data, alternative = "two.sided", mu = cfg$baseline)$statistic
    )] %>% #verify(num_subs == 40) %>%
    
    # get the indices for clusters with consecutive t-values above / below threshold
    # check if number of timepoints matches the true number of TRs (here, 13):
    .[, by = c(cfg$grouping), ":=" (
      n_trs = .N,
      cluster_id = cluster_indices(timeseries = tvalue, threshold = cfg$threshold)
    )] %>% verify(n_trs == cfg$n_trs) %>%
    
    # calculate the cluster mass of each cluster, separately for each
    # classification and speed condition:
    .[, by = c(cfg$grouping, "cluster_id"), .(
      cluster_trs = list(seq_tr),
      cluster_length = .N,
      cluster_mass = sum(tvalue),
      cluster_type = ifelse(sum(tvalue) > 0, "positive", "negative")
    )] %>%
    
    # add marker to the maximum absolute cluster mass
    # rank the clusters according to their absolute cluster mass:
    .[, by = c(cfg$grouping), ":=" (
      cluster_max = as.integer(abs(cluster_mass) == max(abs(cluster_mass))),
      cluster_rank = rank(abs(cluster_mass))
    )]
  
  return(data_clustered)
}

cluster_test <- function(data_true, data_perm, cfg) {
  # subset the permutation data frame depending on the grouping:
  data_perm_select = data_perm %>%
    filter(classification == unique(data_true$classification)) %>%
    filter(tITI == unique(data_true$tITI)) %>%
    filter(cluster_max == 1) %>%
    # extract the cluster mass values of the maximum clusters:
    mutate(cluster_mass_perm = cluster_mass * cluster_id)
  # get the number of "maximum" clusters that were actually "zero"-clusters:
  n_zero = sum(data_perm_select$cluster_id == 0)
  # get the number of cluster mass values above the true cluster mass:
  n_above = sum(abs(data_perm_select$cluster_mass_perm) > abs(data_true$cluster_mass))
  # TODO: add column that specifies if above or below threshold (can be used for later plotting):
  # retrieve the number of permutations that was run:
  n_perms = nrow(data_perm_select)
  # get the monte carlo p-value by dividing:
  p_value = n_above/n_perms
  return(list("p_value" = p_value, "n_perms" = n_perms, "n_zero" = n_zero))
}

cluster_plot <- function(data_true, data_perm){
  
  plot = ggplot(data_perm, aes(x = abs(as.numeric(cluster_mass)))) + 
    facet_wrap(facets = ~ as.factor(tITI), labeller = get_labeller(array = data_perm$tITI)) +
    geom_histogram(binwidth = 0.5) +
    geom_vline(data = data_true, aes(
      xintercept = abs(as.numeric(cluster_mass)), color = as.factor(cluster_type))) +
    xlab("Absolute cluster mass (summed t-values)") +
    ylab("Number of permutations") +
    scale_color_discrete(name = "Cluster type") +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    theme(legend.position = c(1, 0), legend.justification = c(1, 0)) +
    theme(axis.line = element_line(linetype = "solid", lineend = "round")) +
    theme(plot.margin = unit(c(t = 1, r = 3, b = 1, l = 1), "pt"))
  
  return(plot)
}


cluster_permutation <- function(data, cfg) {
  # wrapper function to combine all steps of the cluster permutation test
  # inputs: data = a data frame; variable = string, variable of interest,
  # cfg = list, containing important configuration parameters:
  # variables, threshold, baseline, grouping, n_perms, n_trs
  
  # get the cluster statistics for the true data:
  data_true = cluster_stats(data, cfg, shuffle = FALSE)
  # get the cluster statistics for the permuted data:
  data_perm = do.call(rbind, replicate(cfg$n_perms, list(cluster_stats(data, cfg, shuffle = TRUE))))
  # compare the cluster statistics of the true data to the permuted data:
  data_true = data_true %>%
    .[, c("p_value", "n_perms", "n_zero") := cluster_test(
      data_true = .SD, data_perm = data_perm, cfg = cfg),
      by = c(cfg$grouping, "cluster_id"),
      .SDcols = colnames(data_true)] %>%
    setorder(classification, tITI, cluster_id) %>%
    filter(cluster_id != 0) %>% setDT(.)
  # plot the permutation distribution and true data:
  plot = cluster_plot(
    subset(data_true, classification == "ovr"),
    subset(data_perm, classification == "ovr"))
  return(list(data_true, plot))
                                     
}














