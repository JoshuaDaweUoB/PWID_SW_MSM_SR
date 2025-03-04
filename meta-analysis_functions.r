#  load packages
pacman::p_load("tidyverse")

# convert to numeric
convert_to_numeric <- function(df) {
  df <- transform(df, 
                  effect_unadj = as.numeric(effect_unadj), 
                  effect_unadj_lb = as.numeric(effect_unadj_lb),
                  effect_unadj_ub = as.numeric(effect_unadj_ub),
                  effect_adj1 = as.numeric(effect_adj1), 
                  effect_adj_lb1 = as.numeric(effect_adj_lb1),
                  effect_adj_ub1 = as.numeric(effect_adj_ub1),
                  effect_adj1 = as.numeric(effect_adj2), 
                  effect_adj_lb1 = as.numeric(effect_adj_lb2),
                  effect_adj_ub1 = as.numeric(effect_adj_ub2))
  return(df)
}

# log transform
log_transform <- function(df) {
  df <- transform(df, 
                  effect_unadj_ln = log(effect_unadj),
                  effect_unadj_lb_ln = log(effect_unadj_lb),
                  effect_unadj_ub_ln = log(effect_unadj_ub),
                  adj_est_ln = log(adj_est),
                  adj_lower_ln = log(adj_lower),
                  adj_upper_ln = log(adj_upper),
                  effect_best_ln = log(effect_best),
                  effect_best_lower_ln = log(effect_best_lower),
                  effect_best_upper_ln = log(effect_best_upper))
  return(df)
}
