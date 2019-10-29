## see
## https://jamesmccaffrey.wordpress.com/2019/10/28/roulette-wheel-selection-for-multi-armed-bandit-problems
## for inspiration
roulette_wheel <- function(coins = 40, 
                           starts = 5,
                           true_prob = c(0.3, 0.5, 0.7)){
  # must have enough coins to generate initial empirical distribution
  if (coins < (length(true_prob) * starts)){
    stop("To generate a starting distribution, each machine must be",
         " played ",
         starts,
         " times - not enough coins to do so.")
  }
  # allocate first ("warm up")
  SS <- sapply(true_prob, FUN = function(x) sum(rbinom(starts, 1, x)))
  FF <- starts - SS
  # calculate metrics used for play allocation
  probs <- SS / (SS + FF)
  probs_normalized <- probs / sum(probs)
  cumu_probs_normalized <- cumsum(probs_normalized)
  # update number of coins
  coins <- coins - (length(true_prob) * starts)
  # create simulation data.frame
  sim_df <- data.frame(machine = seq_along(true_prob),
                       true_probabilities = true_prob,
                       observed_probs = probs,
                       successes = SS,
                       failures = FF,
                       plays = SS + FF,
                       machine_played = NA,
                       coins_left = coins)
  # initialize before while loop
  sim_list <- vector('list', length = coins)
  i <- 1
  # play until we run out of original coins
  while(coins > 0){
    # which machine to play?
    update_index <- findInterval(runif(1), c(0, cumu_probs_normalized))
    # play machine
    flip <- rbinom(1, 1, true_prob[update_index])
    # update successes and failure for machine that was played
    SS[update_index] <- SS[update_index] + flip
    FF[update_index] <- FF[update_index] + (1-flip)
    # update metrics used for play allocation
    probs <- SS / (SS + FF)
    probs_normalized <- probs / sum(probs)
    cumu_probs_normalized <- cumsum(probs_normalized)
    # update number of coins
    coins <- coins - 1    
    # update simulation data.frame (very inefficient)
    sim_list[[i]] <- data.frame(machine = seq_along(true_prob),
                                true_probabilities = true_prob,
                                observed_probs = probs,
                                successes = SS,
                                failures = FF,
                                plays = SS + FF,
                                machine_played = seq_along(true_prob) == update_index,
                                coins_left = coins)
    i <- i + 1
  }
  # show success:failure ratio
  message("Success to failure ratio was ",
          round(sum(SS) / sum(FF), 2),
          "\n",
          paste0("(", 
                 paste0(SS, collapse = "+"), 
                 ")/(", 
                 paste0(FF, collapse = "+"), ")"))
  # return data frame of values from experiment
  rbind(sim_df, do.call('rbind', sim_list))
}