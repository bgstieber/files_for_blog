import numpy as np
import pandas as pd

def roulette_wheel(coins = 40, starts = 5, true_probs = [0.3, 0.5, 0.7]):
    # must have enough coins for the warm start
    if coins < (len(true_probs) * starts):
        raise ValueError('Not enough coins to play')        
    # initialize and calculate machine metrics
    SS = [sum(np.random.binomial(1, p, starts)) for p in true_probs]
    FF = [starts - x for x in SS]
    observed_probs = list(map(lambda x, y: x / (x + y), SS, FF))
    probs_normalized = [x / sum(observed_probs) for x in observed_probs]
    cumu_probs = list(np.cumsum(probs_normalized))
    # update coins
    coins -= (starts * len(true_probs))
    # play until we run out of original coins
    while coins > 0:
        # choose machine
        runif_1 = np.random.uniform()
        check_cumu_probs = [runif_1 <= x for x in cumu_probs]
        update_index = np.min(np.where(check_cumu_probs))
        # play machine
        flip = np.random.binomial(1, p = true_probs[update_index])
        # update metrics
        SS[update_index] += flip
        FF[update_index] += (1-flip)        
        observed_probs = list(map(lambda x, y: x / (x + y), SS, FF))
        probs_normalized = [x / sum(observed_probs) for x in observed_probs]        
        cumu_probs = list(np.cumsum(probs_normalized))
        # update coins
        coins -= 1
    # calculate number of plays per machine
    plays = list(map(lambda x,y: x + y, SS, FF))
    # DataFrame of results
    df = pd.DataFrame({
            'machine': list(np.arange(len(true_probs))),
            'true_probabilities': true_probs,
            'observed_probs': observed_probs,
            'successes': SS,
            'failures': FF,
            'plays': plays
            })
    print("Success to failure ratio was ", round(sum(SS) / sum(FF), 2))
    return(df)