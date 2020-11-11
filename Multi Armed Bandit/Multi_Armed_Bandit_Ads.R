data <- data.frame(Ad_1 = rbinom(n=10000,size = 1,prob = 0.17),
                   Ad_2 = rbinom(n=10000,size = 1,prob = 0.13),
                   Ad_3 = rbinom(n=10000,size = 1,prob = 0.07),
                   Ad_4 = rbinom(n=10000,size = 1,prob = 0.12),
                   Ad_5 = rbinom(n=10000,size = 1,prob = 0.27),
                   Ad_6 = rbinom(n=10000,size = 1,prob = 0.015),
                   Ad_7 = rbinom(n=10000,size = 1,prob = 0.11),
                   Ad_8 = rbinom(n=10000,size = 1,prob = 0.21),
                   Ad_9 = rbinom(n=10000,size = 1,prob = 0.095),
                   Ad_10 = rbinom(n=10000,size = 1,prob = 0.05))


#Naive Approach
N = nrow(data)
d = ncol(data)
ad_selected = integer(0)
total_reward = 0
for (n in 1:N) {
  ad = sample(10,1)
  ad_selected = append(ad_selected,ad)
  total_reward = total_reward + data[n,ad]
}

hist(ad_selected)


#Upper Confidence Bound
N = nrow(data)
d = ncol(data)
ad_selected = integer(0)
total_reward = 0
number_of_selections = integer(d)
sum_of_rewards = integer(d)
for (n in 1:N) {
  max_upper_bound = 0
  ad = 0
  for (i in 1:d) {
    if(number_of_selections[i]>0){
      average_reward = sum_of_rewards[i]/number_of_selections[i]
      delta_i = sqrt(log(n)/number_of_selections[i])
      upper_bound = average_reward + delta_i
    }else{
      upper_bound = 1e400
    }
    if(upper_bound>max_upper_bound){
      ad = i
      max_upper_bound = upper_bound
    }
  }
  ad_selected = append(ad_selected,ad)
  reward = data[n,ad]
  sum_of_rewards[ad] = sum_of_rewards[ad] + reward
  total_reward = total_reward + reward
  number_of_selections[ad] = number_of_selections[ad] + 1
}


#Thompson Sampling
N = nrow(data)
d = ncol(data)
ad_selected = integer(0)
total_reward = 0
number_of_rewards_1 = integer(d)
number_of_rewards_0 = integer(d)
for (n in 1:N) {
  ad = 0
  max_random = 0
  for (i in 1:d) {
    random_beta = rbeta(n=1,
                        shape1 = number_of_rewards_1[i]+1,
                        shape2 = number_of_rewards_0[i]+1)
    if(random_beta>max_random){
      max_random = random_beta
      ad = i
    }
  }
  ad_selected = append(ad_selected,ad)
  reward = data[n,ad]
  if(reward==1){
    number_of_rewards_1[ad] = number_of_rewards_1[ad] + 1
  }else{
    number_of_rewards_0[ad] = number_of_rewards_0[ad] + 1
  }
  total_reward = total_reward + reward
}

# Visualising the results
hist(ad_selected,
     col = 'blue',
     main = 'Histogram of ads selections',
     xlab = 'Ads',
     ylab = 'Number of times each ad was selected')

