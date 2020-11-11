data <- data.frame(V_1 = rbinom(n=10000,size = 1,prob = 0.17),
                   V_2 = rbinom(n=10000,size = 1,prob = 0.13),
                   V_3 = rbinom(n=10000,size = 1,prob = 0.07),
                   V_4 = rbinom(n=10000,size = 1,prob = 0.12),
                   V_5 = rbinom(n=10000,size = 1,prob = 0.27),
                   V_6 = rbinom(n=10000,size = 1,prob = 0.015),
                   V_7 = rbinom(n=10000,size = 1,prob = 0.11),
                   V_8 = rbinom(n=10000,size = 1,prob = 0.21),
                   V_9 = rbinom(n=10000,size = 1,prob = 0.095),
                   V_10 = rbinom(n=10000,size = 1,prob = 0.05))

#Naive Approach
N = nrow(data)
d = ncol(data)
var_selected = integer(0)
total_rewards = 0
for (i in 1:N) {
  var = sample(d,1)
  var_selected = append(var_selected,var)
  total_rewards = total_rewards + data[i,var]
}

hist(var_selected)


#Upper Confidence Bound
N = nrow(data)
d = ncol(data)
var_selected = integer(0)
total_rewards = 0
number_of_selections = integer(d)
sum_of_rewards = integer(d)
for (n in 1:N) {
  max_upper_bound = 0
  var = 0
  for (i in 1:d) {
    if(number_of_selections[i]>0){
      average_reward = sum_of_rewards[i]/number_of_selections[i]
      delta_i = sqrt(log(n)/number_of_selections[i])
      upper_bound = average_reward + delta_i
    }else{
      upper_bound = 1e400
    }
    if(upper_bound>max_upper_bound){
      var = i
      max_upper_bound = upper_bound
    }
  }
  var_selected = append(var_selected,var)
  number_of_selections[var] = number_of_selections[var] + 1
  sum_of_rewards[var] = sum_of_rewards[var] + data[n,var]
  total_rewards = total_rewards + data[n,var]
}

hist(var_selected)


#Thompson Sampling
N = nrow(data)
d = ncol(data)
var_selected = integer(0)
total_rewards = 0
number_of_selections_1 = integer(d)
number_of_selections_0 = integer(d)
for (n in 1:N) {
  var = 0
  max_random = 0
  for (i in 1:d) {
    random_beta = rbeta(n=1,shape1=number_of_selections_1[i]+1,shape2=number_of_selections_0[i]+1)
    if(random_beta>max_random){
      var = i
      max_random = random_beta
    }
  }
  var_selected = append(var_selected,var)
  if(data[n,var]==0){
    number_of_selections_0[var] = number_of_selections_0[var] + 1
  }else{
    number_of_selections_1[var] = number_of_selections_1[var] + 1
  }
  total_rewards = total_rewards + data[n,var]
}

hist(var_selected)
