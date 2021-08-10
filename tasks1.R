# Question 1

# 1.
# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
# define uniform prior
prior <- rep( 1 , 20 )
# # likelihood for 0 successes (water) out of 1 toss
likelihood <- dbinom( 0 , size=1 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
# plot the grid 
plot( p_grid , posterior , type="b" , 
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )

# 2.
# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
# define uniform prior
prior <- rep( 1 , 20 )
# likelihood for 2 successes (water) out of 6 tosses
likelihood <- dbinom( 2 , size=6 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
# plot the grid 
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )


# Question 2
# The estimate will become more accurate as the number of tosses increase.
# A "good" estimate would include a higher proportion of land over water based 
# on the observations from question 1.
# To obtain this estimate, one should toss the globe at least over a 100 times.


# Question 3

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 8 , size=14 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(1)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

# 1.
sum( samples < 0.5 ) / 1e4
# Ans: There is a 0.3063 chance that it will rain 7 days or less over the last 2 
# weeks given a uniform prior.

#2.
sum( samples > 0.9 ) / 1e4
# Ans: There is a 5e-04 chance that it will rain more than 12.6 days or more 
# over the last 2 weeks given a uniform prior.

#3.
sum( samples > 0.5 & samples < 0.9 ) / 1e4
# Ans: There is a 0.6932 chance that it will rain more than 7 days and less than
# 12.6 days over the last 2 weeks given a uniform prior.

#4.
HPDI( samples , prob=0.88 )
# Ans: |0.88     0.88| 
#  0.3793794 0.7547548 
# There is a chance it will rain between the two answers above for 12.32 days 
# over the last 2 weeks given a uniform prior.


# Question 4
# I would use an informative prior that involves a normal distribution. 
# The reason for this is that there are a range of values the battery can last 
# (e.g. 4-12 hours in one hour intervals). 
# As such, by using a normal distribution prior, one would be able to estimate 
# the exact time the battery lasts in the phone.





