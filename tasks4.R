library(rethinking)

rm(list=ls())

hw_1 <- read.csv("caffeine_data.csv")



#############
# Question 1
#############

# Part a

# Create the dummy variable
hw_1$is_extro <- ifelse(hw_1$group == "extrovert", 1, 0)

# Define the model 

# likelihood
# GRE_change, Normal(mu, sigma)

# explanation
# It makes sense to use a normal distribution as the distributions of the changes in GRE scores are not well known.

# linear regression equation
# y=mx+c

# explanation
# linear models are represented in its most basic form as y=mx+c.

# first prior (a)
# Normal distribution (5, 20)

# explanation
# I think it is reasonable to assume that average GRE scores to be around 5 as it seems that most scores are around that value.
# I believe that it is near certain that average GRE scores lie between -40 and 40.

# second prior (bc)
# Normal distribution (0, 5)

# explanation
# I think it is safe to assume that there is a possibility of there being no relationship between the amount of caffeine taken and changes in GRE score.
# An increase in the amount of caffeine taken should have an increase of around 5 for changes in GRE score.

# third prior (bg)
# Normal distribution (0,5)

# explanation
# There is always the possibility of there being no relationship between one's extroversion and changes in GRE score.
# Being classed as an extrovert may increase/decrease one's GRE score by 5.

# fourth prior (bcg)
# Normal distribution (0,2)

# explanation
# The 0 indicates a possible interaction between caffeine and extroversion.
# If there is an interaction between caffeine and extroversion, the effect of caffeine on extroversion (and vice versa), might amount to an increase/decrease in GRE scores of 2.

# sigma prior
# Cauchy distribution (0, 100)

# explanation
# A Cauchy distribution is used instead of a uniform distribution as it helps to account for variances that are less likely to occur better than a uniform distribution.
# The standard deviation of sigma should be positive therefore, it makes sense to bound it at zero as there can either be a change in GRE scores, or no change at all. 
# A value of 100 should be appropriate to accommodate the variance in this model. 

# Create the model using map2stan
m1 <- map2stan(
  alist(
    GRE_change ~ dnorm( mu , sigma ) ,
    mu <- a + bc*caffeine + bg*is_extro + bcg*caffeine*is_extro ,
    a ~ dnorm(5,25),
    bc ~ dnorm(0,5),
    bg ~ dnorm(0,5),
    bcg ~ dnorm(0,2),
    sigma ~ dcauchy(0,100)
  ) ,
  data=hw_1 )

# Summarize the model
precis (m1)

# Part b

# plot the model
plot( GRE_change ~ caffeine , hw_1 , type="n",xlim=c(1.8,4.2),ylim=c(-19,31),xlab="caffeine intake [mg/Kg]", ylab="GRE score change")

# plot the extroverts 

caffeine.seq <- seq(from= 1,to= 5,length.out= 100)
hw_1.predict_extro <- list(
  caffeine = caffeine.seq,
  is_extro = 1
)

pred_extro <- link(m1 , data=hw_1.predict_extro, n=1e4)

mu_extro <- apply( pred_extro, 2, mean )
CI_extro <- apply( pred_extro, 2, HPDI , prob=0.95 )

with(hw_1[hw_1$is_extro==1,], points(caffeine-0.03, GRE_change,col="blue"))

lines( caffeine.seq , mu_extro, col="blue", lwd=3)
shade(CI_extro ,  caffeine.seq , col=rgb(0, 0, 1, 0.2))

lines( caffeine.seq , CI_extro[1,] , lty=2 ,col="blue")
lines( caffeine.seq , CI_extro[2,] , lty=2 ,col="blue")

# plot the introverts

caffeine.seq <- seq(from= 1,to= 5,length.out= 100)
hw_1.predict_intro <- list(
  caffeine = caffeine.seq,
  is_extro = 0
)

pred_intro <- link(m1 , data=hw_1.predict_intro, n=1e4)

mu_intro <- apply( pred_intro, 2, mean )
CI_intro <- apply( pred_intro, 2, HPDI , prob=0.95 )

with(hw_1[hw_1$is_extro==0,], points(caffeine+0.03, GRE_change,col="red"))

lines( caffeine.seq , mu_intro, col="red", lwd=3)
shade(CI_intro ,  caffeine.seq , col=rgb(1, 0, 0, 0.2))

lines( caffeine.seq , CI_intro[1,] , lty=2 ,col="red")
lines( caffeine.seq , CI_intro[2,] , lty=2 ,col="red")

# label the lines
legend(2.3,30, legend=c("introvert","extrovert"), col=c("red","blue"), lwd=2, pch=21, bty="n")

# Part c

# Refit model from previous homework assignment

model_2 <- map(
  alist(
    GRE_change ~ dnorm( mu , sigma ) ,
    mu <- a + bc*caffeine + bg*is_extro ,
    a ~ dnorm( 5 , 25 ) ,
    bc ~ dnorm( 0 , 5 ) ,
    bg ~ dnorm( 0 , 5 ) ,
    sigma ~ dunif( 0 , 100 )
  ) ,
  data= hw_1 )

# Summarize the model
precis (model_2)

# Compare the two models

# The mean GRE_change score for model 1 may be postive or negative whereas the mean GRE change scores for model 2 is most likely to be positive.
# There is most likely a positive relationship between caffeine and GRE change scores for model 1 whereas there is a possible negative (as well as positive) relationship for the two variables in model 2.
# A positive relationship is likely to occur for extroversion and GRE change scores for model 1 whereas this positive relationship may also be negative in model 2.
# The variances for the two models do not vary too far from each other.


############# 
# Question 2
#############

# model from homework

#d <- read.csv("math_skill_data.csv")
# library(rethinking)
# m0 <- map(alist(
#  math_skill ~ dnorm(mu, sigma),
#  mu <- beta0 + beta1 * school_year + beta2 * age_months,
# beta0 ~ dunif(-1000,1000),
#  beta1 ~ dunif(-1000,1000),
#  beta2 ~ dunif(-1000,1000),
#  sigma ~ dunif(0,1000)
# ),
# data=d, start = c(0,0,0,10))

# precis(m0)


# Part a

# read the dataset
d <- read.csv("math_skill_data.csv")

# Fix the code

# changed "map" function to "map2stan"
m0 <- map2stan(
  alist(
    
    math_skill ~ dnorm(mu, sigma),
    
# added interactions for age and school year    
  mu <- beta0 + beta1 * school_year + beta2 * age_months + beta3*school_year*age_months,
  
# changed prior for beta0

# explanation
# An average of 80 as a score for math skills seems reasonable. 
# It seems that the scores on the math test lie below a 100 therefore, an SD of 50 makes sense.
 
# changed function from dunif to dnorm

# explanation
# It would be reasonable to assume that math skills may be normally distributed in the real world.

  beta0 ~ dnorm(80, 50),

# changed prior for beta1

# explanation
# The 0 indicates an assumption that there is no relationship between year in school and math skills.
# An increase in a school year might amount to an increase in math skill scores by 5.

# changed function from dunif to dnorm

# explanation
# It might be the case that the math skill scores obtained based on a school year are normally distributed.
  
  beta1 ~ dnorm(0, 5),

# changed prior for beta2

# explanation
# The 0 indicates an assumption that there is no relationship between age in months and math skills.
# When one gets older, they may have an increase in math skill scores by 2.5.

# changed function from dunif to dnorm

# explanation
# The effect of one's age on math skills is possibly explained by a normal distribution.

  beta2 ~ dnorm(0, 2.5),

# changed prior for beta3

# explanation
# The 0 indicates an assumption that there is no interaction between year in school and one's age in months.
# If there is an interaction between year in school and age in months, this effect would lead to an increase/decrease in math skills by 5.

# changed function from dunif to dnorm

# explanation
# The interaction between the two variables may be normally distributed when measured in math skills.

  beta3 ~ dnorm(0, 5),

# changed prior for sigma

# explanation
# The standard deviation of math skills should be positive (bound at 0). 
# That is, there should be a change in scores for math skills or no change at all.
# A value of 100 should be able to accommodate the variance in this model.

# changed dunif to dcauchy

# explanation
# A Cauchy distribution would help explain the variance in the data better than a uniform distribution.
  
 sigma ~ dcauchy(0, 100)
),
data=d, )

# Part b
precis(m0)

# Comment on model fit
# The model seems to be working fine without any errors.
# The mean scores of math skills is around 52.53 with an SD of 21.72. 
# There is a possible negative effect of school year on math skills (mean=0.20).
# However, this effect is also equally likely to be positive (5.5% quartile= -5.21, 94.5% quartile= 5.20).
# There is a possible positive effect of age on math skills (mean=0.15).
# However, this effect is also likely to have a negative impact on math skills (5.5% quartile= -0.33, 94.5% quartile= 0.62).
# The interaction between one's year in school and age may be a small one (mean= 0.02, sd= 0.03).
# This interaction may be negative or positive (5.5% quartile= -0.03, 94.5% quartile= 0.07).
# The variance for this model is adequate with a value of 7.95 and a standard deviation of 0.59.
# Overall, it is hard to say if school year and age increases one's math skills.
# It is also unclear whether there is a strong interaction between school year and age as well as whether this interaction is positive or negative.