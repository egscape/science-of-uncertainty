# Homework 2
library(rethinking)
 
   
# Question 1
   
 model_spec <- alist(
       y ~ dnorm(mu, sigma), 
       mu <- beta0 + beta1 * x,
       beta0 ~ dnorm(0, 10), 
       beta1 ~ dnorm(0, 5),  
       sigma ~ dunif(0, 50))
 
# Translate into a mathematical model definition
     
# y ∼ Normal(µi, σ) 
# µi = β0 + β1xi
# β0 ∼ Normal(0, 10)
# β1 ∼ Normal(0, 5)
# σ ∼ Uniform(0, 50)
     
# Label and describe which is the likelihood and prior
     
# likelihood
# y ∼ Normal(µi, σ)
     
# Description 
# likelihood of y based on a normal distribution with a mean of mu and a standard deviation of sigma
     
# First prior
# β0 ∼ Normal(0, 10)
     
# Description
# A prior normally distributed with a value of 0 and an uncertainty of 10 units. It is the y-intercept of the best fit line
     
# Second prior
# β1 ∼ Normal(0, 5)
     
# Description
# A prior with a value of 0 and an uncertainty of 5 units. It is the slope of the best fit line
     
# Third prior
# σ ∼ Uniform(0, 50)
     
# Description
# A prior uniformly distributed with a minimum value of 0 and a maximum value of 50. It describes how individual points may differ along the best fit line 
     
     
# Question 2
     
     d <- data.frame(relationship = c("mz_twin", "dz_twin", "siblings",
     "parent_child", "adoptive_parent_child", "unrelated_children"),
          shared_genes = c(1, .50, .50, .50, 0, 0),
          IQ_correlation = c(0.88, 0.59, 0.48, 0.43, 0.17, 0.32))
     
# Part 1
       
# Define the model 
       
# likelihood
# IQ_correlation ∼ Normal(µi, σ)
       
# explanation
# It makes sense to use a normal distribution as the distributions of IQ correlation are unknown (at least to me).
       
# linear regression equation
# µi = α + βxi
       
# explanation
# linear models are represented in its most basic form as y=mx+c.
       
# alpha prior
# α ∼ Normal(0.5, 0.25)
       
# explanation
# I think it is reasonable to assume that average correlations of IQ to be around 0.5 as it is right in the centre between 0 and 1.
# I believe that it is near certain that average correlations of IQ lie between 0 and 1.
       
# beta prior
# β ∼ Normal(0, 0.10)
       
# explanation
# To be on the safe side, I think it is safe to assume that there is a possibility of there being no relationship between the proportion of shared genes and correlations in IQ.
# An increase in the proportion of shared genes should amount to an increase of around 0.10 for correlations of IQ.
       
# sigma prior
# σ ∼ Uniform(0, 10)
       
# explanation
# The standard deviation of σ should be positive therefore, it makes sense to bound it at zero as there can either be a postive correlation of IQ or no correlation at all. 
# A value of 10 should be appropriate to accommodate the variance in this model. 
       
# Fit the model using MAP
       Q2_model <- map(
                  alist(
               IQ_correlation ~ dnorm( mu , sigma ) ,
               mu <- a + b*shared_genes,
               a ~ dnorm( 0.50 , 0.25 ) ,
               b ~ dnorm( 0 , 0.10 ) ,
               sigma ~ dunif( 0 , 10 )
           ),
         data=d
         )
       
# Part 2
         
# summarize the model fit
summary(Q2_model)
      
# Part 3
         
# extract 20 samples from the posterior of the model
post<- extract.samples(Q2_model,20)
       
# make a graph of the proportion of shared genes against correlations in IQ
plot(d$shared_genes, d$IQ_correlation)
       
# plot 20 transparent uncertainty lines on the graph
  for (i in 1:20) {
      abline(post$a[i], post$b[i], col= col.alpha("blue",0.1 ))
  }
       
# plot the MAP line
abline( a=coef(Q2_model)["a"] , b=coef(Q2_model)["b"] )
       
         
# comment on model fit 
# the graph based on the model fit shows a positive correlation between the proportion of shared genes and larger correlations in intelligence.
         
# Part 4
         
# The model tells us that half-siblings are more likely to have higher correlations in IQ especially when compared to those who are unrelated or are adopted.
# However certain relationships (i.e. pairs of twins, siblings, parent/child pairs) are still more likely to have higher correlations in IQ than half-siblings.
         
         
# Question 3
         
# linear regression equation: y=mx+c
# y= severity of depression symptoms (dependent variable)
# x= time of assessment (independent variable)
# A possible limitation of linear regression in this case would be the assumption that there is a linear relationship between the two variables. 
# Depressive symptoms may be more/less prevalent at different times of the day and may differ from day to day.
# These can be due to external factors such as life events or medications taken at different times. 
# As such, it may not be reasonable to assume that there is a linear relationship between the two variables as there may be other variables at play which affect one's depressive symptoms.