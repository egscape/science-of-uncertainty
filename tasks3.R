library(rethinking)

hw_d <- read.csv("caffeine_data.csv")


##############
# Question 1
##############

# Part 1

# Create the linear model using MAP

model_1 <- map(
    alist(
        GRE_change ~ dnorm(mu, sigma),
    mu <- a + bc*caffeine,
    a ~ dnorm(0, 20),
    bc ~ dnorm(0, 10),
    sigma ~ dunif(0, 100)
    ),
 data= hw_d )

# Summarise the model
summary(model_1)

# Comment on model
# This model tells us that those given high caffeine would have lower GRE test scores on average, compared to those who were given a lower amount of caffeine.


# Part 2

# Create a dummy variable
hw_d$is_extro <- ifelse(hw_d$group == "extrovert", 1, 0)

# Create the linear model using MAP
model_2 <- map(
    alist(
        GRE_change ~ dnorm( mu , sigma ) ,
        mu <- a + bc*caffeine + bg*is_extro ,
        a ~ dnorm( 0 , 20 ) ,
        bc ~ dnorm( 0 , 10 ) ,
        bg ~ dnorm( 0 , 10 ) ,
        sigma ~ dunif( 0 , 100 )
    ) ,
    data= hw_d )

# Summarise the model
summary(model_2)

# Comment on model
# This model tells us that taking caffeine will reduce GRE test scores on average.
# However, this effect is offset if one is classed as an extrovert.
# This means that on average, caffeine will reduce GRE test scores unless one is classed as an extrovert, in which that effect is nullified.
# If one is classed as an introvert however, their GRE test scores will on average, still tend to decrease.


# Part 3 

# Compute counterfactual predictions
caff.seq<- 0:100
caffeine.seq <- seq(from= 2,to= 4,length.out= 100)
d.predict <- list(
    caffeine = caffeine.seq,
    is_extro = rep(0,1)
    )

pred.model_2 <- link( model_2 , data=d.predict )
mu <- link( model_2, data=hw_d, n=1e4)
mu <- apply( model_2, 2, mean )
mu.PI <- apply( model_2, 2, I )

# Plot the lines
plot( GRE_change ~ caffeine , hw_d , type="n")
lines( caffeine.seq , mu, lty=2 )
lines( caffeine.seq , mu.PI[1,] , lty=2 )
lines( caffeine.seq , mu.PI[2,] , lty=2 )

# NOTE:
# I am unable to resolve the error for this sequence of code.
# I understand it is to do with the different lengths of x and y for mu (when using the apply function), but I am unsure how to correct it.


# Part 4

# Compute WAIC values for model_1
WAIC(model_1)

# Compute WAIC values for model_2
WAIC(model_2)

# Compare the two models together by ranking them by their respective WAIC values
( milk.models <- compare( model_1 , model_2 ) )

# Plot the models against each other to have a better outlook on the comparisons
plot( milk.models , SE=TRUE , dSE=TRUE )

# Comment on appropriate choice of model for question 1
# According to WAIC values, model_1 has a smaller value than model_2 which indicates better out-of-sample deviance.
# Furthermore, the weight of model_1 is higher (0.71) compared to model_2 (0.29).
# Therefore, it is safe to assume that model_1 fits the data better than model_2.



##############
# Question 2
##############

# Informative priors give the model a bit more information to narrow down the possible values that the parameters can take. 
# That is, these priors narrow down the posterior probability of parameters that are highly unlikely. 
# As such, it can be inferred that the model learns less from the data than it would with other priors.
