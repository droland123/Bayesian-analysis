library(titanic)
library(brms)
library(dplyr)
library(tidyr)
library(ggplot2)


# https://cran.r-project.org/web/packages/brms/vignettes/brms_overview.pdf section 4.1 provides an example of how to work through setting up a Bayesian analysis in r.
# We will be using the 'brms' library. BRMS stands for "bay"Bayesian Regression Models using Stan"

#example using the 'kidney' data set. 
data("kidney")

# In line 17 + 18 we are defining the formula that will be used for analysis. 
# Line 19 points to the data set we are using 
# Line 20 uses the type of probability distribution that we will be using, another example would be Bernoulli distribution. Which distribution to use will depend on the type of data being used.
# Lines 22 -24 specify the prior distributions for the parameters. 
# Line 25 defines the parameters for MCMC sampling while 26 sets the target acceptance probability for the adaptive step size algorithm.
fit1 <- brm(formula = time | cens(censored) ~ age * sex + disease
            + (1 + age|patient),
            data = kidney, 
            family = lognormal(),
            prior = c(set_prior("normal(0,5)", class = "b"),
                      set_prior("cauchy(0,2)", class = "sd"),
                      set_prior("lkj(2)", class = "cor")),
            warmup = 1000, iter = 2000, chains = 4,
            control = list(adapt_delta = 0.95))
#summary will give us more information about the model we have created. 
summary(fit1)

# We can see the population-level effects and the estimated for each of the variables. 

# Using the titanic data set form the last practicum we can set up a similar analysis. 
# Load the titanic_train dataset
data("titanic_train")

# Convert survival status to a factor with levels "died" and "survived"
titanic_train$Survived <- factor(titanic_train$Survived, levels = c(0, 1), labels = c("died", "survived"))

# Remove rows with missing values
titanic.df.natreated <- titanic.df %>% 
  replace_na(list(Age = median(.$Age, na.rm = TRUE)))

#just to double check that there are no NA values left 
is.na(titanic.df.natreated)

#changing sex to a numerical value
titanic.df.natreated$Sex <- ifelse(titanic.df$Sex == "male", 0, 1)

#for this example, we are going to use the formula Survived ~ Age + Sex + Pclass
#we can use 'get_prior' to help us get some information about the prior parameters we should use

get_prior(Survived ~ Age + Sex + Pclass, data = titanic.df.natreated)

# From this we can see that for our 3 variables the class is b

# Fit a Bayesian logistic regression model
#like above, in line 64 we are defining our formula
# in line 67 we are using the bernoulli distribution because survived is a binomial outcome. 
# we can set out prior parameters based on the information we got out of 'get_prior'
# line 72 defines our mcmc sampling parameters.

fit2 <- brm(
  formula = Survived ~ Age + Sex + Pclass, 
  data = titanic.df.natreated, 
  family = bernoulli(),
  prior = c(
    set_prior("student_t(3, 0, 2.5)", class = "Intercept"), 
    set_prior("normal(0,5)", class = "b"),
    set_prior("normal(0,5)", class = "b", coef = "Sex"),
    set_prior("normal(0,5)", class = "b", coef = "Pclass")),
  warmup = 2000, iter = 4000, chains = 4,
  control = list(adapt_delta = 0.95)
)

# Summarize the model results
summary(fit2)

# As we saw before in the practicum, Pclass has a negative relations with survival. Sex has a positive relationship since we changed the values to '0' for male and '1' for female which also lines up with what we saw before.

#we can use the bayesplot package to create graphs for the model we created above. 
library(bayesplot)

# Create the mcmc_intervals graph.
mcmc_intervals(
  fit2,
  pars = c("b_Intercept", "b_Age", "b_Sex", "b_Pclass"),
   )
# Create the mcmc_histogram graph. 
mcmc_hist(
  fit2,
  pars = c("b_Intercept", "b_Age", "b_Sex", "b_Pclass"),
  alpha = 0.8,
  xlab = "Parameter Value",
  ylab = "Density",
)

