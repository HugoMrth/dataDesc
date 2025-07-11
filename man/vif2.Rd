\title{
  VIF for multinomial logistic regression
}
\name{vif2}
\alias{vif2}
\description{
  Adaption of the \code{car::vif()} function to a \code{nnet::multinom() object}.
}\usage{
  vif2(model, multinom = TRUE)
}\arguments{
  \item{model}{
    glm or multinom object
}\item{multinom}{
    Specify whether \code{model} is a multinomial regression or a classic \code{glm} model.
}
}\value{
  VIF value, data framme or list of data frame depending on the model.
}\author{
  Hugo Marthinet
}\examples{
library(nnet)
library(dplyr)


mod <- nnet::multinom(Species ~ Sepal.Width + Petal.Length, data = iris)
vif2(mod, multinom = TRUE)

mod <- nnet::multinom(Species ~ Sepal.Length + Sepal.Width +
                        Petal.Length + Petal.Width, data = iris)
vif2(mod, multinom = TRUE)



BEPS2 <- carData::BEPS %>%
  mutate(economic.cond.national = as.factor(economic.cond.national),
         economic.cond.household = as.factor(economic.cond.household))
mod <- nnet::multinom(vote ~ gender + economic.cond.national + economic.cond.household, 
                      data = BEPS2)
vif2(mod, multinom = TRUE)




BEPS2 <- BEPS2 %>%
  mutate(economic.cond.national2 = ifelse(
    economic.cond.national == "1" | economic.cond.national == "2", "Low",
    ifelse(economic.cond.national == "3", "Middle", "High")
  ))
mod <- nnet::multinom(vote ~ gender + economic.cond.national + 
                        economic.cond.household + economic.cond.national2, 
                      data = BEPS2)
vif2(mod, multinom = TRUE)
}
