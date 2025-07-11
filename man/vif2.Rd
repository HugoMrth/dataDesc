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

}
