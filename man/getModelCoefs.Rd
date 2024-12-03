\title{
  Get exponentionnal coefficient of a model
}
\name{sankey}
\alias{getModelCoefs}
\alias{getModelCoefs}
\description{
  Return the exponentionnal of coefficient from a model that uses such output (Or, RR, HR...) along with the 95 percent confidence interval and p value associated to the ratio.
}\usage{
  getModelCoefs(
    model, 
    coefs_digits = 2,
    p_digits = 3,
    p_threshold = 0.001,
    merge_p = TRUE
    ) 
}\arguments{
  \item{model}{
 Output of a model such as glm, cox, geeglm... Any model that have a \code{coefficients} element structured in four columns consisting of 
    the estimate, estimate standard error, statistic value and p-value of the statistic.
}
 \item{coefs_digits}{
 Number of digits to include for the coefficient and confidence interval.
}
   \item{p_digits}{
 Number of digits to include for the p-value.
}
   \item{p_threshold}{
 Threshold under wich the p-value will be replaced by \code{<0.001} for instance.
}
   \item{merge_p}{
 Whether or not to merge de coefficient an p-value columns into one.
}
}\value{
  A data frame of 2 or 3 columns depending on \code{merge_p} and as many rows and the model coefficient minus one (excluding the intercept).
}\author{
  Hugo Marthinet
}\examples{
  ## Dobson (1990) Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
data.frame(treatment, outcome, counts) # showing data
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
anova(glm.D93)
summary(glm.D93)

getModelCoefs(glm.D93) 
}
