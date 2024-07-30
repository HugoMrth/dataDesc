\title{
  Shorten a descriptive by tidying Yes/No variable
}
\name{describeTidy}
\alias{describeTidy}
\description{
  Detects and suppress Yes/No variables in a descriptive object from the \code{describe} function. 
  Only the "Yes" answer is kept and copied in the same line as the label, thus suppressing two lines and keeping the important informations.
}\usage{
  describeTidy(desc.object)
}\arguments{
  \item{desc.object}{
    A descriptive object as outputed by the \code{describe} function.
}
}\value{
  A tidy descriptive object
}\author{
  Hugo Marthinet
}\examples{
  iris2 <- iris
  iris2$Species <- ifelse(iris2$Species == "setosa", "Yes", "No")

  desc1 <- describe(iris2)
  desc2 <- describeTidy(desc1)
}
