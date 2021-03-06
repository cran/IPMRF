\name{ipmparty}
\alias{ipmparty}
\title{IPM casewise with CIT-RF by \pkg{party} for OOB samples}
\description{
The IPM for a case in the training set is calculated by considering and averaging
over only the trees where the case belongs to the OOB set. The case is put
down each of the trees  where the case belongs to the OOB set. For each tree, the case goes from the root node to a leaf through a series of nodes. The variable split in these nodes is recorded. The percentage of times a variable is selected along the case's way from the root to the terminal node is calculated for each tree. Note that we do not count the percentage of times a split occurred on variable k in tree t, but only the variables that intervened in the prediction of the case. The IPM for this case is obtained by averaging those percentages over only the trees where the case belongs to the OOB set. The random forest is based on CIT (Conditional Inference Trees).
}
\usage{
ipmparty(marbol, da, ntree)
}
\arguments{
  \item{marbol}{
Random forest obtained with \code{\link[party]{cforest}}
. Responses can be of the same type supported by \code{\link[party]{cforest}}, not only numerical or nominal, but also  ordered responses,  censored response variables and multivariate responses.}
  \item{da}{
Data frame with the predictors only, not responses, of the training set used for computing \emph{marbol}. Each row corresponds to an observation and each column corresponds to a predictor. Predictors can be numeric, nominal or an ordered factor.}
  \item{ntree}{
Number of trees in the random forest.}
}
\details{
All details are given in Epifanio (2017).
}
\value{
It returns IPM for cases in the training set. It is estimated when they are OOB observations. It is a matrix with as many rows as cases are in da, and as many columns as predictors are in da.
IPM can be estimated for any kind of RF computed by \code{\link[party]{cforest}}, including
multivariate RF.
}
\references{
Pierola, A. and Epifanio, I. and Alemany, S. (2016) An ensemble of ordered logistic regression and random forest for child garment size matching. \emph{Computers & Industrial Engineering}, \bold{101}, 455--465.

Epifanio, I. (2017) Intervention in prediction measure: a new approach to assessing variable importance for random forests. \emph{BMC Bioinformatics}, \bold{18}, 230.
}
\author{
Irene Epifanio
}
\note{
See Epifanio (2017) about advantages and limitations of IPM, and about the parameters to be used in \code{\link[party]{cforest}}.
}
\seealso{
\code{\link{ipmpartynew}}, \code{\link{ipmrf}}, \code{\link{ipmranger}}, \code{\link{ipmrfnew}}, \code{\link{ipmrangernew}}, \code{\link{ipmgbmnew}}
}
\examples{
#Note: more examples can be found at 
#https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-017-1650-8

## -------------------------------------------------------
## Example from \code{\link[party]{varimp}} in \pkg{party}
## Classification RF
## -------------------------------------------------------

\dontrun{
library(party)

#from help in varimp by party package
set.seed(290875)
readingSkills.cf <- cforest(score ~ ., data = readingSkills,
control = cforest_unbiased(mtry = 2, ntree = 50))

# standard importance
varimp(readingSkills.cf)

# the same modulo random variation
varimp(readingSkills.cf, pre1.0_0 = TRUE)

# conditional importance, may take a while...
varimp(readingSkills.cf, conditional = TRUE)
}

#IMP based on CIT-RF (party package)
library(party)

ntree<-50
#readingSkills: data from party package
da<-readingSkills[,1:3] 
set.seed(290875)
readingSkills.cf3 <- cforest(score ~ ., data = readingSkills,
control = cforest_unbiased(mtry = 3, ntree = 50))

#IPM case-wise computed with OOB with party
pupf<-ipmparty(readingSkills.cf3 ,da,ntree)
 
#global IPM
pua<-apply(pupf,2,mean) 
pua

## -------------------------------------------------------
## Example from \code{\link[randomForestSRC]{var.select}} in \pkg{randomForestSRC} 
## Multivariate mixed forests
## -------------------------------------------------------

\dontrun{
library(randomForestSRC)

#from help in var.select by randomForestSRC package
mtcars.new <- mtcars
mtcars.new$cyl <- factor(mtcars.new$cyl)
mtcars.new$carb <- factor(mtcars.new$carb, ordered = TRUE)
mv.obj <- rfsrc(cbind(carb, mpg, cyl) ~., data = mtcars.new,
importance = TRUE)
var.select(mv.obj, method = "vh.vimp", nrep = 10) 

#different variables are selected if var.select is repeated 

}

#IMP based on CIT-RF (party package)
library(randomForestSRC)
mtcars.new <- mtcars

ntree<-500
da<-mtcars.new[,3:10] 
mc.cf <- cforest(carb+ mpg+ cyl ~., data = mtcars.new,
control = cforest_unbiased(mtry = 8, ntree = 500))

#IPM case-wise computing with OOB with party
pupf<-ipmparty(mc.cf ,da,ntree) 

#global IPM
pua<-apply(pupf,2,mean) 
pua

#disp and hp are consistently selected as more important if repeated


}
\keyword{tree}
\keyword{multivariate}
