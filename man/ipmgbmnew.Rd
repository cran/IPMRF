\name{ipmgbmnew}
\alias{ipmgbmnew}
\title{
IPM casewise with gbm object by \pkg{gbm} for new cases, whose responses do not need to be known}
\description{
The IPM of a new case, i.e. one not used to grow the forest and whose true
response does not need to be known, is computed as follows. The new case is put
down each of the \emph{ntree} trees in the forest. For each tree, the case goes from the
root node to a leaf through a series of nodes. The variable split in these nodes
is recorded. The percentage of times a variable is selected along the case's way
from the root to the terminal node is calculated for each tree. Note that we do not
count the percentage of times a split occurred on variable k in tree t, but only the
variables that intervened in the prediction of the case. The IPM for this new case
is obtained by averaging those percentages over the \emph{ntree} trees.}
\usage{
ipmgbmnew(marbolr, da, ntree)
}
\arguments{
  \item{marbolr}{
Generalized Boosted Regression object obtained with \code{\link[gbm]{gbm}}.}
  \item{da}{
Data frame with the predictors only, not responses, for the new cases. Each row corresponds to an observation and each column corresponds to a predictor, which obviously must be the same variables used as predictors in the training set.}
  \item{ntree}{
Number of trees.}
}
\details{
All details are given in Epifanio (2017).
}
\value{
It returns IPM for new cases. It is a matrix with as many rows as cases are in da, and as many columns as predictors are in da.
}
\references{
Pierola, A. and Epifanio, I. and Alemany, S. (2016) An ensemble of ordered logistic regression and random forest for child garment size matching. \emph{Computers & Industrial Engineering}, \bold{101}, 455--465.

Epifanio, I. (2017) Intervention in prediction measure: a new approach to assessing variable importance for random forests. \emph{BMC Bioinformatics}, \bold{18}, 230.
}
\author{
Stefano Nembrini
}
\note{
See Epifanio (2017) about the parameters of RFs to be used, the advantages and limitations of IPM, and in particular when CART is considered with predictors of different types.
}
\seealso{
\code{\link{ipmparty}}, \code{\link{ipmrf}}, \code{\link{ipmranger}}, \code{\link{ipmpartynew}}, \code{\link{ipmrfnew}}
}
\examples{

\dontrun{
library(party)
library(gbm)
gbm=gbm(score ~ ., data = readingSkills, n.trees=50, shrinkage=0.05, interaction.depth=5, 
        bag.fraction = 0.5, train.fraction = 0.5, n.minobsinnode = 1, 
        cv.folds = 0, keep.data=F, verbose=F)
apply(ipmgbmnew(gbm,readingSkills[,-4],50),FUN=mean,2)->gbm_ipm
gbm_ipm}
}
\keyword{tree}
\keyword{multivariate}
