\name{ResolveNames}
\alias{ResolveNames}
\alias{CompareNames}
\title{Resolving Names}
\description{
	Function ResolveNames changes names that aren't quite correct while compareTNRS will compare the new list of names to the original list of names to see which names changed.
}
\usage{
ResolveNames(names, max.per.call=100, verbose=TRUE) 
CompareNames(old.names, new.names, verbose=TRUE)
}
\arguments{
	\item{names}{Names to be resolved}
	\item{max.per.call}{, default is 100}
	\item{verbose}{optional screen output that displays all of the results from the api, default = TRUE}
	\item{old.names}{original names}
	\item{new.names}{new names}
}
\value{
	The list of names.
}
\examples{
	# data(ex.lp.fasta)
	# speciesNames<-getName(ex.lp.fasta)

	# TNRSspeciesNames<-ResolveNames(speciesNames, 100, verbose=F)

	# CompareNames(speciesNames, TNRSspeciesNames, verbose=TRUE)
}
