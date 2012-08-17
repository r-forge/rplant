\name{ResolveNames}
\alias{ResolveNames}
\alias{CompareNames}
\title{Resolving Names}
\description{
	The function ResolveNames interacts with the TNRS server at iPlant that uses fuzzy name matching to find standardized taxonomic plant names.  CompareTNRS will compare the original list of names to the returned TNRS names to see which names changed.
}
\usage{
ResolveNames(names, max.per.call=100, verbose=TRUE) 
CompareNames(old.names, new.names, verbose=TRUE)
}
\arguments{
	\item{names}{Vector of names to be resolved via TNRS}
	\item{max.per.call}{The number of names to submit at a time, default is 100}
	\item{verbose}{Optional screen output that displays all of the results from the api, default = TRUE}
	\item{old.names}{Original names}
	\item{new.names}{Returned TNRS names}
}
\value{
	Vector of taxonomic names
}
\examples{
	# data(ex.lp.fasta)
	# speciesNames<-getName(ex.lp.fasta)

	# TNRSspeciesNames<-ResolveNames(speciesNames, 100, verbose=F)

	# CompareNames(speciesNames, TNRSspeciesNames, verbose=TRUE)
}
