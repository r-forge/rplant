\name{rplant-package}
\alias{rplant-package}
\alias{rplant}
\alias{rPlant}
\docType{package}
\title{rplant}
\description{
	R interface to iPlant's Discovery Enviornment and the Taxonomic Name Resolution Service (TNRS)
}
\details{
	To access iPlant and all of its resources one must attain credentials.  Go to \url{http://user.iplantcollaborative.org/} and register.  Then using proper credentials one can upload a file and run a simple job.  To view detailed worked out examples please check out the rPlant vignette \url{}.

	\tabular{ll}{
	Package: \tab rplant\cr
	Type: \tab Package\cr
	Version: \tab 3.0.0\cr
	Date: \tab 2013-9-16\cr
	License: \tab GPL (>= 2)\cr}
}
\seealso{
	\code{\link{Validate}, \link{UploadFile}, \link{Muscle}}
}
\examples{
\dontrun{data(PROTEIN.fasta)}
\dontrun{write.fasta(sequences = PROTEIN.fasta, names = names(PROTEIN.fasta), file.out = "PROTEIN.fasta")}
\dontrun{Validate("username","password")}
\dontrun{UploadFile("PROTEIN.fasta", filetype="FASTA-0")}
\dontrun{SubmitJob(application="Muscle-3.8.32u4", file.list=list("PROTEIN.fasta"), input.list=list("stdin"), args.list=list(c("arguments", "-phyiout")), job.name="MuscleSJAA")}
### Or, one can use one of the wrapper functions. ###
\dontrun{Muscle("PROTEIN.fasta", aln.filetype="PHYLIP_INT", job.name="muscleAA")}
}
\author{
Barb Banbury, Kurt Michels, Jeremy Beaulieu, Brian O'Meara
Maintainers: Kurt Michels <kamichels@math.arizona.ed>
}
