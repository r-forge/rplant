\name{RunRAxMLdna}
\alias{RunRAxMLdna}
\title{RAxML-DNA}
\description{
	RAxML (Randomized Accelerated Maximum Likelihood) is a program for sequential and parallel Maximum Likelihood based inference of large phylogenetic tress.  It has originall been derived from from fastDNAml which in turn was derived from Joe Felsentein's dnaml which is part of the PHYLIP package. See \url{http://sco.h-its.org/exelixis/oldPage/RAxML-Manual.7.0.4.pdf} for details.
}
\usage{
RunRAxMLdna(file.name, file.path="", job.name=NULL,
            model="GTRCAT", bootstrap=NULL, algorithm="d",
            multipleModelFileName=NULL, args=NULL, numcat=25,
            nprocs=12, version="raxml-lonestar-7.2.8u1",
            print.curl=FALSE, shared.username=NULL,
            suppress.Warnings=FALSE)
}
\arguments{
	\item{file.name}{Name of file to be evaluated on the Discovery Environment (DE), see details for supported input formats.}
	\item{file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{the name to give the job being submitted}
	\item{model}{(-m) Substitution model.  Choices are \code{GTRCAT}, \code{GTRGAMMA}, \code{GTRCAT.GAMMA}, \code{GTRGAMMAI}, \code{GTRCAT.GAMMAI}.  The details p. 10-11 of manual.}
	\item{bootstrap}{(-b) Random Seed Number for non-parametric bootstrapping, details bottom of p. 7 of manual}
	\item{algorithm}{(-f) Select the type of algorithm/function you want. "d" is default, details bottom of p. 8 of manual}
	\item{multipleModelFileName}{(-q) substitution model file, details bottom of p. 7 of manual}
	\item{numcat}{(-c) Specify the number of distinct rate categories, details top of p. 13 of manual}
	\item{nprocs}{The number of processors to be allocated to the job, default = 12}
	\item{version}{RAxML version, options include \code{raxml-lonestar-7.2.8u1}}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{shared.username}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.  Then the dir.path becomes the path to the SHARED user's shared folder.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
	\item{args}{Optional for arguments (i.e. flags).  The RAxML model has much additional functionality that is not fit into this wrapper function (\url{http://sco.h-its.org/exelixis/oldPage/RAxML-Manual.7.0.4.pdf}), see details.  This option allows users to add anything that is not included (.i.e. args="-d"), which starts the RAxML search with a random starting tree, see detals.}
}
\details{
	The input file format that is supported is the interleaved phylip format \url{http://www.bioperl.org/wiki/PHYLIP_multiple_alignment_format}.

	Additional arguments, \code{args}, can be found at \url{http://sco.h-its.org/exelixis/oldPage/RAxML-Manual.7.0.4.pdf}.  The args input is text with the flags and inputs for those flags in a string like on the command line.
}
\value{
	A list containing the job id and the job name is provided for jobs submitted.  If an error, then a message stating the error should also be reported.
}
\seealso{
	\code{\link{SubmitJob}}
}
\examples{
\dontrun{RunRAxMLdna("muscle.phy", model="GTRCAT", bootstrap=NULL, numcat=25)}
}	
