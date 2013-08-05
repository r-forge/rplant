\name{RunParsprotein}
\alias{RunParsprotein}
\title{PHYLIP 3.69}
\description{
PHYLIP is a free package of programs for inferring phylogenies. It is distributed as source code, documentation files, and a number of different types of executables. The web page: \url{http://evolution.genetics.washington.edu/phylip/doc/main.html}, by Joe Felsenstein of the Department of Genome Sciences and the Department of Biology at the University of Washington, contain information on PHYLIP.  PHYLIP (the PHYLogeny Inference Package) is a package of programs for inferring phylogenies (evolutionary trees).  Methods that are available in the package include parsimony, distance matrix, and likelihood methods, including bootstrapping and consensus trees.
}
\usage{
RunParsprotein(file.name, file.path="", job.name=NULL,
               version="phylip-protein-parsimony-lonestar-3.69u1",
               print.curl=FALSE, shared.user.name=NULL, 
               suppress.Warnings=FALSE)
}
\arguments{
	\item{file.name}{Name of file to be evaluated on the Discovery Environment (DE).}
	\item{file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{The name to give the job being submitted}
	\item{version}{Protein parsimony version, options include \code{phylip-protein-parsimony-lonestar-3.69u2}}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{shared.user.name}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.  Then the dir.path becomes the path to the SHARED user's shared folder.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
}
\details{
	The input file format that is supported is the interleaved phylip format \url{http://www.bioperl.org/wiki/PHYLIP_multiple_alignment_format}.
}
\seealso{
	\code{\link{SubmitJob}}
}
\examples{
\dontrun{RunParsdna("your-protein-file.phy", job.name="PhylipParsimonyProtein")}
}	
