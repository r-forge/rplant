\name{RunQuicktree}
\alias{RunQuicktree}
\title{Quicktree}
\description{
	QuickTree allows the reconstruction of phylogenies for very large protein families that would be infeasible using other popular methods. see \url{http://bioinformatics.oxfordjournals.org/content/18/11/1546}
}
\usage{
RunQuicktree(file.name, file.path="", job.name=NULL, UPGMA=FALSE,
             kimura=FALSE, boot=0, output.file.name=NULL,
             version="quicktree-tree-lonestar-1.1u1", 
             print.curl=FALSE, shared.username=NULL, 
             suppress.Warnings=FALSE)
}
\arguments{
	\item{file.name}{Name of file to be evaluated on the Discovery Environment (DE), see details for supported input formats.}
	\item{file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{UPGMA}{Use the UPGMA method to construct the tree (ignored for distance matrix outputs). \url{http://en.wikipedia.org/wiki/UPGMA}}
	\item{kimura}{Use the kimura translation for pairwise distances (ignored for distance matrix outputs)}
	\item{boot}{Calculate bootstrap values with `n` interations (ignored for distance matrix outputs).  `boot' is set to a value, the number of iterations.}
	\item{job.name}{The name to give the job being submitted}
	\item{version}{QuickTree version, options include \code{quicktree-tree-lonestar-1.1u2}, \code{quicktree-dm-lonestar-1.1u2}}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{shared.username}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.  Then the dir.path becomes the path to the SHARED user's shared folder.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
}
\details{
	The input file formats that are supported are fasta format \url{http://en.wikipedia.org/wiki/FASTA_format} or stockholm format \url{http://en.wikipedia.org/wiki/Stockholm_format}.
}
\value{
	A list containing the job id and the job name is provided for jobs submitted.  If an error, then a message stating the error should also be reported.
}
\seealso{
	\code{\link{SubmitJob}}
}
\examples{
\dontrun{RunQuicktree(file.name="ex.lp.fasta", job.name="QTrun")}
}	
