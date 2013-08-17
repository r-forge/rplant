\name{ClustalW}
\alias{ClustalW}
\alias{Clustal}
\title{ClustalW alignment}
\description{
	An approach for performing multiple alignments of large numbers of amino acid or nucleotide sequences is described. The method is based on first deriving a phylogenetic tree from a matrix of all pairwise sequence similarity scores, obtained using a fast pairwise alignment algorithm.  See details on \url{http://www.clustal.org/clustal2/}.
}
\usage{
ClustalW(file.name, file.path="", job.name=NULL, args=NULL,
         version="ClustalW2-2.1u1", print.curl=FALSE,
         shared.username=NULL, suppress.Warnings=FALSE)
}
\arguments{
	\item{file.name}{Name of file to be evaluated on the Discovery Environment (DE), see details for supported input formats.}
	\item{file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{The name to give the job being submitted}
	\item{version}{ClustalW version, options include \code{ClustalW2-2.1u1}, \code{clustalw2Dispatcher-1.0.13100u1}, \code{clustalw2-lonestar-2.1u2}}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{args}{Optional for arguments (i.e. flags).  The ClustalW model has much additional functionality that is not fit into this wrapper function (\url{http://www.clustal.org/download/clustalw_help.txt}), see details.  This option allows users to add anything that is not included (.i.e. args="-ITERATION=TREE"), to iterate at each step, see details.}
	\item{shared.username}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.  Then the dir.path becomes the path to the SHARED user's shared folder.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
}
\seealso{
	\code{\link{SubmitJob}}
}
\details{
	The supported input file format is the fasta format \url{http://en.wikipedia.org/wiki/FASTA_format}.

	Additional arguments, \code{args}, can be found at \url{http://www.clustal.org/download/clustalw_help.txt}.  The args input is text with the flags and inputs for those flags in a string like on the command line.
}
\value{
	A list containing the job id and the job name is provided for jobs submitted.  If an error, then a message stating the error should also be reported.
}
\examples{
\dontrun{ClustalW(file.name="ex.lp.fasta", job.name="clustalw.Job1", version="clustalw2-lonestar-2.1u2")}
}	
