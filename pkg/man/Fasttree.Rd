\name{RunFasttree}
\alias{RunFasttree}
\title{FastTree Dispatcher}
\description{
	FastTree infers approximately-maximum-likelihood phylogenetic trees from alignments of nucleotide or protein sequences. See \url{http://meta.microbesonline.org/fasttree/}
}
\usage{
RunFasttree(file.name, file.path="", job.name=NULL, args=NULL,
            print.curl=FALSE, version="fasttreeDispatcher-1.0.0u1", 
            shared.username=NULL, suppress.Warnings=FALSE)
}
\arguments{
	\item{file.name}{name of file to be evaluated on the Discovery Environment (DE), see details for supported input formats.}
	\item{file.path}{optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{the name to give the job being submitted}
	\item{version}{FastTree version, options include \code{fasttreeDispatcher-1.0.0u1}}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{shared.username}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.  Then the dir.path becomes the path to the SHARED user's shared folder.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
	\item{args}{Optional for arguments (i.e. flags).  The Fasttree model has much additional functionality that is not fit into this wrapper function (\url{http://meta.microbesonline.org/fasttree/#Usage}), see details.  This option allows users to add anything that is not included (.i.e. args="-gtr"), which uses the `Jukes-Cantor + CAT' model instead, see details.}
}
\details{
	The input file formats that are supported are fasta format \url{http://en.wikipedia.org/wiki/FASTA_format} or interleaved phylip format \url{http://www.bioperl.org/wiki/PHYLIP_multiple_alignment_format}.

	Additional arguments, \code{args}, can be found at \url{http://meta.microbesonline.org/fasttree/}.  The args input is text with the flags and inputs for those flags in a string like on the command line.
}
\value{
	A list containing the job id and the job name is provided for jobs submitted.  If an error, then a message stating the error should also be reported.
}
\seealso{
	\code{\link{SubmitJob}}
}
\examples{
	\dontrun{RunFasttree("muscle.phy")}
}	
