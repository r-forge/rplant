\name{RunMafft}
\alias{RunMafft}
\title{MAFFT Multiple Sequence Alignment}
\description{
	MAFFT is a multiple sequence alignment program for unix-like operating systems.  It offers a range of multiple alignment methods, L-INS-i (accurate; for alignment of <∼200 sequences), FFT-NS-2 (fast; for alignment of <∼10,000 sequences), etc.  See \url{http://mafft.cbrc.jp/alignment/software/}.  The manual is also available here: \url{http://mafft.cbrc.jp/alignment/software/manual/manual.html} 
}
\usage{
RunMafft(file.name, file.path="", job.name=NULL, print.curl=FALSE,
         version="mafft-lonestar-6.864u1",  args=NULL,
         shared.user.name=NULL, suppress.Warnings=FALSE)
}
\arguments{
	\item{file.name}{name of file to be evaluated on the Discovery Environment (DE), see details for supported input formats.}
	\item{file.path}{optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{the name to give the job being submitted}
	\item{version}{Mafft version, options include \code{mafft-lonestar-6.864u1}, \code{mafftDispatcher-1.0.13100u1}}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{shared.user.name}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.  Then the dir.path becomes the path to the SHARED user's shared folder.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
	\item{args}{Optional for arguments (i.e. flags).  The Mafft model has much additional functionality that is not in this wrapper function (\url{http://mafft.cbrc.jp/alignment/software/manual/manual.html#lbAI}), see details.  This option allows users to add anything that is not included (.i.e. args="--auto"), which automatically selects an appropriate strategy according to data size, see details.}
}
\details{
	The supported input file format is the fasta format \url{http://en.wikipedia.org/wiki/FASTA_format}.

	Additional arguments, \code{args}, can be found at \url{http://mafft.cbrc.jp/alignment/software/manual/manual.html#lbAI}.  The args input is text with the flags and inputs for those flags in a string like on the command line.
}
\value{
	A list containing the job id and the job name is provided for jobs submitted.  If an error, then a message stating the error should also be reported.
}
\seealso{
	\code{\link{SubmitJob}}
}
\examples{
\dontrun{RunMafft("ex.lp.fasta", args="--auto -nofft")}
}	
