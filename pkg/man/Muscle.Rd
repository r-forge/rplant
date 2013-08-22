\name{Muscle}
\alias{Muscle}
\title{MUSCLE alignment}
\description{
	MUSCLE is a program for creating multiple alignments of amino acid or nucleotide sequences. A range of options is provided that give you the choice of optimizing accuracy, speed, or some compromise between the two.  The manual is also available here: \url{http://www.drive5.com/muscle/muscle_userguide3.8.html} 
}
\usage{
Muscle(file.name, file.path="", job.name=NULL, args=NULL, 
       version="Muscle-3.8.32u1", print.curl=FALSE,
       shared.username=NULL, suppress.Warnings=FALSE)
}
\arguments{
	\item{file.name}{name of file to be evaluated on the Discovery Environment (DE), see details for supported input formats.}
	\item{file.path}{optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{the name to give the job being submitted}
	\item{version}{Muscle version, do not use any other Muscle versions.}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{shared.username}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
	\item{args}{Optional for arguments (i.e. flags).  The Muscle model has much additional functionality that is not fit into this wrapper function (\url{http://www.drive5.com/muscle/muscle_userguide3.8.html#_Toc260497051}), see details.  This option allows users to add anything that is not included (.i.e. args="-spscore"), which computes SP objective score, see details.}
}
\details{
	The supported input file format is the fasta format \url{http://en.wikipedia.org/wiki/FASTA_format}.

	Additional arguments, \code{args}, can be found at \url{http://www.drive5.com/muscle/muscle_userguide3.8.html#_Toc260497051}.  The args input is text with the flags and inputs for those flags in a string like on the command line.

	There are five alignment output files, named appropriately: \code{phylip_interleaved.aln} \url{http://www.bioperl.org/wiki/PHYLIP_multiple_alignment_format}, \code{phylip_sequential.aln} \url{http://www.bioperl.org/wiki/PHYLIP_multiple_alignment_format}, \code{fasta.aln} \url{http://en.wikipedia.org/wiki/FASTA_format} and \code{clustalw.aln} \url{http://meme.nbcr.net/meme/doc/clustalw-format.html}.
}
\value{
	A list containing the job id and the job name is provided for jobs submitted.  If an error, then a message stating the error should also be reported.
}
\seealso{
	\code{\link{SubmitJob}}
}
\examples{
\dontrun{Muscle("ex.lp.fasta", job.name="muscleJob1")}
}	
