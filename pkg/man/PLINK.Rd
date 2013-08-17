\name{PLINK}
\alias{PLINK}
\title{PLINK-1.07}
\description{
	PLINK is an open-source whole genome association analysis toolset, designed to perform a range of basic, large-scale analyses in a computationally efficient manner, check \url{http://pngu.mgh.harvard.edu/~purcell/plink/} for details.
}
\usage{
PLINK(file.list="", file.path="", job.name=NULL,
         association.method="--assoc", no.sex=TRUE, args=NULL,
         print.curl=FALSE, multi.adjust=TRUE, version="plink-1.07", 
         shared.username=NULL, suppress.Warnings=FALSE)
}
\arguments{
	\item{file.list}{Names of files, in a list format, to be evaluated on the Discovery Environment (DE).  There are only three possible input groups for file.list, regular fileset (.map/.ped),  transposed fileset (.tfam/.tped), and binary fileset (.bed/.bim/.fam).}
	\item{file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{The name to give the job being submitted}
	\item{association.method}{PLINKs association methods.  Choices are outlined on the PLINK webpage, http://pngu.mgh.harvard.edu/~purcell/plink/anal.shtml}
	\item{no.sex}{The sex column (5) is all zeroes (No sex field) (default is TRUE)}
	\item{multi.adjust}{Adjustment for multiple testing (recommended).  A file of adjust significance values that correct for all tests performed and other metrics will be created (default is TRUE).}
	\item{args}{Optional for arguments (i.e. flags).  The PLINK model has so much additional functionality that it cannot all be fit into this wrapper function (\url{http://pngu.mgh.harvard.edu/~purcell/plink/reference.shtml#options}).  This option allows users to add anything that is not included (.i.e. args="--silent"), to suppress output to console, see details.}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{version}{PLINK version, options include \code{plink-1.07}}
	\item{shared.username}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.  Then the dir.path becomes the path to the SHARED user's shared folder.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
}
\details{
	Not all of the inputs, for this application will be used at the same time, in fact the inputs are to be used only in three very strict groups.  Group 1: inputTFAM, inputTPED, these are the PLINK transposed filesets.  Group 2: inputMAP, inputPED, these are the PLINK regular filesets.  Group 3: inputBED, inputBIM, inputFAM, these are the PLINK binary filesets.

See the PLINK website for more information. \url{http://pngu.mgh.harvard.edu/~purcell/plink}

	Additional arguments, \code{args}, can be found at \url{http://pngu.mgh.harvard.edu/~purcell/plink/reference.shtml#options}.  The args input is text with the flags and inputs for those flags in a string like on the command line.
}
\value{
	A list containing the job id and the job name is provided for jobs submitted.  If an error, then a message stating the error should also be reported.
}
\seealso{
	\code{\link{SubmitJob}}
}
\examples{
\dontrun{PLINK(file.list=list("geno_test.tfam","geno_test.tped"), file.path="data/Default", print.curl=TRUE)}
}	
