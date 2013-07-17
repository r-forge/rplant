\name{RunPLINK}
\alias{RunPLINK}
\title{PLINK-1.07}
\description{
	This function will submit a PLINK job to iPlant's DE
}
\usage{
RunPLINK(user.name, token, DE.file.list="", DE.file.path="",
         job.name=NULL, association.method="--assoc", no.sex=TRUE,
         print.curl=FALSE, multi.adjust=TRUE, nprocs=1, args=NULL,
         version="plink-1.07", shared.user.name=NULL)
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{A validation tool used in place of unencrypted passwords}
	\item{DE.file.list}{Names of files, in a list format, to be evaluated on the Discovery Environment (DE).  There are only three possible input groups for DE.file.list, regular fileset (.map/.ped),  transposed fileset (.tfam/.tped), and binary fileset (.bed/.bim/.fam).}
	\item{DE.file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{The name to give the job being submitted}
	\item{association.method}{PLINKs association methods.  Choices are outlined on the PLINK webpage, http://pngu.mgh.harvard.edu/~purcell/plink/anal.shtml}
	\item{no.sex}{The sex column (5) is all zeroes (No sex field) (default is TRUE)}
	\item{multi.adjust}{Adjustment for multiple testing (recommended).  A file of adjust significance values that correct for all tests performed and other metrics will be created (default is TRUE).}
	\item{args}{Optional for arguments (i.e. flags).  The PLINK model has so much additional functionality that it cannot all be fit into this wrapper function (\url{http://pngu.mgh.harvard.edu/~purcell/plink}).  This option allows users to add anything that is not included (.i.e. args="--silent"), to suppress output to console. }
	\item{print.curl}{Print the curl statement}
	\item{nprocs}{the number of processors to be allocated to the job, default is 1}
	\item{version}{PLINK version, options include \code{plink-1.07}}
	\item{shared.user.name}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.  Then the DE.dir.path becomes the path to the SHARED user's shared folder. }
}
\details{
	Not all of the inputs, for this application will be used at the same time, in fact the inputs are to be used only in three very strict groups.  Group 1: inputTFAM, inputTPED, these are the PLINK transposed filesets.  Group 2: inputMAP, inputPED, these are the PLINK regular filesets.  Group 3: inputBED, inputBIM, inputFAM, these are the PLINK binary filesets.

See the PLINK website for more information. \url{http://pngu.mgh.harvard.edu/~purcell/plink}
}
\seealso{
	\code{\link{SubmitJob}}
}
\value{
	Text listing action as either being a "success" or "error". If an error, then a message stating the error should also be reported.  
}
\examples{
# RunPLINK(user.name, token, DE.file.list=list("geno_test.tfam","geno_test.tped"),
#          DE.file.path="data/Default", print.curl=TRUE)
}	
