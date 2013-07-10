\name{RunPLINK}
\alias{runplink}
\title{PLINK-1.07}
\description{
	This function will submit a PLINK job to iPlant's DE
}
\usage{
RunPLINK <- function(user.name, token, DE.file.list="", DE.file.path="",
                     input.list=list("inputMAP","inputPED"), input.type="R", 
                     job.name=NULL, association.method="--assoc", no.sex=TRUE,
                     print.curl=FALSE, multi.adjust=TRUE, nprocs=1, version="plink-1.07")
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{A validation tool used in place of unencrypted passwords}
	\item{DE.file.list}{Names of files, in a list format, to be evaluated on the Discovery Environment (DE).}
	\item{DE.file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{input.list}{A list of the name of inputs.  See details for more information.}
	\item{input.type}{Correponds with input list, there are only three possible inputs, "R" for Regular fileset (.map/.ped), "T" for Transposed fileset (.tfam/.tped), and "B" for Binary fileset (.bed/.bim/.fam).}
	\item{job.name}{The name to give the job being submitted}
	\item{association.method}{PLINKs association methods.  Choices are outlined on the PLINK webpage, http://pngu.mgh.harvard.edu/~purcell/plink/anal.shtml}
	\item{no.sex}{The sex column (5) is all zeroes (No sex field) (default is TRUE)}
	\item{multi.adjust}{Adjustment for multiple testing (recommended).  A file of adjust significance values that correct for all tests performed and other metrics will be created (default is TRUE).}
	\item{print.curl}{Print the curl statement}
	\item{nprocs}{the number of processors to be allocated to the job, default = 2}
	\item{version}{RAxML version, options include \code{raxml-lonestar-7.2.8u1}}
}
\details{
	The input.list for PLINK contains inputTFAM, inputTPED, inputMAP, inputPED, inputBED, inputBIM, inputFAM.  Not all of the inputs will be used at the same time, in fact the inputs are to be used only in three very strict groups.  Group 1: inputTFAM, inputTPED, these are the PLINK transposed filesets.  Group 2: inputMAP, inputPED, these are the PLINK regular filesets.  Group 3: inputBED, inputBIM, inputFAM, these are the PLINK binary filesets.
}
\seealso{
	\code{\link{SubmitJob}}
}
\value{
	Text listing action as either being a "success" or "error". If an error, then a message stating the error should also be reported.  
}
\examples{
        # RunPLINK(user.name, token, DE.file.list=list("geno_test.tfam","geno_test.tped"),
        #          DE.file.path="data/Default", input.list=list("inputTFAM","inputTPED"),
        #          input.type="T", print.curl=TRUE)
}	
