\name{RunFaST_LMM}
\alias{RunFaST_LMM}
\title{FaST-LMM-1.09}
\description{
	This function will submit a FaST-LMM job to iPlant's DE
}
\usage{
RunFaST_LMM(user.name, token, input.file.list="", ALL.file.path="",
            sim.file.list=NULL, pheno.file.name=NULL, mpheno=1, nprocs=1,
            print.curl=FALSE, job.name=NULL, covar.file.name=NULL, 
            version="FaST-LMM-1.09", shared.user.name=NULL, args=NULL)
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{A validation tool used in place of unencrypted passwords}
	\item{input.file.list}{(required) Names of files, in a list format, to be evaluated on the Discovery Environment (DE).  There are only three possible input groups for DE.file.list, regular fileset (.map/.ped),  transposed fileset (.tfam/.tped), and binary fileset (.bed/.bim/.fam).}
	\item{ALL.file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory.  All files specified including optional files must be in this directory.}
	\item{job.name}{The name to give the job being submitted}
	\item{sim.file.list}{(optional) A list containing the names of genetic similarity files.  These are used to determine the genetic similarities between individuals. The may be different from the input.file.list but MUST have the same formats as the input.file.list.}
	\item{pheno.file.name}{(optional) The full file name of the PLINK alternative phenotype file. This includes at least three columns, family ID, individual ID, and phenotype value, and may have any number of phenotype columns. Missing value as default is -9 but it can be changed.}
	\item{mpheno}{(optional) This value MUST be entered if a phenotype file is provided.  This value is the index for phenotype in -pheno file to process, starting at 1 for the first phenotype column.  Default is 1}
	\item{covar.file.name}{(optional) Must have at least three tab-delimited columns: family ID, individual ID, and covariate value, and may have any number of covariate values. The same missing value signifier from the phenotype file must be used. Note: the file should not have a header row.}
	\item{args}{Optional for arguments (i.e. flags).  The FaST-LMM model has much additional functionality that is not fit into this wrapper function (\url{http://fastlmm.codeplex.com/}), see manual.  This option allows users to add anything that is not included (.i.e. args="-Ftest"), to automatically invoke the F-test. }
	\item{print.curl}{Print the curl statement}
	\item{nprocs}{The number of processors to be allocated to the job, default is 1}
	\item{version}{FaST-LMM version, options include \code{FaST-LMM-1.09}}
	\item{shared.user.name}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.  Then the DE.dir.path becomes the path to the SHARED user's shared folder. }
}
\details{
	Not all of the inputs, for this application will be used at the same time, in fact the inputs are to be used only in three very strict groups.  Group 1: inputTFAM, inputTPED, these are the PLINK transposed filesets.  Group 2: inputMAP, inputPED, these are the PLINK regular filesets.  Group 3: inputBED, inputBIM, inputFAM, these are the PLINK binary filesets.

Not all information on the FaST-LMM model is here, see the FaST-LMM website \url{http://fastlmm.codeplex.com/}, or the FaST-LMM manual for more information.
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
