\name{PLINKConversion}
\alias{PLINKConversion}
\title{PLINKConversion}
\description{
	This function will submit a PLINK job to iPlant's DE
}
\usage{
PLINKConversion(user.name, token, DE.file.list="", DE.file.path="",
                job.name=NULL, output.type="--recode", nprocs=1,
                print.curl=FALSE, version="plink-1.07",
                shared.user.name=NULL)
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{A validation tool used in place of unencrypted passwords}
	\item{DE.file.list}{Names of files, in a list format, to be evaluated on the Discovery Environment (DE).  There are only three possible input groups for DE.file.list, regular fileset (.map/.ped),  transposed fileset (.tfam/.tped), and binary fileset (.bed/.bim/.fam).}
	\item{DE.file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{The name to give the job being submitted}
	\item{output.type}{PLINKs conversion methods.  There are.  Choices are outlined on the PLINK webpage, see details for more information.}
	\item{print.curl}{Print the curl statement}
	\item{nprocs}{the number of processors to be allocated to the job, default is 1}
	\item{version}{PLINK version, options include \code{plink-1.07}}
	\item{shared.user.name}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.  Then the DE.dir.path becomes the path to the SHARED user's shared folder. }
}
\details{
	Not all of the inputs, for this application will be used at the same time, in fact the inputs are to be used only in three very strict groups.  Group 1: inputTFAM, inputTPED, these are the PLINK transposed filesets.  Group 2: inputMAP, inputPED, these are the PLINK regular filesets.  Group 3: inputBED, inputBIM, inputFAM, these are the PLINK binary filesets.

"output.type"			"explanation"
--make-bed			Make .bed, .fam and .bim
--recode			Output new .ped and .map files
--recode12			As above, with 1/2 allele coding
--recode-rlist			List individuals with minor allele genotypes
--recode-lgen			Output data in long LGEN format	  	 
--recodeHV			As above, with Haploview .info file
--recode-fastphase		Ouput fastphase format file
--recode-bimbam			Ouput bimbam format file
--recode-structure		Ouput structure format file  	 
--recodeA			Raw data file with additive coding
--recodeAD			Raw data file with additive/dominance coding

See the PLINK website for more information. \url{http://pngu.mgh.harvard.edu/~purcell/plink}
}
\seealso{
	\code{\link{SubmitJob}}
}
\value{
	Text listing action as either being a "success" or "error". If an error, then a message stating the error should also be reported.  
}
\examples{
# PLINKConversion(user.name, token, DE.file.list=list(geno_test.tfam,
#                 geno_test.tped), DE.file.path="data/Syngenta")
}	
