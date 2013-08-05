\name{PLINKConversion}
\alias{PLINKConversion}
\title{PLINKConversion}
\description{
	PLINK is an open-source whole genome association analysis toolset, designed to perform a range of basic, large-scale analyses in a computationally efficient manner, check \url{http://pngu.mgh.harvard.edu/~purcell/plink/} for details. This function converts the standard PLINK file formats (Regular (ped/map), Transposed (tped/tfam), and Binary (bed/bim/fam)) to various other PLINK file formats.
}
\usage{
PLINKConversion(file.list="", file.path="", output.type="--recode",
                job.name=NULL, shared.user.name=NULL, print.curl=FALSE,
                version="plink-1.07", suppress.Warnings=FALSE)
}
\arguments{
	\item{file.list}{Names of files, in a list format, to be evaluated on the Discovery Environment (DE).  There are only three possible input groups for file.list, regular fileset (.map/.ped),  transposed fileset (.tfam/.tped), and binary fileset (.bed/.bim/.fam).}
	\item{file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{The name to give the job being submitted}
	\item{output.type}{PLINKs conversion methods.  There are.  Choices are outlined on the PLINK webpage, see details for more information.}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{version}{PLINK version, options include \code{plink-1.07}}
	\item{shared.user.name}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.  Then the dir.path becomes the path to the SHARED user's shared folder.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
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
\value{
	A list containing the job id and the job name is provided for jobs submitted.  If an error, then a message stating the error should also be reported.
}
\seealso{
	\code{\link{SubmitJob}}
}
\examples{
\dontrun{PLINKConversion(file.list=list(geno_test.tfam, geno_test.tped), file.path="data/Syngenta")}
}	
