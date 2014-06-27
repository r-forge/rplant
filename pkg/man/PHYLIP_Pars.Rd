\name{PHYLIP_Pars}
\alias{PHYLIP_Pars}
\title{PHYLIP 3.69}
\description{
PHYLIP is a free package of programs for inferring phylogenies. It is distributed as source code, documentation files, and a number of different types of executables. The web page: \url{http://evolution.genetics.washington.edu/phylip/doc/main.html}, by Joe Felsenstein of the Department of Genome Sciences and the Department of Biology at the University of Washington, contain information on PHYLIP.  PHYLIP (the PHYLogeny Inference Package) is a package of programs for inferring phylogenies (evolutionary trees).  Methods that are available in the package include parsimony, distance matrix, and likelihood methods, including bootstrapping and consensus trees.
}
\usage{
PHYLIP_Pars(file.name, file.path="", job.name=NULL, type="DNA",
            print.curl=FALSE, shared.username=NULL, 
            suppress.Warnings=FALSE, email=TRUE)
}
\arguments{
	\item{file.name}{Name of file to be evaluated on the Discovery Environment (DE).}
	\item{file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{The name to give the job being submitted}
	\item{type}{Two options "PROTEIN" or "DNA".  This defines the type of sequences in the file, either proteins or nucleotides.}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{shared.username}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
	\item{email}{By default an email will be sent to the user when the job finishes.}
}
\details{
	The input file format that is supported is the interleaved phylip format \url{http://www.bioperl.org/wiki/PHYLIP_multiple_alignment_format}.

	PHYLIP Parsimony outputs trees in Newick format \url{http://en.wikipedia.org/wiki/Newick_format}.
}
\value{
	A list containing the job id and the job name is provided for jobs submitted.  If an error, then a message stating the error should also be reported.
}
\seealso{
	\code{\link{SubmitJob}, \link{Validate}, \link{UploadFile}}
}
\examples{
\dontrun{data(DNA.fasta)}
\dontrun{write.fasta(sequences = DNA.fasta, names = names(DNA.fasta), file.out = "DNA.fasta")}
\dontrun{Validate("username","password")}
\dontrun{UploadFile("DNA.fasta", filetype="FASTA-0")}
\dontrun{myJobMuD <- Muscle("DNA.fasta", aln.filetype="PHYLIP_PARS", job.name="musclePHYdna")}
\dontrun{minWait = 5 # seconds}
\dontrun{maxWait = 1800 # 30 min in seconds}
\dontrun{Wait(myJobMuD[[1]], minWait, maxWait)}
\dontrun{myJobPhyD <- PHYLIP_Pars("phylip_pars.aln", file.path=paste("analyses/",myJobMuD[[2]],
                                  sep=""), job.name="phylipDNA")}
\dontrun{Wait(myJobPhyD[[1]], minWait, maxWait)}
\dontrun{RetrieveJob(myJobPhyD[[1]], c("outtree.nwk"))}

\dontrun{require(ape)}
\dontrun{read.tree(paste(getwd(), myJobPhyD[[2]], "outtree.nwk", sep="/")) -> Tree}
\dontrun{plot(Tree)}
}	
