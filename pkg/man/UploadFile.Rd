\name{UploadFile}
\alias{UploadFile}
\alias{RenameFile}
\alias{MoveFile}
\alias{DeleteFile}
\alias{SupportFile}
\title{File management}
\description{
	Functions for uploading and manipulating files in the iPlant infrastructure
}
\usage{
UploadFile(local.file.name, local.file.path="", file.type, print.curl=FALSE)
RenameFile(file.name, new.file.name, file.path="", print.curl=FALSE,
           suppress.Warnings=FALSE)
MoveFile(file.name, file.path="", end.path="", print.curl=FALSE,
         suppress.Warnings=FALSE)
DeleteFile(file.name, file.path="", print.curl=FALSE, suppress.Warnings=FALSE)
SupportFile(print.curl=FALSE) 
}
\arguments{
	\item{local.file.name}{Name of local file on user's computer.  This file cannot be an object in the R workspace.}
	\item{local.file.path}{optional path to a sub directory where local file on user's computer is located (include ENTIRE path); default path is empty which leads to a users home directory}
	\item{file.type}{format of file that is to be uploaded. Currently 34 file formats supported}
	\item{file.name}{Name of file to be modified on the Discovery Environment (DE).}
	\item{new.file.name}{the new name of the file on DE to be renamed} 
	\item{file.path}{optional path to a sub directory where file on DE is located; default path is empty which leads to a users home directory}
	\item{end.path}{Path to destination sub directory where file on DE is moved to; default path is empty which leads to a users home directory}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
}
\details{
	The \code{SupportFile} function provides a list of the supported file types that can be uploaded onto iPlant.  The \code{UploadFile} function uploads a file, \code{local.file.name}, that is on your computer (i.e. local directory), in the directory \code{local.file.path}.  If the \code{local.file.path} is not specified your working directory is the default path (use \code{getwd()} to find that).  The \code{file.type} is required, use the \code{SupportFile} function to view the 34 file formats that are supported (i.e. for a fasta file use \code{FASTA-0}).  The \code{RenameFile} function renames the \code{file.name} in \code{file.path} (on the Discovery Environment), to the \code{new.file.name}.  The \code{MoveFile} function moves a \code{file.name} in the \code{file.path} to the \code{end.path}.  The \code{DeleteFile} function  deletes the \code{file.name} in the \code{file.path}.
}
\seealso{
	\code{\link{ListDir}}
}
\value{
	If an error, then a message stating the error should also be reported.  
}
\examples{
# Write .fasta file to home directory
\dontrun{data(ex.lp.fasta)}
\dontrun{write.fasta(sequences = ex.lp.fasta, names = names(ex.lp.fasta), file.out = "ex.lp.fasta")}
         
# Upload a file to the DE
\dontrun{UploadFile(local.file.name="ex.lp.fasta", file.type="FASTA-0")}
\dontrun{UploadFile(local.file.name="ex.lp.fasta", local.file.path="path/to/dir", file.type="FASTA-0")}
     
# Rename a file 
\dontrun{RenameFile(file.name="ex.lp.fasta", new.file.name="lp.fasta")}
             
# Move a file from the subdirectory rplant to the home directory
\dontrun{MoveFile(file.name="lp.fasta", file.path="", end.path="data")}
     
# Delete a file in the home directory
\dontrun{DeleteFile(file.name="lp.fasta", DE.file.path="data")}

# Lists file types that are supported
\dontrun{SupportFile()}
}	
