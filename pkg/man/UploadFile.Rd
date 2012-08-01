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
UploadFile(user.name, token, file.name, file.path, file.type)
RenameFile(user.name, token, old.file.name, new.file.name, file.path)
MoveFile(user.name, token, file.name, file.path, end.path)
DeleteFile(user.name, token, file.name, file.path)
SupportFile(user.name, token)
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{a validation tool used in place of unencrypted passwords}
	\item{file.name}{Name of file to be modified.  This file cannot be an object in the R workspace.}
	\item{file.type}{format of file that is to be uploaded. Currently 34 file formats supported} 
	\item{old.file.name}{the name of the file to be renamed}
	\item{new.file.name}{the new name of the file to be renamed} 
	\item{file.path}{optional path to a sub directory where file is located; default path is empty which leads to a users home directory}
	\item{end.path}{optional path to destination sub directory where file is moved to; default path is empty which leads to a users home directory}
}
\details{
	The function SupportFile provides a list of the supported file types that can be uploaded onto iPlant. Currently does not work.
}
\seealso{
	\code{\link{SupportFile}}
	\code{\link{ListDir}}
}
\value{
	Text listing action as either being a "success" or "error". If an error, then a message stating the error should also be reported.  
}
\examples{
        # Write .fasta file to home directory
        data(landplant.fasta)
        write.fasta(sequences=landplant.fasta,names=names(landplant.fasta),file.out="lp.fasta")        

	# Upload a file to the DE
	UploadFile("User", token, file.name="landplant.fasta", file.type="FASTA-0")
	# UploadFile("User", token, file.name="landplant.fasta", file.path="/path/", file.type="FASTA-0")

	# Rename a file 
	RenameFile("User", token, old.file.name="landplant.fasta", new.file.name="lp.fasta", file.path="/rplant/")
	
	# Move a file from the home directory to a subdirectory
	MoveFile("User", token, file.name="lp.fasta", file.path="", end.path="/rplant/")

	# Delete a file on the DE
	DeleteFile("User", token, file.name="lplant.fasta", file.path="/rplant/")

	# SupportFile("User", token)  
}	
