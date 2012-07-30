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
UploadFile(user.name, token, file.name, file.type)
RenameFile(user.name, token, old.file.name, new.file.name)
MoveFile(user.name, token, file.name, path)
DeleteFile(user.name, token, file.name)
SupportFile(user.name, token)
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{a validation tool used in place of unencrypted passwords}
	\item{file.name}{name of file to be modified in the users home iPlant directory.  This file cannot be an object in the R workspace.  If the file is not in the home directory, then the path should also be specified (i.e. "/path/file")}
	\item{file.type}{format of file that is to be uploaded. Currently 34 file formats supported} 
	\item{old.file.name}{the name of the file to be renamed}
	\item{new.file.name}{the new name of the file to be renamed} 
	\item{path}{optional path to a sub directory; default path is empty, which leads to a users home directory}
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
        # data(landplant.fasta)
        # write.fasta(sequences=landplant.fasta,names=names(landplant.fasta),file.out="lp.fasta")        

	# Upload a file to the DE
	# UploadFile("User", token, file.name="lp.fasta", file.type="FASTA-0")

	# Rename a file 
	# RenameFile("User", token, old.file.name="lp.fasta", new.file.name="lplant.fasta")
	
	# Move a file from the home directory to a subdirectory
	# MoveFile("User", token, fileName="lplant.fasta", path="/newDirectory") 				
	# Delete a file on the DE
	# DeleteFile("User", token, "lplant.fasta")
	# SupportFile("User", token)  
}	
