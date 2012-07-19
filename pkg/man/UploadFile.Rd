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
	\item{file.name}{name of file to be modified in the users home iPlant directory. If the file is not in the home directory, then the path should also be specified (i.e. "/path/file")}
	\item{file.type}{format of file that is to be uploaded. Currently 34 file formats supported} 
	\item{old.file.name}{the name of the file to be renamed}
	\item{new.file.name}{the new name of the file to be renamed} 
	\item{path}{the path to any directory other than users home}   
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
	# UploadFile("User", token, file.name="/rplant/pkg/data/corrected.landplant.fasta", fileType="FASTA-0")
	# RenameFile("User, token, "corrected.landplant.fasta", "clp.fasta")
	# MoveFile("User", token, fileName="clp.fasta", path2newdir="/newDirectory") 				
	# DeleteFile("User", token, "corrected.landplant.fasta")
	# SupportFile("User", token)  
}	