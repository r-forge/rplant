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
file.upload(user.name, token, file.name, file.type)
file.rename(user.name, token, old.file.name, new.file.name)
file.move(user.name, token, file.name, path)
file.delete(user.name, token, file.name)
file.support(user.name, token)
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{a validation tool used in place of unencrypted passwords}
	\item{file.name}{name of file to be modified in the users home iPlant directory. If the file is not in the home directory, the file.name name should also specify the path (i.e. "/path/file")}
  \item{file.type}{format of file that is to be uploaded. Currently 34 file formats supported} 
  \item{oldName}{the name of the file to be renamed.} 
  \item{newName}{the new name of the file to be renamed.} 
  \item{fileName}{the name of the file to be moved to a new directory.}   
  \item{path2newdir}{the path to the new directory the file is to be moved.}   
  \item{file2delete}{the name of the file to be deleted. If the file is not in the home directory, then file2delete name should also specify the path (i.e. "/user.name/analyses/file2delete.txt").}   
}
\details{
	The function SupportFile provides a list of the supported file types that can be uploaded onto iPlant. Currently does not work.
}
\seealso{
	\code{\link{SupportFile}}
}
\value{
	Text listing action as either being a "success" or "error".
}

