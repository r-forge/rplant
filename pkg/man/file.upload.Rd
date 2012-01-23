\name{file.upload}
\alias{file.upload}
\alias{file.rename}
\alias{file.move}
\alias{file.delete}
\alias{file.support}
\title{File management}
\description{
	Functions for uploading and manipulating files in the iPlant infrastructure
}
\usage{
file.upload(user.name, token, file2upload, fileType)
file.rename(user.name, token, oldName, newName)
file.move(user.name, token, fileName, path2newdir)
file.delete(user.name, token, file2delete)
file.support(user.name, token)
}
\arguments{
  \item{user.name}{an iPlant accepted user name.}
  \item{token}{a token used in place of unencrypted passwords.}
  \item{file2upload}{name of file to be uploaded onto the users iPlant directory. If the file is not in the working directory, the file2upload name should also specify the path (i.e. "/path/file2upload.fasta").}
  \item{fileType}{format of file that is to be uploaded. Currently 34 file formats supported.} 
  \item{oldName}{the name of the file to be renamed.} 
  \item{newName}{the new name of the file to be renamed.} 
  \item{fileName}{the name of the file to be moved to a new directory.}   
  \item{path2newdir}{the path to the new directory the file is to be moved.}   
  \item{file2delete}{the name of the file to be deleted. If the file is not in the home directory, then file2delete name should also specify the path (i.e. "/user.name/analyses/file2delete.txt").}   
}
\details{
	The function file.support provides a list of the supported file types that can be uploaded onto iPlant. Currently does not work.
}
\value{
	Text listing action as either being a "success" or "error".
}

