\name{ListDir}
\alias{ListDir}
\alias{MakeDir}
\alias{DeleteDir}
\title{Directory management}
\description{
	Functions for listing the contents of a directory, making new directories, or deleting entire directories in the iPlant infrastructure
}
\usage{
ListDir(user.name, token, dir.path="")
MakeDir(user.name, token, dir.name, dir.path="")
DeleteDir(user.name, token, dir.name, dir.path="")
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{a validation tool used in place of unencrypted passwords}
	\item{dir.path}{optional path to a user's sub directory; default path is empty, which leads to the home directory}
	\item{dir.name}{name of subdirectory to be modified in the users home iPlant directory.}
}
\value{
	Text listing action as either being a "success" or "error". If an error, then a message stating the error should also be reported.  
}
\seealso{
	\code{\link{UploadFile}}
}
\examples{
	# Makes the subdirectory named "new" in the users rplant directory
	# MakeDir("User", token, dir.name="new", dir.path="/rplant/")
	
	# Lists the contents of a user's subdirectory "new"
	# ListDir("User", token, dir.path="/rplant/new/")
	
	# Deletes the subdirectory "new"
	# DeleteDir("User", token, dir.name="new", dir.path="/rplant/")
}

