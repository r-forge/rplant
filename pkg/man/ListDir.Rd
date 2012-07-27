\name{ListDir}
\alias{ListDir}
\alias{MakeDir}
\alias{DeleteDir}
\title{Directory management}
\description{
	Functions for listing the contents of a directory, making new directories, or deleting entire directories in the iPlant infrastructure
}
\usage{
ListDir(user.name, token, path="")
MakeDir(user.name, token, dir.name, path="")
DeleteDir(user.name, token, dir.name)
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{a validation tool used in place of unencrypted passwords}
	\item{path}{optional path to a user's sub directory; default path is empty, which leads to the home directory}
	\item{dir.name}{name of subdirectory to be modified in the users home iPlant directory. If the subdirectory is not in the home directory, the the path should also be specified (i.e. "/path/file")}
}
\value{
	Text listing action as either being a "success" or "error". If an error, then a message stating the error should also be reported.  
}
\seealso{
	\code{\link{UploadFile}}
}
\examples{
	# Lists the contents of a user's home directory
	# ListDir("User", token, path="")
	
	# Makes a new subdirectory in the users home directory
	# MakeDir("User", token, dir.name="rplant", path="")
	
	# Lists the contents of a user's subdirectory
	# ListDir("User", token, path="rplant")
	
	# Deletes a subdirectory
	# DeleteDir("User", token, dir.name="/rplant/newDirectory")
}

