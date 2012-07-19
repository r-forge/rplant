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
	\item{path}{the path to any directory other than users home}   
	\item{dir.name}{name of subdirectory to be modified in the users home iPlant directory. If the subdirectory is not in the home directory, the the path should also be specified (i.e. "/path/file")}
}
\value{
	Text listing action as either being a "success" or "error". If an error, then a message stating the error should also be reported.  
}
\seealso{
	\code{\link{UploadFile}}
}
\examples{
	#ListDir("User", token, path="")
	#MakeDir("User", token, "newDirectory", path="rplant")
	#DeleteDir("User", token, "/rplant/newDirectory")
}

