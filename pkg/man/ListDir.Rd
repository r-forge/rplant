\name{ListDir}
\alias{ListDir}
\alias{MakeDir}
\alias{DeleteDir}
\title{Directory management}
\description{
	Functions for listing the contents of a directory, making new directories, or deleting entire directories in the iPlant infrastructure
}
\usage{
ListDir(dir.path="", print.curl=FALSE, shared.user=NULL, suppress.Warnings=FALSE)
MakeDir(dir.name, dir.path="", print.curl=FALSE, suppress.Warnings=FALSE)
DeleteDir(dir.name, dir.path="", print.curl=FALSE, suppress.Warnings=FALSE)
}
\arguments{
	\item{dir.path}{optional path to a user's sub directory on the Discovery Environment (DE); default path is empty, which leads to the home directory}
	\item{dir.name}{name of subdirectory to be modified on the DE in the users home iPlant directory.}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{shared.user.name}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to view them, enter their username for this input.  Then the dir.path becomes the path to the SHARED user's shared folder. }
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
}
\details{
	The \code{ListDir} function lists the files in the given directory, `\code{dir.path}'.  The \code{MakeDir} function makes a directory `\code{dir.name}' in the directory `\code{dir.path}'.  The \code{DeleteDir} function deletes the directory `\code{dir.name}' in the directory `\code{dir.path}'.
}
\value{
	If an error, then a message stating the error should also be reported.  
}
\seealso{
	\code{\link{UploadFile}}
}
\examples{
# Makes the subdirectory named "new" in the users rplant directory
\dontrun{MakeDir(dir.name="new", dir.path="data")}
             
# Lists the contents of a user's subdirectory "new"
\dontrun{ListDir(dir.path="data/new")}

# Deletes the subdirectory "new"
\dontrun{DeleteDir(dir.name="new", dir.path="data")}
}

