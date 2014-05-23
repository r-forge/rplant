\name{Validate}
\alias{Validate}
\title{Authentication functions}
\description{
	This basic function simply authenticates users.  
}
\usage{
Validate(user, pwd, api="agave", print.curl=FALSE)
}
\arguments{
	\item{user}{iPlant Discovery Environment user name}
	\item{pwd}{iPlant Discovery Environment password}
	\item{api}{The API to be interfaced with.  The only supported API's are the "agave" and "foundation", default is "agave"}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
}
\details{
The \code{Validate} function works the same for a user trying to logon to Agave or Foundation.  All that is necessary for the \code{Validate} function is for the user to provide the username and password combination; then it will verify that they are a valid pair for iPlant.  To get a valid pair of credentials go to \url{http://user.iplantcollaborative.org/} and register.
}
\value{
	The function will print the error "Authentication failed" if if fails. 
}
\examples{
\dontrun{Validate("username", "password")}
}
