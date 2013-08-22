\name{Validate}
\alias{Validate}
\title{Authentication functions}
\description{
	Authenticates users and provides tokens to be used in place of unencrypted passwords
}
\usage{
Validate(user, pwd, api="iplant", print.curl=FALSE)
}
\arguments{
	\item{user}{iPlant Discovery Environment user name}
	\item{pwd}{iPlant Discovery Environment password}
	\item{api}{the API to interfaced with, currently only the iPlant API is supported.  There are three options: "iplant", "cipres" and "tnrs"}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
}
\details{
	The authentication function will verify that the provided username and password combination are a valid pair for iPlant.    
}
\value{
	The function will print the error "Authentication failed" if the username/password combination is not valid. 
}
\examples{
\dontrun{Validate(user, pwd)}
}
