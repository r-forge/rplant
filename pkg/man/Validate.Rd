\name{Validate}
\alias{Validate}
\title{Authentication functions}
\description{
	This basic function simply authenticates users.  
}
\usage{
Validate(user, pwd, consumer_key=NULL, consumer_secret=NULL, api="foundation", print.curl=FALSE)
}
\arguments{
	\item{user}{iPlant Discovery Environment user name}
	\item{pwd}{iPlant Discovery Environment password}
	\item{api}{The API to be interfaced with.  The only supported API's are the "agave" and "foundation"}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
}
\details{
The \code{Validate} function works slightly differently for the \code{agave} API and the \code{foundation} API.  For the Foundation API all that is necessary for the \code{Validate} function is for the user to provide the username and password combination; then it will verify that they are a valid pair for iPlant.  To get a valid pair of credentials go to \url{http://user.iplantcollaborative.org/} and register.

For the Agave API a user must provide the username, password, consumer key and consumer secret.  To get a valid pair of username/password, again go to \url{http://user.iplantcollaborative.org/} and register.  Once registered the user must go to \url{https://agave.iplantc.org/store/?} and obtain the \code{Consumer Key} and \code{Consumer Secret}.  The user only needs to do this once, but they have to have their key and secret for every session of rPlant.
}
\value{
	The function will print the error "Authentication failed" if if fails. 
}
\examples{
\dontrun{Validate("username", "password", api="foundation")}
\dontrun{Validate("username", "password", "client_key", "client_secret", api="agave")}
}
