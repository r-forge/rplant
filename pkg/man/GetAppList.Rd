\name{ListApps}
\alias{ListApps}
\alias{GetAppInfo}
\alias{GetCitations}
\title{Deployed applications}
\description{
	Functions for listing applications deployed in the iPlant software infrastructure and information about them.  GetCitations is a work in progress, currently only supports "muscle-ranger-2.0".
}
\usage{
ListApps(user.name, token)
GetAppInfo(user.name, token, application, verbose=FALSE)
GetCitations(application, save, file)
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{a validation tool used in place of unencrypted passwords}
	\item{application}{name of DE application}
	\item{verbose}{optional screen output that displays all of the results from the api, default = FALSE}
	\item{save}{optional to save information}
	\item{file}{optional to get specific file citation, default is all files}
}
\value{
	A list of applications deployed in the DE.
}
\examples{
	# ListApps("User", token)
	# GetAppInfo("User", token, "muscle-ranger-2.0", verbose=T)
        # GetCitations(application="muscle-ranger-2.0")
}
