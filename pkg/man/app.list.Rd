\name{app.list}
\alias{app.list}
\alias{app.info}
\title{Deployed applications}
\description{
	Functions for listing applications deployed in the iPlant software infrastructure and information about them
}
\usage{
app.list(user.name, token)
app.info(user.name, token, application)
}
\arguments{
  \item{user.name}{an iPlant accepted user name.}
  \item{token}{a token used in place of unencrypted passwords.}
  \item{application}{the application to have information returned.}
}
\details{
	The function file.support provides a list of the supported file types that can be uploaded onto iPlant. Currently does not work.
}
\value{
	A list of applications deployed. For app.info, the a json is returned listing the input requirements, input parameters, and output formats.
}
