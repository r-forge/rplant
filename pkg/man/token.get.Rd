\name{token.get}
\alias{token.get}
\alias{token.renew}
\title{Authentication functions}
\description{
	Authenticates users and provides tokens to be used in place of unencrypted passwords
}
\usage{
token.get(user.name, user.pwd, API=c("iplant", "cipres", "tnrs"))
token.renew(user.name, user.pwd, token, API=c("iplant", "cipres", "tnrs"))
}
\arguments{
	\item{user.name}{an iPlant accepted user name.}
	\item{user.pwd}{an iPlant accepted password.}
	\item{token}{a token used in place of unencrypted passwords.}
	\item{API}{the API to interfaced with, currently only the iPlant API is supported.}
}
\details{
	More soon.
}
\value{
	A token used in place of an unencrypted password.
}
