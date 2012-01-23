\name{list.dir}
\alias{list.dir}
\alias{make.dir}
\alias{delete.dir}
\title{Directory management}
\description{
	Functions for listing the contents of a directory, making new directories, or deleting entire directories in the iPlant infrastructure
}
\usage{
list.dir(user.name, token, path2directory=NULL)
make.dir(user.name, token, newDirect, sub.dir=FALSE, path2parent=NULL)
delete.dir(user.name, token, delDirect)
}
\arguments{
  \item{user.name}{an iPlant accepted user name.}
  \item{token}{a token used in place of unencrypted passwords.}
  \item{path2directory}{if home directory is not the directory of interest, then the user supplies the path to the directory to have contents listed.}
  \item{newDirect}{the name of the new directory to be created.}
  \item{sub.dir}{logical indicating whether the directory to be made is a subdirectory.}
  \item{path2parent}{if sub.dir=TRUE, then this is the path to the parent directory.}
  \item{delDirect}{the name of the directory to be deleted. If the file is not in the home directory, then delDirect should also specify the path (i.e. '/user.name/analyses/muscle02').}   
}
\details{
	More soon.
}
\value{
	Text listing action as either being a success or error.
}
