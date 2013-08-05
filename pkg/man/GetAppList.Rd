\name{ListApps}
\alias{ListApps}
\alias{GetAppInfo}
\title{Deployed applications}
\description{
	Functions for listing applications deployed in the iPlant software infrastructure and information about them.
}
\usage{
ListApps(print.curl=FALSE)
GetAppInfo(application, verbose=FALSE, print.curl=FALSE)
}
\arguments{
	\item{application}{name of DE application}
	\item{verbose}{optional screen output that displays all of the results from the api, default = FALSE}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
}
\value{
	GetAppInfo, for verbose=F gives critical information on the Application that is needed in the SubmitJob function.   A list of information is outputted.  The first list gives basic information on the application including is it a public application and if it is the newest version.  Both are important information.  If the application is a private application it can only be run by the person who submitted the application to the Foundation API, and clearly you want to run the newest version of the public application.

	The second list is detailed in this example, let's use the application "velveth-1.2.07u1" as an example.  The matrix outputted for verbose=F gives three columns of information.  The first column, labeled 'kind', tells the "input" and sometimes the "output" from the application.  The second column, labeled 'id', give the name of the "input", or the name of the output.  For example, \code{GetAppInfo("velveth-1.2.07u1")[[2]]}, the 'kind' column states there are six inputs for this app, and the 'id' column the names of those inputs are "reads5","reads3", etc.  The last column in the matrix is 'fileType'.  This is important, because if the wrong fileType is inputted into the function, then the function will not work.

	The ListApps function only outputs the newest versions of the public applications.  All of the public applications in that list can be used in the SubmitJob function. 
}
\examples{
	\dontrun{ListApps()}
	\dontrun{GetAppInfo("Muscle-3.8.31u1")}
}
