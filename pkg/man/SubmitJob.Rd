\name{SubmitJob}
\alias{SubmitJob}
\alias{CheckJobStatus}
\alias{DeleteJob}
\alias{RetrieveJob}
\alias{GetJobHistory}

\title{Executing analytical applications}
\description{
	Functions for executing and managing analytical applications deployed in the iPlant infrastructure
}
\usage{
SubmitJob(user.name, token, application, job.name, file.name, file.path, nprocs=1)
CheckJobStatus(user.name, token, job.id, verbose=FALSE)
DeleteJob(user.name, token, job.id)
RetrieveJob(user.name, token, job.id, files)
GetJobHistory(user.name, token, verbose=FALSE)
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{file.name}{name of file to be evaluated.}
	\item{token}{a validation tool used in place of unencrypted passwords}
	\item{application}{name of DE application}
	\item{job.name}{the name to give the job being submitted}
	\item{nprocs}{the number of processors to be allocated to the job, default = 1}
	\item{job.id}{the unique ID number given to a submitted job} 
	\item{file.path}{optional path to a user's subdirectory; default path is empty, which leads to the home directory}
	\item{verbose}{optional screen output that displays all of the results from the api, default = FALSE}
	\item{files}{names of output files to download, can be one or many}
}
\details{
	Application must match an application name directly out of the DE, to ensure, you can check using \code{ListApps}. The job.id is returned after submitting, and it is used to check the status or download results. It can also be retrieved using \code{GetJobHistory}.
}
\value{
	A job ID is provided for jobs submitted via job.submit.
}
\seealso{
	\code{\link{ListApps}}
}
\examples{
	# Submit a MUSCLE job using landplant data, which will return a job.id
	# myJob<-SubmitJob("User", token, application="muscle-ranger-2.0", file.name="lp.fasta", file.path="/rplant/", job.name="MUSCLE", nprocs="1")
	
	# Check the status of any job
	# CheckJobStatus("User", token, myJob, verbose=F)
	
	# Lists and output files a job has created
	# ListJobOutput("User", token, myJob)  
	
	# Download output files
	# RetrieveJob("User", token, myJob, ListJobOutput("User", token, myJob))
}
