\name{job.submit}
\alias{job.submit}
\alias{job.status}
\alias{job.delete}
\alias{job.retrieve}
\title{Executing analytical applications}
\description{
	Functions for executing and managing analytical applications deployed in the iPlant infrastructure
}
\usage{
job.submit(user.name, token, application, path2inputSeqs, jobName, nprocs)
job.status(user.name, token, jobID)
job.delete(user.name, token, jobID)
job.retrieve(user.name, token, jobID, file2retrieve)
}
\arguments{
  \item{user.name}{an iPlant accepted user name.}
  \item{token}{a token used in place of unencrypted passwords.}
  \item{application}{the application to have information returned.}
  \item{path2inputSeqs}{the path of the input to be submitted. At a minimum it must contain the home directory (i.e. "user.name/inputSeq.fasta").}
  \item{jobName}{the name to give the job being submitted.}
  \item{nprocs}{the number of processors to be allocated to the job.}
  \item{jobID}{the ID number given to a submitted job.} 
  \item{file2retrieve}{the name of the file to be retrieved. If the file is not in the home directory, then file2retrieve name should also specify the path (i.e. "/user.name/analyses/file2retrieve.txt").}   
}
\details{
	This will only run an alignment muscle. More functionality currently underdevelopment. Also, the function job.retrieve still not working.
}
\value{
	A job ID is provided for jobs submitted via job.submit.
}
