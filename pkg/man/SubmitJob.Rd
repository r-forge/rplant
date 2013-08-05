\name{SubmitJob}
\alias{SubmitJob}
\alias{CheckJobStatus}
\alias{DeleteJob}
\alias{RetrieveJob}
\alias{ListJobOutput}
\alias{GetJobHistory}

\title{Executing analytical applications}
\description{
	Functions for executing and managing analytical applications deployed in the iPlant infrastructure
}
\usage{
SubmitJob(application, file.path="", file.list=NULL, input.list, 
          options.list=NULL, job.name, args=NULL, nprocs=1, 
          print.curl=FALSE, shared.user=NULL, suppress.Warnings=FALSE) 
CheckJobStatus(job.id, verbose=FALSE, print.curl=FALSE)
DeleteJob(job.id, print.curl=FALSE) 
RetrieveJob(job.id, file.vec, zip=TRUE, print.curl=FALSE, verbose=FALSE)
ListJobOutput(job.id, print.curl=FALSE, print.total=TRUE)
GetJobHistory(verbose=FALSE, print.curl=FALSE)
}
\arguments{
	\item{application}{Name of DE application.  Use the ListApps() function for a list of eligible applications.  To run your own private application use suppress.Warnings=TRUE.}
	\item{file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{file.list}{A list of input files, many functions only have one input file, but some have multiple input files, in that case input all those files in list form.  This list is the same length as input.list, and each object in the input.list corresponds to same object in file.list.  See details for more information}
	\item{job.name}{The name to give the job being submitted}
	\item{nprocs}{The number of processors to be allocated to the job, default = 1}
	\item{args}{Optional for arguments (i.e. flags).  For example, for the "raxml-lonestar-7.2.8u1" application args="arguments=-f d -b 100", see details.}
	\item{job.id}{The unique ID number given to a submitted job} 
	\item{input.list}{A list of the name of inputs.  See details for more information.}
	\item{options.list}{A list of options available is input for the Foundation API application.  See details for more information.}
	\item{verbose}{Optional screen output that displays all of the results from the api, default = FALSE}
	\item{file.vec}{Names of output files to download, can be one or many}
	\item{zip}{Zip job files together, default is TRUE}
	\item{print.curl}{Prints the Curl Statement that can be used in the terminal}
	\item{shared.user.name}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.  Then the dir.path becomes the path to the SHARED user's shared folder.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.  If the application you are running is a private application have suppress.Warnings=TRUE}
}
\details{
	Application must match an application name directly out of the DE, to ensure, you can check using \code{ListApps}. The job.id is returned after submitting, and it is used to check the status or download results. It can also be retrieved using \code{GetJobHistory}. For \code{input.list} use the \code{GetAppInfo} function, the 'kind' column verifies if "input" or "output".  What goes in the \code{input.list} is only the name in the 'id' column when the 'kind' column is "input".  For example, when the application is "muscle-lonestar-3.8.31u2", from the \code{GetAppInfo(user.name, token, "muscle-lonestar-3.8.31u2")[[2]]}, we get the \code{input.list=list("stdin")}.  For the application "velveth-1.2.07u1", again using \code{GetAppInfo(user.name, token, "velveth-1.2.07u1")[[2]]}, the \code{input.list=list("reads1", "reads2", "reads3", "reads4", "reads5", "reads6")}.  A couple notes, the \code{input.list} can be shorter than the the number of inputs, for example, using the "velveth-1.2.07u1" application, the input list could be \code{input.list=list("reads1", "reads2", "reads3")}.  Also, the \code{file.list} is the same length as \code{input.list}, and in this example, lets say \code{file.list=list("filer", "filea", "filez")}, then "filer" corresponds with the "read1" input.

Also for \code{options.list} use \code{GetAppInfo} function, when the 'kind' function is anything other than input or output. The format for \code{options.list} is as follows, \code{list(c(id (from GetAppInfo), value),c(id, value), . . .)}.  The list can be as long as the number of options.
}
\value{
	A job ID is provided for jobs submitted via job.submit.  If an error, then a message stating the error should also be reported. 
}
\seealso{
	\code{\link{ListApps}}
}
\examples{
# Submit a MUSCLE job using landplant data, which will return a job.id
\dontrun{myJob <- SubmitJob(application="muscle-lonestar-3.8.31u2", file.list=list("ex.lp.fasta"), input.list=list("stdin"), file.path="data", job.name="MUSCLE")}

# Check the status of any job
\dontrun{CheckJobStatus(myJob)}
             
# Lists and output files a job has created
\dontrun{ListJobOutput(myJob)  }
             
# Download output files
\dontrun{RetrieveJob(myJob, ListJobOutput(myJob))}
     
# View job history
\dontrun{GetJobHistory()}

# Delete Job
\dontrun{DeleteJob(myJob)}
}
