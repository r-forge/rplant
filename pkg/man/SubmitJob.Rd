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
          args.list=NULL, job.name, nprocs=1, private.APP=FALSE, 
          suppress.Warnings=FALSE,  shared.username=NULL,
          print.curl=FALSE)
CheckJobStatus(job.id, return.json=FALSE, print.curl=FALSE)
ListJobOutput(job.id, print.curl=FALSE, print.total=TRUE)
RetrieveJob(job.id, file.vec, print.curl=FALSE, verbose=FALSE)
GetJobHistory(return.json=FALSE, print.curl=FALSE)
DeleteJob(job.id, print.curl=FALSE, ALL=FALSE) 
}
\arguments{
	\item{application}{Name of DE application.  Use the ListApps() function for a list of eligible applications.  To run your own private application use suppress.Warnings=TRUE.}
	\item{file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{file.list}{A list of input files, many functions only have one input file, but some have multiple input files, in that case input all those files in list form.  This list is the same length as input.list, and each object in the input.list corresponds to same object in file.list.  See details for more information}
	\item{job.name}{The name to give the job being submitted}
	\item{nprocs}{The number of processors to be allocated to the job, default = 1}
	\item{private.APP}{Use if you are submitting a job on your own private application}
	\item{job.id}{The unique ID number given to a submitted job} 
	\item{input.list}{A list of the name of inputs.  See details for more information.}
	\item{args.list}{A list of options available is input for the Foundation API application.  (i.e.) For the application \code{velveth-1.2.07u1} \code{args.list=list(c("kmer","100"),c("Output","name"))}.  See details for more information.}
	\item{return.json}{Optional screen output that displays all of the results from the api, default = FALSE}
	\item{file.vec}{Names of output files to download, can be one or many}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{print.total}{For the \code{ListJobOuput} function this option will print the total number of files in the folder.}
	\item{verbose}{For the \code{RetrieveJob} function this option will print the names of the files as they are downloaded.}
	\item{shared.username}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.  Then the dir.path becomes the path to the SHARED user's shared folder.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.  If the application you are running is a private application have suppress.Warnings=TRUE}
	\item{ALL}{This option is only on the \code{DeleteJob} function.  If ALL=TRUE then all jobs in the job history will be deleted.}
}
\details{
	The function \code{SubmitJob}, takes inputs and arguments and submits a job on the Foundation API.  For example, take the application \code{velveth-1.2.07u1}.  If the appropriate inputs are put into the \code{SubmitJob} function then \code{velveth-1.2.07u1} will run on the Foundation API. function  provides a list of the supported file types that can be uploaded onto iPlant.  The \code{SubmitJob} function will run the \code{application} with the file inputs \code{file.list} that are in the directory \code{file.path}.  The appropriate options for the \code{application} need to be outlined in \code{input.list} and potentially \code{args.list}.  The \code{SubmitJob} function outputs the \code{job.id} and with that \code{job.id} you can run \code{CheckJobStatus(job.id)}, and this function will tell you what stage your job is at.  The stages are: 

\tabular{l}{
	\code{PENDING}            \cr 
	\code{STAGING_INPUTS}     \cr 
	\code{CLEANING_UP}        \cr 
	\code{ARCHIVING}          \cr 
	\code{STAGING_JOB}        \cr 
	\code{FINISHED}           \cr 
	\code{KILLED}             \cr 
	\code{FAILED}             \cr 
	\code{STOPPED}            \cr 
	\code{RUNNING}            \cr 
	\code{PAUSED}             \cr 
	\code{QUEUED}             \cr 
	\code{SUBMITTING}         \cr 
	\code{STAGED}             \cr 
	\code{PROCESSING_INPUTS}  \cr 
	\code{ARCHIVING_FINISHED} \cr 
	\code{ARCHIVING_FAILED}  
}

When it is finished it will read \code{ARCHIVING_FINISHED}, unless it failed.  When the job is finished then use the \code{ListJobOutput} function to see all of the files in your job.  The number of output files varies by \code{application}.  The \code{RetrieveJob} function takes the \code{job.id} and the \code{file.vec} as input, it will then download the specified files in the \code{file.vec}.  The files will be downloaded to your working directory (\code{getwd()}).  As said before the \code{file.vec} contains the file names that you want to download.  This vector is a subset of the output from \code{ListJobOutput}.  The \code{DeleteJob} function then deletes the job and the correponding output folder that was generated from running the job.  Using the option \code{DeleteJob(ALL=TRUE)} will delete all jobs in the job history.  The \code{GetJobHistory} function displays all jobs in your history that have not been deleted.

	For the \code{SubmitJob} function the \code{application} must match an application name that is in the output from the \code{ListApps} function.  For the \code{input.list} use the \code{GetAppInfo} function, the `kind' column verifies if "input" or "output".  What goes in the \code{input.list} is only the name in the 'id' column when the 'kind' column is "input".  For example, when the application is "muscle-lonestar-3.8.31u2", from the \code{GetAppInfo(user.name, token, "muscle-lonestar-3.8.31u2")[[2]]}, we get the \code{input.list=list("stdin")}.  For the application "velveth-1.2.07u1", again using \code{GetAppInfo(user.name, token, "velveth-1.2.07u1")[[2]]}, the \code{input.list=list("reads1", "reads2", "reads3", "reads4", "reads5", "reads6")}.  A couple notes, the \code{input.list} can be shorter than the the number of inputs, for example, using the "velveth-1.2.07u1" application, the input list could be \code{input.list=list("reads1", "reads2", "reads3")}.  Also, the \code{file.list} is the same length as \code{input.list}, and in this example, lets say \code{file.list=list("filer", "filea", "filez")}, then "filer" corresponds with the "read1" input.  Also for \code{args.list} use \code{GetAppInfo} function, when the 'kind' column is 'parameters', those are the inputs for \code{args.list}.  For \code{velveth-1.2.07u1} the \code{args.list} is as follows, \code{list(c("format1", value), c("kmer", value), c("Output", value))}.  The list can be as long as the number of options.
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
\dontrun{ListJobOutput(myJob)}
             
# Download output files
\dontrun{RetrieveJob(myJob, ListJobOutput(myJob))}
     
# View job history
\dontrun{GetJobHistory()}

# Delete Job
\dontrun{DeleteJob(myJob)}
}
