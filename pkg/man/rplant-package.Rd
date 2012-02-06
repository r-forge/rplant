\name{rplant-package}
\alias{rplant-package}
\alias{rplant}
\docType{package}
\title{rplant}
\description{
	R interface to iPlant's Discovery Enviornment and the Taxonomic Name Resolution Service (TNRS)
}
\details{
\tabular{ll}{
Package: \tab rplant\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2-6-2012\cr
License: \tab GPL (>= 2)\cr
}
\author{
Barb Banbury, Jeremy Beaulieu, Brian O'Meara
Maintainers: Barb Banbury <bbanbury@utk.edu>, Brian O'Meara <bomeara@utk.edu>
}
\examples{

###This is a supplementary example of how to use rPlant to interface with the TNRS and DE API's (see Section 3 of rPlant's White Paper).

#Attach data
data(landplant.fasta)  
attach(landplant.fasta)  

#Check species names using iPlants TNRS
attr(landplant.fasta, "name")->speciesNames  #extract species names from fasta file
resolveNames(speciesNames, maxPerCall=100)->TNRSspeciesNames  #Checks species names in the fasta using TNRS
compareTNRS(speciesNames, TNRSspeciesNames)  #Compares original species names with post-TNRS names
attr(landplant.fasta, "name")<-TNRSspeciesNames  #Replaces original FASTA species with corrected names

#Submit MUSCLE job on iplant DE
token.get(user.name, user.pwd, API="iplant")->token  #Authenticate user on iPlant and receive token to work locally
token.renew(user.name, user.pwd, token=token, API="iplant")  #Renew token 
list.dir(user.name=user.name, token=token) #List files in user directory
file.upload(user.name, token, file2upload="corrected.landplant.fasta", fileType="FASTA-0")  #Upload a fasta formatted file (other supported file types can be found using file.support())
make.dir(user.name, token, newDirect="rplant")  #Make a new working directory in the DE
file.move(user.name, token, fileName="landplant.fasta.txt", path2newdir="/rplant")  #Move the fasta file inside
list.dir(user.name, token, path2directory="/rplant")  #List files in new subdirectory
app.list(user.name, token)  #Make sure that MUSCLE is loaded onto the iplant cluster
app.info(user.name, token, application="muscle-ranger-2.0")  #List any aplication information
job.submit(user.name, token, application="muscle-ranger-2.0", path2inputSeqs="/<user.name>/corrected.landplant.fasta", jobName="MUSCLE", nprocs="1")->myJob  #Submit MUSCLE job

#Check and retreive results from MUSCLE job
job.status(user.name, token, jobID=myJob)  #Check job status
job.output.list(user.name, token, jobID=myJob)  #Once a job is complete, this will list output files
job.retrieve(user.name, token, jobID=myJob, file2retrieve="landplant.fasta.aln") #Downloads file

#Mistakes can be easily fixed by renaming files and deleting files, directories, or jobs
file.rename(user.name=user.name, token=token, oldName="/rplant-MUSCLE/landplant.fasta.txt", newName="/rplant-MUSCLE/landplant.fasta")  
file.delete(user.name=user.name, token=token, file2delete="/rplant/landplant.fasta.txt")
delete.dir(user.name=user.name, token=token, delDirect="/rplant")
job.delete(user.name=user.name, token=token, jobID=myJob)
}
