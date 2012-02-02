###This is a supplementary example of how to use rPlant to interface with the TNRS and DE API's (see Section 3 of rPlant's White Paper).


### (1) Install packages and attach data (will only work on the latest version of R)
install.packages("seqinr")  #package that supports manipulating fasta files
install.packages("rplant", repos="http://R-Forge.R-project.org", type="source")  #package that interacts with iPlant's API
library(seqinr) 
library(rplant) 
source("/Users/Barb/Desktop/rplant/pkg/R/rPlant.R")
data(landplant.fasta)  #not working
attach(landplant.fasta)  #not working
read.fasta("/Users/Barb/Desktop/rplant/pkg/data/landplant.fasta.txt")->landplant.fasta  #read in fasta file

### (2) Check species names using iPlants TNRS
attr(landplant.fasta, "name")->speciesNames  #extract species names from fasta file
resolveNames(speciesNames, maxPerCall=100)->TNRSspeciesNames  #Checks species names in the fasta using TNRS
compareTNRS(speciesNames, TNRSspeciesNames)  #Compares original species names with post-TNRS names
attr(landplant.fasta, "name")<-TNRSspeciesNames  #Replaces original FASTA species with corrected names
write.fasta(landplant.fasta, file="corrected.landplant.fasta")  #Writes a fasta file

### (3) Steps to complete MUSCLE job on iplant 
token.get(user.name="XXXX", user.pwd ="XXXX", API="iplant")->token  #Authenticate user on iPlant and receive token to work locally
token.renew(user.name="XXXX", user.pwd ="XXXX", token=token, API="iplant")  #Renew token 
list.dir(user.name=user.name, token=token) #List files in user directory
file.upload(user.name=user.name, token=token, file2upload="corrected.landplant.fasta", fileType="FASTA-0")  #Upload a fasta formatted file (other supported file types can be found using file.support())
make.dir(user.name=user.name, token=token, newDirect="rplant")  #Make a new working directory in the DE
file.move(user.name=user.name, token=token, fileName="landplant.fasta.txt", path2newdir="/rplant")  #Move the fasta file inside
list.dir(user.name=user.name, token=token, path2directory="/rplant")  #List files in new subdirectory
app.list(user.name=user.name, token=token)  #Make sure that MUSCLE is loaded onto the iplant cluster
app.info(user.name=user.name, token=token, application="muscle-ranger-2.0")  #List any aplication information
job.submit(user.name=user.name, token=token, application="muscle-ranger-2.0", path2inputSeqs="/<user.name>/corrected.landplant.fasta", jobName="MUSCLE", nprocs="1")->myJob  #Submit MUSCLE job

### (4) Check and retreive results from MUSCLE job
job.status(user.name=user.name, token=token, jobID=myJob)  #Check job status
job.output.list(user.name=user.name, token=token, jobID=myJob)  #Once a job is complete, this will list output files
job.retrieve(user.name=user.name, token=token, jobID=myJob, file2retrieve="psba.cln.aln") #Retreives file fr downloading

#Mistakes can be easily fixed by renaming files and deleting files, directories, or jobs
file.rename(user.name=user.name, token=token, oldName="/rplant-MUSCLE/landplant.fasta.txt", newName="/rplant-MUSCLE/landplant.fasta")  
file.delete(user.name=user.name, token=token, file2delete="/rplant/landplant.fasta.txt")
delete.dir(user.name=user.name, token=token, delDirect="/rplant")
job.delete(user.name=user.name, token=token, jobID=myJob)

### (5) Calculate N-J start tree
read.alignment("/Users/BarbBanbury/psba.cln.aln", format="fasta")->landplant.aligned  #Load aligned sequences
as.matrix(dist.alignment(landplant.aligned, matrix="similarity"))->dist.matrix  #Calculate distances
nj(dist.matrix)->landplant.startTree  #Quick neighbor-joining tree







