#install rplant (will only work on the latest version of R)
install.packages("rplant", repos="http://R-Forge.R-project.org", type="source")  #not working
source("/Users/BarbBanbury/Desktop/rplant/pkg/R/rPlant.R")

data(landplant.fasta)  #not working
attach(landplant.fasta)  #not working

install.packages("seqinr")
library(seqinr)
read.fasta("/Users/BarbBanbury/Desktop/rplant/pkg/data/landplant.fasta.txt")->landplant.fasta


### Steps to complete MUSCLE job on iplant 

# 1) Enter iPlant ID
user.name="####"

# 2) Authenticate user on iPlant, receive token for working locally or renew a token 
token.get(user.name=user.name, user.pwd ="####", API="iplant")->token
token.renew(user.name=user.name, user.pwd ="####", token=token, API="iplant")

# 3) List what files are already available in the Data Store directory
list.dir(user.name=user.name, token=token)

# 4) Upload a fasta formatted file (other supported file types can be found using file.support())
file.upload(user.name=user.name, token=token, file2upload="/Users/BarbBanbury/Desktop/rplant/pkg/data/landplant.fasta.txt", fileType="FASTA-0")  #We need to change this to make it from the package

# 5) Make a new working directory in the DE and move the fasta file inside
make.dir(user.name=user.name, token=token, newDirect="rplant")  #no hyphens
file.move(user.name=user.name, token=token, fileName="landplant.fasta.txt", path2newdir="/rplant")
list.dir(user.name=user.name, token=token, path2directory="/rplant")

# 6) Make sure that MUSCLE is loaded onto the iplant cluster and get pertinent information
app.list(user.name=user.name, token=token)
app.info(user.name=user.name, token=token, application="muscle-ranger-2.0")

# 7) Submit MUSCLE job & check job status
job.submit(user.name=user.name, token=token, application="muscle-ranger-2.0", path2inputSeqs="/bbanbury/rplant/landplant.fasta.txt", jobName="BarbsMUSCLE", nprocs="1")->myJob
job.status(user.name=user.name, token=token, jobID=myJob)

#retreive results from MUSCLE job
job.retreive()  #when Matt emails back

#Mistakes can be easily fixed by renaming files and deleting files, directories, or jobs
file.rename(user.name=user.name, token=token, oldName="/rplant-MUSCLE/landplant.fasta.txt", newName="/rplant-MUSCLE/landplant.fasta")  
file.delete(user.name=user.name, token=token, file2delete="/rplant/landplant.fasta.txt")
delete.dir(user.name=user.name, token=token, delDirect="/rplant")
job.delete(user.name=user.name, token=token, jobID=myJob)

 







