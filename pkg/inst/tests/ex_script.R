require(rPlant)
require(ape)

# GetNames <- function() {
# This function works fine, but it can not be used in a batch script such as this, it will just read the next line down instead of waiting for input.  Bummer.  
# 	user.name <- readline("enter your username")
# 	user.pwd <- readline("enter your secret password")
# 	return(list=c(user.name, user.pwd))
# }

Validate("user.name", "user.pwd")  # add your own user.name and user.pwd

# Wait function
Wait <- function(job.id, minWaitsec, maxWaitsec, print=FALSE){
  currentStatus= ''
  currentWait = minWait
  while (( currentStatus != 'FAILED' ) && (currentStatus != 'ARCHIVING_FINISHED')) {
    # cache the status from previous inquiry
    oldStatus = currentStatus
    currentStatus = CheckJobStatus( job.id )

    if (currentStatus == oldStatus) {# status hasn't changed from last time we asked so
      currentWait = currentWait * 1.10 # wait 10% longer to poll in the future

      if (currentWait > maxWait) {
        currentWait = maxWait # but don't wait too long
      }
    } else {
      currentWait = minWait # status changed so reset wait counter to min value
    }
    
    if (print == TRUE) {
      print(paste("Wait Time:", currentWait, "Status:", currentStatus))
    }
    
    Sys.sleep(currentWait) # sit idle for proscribed time. If you are using an event-based programming model, you could just schedule the next check currentWait sec in the future 
  }
}

minWait = 5 # seconds
maxWait = 1800 # 30 min in seconds

# Upload a file to the DE
data(DNA.fasta)
write.fasta(sequences = DNA.fasta, names = names(DNA.fasta), file.out = "DNA.fasta")
UploadFile(local.file.name="DNA.fasta", filetype="FASTA-0")

data(PROTEIN.fasta)
write.fasta(sequences = PROTEIN.fasta, names = names(PROTEIN.fasta), file.out = "PROTEIN.fasta")
UploadFile(local.file.name="PROTEIN.fasta", filetype="FASTA-0")


### Workflow One ###
myJobW1MuP <- Muscle("PROTEIN.fasta", aln.filetype="PHYLIP_PARS", job.name="muscleWORKFLOW1")
Wait(myJobW1MuP[[1]], minWait, maxWait)
myJobW1PPP <- PHYLIP_Pars("phylip_pars.aln", file.path=paste("analyses/",myJobW1MuP[[2]], sep=""), type="PROTEIN", job.name="phylipWORKFLOW1")
Wait(myJobW1PPP[[1]], minWait, maxWait)
RetrieveJob(myJobW1PPP[[1]], c("outtree.nwk"))
read.tree(paste(getwd(), myJobW1PPP[[2]], "outtree.nwk", sep="/")) -> Tree1
plot(Tree1)



### Workflow Two ###
myJobW2MaP <- Mafft("PROTEIN.fasta", type="PROTEIN", job.name="mafftWORKFLOW2")
Wait(myJobW2MaP[[1]], minWait, maxWait)
myJobW2FaP <- Fasttree("mafft.fa", type="PROTEIN", file.path=paste("analyses/",myJobW2MaP[[2]], sep=""), job.name="fasttreeWORKFLOW2")
Wait(myJobW2FaP[[1]], minWait, maxWait)
RetrieveJob(myJobW2FaP[[1]], c("fasttree.nwk"))
read.tree(paste(getwd(), myJobW2FaP[[2]], "fasttree.nwk", sep="/")) -> Tree2
plot(Tree2)



### Workflow Three ###
myJobW3CWD <- ClustalW("DNA.fasta", job.name="clustalwWORKFLOW3")
Wait(myJobW3CWD[[1]], minWait, maxWait)
myJobW3FaD <- Fasttree("clustalw2.fa", file.path=paste("analyses/",myJobW3CWD[[2]], sep=""), job.name="fasttreeWORKFLOW3")
Wait(myJobW3FaD[[1]], minWait, maxWait)
RetrieveJob(myJobW3FaD[[1]], c("fasttree.nwk"))
read.tree(paste(getwd(), myJobW3FaD[[2]], "fasttree.nwk", sep="/")) -> Tree3
plot(Tree3)



### Workflow Four ###
myJobW4MuD <- Muscle("DNA.fasta", aln.filetype="PHYLIP_INT", job.name="muscleWORKFLOW4")
Wait(myJobW4MuD[[1]], minWait, maxWait)
myJobW4RD <- RAxML("phylip_interleaved.aln", file.path=paste("analyses/",myJobW4MuD[[2]], sep=""), job.name="raxmlWORKFLOW4")
Wait(myJobW4RD[[1]], minWait, maxWait)
RetrieveJob(myJobW4RD[[1]], c("RAxML_bestTree.nwk"))
read.tree(paste(getwd(), myJobW4RD[[2]], "RAxML_bestTree.nwk", sep="/")) -> Tree4
plot(Tree4)



### Or if you want to do all four workflows at the same time, to do them more quickly do something like this: ###

### These first four can be run because they don't depend on each other ###
# myJobW1MuP <- Muscle("PROTEIN.fasta", aln.filetype="PHYLIP_PARS", job.name="muscleWORKFLOW1")
# myJobW2MaP <- Mafft("PROTEIN.fasta", type="PROTEIN", job.name="mafftWORKFLOW2")
# myJobW3CWD <- ClustalW("DNA.fasta", job.name="clustalwWORKFLOW3")
# myJobW4MuD <- Muscle("DNA.fasta", aln.filetype="PHYLIP_INT", job.name="muscleWORKFLOW4")

### Once all four are finished move on ###
# Wait(myJobW1MuP[[1]], minWait, maxWait)
# Wait(myJobW2MaP[[1]], minWait, maxWait)
# Wait(myJobW3CWD[[1]], minWait, maxWait)
# Wait(myJobW4MuD[[1]], minWait, maxWait)

### Now run the tree building applications ###
# myJobW1PPP <- PHYLIP_Pars("phylip_pars.aln", file.path=paste("analyses/",myJobW1MuP[[2]], sep=""), type="PROTEIN", job.name="phylipWORKFLOW1")
# myJobW2FaP <- Fasttree("mafft.fa", type="PROTEIN", file.path=paste("analyses/",myJobW2MaP[[2]], sep=""), job.name="fasttreeWORKFLOW2")
# myJobW3FaD <- Fasttree("clustalw2.fa", file.path=paste("analyses/",myJobW3CWD[[2]], sep=""), job.name="fasttreeWORKFLOW3")
# myJobW4RD <- RAxML("phylip_interleaved.aln", file.path=paste("analyses/",myJobW4MuD[[2]], sep=""), job.name="raxmlWORKFLOW4")

### Once all four are finished move on ###
# Wait(myJobW1PPP[[1]], minWait, maxWait)
# Wait(myJobW2FaP[[1]], minWait, maxWait)
# Wait(myJobW3FaD[[1]], minWait, maxWait)
# Wait(myJobW4RD[[1]], minWait, maxWait)

### Retrieve the jobs ###
# RetrieveJob(myJobW1PPP[[1]], c("outtree.nwk"))
# RetrieveJob(myJobW2FaP[[1]], c("fasttree.nwk"))
# RetrieveJob(myJobW3FaD[[1]], c("fasttree.nwk"))
# RetrieveJob(myJobW4RD[[1]], c("RAxML_bestTree.nwk"))

### Read the tree in ... ###
# read.tree(paste(getwd(), myJobW1PPP[[2]], "outtree.nwk", sep="/")) -> Tree1
# read.tree(paste(getwd(), myJobW2FaP[[2]], "fasttree.nwk", sep="/")) -> Tree2
# read.tree(paste(getwd(), myJobW3FaD[[2]], "fasttree.nwk", sep="/")) -> Tree3
# read.tree(paste(getwd(), myJobW4RD[[2]], "RAxML_bestTree.nwk", sep="/")) -> Tree4

### Plot . . . ###
# plot(Tree1)
# plot(Tree2)
# plot(Tree3)
# plot(Tree4)
