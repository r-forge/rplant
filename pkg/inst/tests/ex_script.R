require(rPlant)

load("user.name.RData")
load("user.pwd.RData")
Validate(user.name,user.pwd)

data(ex.lp.fasta)
write.fasta(sequences = ex.lp.fasta, names = names(ex.lp.fasta), file.out = "ex.lp.fasta")
         
# Upload a file to the DE
UploadFile(local.file.name="ex.lp.fasta", file.type="FASTA-0")

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

myJobM <- Muscle(file.name="ex.lp.fasta", job.name="MUSCLE")

Wait(myJobM[[1]], minWait, maxWait)

myJobR <- RAxML(file.name="phylip_interleaved.aln", file.path=paste("analyses/", myJobM[[2]], sep=""), job.name="RAxML")

Wait(myJobR[[1]], minWait, maxWait)

RetrieveJob(myJobR[[1]], c("RAxML_bestTree.nwk"))
