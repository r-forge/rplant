RunMuscle <- function(user.name, token, fasta.file, job.name=NULL, tnrs=FALSE) {
  if (is.null(job.name))
    job.name <- paste(user.name, "MUSCLEviaAPI", sep="")
  if (!is.SeqFastadna(fasta.file[[1]]))
    stop("fasta.file is not in fasta formatting")
  # if (tnrs) {
  #  speciesNames <- attr(fasta.file, "name")  
  #  TNRSspeciesNames <- ResolveNames(speciesNames, maxPerCall=100, verbose=F)
  #  attr(fasta.file, "name") <- TNRSspeciesNames
  #  CompareTNRS(speciesNames, TNRSspeciesNames, verbose=F)
  # }
  myJob<-SubmitJob(user.name, token, application="muscle-ranger-2.0", path=fasta.file, job.name=job.name, nprocs="1")
  return(myJob)
}