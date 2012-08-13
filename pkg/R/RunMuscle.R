RunMuscle <- function(user.name, token, DE.file.name="", DE.file.path="", job.name=NULL, tnrs=FALSE) {
  if (is.null(job.name))
    job.name <- paste(user.name, "MUSCLEviaAPI", sep="")
  # if (tnrs) {
  #  speciesNames <- attr(fasta.file, "name")  
  #  TNRSspeciesNames <- ResolveNames(speciesNames, maxPerCall=100, verbose=F)
  #  attr(fasta.file, "name") <- TNRSspeciesNames
  #  CompareTNRS(speciesNames, TNRSspeciesNames, verbose=F)
  # }
  myJob<-SubmitJob(user.name, token, application="muscle-ranger-2.0", 
                   DE.file.name=DE.file.name, DE.file.path=DE.file.path, 
                   job.name=job.name, nprocs="1")
  return(myJob)
}
