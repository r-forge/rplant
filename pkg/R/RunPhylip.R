RunPhylip <- function(user.name, token, DE.file.name, DE.file.path="", job.name=NULL, tnrs=FALSE) {

  # At this point must have DE.file.name NOT IN QOUTES!!!!  This is talked about later because the function looks for an object (DE.file.name) in the R workspace.  So if we get rid of checking for formatting it can then have quotes.  (Function below puts quotes around DE.file.name)

  quote <- deparse(substitute(DE.file.name))
  if (is.null(job.name))
    job.name <- paste(user.name, "PhylipviaAPI", sep="")

  # This function has an error because it searches for the fasta.file on the DE in the R workspace.  And of course the file on the DE does not have to be in the workspace.  We could potentially download the file from the DE to the home directory and then use some function to read it into the workspace.  Now thinking about that it won't work because how do we read the file into the workspace?

  # The following checks the workspace, if the object isn't there it proceeds with SubmitJob

  if (is.na(tryCatch(DE.file.name[[1]][1], error=function(x){x <- NA; return(x)}))){
    JobNum <- SubmitJob(user.name,
                        token,
                        application="phylip-dna-parsimony-lonestar-3.69",
                        DE.file.name=quote,
                        DE.file.path="",
                        job.name=job.name,
                        nprocs="1")}else{

  # If the file is in the workspace it is checked for formatting

  # It would be more useful to check for formatting on the DE but I don't know how to do that.

    if (!is.SeqFastadna(DE.file.name[[1]]))
      stop("fasta.file is not in fasta formatting")
  # if (tnrs) {
  #  speciesNames <- attr(fasta.file, "name")  
  #  TNRSspeciesNames <- ResolveNames(speciesNames, maxPerCall=100, verbose=F)
  #  attr(fasta.file, "name") <- TNRSspeciesNames
  #  CompareTNRS(speciesNames, TNRSspeciesNames, verbose=F)
  # }
    JobNum <- SubmitJob(user.name,
                        token, 
                        application="phylip-dna-parsimony-lonestar-3.69",
                        DE.file.name=quote,
                        DE.file.path=DE.file.path,
                        job.name=job.name,
                        nprocs="1")
  }
  return(JobNum)
}
