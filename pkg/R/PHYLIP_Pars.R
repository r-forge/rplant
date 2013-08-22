PHYLIP_Pars <- function(file.name, file.path="", job.name=NULL, type="DNA",
                        print.curl=FALSE, shared.username=NULL, 
                        suppress.Warnings=FALSE) {

  type <- match.arg(type, c("DNA", "PROTEIN"))

  if (type == "DNA") {
    version="phylip-dna-parsimony-lonestar-3.69u2"
  } else {
    version="phylip-protein-parsimony-lonestar-3.69u2"
  }

  nprocs <- 1
  App <- GetAppInfo(version)[[3]]
  input.list <- vector("list",1)
  input.list[[1]] <- App[,2][1]

  if (is.null(job.name))
    job.name <- paste(rplant.env$user,"_",version,"_viaR", sep="")

  myJob<-SubmitJob(application=version, job.name=job.name, nprocs=nprocs,
                   file.list=list(file.name), file.path=file.path, 
                   input.list=input.list, suppress.Warnings=suppress.Warnings,
                   print.curl=print.curl, shared.username=shared.username)

  return(myJob)
}
