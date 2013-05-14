RunMafft <- function(user.name, token, DE.file.name="", DE.file.path="", 
                     job.name=NULL, nprocs=1) {

  App <- GetAppInfo(user.name, token, application)[[2]]
  input.list <- vector("list",1)
  input.list[[1]] <- App[,2][1]

  if (is.null(job.name))
    job.name <- paste(user.name,"_Mafft_viaR", sep="")

  myJob<-SubmitJob(user.name, token, application="mafft-lonestar-6.864u1", 
                   DE.file.list=list(DE.file.name), DE.file.path=DE.file.path, 
                   input.list=input.list, job.name=job.name, nprocs=nprocs)

  return(myJob)
}
