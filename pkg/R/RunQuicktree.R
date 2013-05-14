RunQuicktree <- function(user.name, token, DE.file.name="", DE.file.path="",
                         job.name=NULL, nprocs=1) {

  App <- GetAppInfo(user.name, token, "quicktree-tree-lonestar-1.1u1")[[2]]
  input.list <- vector("list",1)
  input.list[[1]] <- App[,2][1]

  if (is.null(job.name))
    job.name <- paste(user.name,"_Quicktree_viaR", sep="")

  myJob<-SubmitJob(user.name, token, application="quicktree-tree-lonestar-1.1u1", 
                   DE.file.list=list(DE.file.name), DE.file.path=DE.file.path, 
                   input.list=input.list, job.name=job.name, nprocs=nprocs)

  return(myJob)
}
