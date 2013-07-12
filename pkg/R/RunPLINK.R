RunPLINK <- function(user.name, token, DE.file.list="", DE.file.path="",
                     input.list=list("inputMAP","inputPED"), input.type="R", 
                     job.name=NULL, association.method="--assoc", no.sex=TRUE,
                     print.curl=FALSE, multi.adjust=TRUE, nprocs=1,
                     version="plink-1.07") {
  

  args <- paste("arguments=", association.method)

  if (multi.adjust){args <- append(args, c("--adjust"))}

  if (no.sex){args <- append(args, c("--allow-no-sex"))}

  if (input.type=="T"){
    BASE1 <- substr(DE.file.list[[1]],1,nchar(DE.file.list[[1]])-5)
    EXT1 <- substr(DE.file.list[[1]],nchar(DE.file.list[[1]])-3,nchar(DE.file.list[[1]]))
    BASE2 <- substr(DE.file.list[[2]],1,nchar(DE.file.list[[2]])-5)
    if (EXT1 == "tfam"){
      job.name <- paste(BASE1,"_",BASE2,"_", association.method, sep="")
    } else {
      job.name <- paste(BASE2,"_",BASE1,"_", association.method, sep="")
    }
    args <- append(args, c("--out",job.name))
  } else if (input.type=="B") {
    BASE1 <- substr(DE.file.list[[1]],1,nchar(DE.file.list[[1]])-4)
    EXT1 <- substr(DE.file.list[[1]],nchar(DE.file.list[[1]])-2,nchar(DE.file.list[[1]]))
    BASE2 <- substr(DE.file.list[[2]],1,nchar(DE.file.list[[2]])-4)
    EXT2 <- substr(DE.file.list[[2]],nchar(DE.file.list[[2]])-2,nchar(DE.file.list[[1]]))
    BASE3 <- substr(DE.file.list[[3]],1,nchar(DE.file.list[[3]])-4)
    EXT3 <- substr(DE.file.list[[3]],nchar(DE.file.list[[3]])-2,nchar(DE.file.list[[1]]))
    if (EXT1 == "bed" && EXT3 == "bam"){
      job.name <- paste(BASE1, "_", BASE3, "_", BASE2, "_", association.method, sep="")
    } else if (EXT1 == "bed" && EXT2 == "bam") {
      job.name <- paste(BASE1, "_", BASE2, "_", BASE3, "_", association.method, sep="")
    } else if (EXT2 == "bed" && EXT3 == "bam"){
      job.name <- paste(BASE2, "_", BASE3, "_", BASE1, "_", association.method, sep="")
    } else if (EXT3 == "bed" && EXT1 == "bam"){
      job.name <- paste(BASE3, "_", BASE1, "_", BASE2, "_", association.method, sep="")
    } else if (EXT2 == "bed" && EXT1 == "bam") {
      job.name <- paste(BASE2, "_", BASE1, "_", BASE3, "_", association.method, sep="")
    } else {
      job.name <- paste(BASE3, "_", BASE2, "_", BASE1, "_", association.method, sep="")
    }
    args <- append(args, c("--out",job.name))
  } else {
    BASE1 <- substr(DE.file.list[[1]],1,nchar(DE.file.list[[1]])-4)
    job.name <- paste(BASE1,"_", association.method, sep="")
    args <- append(args, c("--out",job.name))
  }

  # make a single statement
  args <- paste(args, collapse=" ") 

  # Submit
  myJob<-SubmitJob(user.name, token, application=version, 
                   DE.file.list=DE.file.list, DE.file.path=DE.file.path,
                   input.list=input.list, job.name=job.name, nprocs=nprocs, 
                   args=args, print.curl=print.curl)

  return(list(myJob,job.name))

}
