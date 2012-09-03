GetCitations <- function(application, save=FALSE, file="rPlant.bib") {
  # Returns citations for programs and analyses
  # So we are going to make the library of citations.  I would suggest we make a bibtex entry alongside these entries, someone might want a different citation method.
  citations<-c()
  if (application == "muscle-ranger-2.0") {
  	citations <-append(citations, ref("10.1093/nar/gkh340"))
  	citations <-append(citations, ref("10.1186/1471-2105-5-113"))
  }
  if (application != "muscle-ranger-2.0") {
  	print("working on more applications")
  }
  if(save)
    write.bibtex(citations, file=file, append=T) 
  else  
    return(citations)
}
