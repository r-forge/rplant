GetCitations <- function(application, save=FALSE, file="rPlant.bib") {
  # Returns citations for programs and analyses
  # So we are going to make the library of citations.  I would suggest we make a bibtex entry alongside these entries, someone might want a different citation method.
  citations<-c()
  if (application == "muscle-ranger-2.0") {
  	citations <- append(citations, ref("10.1093/nar/gkh340"))
  	citations <- append(citations, ref("10.1186/1471-2105-5-113"))
  }
  if (application != "raxml-lonestar-7.2.8") {
  	citations <- append(citations, ref("10.1093/bioinformatics/btl446"))
    
  }
  if (application != "phylip-dna-parsimony-lonestar-3.69") {
  	citations <- append(citations, ref(""))
    #Felsenstein, J. 2005. PHYLIP (Phylogeny Inference Package) version 3.6. Distributed by the author. Department of Genome Sciences, University of Washington, Seattle.
	#Felsenstein, J. 1989. PHYLIP - Phylogeny Inference Package (Version 3.2). Cladistics 5: 164-166. 

  }
  else 
  	print("working on more applications")
  if(save)
    write.bibtex(citations, file=file, append=T) 
  else  
    return(citations)
}
