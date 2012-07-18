
##AUTHENTICATION FUNCTIONS##
token.get<-function(user.name, user.pwd, API=c("iplant", "cipres", "tnrs")){
	if (is.character(API)) {
		if (API=="iplant") {
			curl.string<-paste("curl -X POST -sku '", user.name, ":", user.pwd, "' https://foundation.iplantc.org/auth-v1/", sep="")
			res<-fromJSON(system(curl.string,intern=TRUE))
			if (res$status == "error"){return(res$message)} #returns error
			else (return(res$result$token)) #returns token
		}
		else {
			warning("Not yet implemented")
		}
	}
}

token.renew<-function(user.name, user.pwd, token, API=c("iplant", "cipres", "tnrs")){
	if (is.character(API)) {
		if (API=="iplant") {
			curl.string<-paste("curl -X POST -sku '", user.name, ":", user.pwd, "' -d 'token=", token, "' https://foundation.iplantc.org/auth-v1/renew",sep="")
			res<-fromJSON(system(curl.string,intern=TRUE))		
			res$status #Outputs a message renewal success
		}
		else {
			warning("Not yet implemented")
		}
	}
}
##END##


##FILE AND DATA FUNCTIONS##
file.upload<-function(user.name, token, file2upload, fileType){
	curl.string<-paste("curl -sku '", user.name, ":", token, "' -F 'fileToUpload=@", file2upload, "' -F 'fileType=", fileType, "' https://foundation.iplantc.org/io-v1/io/", user.name, sep="")
	res<-fromJSON(system(curl.string,intern=TRUE))
	res$status #Should output a status of success:
}

file.rename<-function(user.name, token, oldName, newName){
	curl.string<-paste("curl -sku '", user.name, ":", token, "' -X PUT -d 'newName=", newName, " &action=rename", "' https://foundation.iplantc.org/io-v1/io/", user.name, "/", oldName, sep="")
	res<-fromJSON(system(curl.string,intern=TRUE))
	if (res$status == "error"){return(paste(res$status, ":", res$message))}
	else {return(res$status)} 
}

file.move<-function(user.name, token, fileName, path2newdir){
	curl.string<-paste("curl -sku '", user.name, ":", token, "' -X PUT -d 'newPath=", user.name, "/", path2newdir, "/", fileName, "&action=move", "' https://foundation.iplantc.org/io-v1/io/", user.name, "/", fileName, sep="")
	res<-fromJSON(system(curl.string,intern=TRUE))
	if (res$status == "error"){return(paste(res$status, ":", res$message))}
	else {return(res$status)} 
}

file.delete<-function(user.name, token, file2delete){
	curl.string<-paste("curl -sku '", user.name, ":", token, "' -X DELETE", " https://foundation.iplantc.org/io-v1/io/", user.name, "/", file2delete, "/", sep="")
	res<-fromJSON(system(curl.string,intern=TRUE))
	if (res$status == "error"){return(paste(res$status, ":", res$message))}
	else {return(res$status)} 
}

file.support<-function(user.name, token){ #lists the supported file types -- does not work! It should. 
	curl.string<-paste("curl -X GET -sku '", user.name, ":", token, "' https://foundation.iplantc.org/data-v1/data/tranforms", sep="")
	res<-fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
	return(res) #might change when it works
}
##END##


##DIRECTORY FUNCTIONS##
list.dir<-function(user.name, token, path2directory=""){ 
	curl.string<-paste("curl -sku '", user.name, ":", token, "' https://foundation.iplantc.org/io-v1/io/list/", user.name, "/", path2directory, sep="")
	tmp<-fromJSON(system(curl.string,intern=TRUE))
	res<-matrix(,length(tmp$result),2)
	colnames(res)<-c("name", "type")
	for (i in 1:length(tmp$result)){
		res[i,1]<-tmp$result[[i]]$name
		res[i,2]<-tmp$result[[i]]$type
	}
	return(res)
}

make.dir<-function(user.name, token, newDirect, path2parent=""){
	curl.string<-paste("curl -sku '", user.name, ":", token, "' -X PUT -d 'dirName=", newDirect, "&action=mkdir' ", "https://foundation.iplantc.org/io-v1/io/", user.name, "/", path2parent, sep="")
	res<-fromJSON(system(curl.string,intern=TRUE))
	if (res$status == "error"){return(paste(res$status, ":", res$message))}
	else {return(res$status)} 
}

delete.dir<-function(user.name, token, delDirect){
	curl.string<-paste("curl -sku '", user.name, ":", token, "' -X DELETE https://foundation.iplantc.org/io-v1/io/", user.name, "/", delDirect, sep="")
	res<-fromJSON(system(curl.string,intern=TRUE))
	if (res$status == "error"){return(paste(res$status, ":", res$message))}
	else {return(res$status)} 
}
##END##


##APPLICATION FUNCTIONS##
app.list<-function(user.name, token){
	curl.string<-paste("curl -sku '", user.name, ":", token, "' https://foundation.iplantc.org/apps-v1/apps/share/list", sep="")
	tmp<-suppressWarnings(fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse="")))
	res<-matrix(,length(tmp$result))
	colnames(res)="Application"
	for (i in 1:length(tmp$result)){
		res[i,1]<-tmp$result[[i]]$id
	}
	return(sort(res))
}

app.info<-function(user.name, token, application, verbose=FALSE){
	curl.string<-paste("curl -X GET -sku '", user.name, ":", token, "' https://foundation.iplantc.org/apps-v1/apps/share/name/", application, sep="") 
	res<-fromJSON(system(curl.string,intern=TRUE))
	if(verbose){return(res)}
	else{
		res<-(list(application=res$result[[1]]$id, inputFileTypes=res$result[[1]]$inputs[[1]]$semantics$fileTypes, output=res$result[[1]]$outputs[[1]]$defaultValue)) #This needs to be cleaned up. I think the relevant info is a) inputs, b) possible input parameters, and c) outputs
		return(res)
	}
}
##END##


##JOB FUNCTIONS##

job.submit<-function(user.name, token, application, path2inputSeqs, jobName, nprocs=1){ #expand for other aps and additional input files
	curl.string<-paste("curl -X POST -sku '", user.name, ":", token, "' -d 'jobName=", jobName, "&softwareName=", application, "&archive=1&inputSeqs=", path2inputSeqs, "&processorCount=", nprocs, "&archivePath=/", user.name, "/analyses", jobName, "&requestedTime=24:00:00&outputFormat=fasta&mode=auto' https://foundation.iplantc.org/apps-v1/job", sep="")
	res<-fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
	if(res$status=="success"){
		cat("Job submitted. You can check the status of your job using this id:", res$result$id, "\n")
	}
	else{
		cat("Error.", res$message, "\n")
	}
	return(res$result$id)
	#also return or print citations
}

job.status<-function(user.name, token, jobID, verbose=FALSE){
	curl.string<-paste("curl -X GET -sku '", user.name, ":", token, "' https://foundation.iplantc.org/apps-v1/job/", jobID, sep="")
	res<-fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
	if (res$status == "error"){
		print(paste("Error in jobID ", jobID, ":", res$message))
		if(verbose){return(res)}
	}
	else {
		if(verbose){return(res)}
		else{
			return(res$result$status)
		}
	}
}

job.delete<-function(user.name, token, jobID){
	for(job in 1:length(jobID)){
		curl.string<-paste("curl -X DELETE -sku '", user.name, ":", token, "' https://foundation.iplantc.org/apps-v1/job/", jobID[job], sep="")
		print(curl.string)
		res<-fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
		res
	}
}

job.retrieve<-function(user.name, token, jobID, file2retrieve){ #what if file doesn't exist...make that an option with a return
	fileList<-job.output.list(user.name, token, jobID)[[1]] #only will work on one jobID now
	JS<-job.status(user.name, token, jobID, verbose=T)
	if (JS$res$status == "ARCHIVING_FINISHED") {
		for (file in 1:length(file2retrieve)){
			if (file2retrieve[file] %in% fileList) { #if file exists in output then download
				curl.string<-paste("curl -X GET -sku '", user.name, ":", token, "' https://foundation.iplantc.org/io-v1/io", JS$result$archivePath, "/", file2retrieve[file], " -o ", file2retrieve[file], sep="")
				res<-paste(system(curl.string,intern=TRUE),sep="", collapse="")
				print(paste("Downloaded", file2retrieve[file], "to", getwd(), "directory"))
			}
			else {return(paste(file2retrieve[file], "is not found within", jobID))}
		}	
	}
	else {
		warning("Job is ", JS)
	}	
}

job.output.list<-function(user.name, token, jobID){
	combRes<-vector("list", length=length(jobID))
	names(combRes)<-paste("jobID", jobID, sep="")
	for (job in 1:length(jobID)){
		files<-c()
		job.status(user.name, token, jobID[job], verbose=T)->JS
		if (JS$res$status == "ARCHIVING_FINISHED") {
			curl.string<-paste("curl -X GET -sku '", user.name, ":", token, "' https://foundation.iplantc.org/apps-v1/job/", jobID[job], "/output/list", sep="")
			res<-fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
			print(paste("There are ", length(res$result), "output files for job", jobID))
			for(i in 1:length(res$result)){
				files<-append(files, res$result[[i]]$name)
			}
		}
		else {files<-paste("Job is ", JS$res$status)}
	combRes[[job]]<-files
	}
	return(combRes)	
}


job.history<-function(user.name, token, verbose=F){
	jobList<-c()
	curl.string<-paste("curl -X GET -sku '", user.name, ":", token, "' https://foundation.iplantc.org/apps-v1/jobs/list", sep="")
	res<-fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
	if (verbose){
		return(res)
	}
	if(length(res$result) != 0){
		for (i in 1: length(res$result)){
			job<-c(res$result[[i]]$id, res$result[[i]]$software, res$result[[i]]$status)	
			jobList<-rbind(jobList, job)
			colnames(jobList)<-c("jobID", "application", "status")
		}	
	}
	return(jobList)
}


##TNRS FUNCTIONS##

resolveNames<-function(names,maxPerCall=100,verbose=TRUE) {
	#takes a list of names and sends it to the iPlant TNRS site (http://tnrs.iplantcollaborative.org/)
	#names<-c("zea mays","acacia","solanum","saltea","rosa_rugoso")
	#returnedNames<-resolveNames(names)
	#print(returnedNames)
	names<-sapply(names,sub,pattern="_",replacement=" ")
	names<-sapply(names,URLencode)
	callBase<-'http://tnrs.iplantc.org/tnrsm-svc/matchNames?retrieve=best&names='
	newNames<-rep(NA,length(names))
	namesInCall<-0
	callActual<-callBase
	startingPosition<-1
	for (nameIndex in sequence(length(names))) {
		namesInCall<-namesInCall+1
		callActual<-paste(callActual,names[nameIndex],",",sep="")
		if (namesInCall==maxPerCall || nameIndex==length(names)) {
			returnedValues<-fromJSON(file=callActual)$items
			for (returnIndex in sequence(length(returnedValues))) {
				newNames[startingPosition+returnIndex-1]<-returnedValues[[returnIndex]]$nameScientific 
			}
			if(verbose) {
				print(paste("finished ",nameIndex,"of ",length(names),"names")) 
			}
			startingPosition<-nameIndex+1
			namesInCall<-0
			callActual<-callBase
		}
	}
	print("Ignore a warning message about incomplete final line")
	if (length(newNames)!=length(names)) {
		warning(paste("the input name list was",length(names),"long but the new one is ",length(newNames),"long"))
	}
	newNames<-sapply(newNames, sub, pattern=" ",replacement="_", USE.NAMES=F)
	return(newNames)
}



compareTNRS<-function(original, TNRS, verbose=TRUE) {
#takes a list of original taxonomic names (same ones given as "names in resolveNames) and compares to the returned names from TNRS
#note that names are changed back to include an "_" instead of the " " they come with out of TNRS first, so that they do not count as taxonomic name changes
	d=0
	names2<-sapply(TNRS, sub, pattern=" ",replacement="_", USE.NAMES=F)
	comp<-cbind(original, names2)
	for (i in 1: dim(comp)[1]){
		if(comp[i,1] != comp[i,2]){
			d<-d+1
			if(verbose){
				print(paste(comp[i,1], " was changed to ", comp[i,2], cat("\n")))
			}
		}
	}
	print(paste(d, "taxa changed names according to TNRS"))
}
