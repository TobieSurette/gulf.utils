compile.gulf.package <- function(path = "C:/gulf package/gulf/", major = R.version$major, version.increment = "build", env="dev", r.exec.path = NULL, bat.file="BuildGulfPackage.bat"){
  # COMPILE.GULF.PACKAGE - Compiles the latest version of the 'gulf' package.
  # path = "C:/rtest/gulf package/gulf/"
  
  # Set R path to directory containing the batch file
  current.path = getwd()
  setwd(path)
  
  if(env == "production")
  {
    # Modify description file and return new version number
    version.num = modify.desc.file(version.increment = version.increment,  file = "DESCRIPTION")
  
    # Need to modify bat file before we execute to ensure the proper version number
    modify.bat.file(version.num = version.num, bat.file =bat.file, r.exec.path)
  }

  # Compile!
  shell(bat.file)
  
  # Take newly created zip file and put in apppropriate folder
  if(!is.null(major))
  {	
    # Create version folder name
    version.dir = paste0("R version ", R.version$major)

    # Go up one level and set temp pathh top ceate directory
    setwd("..")
    temp.path = paste0(getwd(),"/")
    version.path = paste0(temp.path, version.dir,"/")
        
    # Create the directory. If path already exists it will simply abort.
    dir.create(version.dir, showWarnings = FALSE)
    
    # Get path where batch file copies ziff file (1 up from working dir)
    zip.dir = temp.path
    
    # There should only be one ziff and gz file in that folder and it should always be the package file
    file.to.remove=dir(zip.dir,pattern="*.gz$|*.zip$")
    file.to.copy=dir(zip.dir,pattern="*.zip$")
        
    # Copy to specific location
    file.copy(paste0(zip.dir, file.to.copy), paste0(version.path,file.to.copy))
    # remove files created by bat file
    file.remove(file.to.remove)
  }
  # Reset path   
  setwd(current.path)
}


modify.desc.file <- function(version.increment = "build", file = "DESCRIPTION"){
  
  # Read DESCRIPTION file
  
  desc.file = read.table(file = file, sep="\n" , stringsAsFactors = FALSE ,header = FALSE, quote="")
  
  # Remove column name
  desc.file = desc.file[[1]]
  
  # Find row with version
  col.index = grep("Version",desc.file)
  
  # Isolate version number
  x=strsplit(desc.file[col.index], "[:]")
  x[[1]][2] = gsub( " ",x=x[[1]][2], replacement = "")
  # Split into major, minor and build 
  version = strsplit(x[[1]][2], "[.]")
  
  # Increment the only part of the version number
  if(version.increment == "major")
    version[[1]][1] = as.character(as.numeric(version[[1]][1])+1)
  if(version.increment == "minor")
    version[[1]][2] = as.character(as.numeric(version[[1]][2])+1)
  if(version.increment == "build")
    version[[1]][3] = as.character(as.numeric(version[[1]][3])+1)
  
  # Build new version number
  version.num = paste0(version[[1]][1],".",version[[1]][2],".",version[[1]][3])
  version = paste0("Version: ",version.num)
  
  # Put new version in table
  desc.file[col.index] = version
  
  # Write back to the DESCRIPTION file
  write.table(desc.file,file=file,quote = FALSE,  row.names = FALSE, col.names= FALSE)

  return(version.num)
}

modify.bat.file <- function(version.num = NULL, bat.file = NULL, r.exec.path = NULL)
{
  # need to change the version name of the tar file.  
  # We will read the file and only modify that line of code and write back to the file
  
  if(!is.null(version.num))
  {
    x = read.table(bat.file, sep="\n" , stringsAsFactors = FALSE ,header = FALSE, quote="")
    x = x[[1]]
    #find line that contains the tar file
    col.index = grep("tar.gz",x)
    x[col.index] = paste0("R CMD INSTALL --build --data-compress=\"none\" gulf_",version.num,".tar.gz")
    
    if(!is.null(r.exec.path))
    {
      col.index = grep("set Path",x)
      x[col.index] = paste0("set Path=",r.exec.path,";%PATH%")
    }
    write.table(x,file=bat.file,quote = FALSE,  row.names = FALSE, col.names= FALSE )
  }
  else
    stop("No version number available, can't compile")
}
