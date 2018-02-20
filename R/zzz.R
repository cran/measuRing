.onAttach <- function(lib, pkg)
{
  ## unlockBinding("measuRing", asNamespace("measuRing")) 
  version <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
  
  if(interactive())
    { # > figlet measuRing
        packageStartupMessage(
          "measuRing
version: ", version)
}
else
    { packageStartupMessage(
          "Package 'measuRing' version ", version) } 

  packageStartupMessage("Type 'citation(\"measuRing\")' for citing this R package in publications.")
  invisible()
}
  
