###########################################################################
# global_install_missing_packages.R ---------------------------------------
###########################################################################

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
reloadpck = function(reload = TRUE){
  if(reload){
    print(ipak(pckToLoad))
  }
}
writeLines(c("use 'pckToLoad = c(...)' to specify r packages in use,",
             "then 'reloadpck()' to install / load them"))
