
.onAttach <- function(libname, pkgname){
  version <- utils::packageVersion(pkgname)
  msg <- paste0(
    "\nTIP: Met de functie 'install_base_script()' kan je het basisscript ",
    "voor de verwerking in je werkdirectory plaatsen.\n,
    Zie ?install_base_script voor meer informatie."
  )
  packageStartupMessage(
    sprintf("Package '%s' versie %s geladen.\n", pkgname, version),
    "\n\n",
    msg
  )
}
