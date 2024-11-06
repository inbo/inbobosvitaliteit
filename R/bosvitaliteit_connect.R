#' Connect to bosvitaliteit DB
#'
#' @return DBI DB connection object
#' @export
#'
bosvitaliteit_connect <- function(){
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = "SQL Server",
                        Server = "inbo-sql07-prd.inbo.be",
                        port = 1433,
                        Database = "D0004_00_Bosvitaliteit",
                        Trusted_Connection = "True")
  if(!inherits(con, "Microsoft SQL Server"))
    print("Connectie niet gelukt. Ben je op het INBO netwerk of via VPN verbonden? Contacteer de database administrator")
  con
}
