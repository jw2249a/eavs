dir.create("data", showWarnings = F)

create_dir <- function(extract.dir, year) {
  dir.create(paste0(extract.dir, year),  showWarnings = F)
  return(paste0(extract.dir, year))
}


download_unzip_remove <- function(url, extract.dir, year) {
  outdir <- create_dir(extract.dir, year)
  outfile <- paste0(outdir, "/excel.zip")
  out <- tryCatch(
    {
    download.file(url, destfile = outfile, quiet = T, mode = "wb", method="libcurl")
    unzip(outfile, exdir = outdir,overwrite = T)
    
    Sys.sleep(.1)
    closeAllConnections()
    file.remove(outfile)
  }, error=function(cond){
    return(cond)
  },
  warning=function(e) {
    return(e)
  })
  return(out)
}

download_eavs_year <- function(year, extract.dir) {
  if (year == 2008) {
    url <- "http://www.eac.gov/sites/default/files/eac_assets/1/28/2008%20EAVS%20XLS.zip"
    
    out <- download_unzip_remove(url, extract.dir, year)
    
  } else if (year == 2010) {
    url <- "https://www.eac.gov/sites/default/files/eac_assets/1/28/Final%20EAVS%20Data%20-%20Sections%20C%20to%20F_EXCEL.zip"
    out  <- download_unzip_remove(url, extract.dir, year)
    
    # section A and B packaged separately for some reason
    url10_1 <- "http://www.eac.gov/sites/default/files/eac_assets/1/6/2010EAVS_A_Final.xls"
    
    outfile <- paste0(extract.dir, year, "/EAVS Section A.xlsx")
    download.file(url10_1, destfile = outfile, quiet = T, mode = "wb", method="libcurl")
    url10_2 <- "http://www.eac.gov/sites/default/files/eac_assets/1/6/FINAL_UOCAVAData1.xlsx"
    outfile <- paste0(extract.dir, year, "/EAVS Section B.xlsx")
    download.file(url10_2, destfile = outfile, quiet = T, mode = "wb", method="libcurl")
    
  } else if (year == 2012) {
    url <- "http://www.eac.gov/sites/default/files/eac_assets/1/6/Excel%20Files-Part%201.zip"
    out  <- download_unzip_remove(url, extract.dir, year)
    url <- "http://www.eac.gov/sites/default/files/eac_assets/1/6/Excel%20Files-Part%202.zip"
    out  <- download_unzip_remove(url, extract.dir, year)
    
    # section A, B, and F packaged separately for some reason
    url12_1 <- "http://www.eac.gov/sites/default/files/eac_assets/1/1/2012EAVS_NVRAData.xlsx"
    outfile <- paste0(extract.dir, year, "/EAVS Section A.xlsx")
    download.file(url12_1, destfile = outfile, mode = "wb",quiet = T, method="libcurl")
    url12_2 <- "http://www.eac.gov/sites/default/files/eac_assets/1/1/2012EAVS_UOCAVAData.xls"
    outfile <- paste0(extract.dir, year, "/EAVS Section B.xls")
    download.file(url12_2, destfile = outfile, mode = "wb",quiet = T, method="libcurl")
    
    
  } else if (year == 2014) {
    url <- "http://www.eac.gov/sites/default/files/eac_assets/1/1/2014_EAVS_Excel_Files1.zip"
    out  <- download_unzip_remove(url, extract.dir, year)
    
    
  } else if (year == 2016) {
    url <- "http://www.eac.gov/sites/default/files/Research/EAVS_2016_Final_Data_for_Public_Release_v2.csv.zip"
    out  <- download_unzip_remove(url, extract.dir, year)
    
  } else if (year == 2018) {
    url <- "http://www.eac.gov/sites/default/files/Research/EAVS_2018_for_Public_Release_Updates3.csv"
    outdir <- create_dir(extract.dir, year)
    outfile <- paste0(outdir,  "/eavs_2018.csv")
    download.file(url, destfile = outfile, mode = "wb",quiet = T, method="libcurl")
  } else if (year == 2020) {
    url <- "http://www.eac.gov/sites/default/files/2021-08/2020_EAVS_for_Public_Release_nolabel[1].csv"
    
    outdir <- create_dir(extract.dir, year)
    outfile <- paste0(outdir,  "/eavs_2020.csv")
    download.file(url, destfile = outfile, mode = "wb",quiet = T, method="libcurl")
  }
}



download_eavs <- function(years="", extract.dir = paste0(getwd(), "/data/")) {
  if (years == "all" | years=="") years <- seq(from=2008, to=2020, by=2)
  for (year in years) {
    tryCatch(
      {
        download_eavs_year(year, extract.dir)
      }, error=function(e){
        return(e)
      })
    
    Sys.sleep(1.5)
  }
  closeAllConnections()
}






