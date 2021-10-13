require("readxl")
require("gdata")
require("plyr")

read_eavs <- function(year) {
  data.dir <- paste0("data/", year)
  
  
  if (length(dir(data.dir)) > 1) {
    # list of xls and xlsx files to process correctly
    xls.list <- dir(data.dir)[grepl("xls", dir(data.dir)) & !grepl("xlsx", dir(data.dir))]
    xlsx.list <- dir(data.dir)[grepl("xlsx", dir(data.dir))]
    first_run <- 1
    
    for (i in dir(data.dir)) {
      if (i %in% xls.list) {
        # incorrectly formatted xls requires writing to csv and then processing
        f <- gdata::xls2csv(paste0(data.dir, "/", i), blank.lines.skip =T, verbose = T)
        # fixing quoting issue
        x <- readLines(f)
        x <- gsub("\\\\\"", "", x)
        # tempfile to process as csv
        d <- tempfile(fileext = ".csv")
        writeLines(x, d)
        section <- read.csv(d)
        file.remove(d)
      } else if (i %in% xlsx.list) {
        section <- readxl::read_xlsx(paste0(data.dir, "/", i))
      }
      if (first_run){
        first_run <- 0
        frame <- section
      } else {
        if (year == 2008) {
          frame <- plyr::join(frame, section, type="full", by=c("STATE_", "JurisID"))
        } else if (year == 2014) {
          frame$FIPSCode <- as.numeric(frame$FIPSCode)
          section$FIPSCode <- as.numeric(section$FIPSCode)
          frame <- plyr::join(frame, section, type="full", by=c("State", "Jurisdiction", "FIPSCode"))
        } else if (year == 2010 | year == 2012) {
          frame$FIPSCode <- as.numeric(frame$FIPSCode)
          section$FIPSCode <- as.numeric(section$FIPSCode)
          frame <- plyr::join(frame, section, type="full", by=c("State", "FIPSCode"))
        }
        closeAllConnections()
      }
    } 
    
  } else {
    frame <- read.csv(paste0(data.dir, "/", dir(data.dir)))
  }
  frame$year <- year
  return(frame)
}


write_eavs <- function(frame, format, year) {
  if (format == "csv") {
    write.csv(frame, paste0("data/",year, ".csv"))
  }
   
}



fix_eavs_vars <- function(frame) {
  year <- frame$year[1]
  names(frame) <- tolower(names(frame))
  if (year != 2014) {
    names(frame)[grepl("^a", names(frame))] <- paste0("q", names(frame)[grepl("^a", names(frame))])
    names(frame)[grepl("^b", names(frame))] <- paste0("q", names(frame)[grepl("^b", names(frame))])
    names(frame)[grepl("^c", names(frame))] <- paste0("q", names(frame)[grepl("^c", names(frame))])
    names(frame)[grepl("^d", names(frame))] <- paste0("q", names(frame)[grepl("^d", names(frame))])
    names(frame)[grepl("^e", names(frame))] <- paste0("q", names(frame)[grepl("^e", names(frame))])
    names(frame)[grepl("^f", names(frame))] <- paste0("q", names(frame)[grepl("^f", names(frame))])
  }
  
  
  if (year == 2008) {
    names(frame)[grepl("state", names(frame)) & !grepl("name", names(frame))] <- "state_abbv"
    names(frame)[grepl("state", names(frame)) & grepl("name", names(frame))] <- "state"
    
    names(frame)[grepl("jurisid", names(frame))] <- "fipscode"
    names(frame)[grepl("juris", names(frame))] <- "jurisdiction"
    frame$fipscode <- frame$qfips_code
    frame <- frame[, names(frame) != "qfips_code"]
    # get rid of inconsistent not available categories 
    frame <- frame[,!grepl("_notavailable", names(frame))]
    frame <- frame[,!grepl("_notapplicable", names(frame))]
    frame <- frame[,!grepl("qf7.*na", names(frame))]
    

  } else if (year %in% c(2010, 2012, 2014,2016)) {
    names(frame)[grepl("state", names(frame)) & !grepl("full", names(frame))] <- "state_abbv"
    frame$state <- unlist(lapply(frame$state_abbv, function(x) {
      c(state.name, "AMERICAN SAMOA", "DISTRICT OF COLUMBIA", "GUAM", 
        "PUERTO RICO", "VIRGIN ISLANDS")[c(state.abb,"AS","DC","GU","PR","VI")==x]
    }))
    names(frame)[grepl("juris", names(frame))] <- "jurisdiction"
    
    
    
    # get rid of arbitrary columns
    frame <- frame[, names(frame) %in% c("x", "x.1", "x.2", "x.3", 
                                         "qfips_2digit", "preferredorder",
                                         "qansicode", "qansicode.1",
                                         "pk_id", "pk_id.1") == F]
    # fix fipscode issue
    names(frame)[names(frame) == "qfips_code"] <- "fipscode"
    names(frame)[grepl("qfipscode", names(frame))] <- "fipscode"
    if (year == 2012) {
      names(frame)[3] <- "fips_correct"
      frame <- frame[,names(frame) != "fipscode"]
      names(frame)[3] <- "fipscode"
    }
    
    #get rid of arbitrary total categories from 2010-14
    if (year %in% c(2010, 2012, 2014)) {
      frame <- frame[, !grepl("q[a-f].*_total", names(frame))]
    }
    
    
  } else if (year == 2018 | year == 2020) {
    names(frame)[names(frame) == "qfipscode"] <- "fipscode"
    
    names(frame)[grepl("state", names(frame)) & !grepl("full", names(frame))] <- "state_abbv"
    names(frame)[grepl("state", names(frame)) & grepl("full", names(frame))] <- "state"
    names(frame)[grepl("juris", names(frame))] <- "jurisdiction"
  }
  return(frame)
}




