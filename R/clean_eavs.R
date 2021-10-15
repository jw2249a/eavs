#' rename_eavs()
#' @description Year agnostic names for the EAVS
#' @param eavs Data.frame. Output of read_eavs() function. 
#' @return a data frame, with dimensions depending on the year.
#' @export
rename_eavs <- function(eavs) {
  year <- eavs$year[1]
  eavs.names <- read.csv("data/eavs_conversion_file.csv")
  replacement.names <- eavs.names[eavs.names$year == year,]
  match.names <- unlist(lapply(names(eavs), function(x) {
    val <- replacement.names$replacement_new[replacement.names$value == x]
    
    if (identical(val, character(0))) val <- ""
    
    val
  }))
  
  names(eavs)[match.names != ""] <- match.names[match.names != ""]
  
  return(eavs)
}
#' fix_types()
#' @description Year agnostic data types for binding rows of eavs
#' @param x Data.frame. Output of rename_eavs() function. 
#' @return a data frame, with dimensions depending on the year.
#' @export
fix_types <- function(x) {
  char.vec <- names(x)[grepl("comment", names(x)) |
                         grepl("Comment", names(x)) |
                         grepl("_model", names(x)) |
                         grepl("_version", names(x)) |
                         (grepl("_make", names(x)) & !grepl("num", names(x))) |
                         grepl("_vendor", names(x)) |
                         grepl("_whoArranged", names(x)) |
                         grepl("_voterParticipationSource", names(x)) |
                         grepl("_updateVoterHistory", names(x)) |
                         grepl("_infoNotAvailable", names(x)) |
                         grepl("_paper", names(x)) |
                         grepl("_signIn", names(x)) |
                         grepl("_lookUpPollingPlace", names(x)) |
                         grepl("_electronic_other", names(x)) |
              (grepl("_c", names(x)) & !grepl("counters", names(x)) &
                !grepl("_change", names(x)) & !grepl("_con", names(x))) |
             (grepl("_o", names(x)) & grepl("_other([0-9])_o", names(x))) |
             (grepl("_o", names(x)) & !grepl("_other", names(x))) |
               (grepl("_o", names(x)) & grepl("_other_", names(x))) |
               ((grepl("qa", names(x)) | grepl("qb", names(x)) | grepl("qc", names(x)) |
               grepl("qd", names(x)) |grepl("qe", names(x)) | grepl("qf", names(x))) &
                 !grepl("_total", names(x))) |
               grepl("_difficultyRecruitingPollWorkers", names(x)) |

               names(x) %in% c("state", "reg_activeOnly", "jurisdiction",
                               "state_abbv", "reg_allowSameDay", "reg_notAllowSameDay")
               ]


  x[, names(x) %in% char.vec] <- data.frame(apply(x[, names(x) %in% char.vec], 2, function(y) {
    return(toupper(trimws(as.character(y))))
  }))
  # fixing some data types 
  x[x=="DOES NOT APPLY"] <- "-88"
  x[x=="DATA NOT AVAILABLE"] <- "-99"
  x[x %in% c("", "NA", "N/A")] <- NA
  logicals <- unlist(lapply(names(x[, names(x) %in% char.vec == F]), function(y) {
    if (any(x[[y]] %in% c("x", "X"))) {
      y
    }
  }))
  
  x[, names(x) %in% char.vec == F] <- 
    apply(x[, names(x) %in% char.vec == F], 2, function(y) {
    return(as.numeric(y))
  })
  
  # fixing numeric data types
  x[x==-999999] <- -99
  x[x==-888888] <- -88
  
  # fix the not available binary variables
  x[, grepl("_notavailable", names(x))] <- 
    apply(x[, grepl("_notavailable", names(x))], 2, function(y) {
      ret <- replicate(length(y), NA)
      ret[tolower(y) == "x"] <- T
      ret[y==""] <- F
      ret
  })
  
  
  x
}


#' create_eavs_years()
#' @description Create a multi-year EAVS file. 
#' @param years Array of valid years (even years from 2008-2020).
#' @param eavs.dir Defaults to data. Should be the extract.dir argument from download_eavs_year().
#' @return a data frame.
#' @export
create_eavs_years <- function(years = seq(2008, 2020, 2), eavs.dir = "data/") {
  
  b <- lapply(years, function(year) {
    eavs <- fix_eavs_vars(read_eavs(year, data.dir=eavs.dir))
    eavs <- rename_eavs(eavs)
    eavs <- fix_types(eavs)
  })
  
  for (i in 1:length(b)) {
    
    if (i != 1) {
      
      subs <- b[[i]]
      
      for (ii in names(subs)[names(subs) %in% names(main) == F]) main[[ii]] <- NA
      
      for (ii in names(main)[names(main) %in% names(subs) == F]) subs[[ii]] <- NA
      
      main <- rbind(main, subs)
    } else {
      main <- b[[i]]
    }
  }
  main
  
}




