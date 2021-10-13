require("dplyr")

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

fix_types <- function(x) {
  char.vec <- names(x)[grepl("comment", names(x)) | 
                         grepl("Comment", names(x)) |
                         grepl("_model", names(x)) |
                         grepl("_version", names(x)) |
                         grepl("_make", names(x)) |
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
               grepl("qa", names(x)) | grepl("qb", names(x)) | grepl("qc", names(x)) |
               grepl("qd", names(x)) |grepl("qe", names(x)) | grepl("qf", names(x)) |
               grepl("_difficultyRecruitingPollWorkers", names(x)) |
               grepl("_na", names(x)) |
               names(x) %in% c("state", "reg_activeOnly", "jurisdiction",
                               "state_abbv", "reg_allowSameDay")
               ]
  
  x[, names(x) %in% char.vec] <- apply(x[, names(x) %in% char.vec], 2, function(y) {
    return(as.character(y))
  })
  x[, names(x) %in% char.vec == F] <- apply(x[, names(x) %in% char.vec == F], 2, function(y) {
    return(as.numeric(y))
  })
  x
}



create_eavs_years <- function(years = seq(2008, 2020, 2)) {
  
  bind_rows(lapply(years, function(year) {
    eavs <- fix_eavs_vars(read_eavs(year))
    eavs <- rename_eavs(eavs)
    eavs <- fix_types(eavs)
  }))
  
  
}




