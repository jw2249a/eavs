epi_corrections <- function(var="", year="", print.out=T) {
  corrections <- read.csv("data/corrections.csv")
  state.corrections <- read.csv("data/corrections_statewide.csv")
  eavs.names <- read.csv("data/eavs_conversion_file.csv")
  
  all.corrections <- rbind(corrections, state.corrections)
  
  all.corrections$variables[all.corrections$year == 2008] <- paste0("q", all.corrections$variables[all.corrections$year == 2008] )
  
  all.corrections$variable.rename <- 
    unlist(mapply(function(x, y) {
      ret <- eavs.names$replacement_new[eavs.names$value == x & eavs.names$year==y]
      if (length(ret) == 0) {
        ret <- x
      }
      ret
    }, x=all.corrections$variables, y=all.corrections$year))
  
  if (var != "") {
    if (substr(var, 1, 1) == "q") {
      ret <- all.corrections[grepl(tolower(var), tolower(all.corrections$variables)),]
      
    } else {
      ret <- all.corrections[grepl(tolower(var), tolower(all.corrections$year)),]
      
    }
  } else if (year!="") {
    ret <- all.corrections[all.corrections$year==year,]
    
  }  else {
    warning("No argument, need to provide var, question, or topic")
  }
  if (print.out) {
    for (i in seq_along(ret$year)) {
      cat(paste0("Year: ","'",ret$year[i],"'", "\n" ))
      cat(paste0("variables: ", '"',ret$variables[i],'"    ', 'std_variable: "', 
                 ret$variable.rename[i], '"\n'))
      cat(paste0("subset value: ", '"',ret$subset_value[i],'"    ', 'updated value: "', 
                 ret$updated_value[i], '"\n'))
      cat(paste0("Comment: "))
      cat(paste0('"', ret$comment[i], '"'))
      cat(paste0(" \n\n"))
    }
  } else {
    ret
  }
}



apply_epi_corrections <- function(frame) {
  state.list <- c(state.abb, "DC")
  all.corrections <- epi_corrections(var = "*", print.out = F)
  mapply(function(var, update.val, subset.val, yr) {
    if (subset.val %in% state.list) {
      if (grepl(".*_o", var) | grepl(".*_c", var)) {
        frame[[var]][frame$state_abbv == subset.val & frame$year==yr] <<- update.val
      } else {
        frame[[var]][frame$state_abbv == subset.val & frame$year==yr] <<- as.numeric(update.val)
      }
      
    } else {
      subset.val <- as.numeric(subset.val)
      if (grepl(".*_o", var) | grepl(".*_c", var)) {
        frame[[var]][frame$fipscode == subset.val & frame$year==yr] <<- update.val
      } else {
        frame[[var]][frame$fipscode == subset.val & frame$year==yr] <<- as.numeric(update.val)
        
      }
    }
    return(NULL)
  }, var=all.corrections$variable.rename, update.val=all.corrections$updated_val,
  subset.val=all.corrections$subset_value, yr=all.corrections$year)
  return(frame)
}
