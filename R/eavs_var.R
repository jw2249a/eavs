#' download_eavs()
#' @description Used to find and define EAVS variables in the clean EAVS file, or the regular EAVS
#' @param var Searches variable names for matches. (example 'qa1a')
#' @param question Searches question text. (example "registrations")
#' @param topic Searches topics. List of topics can be seen with list_topics() function. 
#' @param print.out logical. FALSE returns data.frame of variables. 
#' @return a data frame or printed output.
#' @export
eavs_var <- function(var="", question="",topic="", print.out=T) {
  if (var != "") {
    if (substr(var, 1, 1) == "q") {
      ret <- defs[grepl(tolower(var), defs$Question.Number.formatted),]
      
    } else {
      ret <- defs[grepl(tolower(var), tolower(defs$Question.Number)),]
      
    }
  } else if (question!="") {
    ret <- defs[grepl(tolower(question), tolower(defs$Question)),]
    
  } else if (topic != "") {
    ret <- defs[grepl(tolower(topic), tolower(defs$Topic)),]
    
  } else {
    warning("No argument, need to provide var, question, or topic")
  }
  if (print.out) {
    for (i in seq_along(ret$Topic)) {
      cat(paste0("Topic: ","'",ret$Topic[i],"'", "\n" ))
      cat(paste0("Question_num: ", '"',ret$Question.Number[i],'"    ', 'Std_variable: "', 
                 ret$variable.rename[i], '"\n'))
      cat(paste0("Question text: "))
      cat(paste0('"', ret$Question[i], '"'))
      cat(paste0(" \n\n"))
    }
  } else {
    ret
  }
  

}

#' download_eavs()
#' @description Lists EAVS section topics 
#' @return printed output
#' @export
list_topics <- function() {
  for (i in unique(defs$Topic)) cat(i, sep = "\n")
}






# how to create source data
# defs <- read.csv("data/eavs_crosswalk.csv")
# defs$Question.Number.formatted <- paste0("q", tolower(defs$Question.Number))
# 
# 
# eavs.names <- read.csv("data/eavs_conversion_file.csv")
# 
# defs$variable.rename <- 
#   unlist(mapply(function(x, y) {
#     ret <- eavs.names$replacement_new[eavs.names$value == x & eavs.names$year==y]
#     if (length(ret) == 0) {
#       ret <- x
#     }
#     ret
#   }, x=defs$Question.Number.formatted, y=2020))
# 
# 
# 
# write.csv(defs, "data/eavs_definitions.csv", row.names = F)
