# source("R/download_eavs.R")
# source("R/read_eavs.R")
# source("R/clean_eavs.R")
# source("R/eavs_var.R")
devtools::load_all()
# 
x <- create_eavs_years()


write.csv(t(summary.data.frame(x)), "summaryofeavs.csv")

binvar <- unlist(lapply(names(x), function(y) {
  if (any(tolower(x[[y]]) %in% "x") & is.character(x[[y]])) {
    y
  }
}))


View(x[, c(binvar, "year")])

table(x[[binvar[1]]])



x$fipscode[is.na(as.numeric(x$fipscode))]


get_non_numeric_values <- function(x) {
  apply(x, 2, function(y) {
    table(y[is.na(as.numeric(y))], useNA = "always")
  })
}

get_non_numeric_values(x[,1:10])


x[x==""] <- NA

summary(x[, "reg_allowSameDay"])

summary(x[, 1:100])

# write.csv(x, "all_years_eavs.csv", row.names = F)
# x <- read.csv("all_years_eavs.csv")

corrections <- read.csv("data/corrections.csv")

state.corrections <- read.csv("data/corrections_statewide.csv")

eavs.names <- read.csv("data/eavs_conversion_file.csv")
corrections$variables[corrections$year == 2008] <- paste0("q", corrections$variables[corrections$year == 2008] )

x <- create_eavs_years()
x <- apply_epi_corrections(x)
x[x==""] <- NA
x[x<0] <- NA

reg.na <- mapply(function(year) {
  subx <- x[x$year==year,(grepl("partic_*.", names(x)) & 
                            names(x) %in% 
                            eavs.names$replacement_new[eavs.names$year==2020] &
                            !grepl(".*_o", names(x)) & !grepl(".*_c", names(x)))]
 sum(is.na(subx))/(ncol(x) * nrow(x))
}, year=seq(2008, 2020, 2))
reg.na*100
names(reg.na) <- seq(2008, 2020, 2)
reg.na*100
names(x)


