source("download_eavs.R")
source("read_eavs.R")
source("clean_eavs.R")
source("eavs_var.R")

# x <- create_eavs_years()
# write.csv(x, "all_years_eavs.csv", row.names = F)
x <- read.csv("all_years_eavs.csv")

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


