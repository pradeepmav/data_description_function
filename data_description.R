require(prettyR)
require(pryr)
data_description <- function(datasetname) {
    varbclass <- as.data.frame(sapply(get(datasetname), class))
    if (nrow(varbclass) == 2) {
        varbclass <-
            as.data.frame(t(varbclass[-2, ]))
    } else {
        varbclass <- varbclass
    }
    varbno <-
        as.data.frame(sapply(get(datasetname), prettyR::valid.n, na.rm = T))
    varbmissing <-
        as.data.frame(sapply(get(datasetname), function(x)
            (sum(is.na(x)))), na.rm = F)
    varbdistinct <-
        as.data.frame(sapply(get(datasetname), function(x)
            length(unique(x))))
    varbmin <- as.data.frame(sapply(get(datasetname), min, na.rm = T))
    varbmax <- as.data.frame(sapply(get(datasetname), max, na.rm = T))
    varbmean <- as.data.frame(sapply(get(datasetname), mean, na.rm = T))
    varbmedian <-
        as.data.frame(sapply(get(datasetname), median, na.rm = T))
    try(varbmode <-
        as.data.frame(sapply(get(datasetname), prettyR::Mode, na.rm = T)))
    if (file.exists("varbmode")) {
        varbsumfile <-
            cbind(
                varbclass,
                varbno,
                varbmissing,
                varbdistinct,
                varbmin,
                varbmax,
                varbmean,
                varbmedian,
                varbmode
            )
    } else {
        varbsumfile <-
            cbind(
                varbclass,
                varbno,
                varbmissing,
                varbdistinct,
                varbmin,
                varbmax,
                varbmean,
                varbmedian
            )
    }
    if (file.exists("varbmode")) {
        names(varbsumfile) <-
            c(
                "Type",
                "No.of Non-Missing Values",
                "Missing Values",
                "Distinct Values",
                "Minimum",
                "Maximum",
                "Mean",
                "Median",
                "Mode"
            )
    } else {
        names(varbsumfile) <-
            c(
                "Type",
                "No.of Non-Missing Values",
                "Missing Values",
                "Distinct Values",
                "Minimum",
                "Maximum",
                "Mean",
                "Median"
            )
    }
    write.csv(as.data.frame(varbsumfile),
              file = "data_description.csv",
              row.names = T)
    pryr::mem_change(rm(list = ls(pattern = c("varb"))))
    pryr::mem_change(rm(datasetname))
}

# how to use
# not to run
# source("https://raw.githubusercontent.com/pradeepmav/data_description_function/master/data_description.R")
# data_description("datasetname")
