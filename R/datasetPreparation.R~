## PREPARE WORKSPACE
require(data.table)
require(ggplot2)
require(gridExtra)

PATH.DIR = "/Users/jankocizel/Documents/Dropbox/Projects/PhD Thesis/Banking_Quick_Tests"
PATH.RESULTS = "/Users/jankocizel/Documents/Dropbox/Projects/PhD Thesis/Banking_Quick_Tests/RESULTS"

setwd(dir = PATH.DIR)


BANKS <-
    fread(input = 'MANUAL_CHECK.csv',
          stringsAsFactors = FALSE)[order(SOURCE,COUNTRY,`_NAME_A_ORIG`,CONSOL)]


## BANK NAMES ARE WRITTEN DIFFERENTLY IN DIFFERENT TESTS. GET ALL NAMES PER
## BANKSCOPEID

.trim <- function(str) gsub("^\\s+|\\s+$", "", str)

.condense <- function(str){
    str.out<- paste(
        {
            .x <- unique(str[!str %in% c("","NOTHING")])
            .x[order(.x)]
        },
        collapse = "; "
    )
}

preparation <-
    BANKS[, {
        presence <- .condense(SOURCE)
        ## names <- .condense(`_NAME_A_ORIG`)
        leicode <- .condense(LEI)
        ebacode <- .condense(BANKCODE)
        consol <-  .condense(CONSOL)

        names <- paste(
            unique(paste0(`_NAME_A_ORIG`," [[",SOURCE,"]]")),
            collapse = "; "
        )
        
        list(consol = consol,
             presence = presence,
             names = names,
             ebacode = ebacode,
             leicode = leicode)
    }
        , by = list(COUNTRY,INDEX)]


output <- 
    preparation[, {

        presence <- .condense(presence)
        leicode <- .condense(leicode)
        ebacode <- .condense(ebacode)
        
        bscode <- paste(
            unique(paste0(INDEX," (",consol,")")),
            collapse = "; "
        )

        list(presence = presence,
             bscode = bscode,
             ebacode = ebacode,
             leicode = leicode)
    }
        , by = list(COUNTRY,names)][order(COUNTRY,names)]


output2 <- 
    output[ebacode!="", {

        presence <- .condense(presence)
        leicode <- .condense(leicode)
        names <- .condense(names)
        bscode <- .condense(bscode)

        list(presence = presence,
             bscode = bscode,
             names = names,
             leicode = leicode)
    }
        , by = list(COUNTRY,ebacode)][order(COUNTRY,ebacode)]

output3 <-
    rbind(output2,
          output[ebacode == ""])[order(COUNTRY, names)]


## ELEMENTS IN SOME FIELDS ARE DUPLICATED. SPLIT BY ";", TAKE UNIQUE, AND
## COLLAPSE TO SINGLE ENTRY AGAIN

output4 <- 
    output3[,{
        lapply(.SD, function (x) {
            l <- strsplit(x,split = "; ")
            
            sapply(l, function (str.vec ) {
                o <- unique(.trim(str.vec))
                paste(o[order(o)],
                      collapse = "; ")
            })
        })
    }]

require(stringr)
output4[, 
        bscode.unique := {
            l <- strsplit(bscode,split = "; ")

            RT <- 
                sapply(l, function (str.vec ) {
                    o <- unique(.trim(str.vec))
                    cat("#### OBJECT:",dput(o),"\n")
                    if (length(o)==1){
                        ## cat("LENGTH OF THE OBJECT = 1\n")
                        SEL <- as.numeric(str_extract(o, "[0-9]+")[1])
                        if (is.na(SEL)){
                            SEL = 0
                            cat("## CHOSE: ",SEL,"\n")
                            return(SEL)
                        } else {
                            cat("## CHOSE: ",SEL,"\n")
                            return(SEL)
                        }
                        cat("## CHOSE: ",SEL,"\n")
                        return(SEL)
                    } else {
                        ## cat("LENGTH OF THE OBJECT > 1\n")
                        .o <- 
                            o[grepl(x = o,
                                    pattern = "C2")]
                        ## cat("#### OBJECT (looking for C2):",dput(.o),"\n")
                        if (length(.o) == 0){
                            .o <- 
                                o[grepl(x = o,
                                        pattern = "C1")]
                            if (length(.o) == 0){
                                .o <- 
                                    o[grepl(x = o,
                                            pattern = "U1")]
                                if (length(.o) == 0){
                                    .o <- 
                                        o[grepl(x = o,
                                                pattern = "U")]
                                    SEL <- as.numeric(str_extract(.o, "[0-9]+")[1])
                                    cat("FLAG!!!!\n")
                                    cat("## CHOSE: ",SEL,"\n")
                                    return(SEL)
                                } else {
                                    SEL <- as.numeric(str_extract(.o, "[0-9]+")[1])
                                    cat("## CHOSE: ",SEL,"\n")
                                    return(SEL)
                                }
                            } else {
                                SEL <- as.numeric(str_extract(.o, "[0-9]+")[1])
                                cat("## CHOSE: ",SEL,"\n")
                                return(SEL)
                            }
                        } else {
                            SEL <- as.numeric(str_extract(.o, "[0-9]+")[1])
                            cat("## CHOSE: ",SEL,"\n")
                            return(SEL)
                        }
                    }
                })
            str(unlist(RT))
            return(unlist(RT))
        }]

write.csv(
    x = output4,
    file = "MANUAL_CHECK_RESHAPED.csv"
)

## .test <- c("22308 (U1)", "29758 (U*)")
## .test[grepl(x = .test,
##             pattern = "C2")]

query = "AQR2014"

QUERYSTRESSTEST <- function (query,
                             outfile){
    out <- 
        output4[grepl(x = presence, pattern = toupper(query)), {
            l <- strsplit(names,split = "; ")

            names <- 
                sapply(l, function (str.vec ) {
                    o <- unique(.trim(str.vec))
                    cat("#### OBJECT:",dput(o),"\n")
                    cat("## Query: ", dput(query),"\n")
                    .o <- 
                        o[grepl(x = o,
                                pattern = query)]
                    cat("## NEW OBJECT:",dput(.o),"\n")
                    cat("## LENGTH OF NEW OBJECT:",length(.o),"\n")                
                    if (length(.o)>1) {
                        ## stop("Query matches multiple names")
                        warning("Query matches multiple names")
                        return(.o[[1]])
                    }
                    return(.o)
                })

            names <- .trim(gsub(x = names, pattern = '(.*)\\[\\[.*\\]\\]','\\1'))
            
            list(names = names,
                 ebacode = ebacode,
                 leicode = leicode,
                 bscode = bscode.unique)
        }]

    write.csv(
        x = out,
        file = paste0(outfile,".csv")
    )
}

QUERYSTRESSTEST(query = "AQR2014",
                outfile = "LOOKUP_AQR2014")
QUERYSTRESSTEST(query = "STRESSTEST2011",
                outfile = "LOOKUP_STRESSTEST2011")
QUERYSTRESSTEST(query = "STRESSTEST2014",
                outfile = "LOOKUP_STRESSTEST2014")
QUERYSTRESSTEST(query = "EBARECAP2012",
                outfile = "LOOKUP_EBARECAP2012")
QUERYSTRESSTEST(query = "EBATRANSPARENCY2013",
                outfile = "LOOKUP_EBATRANSPARENCY2013")


