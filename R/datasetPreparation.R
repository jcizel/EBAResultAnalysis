## ------------------------------------------------------------------------------------------- ##
## SET OF FUNCTIONS THAT PREPARES A LIST OF BANKS INVOLVED IN EBA AND ECB                      ##
## STRESS TESTS, AND CONSTRUCTS A LOOKUP TABLE BETWEEN THE                                     ##
## BANKS' ORIGINAL NAMES (AS USED IN THE TESTS) AND THEIR IDENTIFYING INFORMATION IN BANKSCOPE ##
## ------------------------------------------------------------------------------------------- ##


##' This function loads a table of banks that participated in EBA or ECB
##' tests. Due to a lack of consistency in naming and identification of banks
##' across different test, the participating banks needed to be manually linked
##' acroos the tests. I also manually link the banks with their identifying
##' information in Bankscope. 
##'
##' .. content for \details{} ..
##' @title Load a table of manually linked banks that participated in EBA and
##' ECB tests 
##' @return data.table with banks that participated in EBA and ECB tests.
##' @author Janko Cizel
loadManuallyLinkedBanks <- function(){
    out <-
        fread(input = './inst/extdata/MANUAL_CHECK.csv',
              stringsAsFactors = FALSE)[order(SOURCE,COUNTRY,`_NAME_A_ORIG`,CONSOL)]
    return(out)
}


collapseManuallyLinkedTable <- function(dt){
    .o1 <-
        dt[, {
            presence <- .condense(SOURCE)
            bsname <- .condense(`_NAME_B_ORIG`)
            leicode <- .condense(LEI)
            ebacode <- .condense(BANKCODE)
            consol <-  .condense(CONSOL)
            
            names <- paste(
                unique(paste0(`_NAME_A_ORIG`," [[",SOURCE,"]]")),
                collapse = "; "
            )
            
            list(
                bsname = bsname,
                consol = consol,
                presence = presence,
                names = names,
                ebacode = ebacode,
                leicode = leicode)
        }
          , keyby = list(COUNTRY,INDEX)]

    .o2 <- 
        .o1[, {

            presence <- .condense(presence)
            leicode <- .condense(leicode)
            ebacode <- .condense(ebacode)
            names <- collapseDuplicatesWithinString(.condense(names))
            
            bscode <- paste(
                unique(paste0(INDEX," (",consol,")")),
                collapse = "; "
            )

            list(presence = presence,
                 names = names,
                 bscode = bscode,
                 ebacode = ebacode,
                 leicode = leicode)
        }
          , keyby = list(COUNTRY,bsname)]

    return(.o2)
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Collapse duplicate enteries within a string 
##' @param str Character vector in which individual strings are separated by
##' `sep`, which by default is "; "
##' @param sep separator of enteries within individual strings
##' @return A character vector with removed duplicated enteries.
##' @author Janko Cizel
collapseDuplicatesWithinString <- function(str,
                                           sep = "; "){
    l <- strsplit(str,
                  split = sep)

    out <- 
        sapply(l, function (str.vec) {
            o <- unique(.trim(str.vec))
            paste(o[order(o)],
                  collapse = sep)
        })

    return(out)
}

##' A given bank typically has multiple (manually) matched Bankscope
##' identifiers, corresponding to different levels of accounting
##' consolidation. For a given bank, this function selects an index at the
##' higest available level of consolidation. 
##'
##' TODO: Describe Bankscope consolidation concepts
##
##' @title Bankscope Index Selector
##' @param bsIndexCol Character vector in which each entry has the following
##' strucuture: [BS_INDEX_1] ([CONSOL_1]); [BS_INDEX_2] ([CONSOL_2]);...
##' @return Numeric vector of selected Bankscope Indices
##' @author Janko Cizel
selectUniqueBSIndex <-  function(bsIndexCol){
    l <- strsplit(bsIndexCol,split = "; ")

    out <- 
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
    
    return(out)
}




##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Get a selection of banks involved in a specified test 
##' @param testName Name of the test. Currently supported: "AQR2014",
##' "EBARECAP2012", "EBATRANSPARENCY2013", "STRESSTEST2011", "STRESSTEST2014"
##'
##' @param outfile path of the output csv file. By default, no file is written.
##' @return data.table with banks involved in the querried test.
##' @author Janko Cizel
selectBanksInTest <- function (testName = 'AQR2014',
                               outfile = NULL){
    banks <- loadManuallyLinkedBanks()
    
    t1 <- collapseManuallyLinkedTable(dt = banks)
    
    t2 <- t1[, lapply(.SD, collapseDuplicatesWithinString)]
    
    t2[, bscode.unique := selectUniqueBSIndex(bscode)]
    
    out <- 
        t2[grepl(x = presence, pattern = toupper(query)), {
            l <- strsplit(names,split = "; ")

            names <- 
                sapply(l, function (str.vec ) {
                    o <- unique(.trim(str.vec))
                    cat("#### OBJECT:",dput(o),"\n")
                    cat("## TestName: ", dput(testName),"\n")
                    .o <- 
                        o[grepl(x = o,
                                pattern = testName)]
                    cat("## NEW OBJECT:",dput(.o),"\n")
                    cat("## LENGTH OF NEW OBJECT:",length(.o),"\n")                
                    if (length(.o)>1) {
                        ## stop("TestName matches multiple names")
                        warning("TestName matches multiple names")
                        return(.o[[1]])
                    }
                    return(.o)
                })

            names <- .trim(gsub(x = names, pattern = '(.*)\\[\\[.*\\]\\]','\\1'))
            
            list(bsname = bsname,
                 nameOrig = names,
                 ebacode = ebacode,
                 leicode = leicode,
                 bscode = bscode.unique)
        }]
    
    if (!is.null(outfile)){
        write.csv(
            x = out,
            file = paste0(outfile,".csv")
        )
    }

    return(out)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Convinience function: Get post-processed list of all banks involved
##' in any of EBA or ECB tests. 
##' @param outfile  path of the output csv file. By default, no file is written.
##' @return data.table with banks (containing Bankscope lookup information).
##' @author Janko Cizel
getAllBanks <- function (outfile = NULL){
    banks <- loadManuallyLinkedBanks()
    
    t1 <- collapseManuallyLinkedTable(dt = banks)
    
    t2 <- t1[, lapply(.SD, collapseDuplicatesWithinString)]
    
    t2[, bscode.unique := selectUniqueBSIndex(bscode)]

    if (!is.null(outfile)){
        write.csv(
            x = t2,
            file = paste0(outfile,".csv")
        )
    }
    
    return(t2)
}
