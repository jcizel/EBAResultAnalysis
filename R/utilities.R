.trim <- function(str) gsub("^\\s+|\\s+$", "", str)

.condense <- function(str){
    str.out<- paste(
        {
            .x <- unique(str[!str %in% c("","NOTHING")])
            .x[order(.x)]
        },
        collapse = "; "
    )
    return(str.out)
}
