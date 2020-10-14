abbr_name <- function(X) {
    
    # Make a character vector to store the result
    abbreviations <- character(length = length(X))
    
    # Split the scientific names into genus and epithet
    splitnames <- strsplit(X, " ")
    
    # Pick the first letter of the genus and paste it together with the epithet
    for (i in 1:length(X)) {
        X[i] <- if(splitnames[[i]][2] == "sp.") {
            paste(splitnames[[i]][1], 
                  splitnames[[i]][2], sep = " ") 
        } else {
            paste(substr(splitnames[[i]][1],1,1), 
                  splitnames[[i]][2], sep = ". ")  
        }
    }
    X
}