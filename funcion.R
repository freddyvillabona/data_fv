library("xts")

.pdenv <- new.env(parent=emptyenv())

pdfetch_YAHOO2 <- function(identifiers, 
                           fields=c("open","high","low","close","adjclose","volume"),
                           from=as.Date("2007-01-01"),
                           to=Sys.Date(),
                           interval="1d") {
  
  valid.fields <- c("open","high","low","close","adjclose","volume")
  interval <- match.arg(interval, c("1d","1wk","1mo"))
  
  if (!missing(from))
    from <- as.Date(from)
  if (!missing(to))
    to <- as.Date(to)
  
  if (missing(fields))
    fields <- valid.fields
  if (length(setdiff(fields,valid.fields)) > 0)
    stop(paste0("Invalid fields, must be one of ", valid.fields))
  
  results <- list()
  from <- as.numeric(as.POSIXct(from))
  to <- as.numeric(as.POSIXct(to))
  
  # The following borrows from quantmod, thank you to Joshua Ulrich.
  if (is.null(.pdenv$handle)) {
    h <- list()
    
    # establish session
    new.session <- function() {
      tmp <- tempfile()
      on.exit(unlink(tmp))
      
      for (i in 1:5) {
        h <- curl::new_handle()
        # random query to avoid cache
        ru <- paste(sample(c(letters, 0:9), 4), collapse = "")
        cu <- paste0("https://finance.yahoo.com?", ru)
        curl::curl_download(cu, tmp, handle = h)
        if (NROW(curl::handle_cookies(h)) > 0)
          break;
        Sys.sleep(0.1)
      }
      
      return(h)
    }
    
    h$ch <- new.session()
    
    n <- if (unclass(Sys.time()) %% 1L >= 0.5) 1L else 2L
    query.srv <- paste0("https://query", n, ".finance.yahoo.com/",
                        "v1/test/getcrumb")
    cres <- curl::curl_fetch_memory(query.srv, handle = h$ch)
    
    h$crumb <- rawToChar(cres$content)
    .pdenv$handle <- h
  }
  
  h <- .pdenv$handle
  
  for (i in 1:length(identifiers)) {
    url <- paste0("https://query1.finance.yahoo.com/v7/finance/download/",identifiers[i],
                  "?period1=",from,"&period2=",to,"&interval=",interval,"&events=history&crumb=",
                  h$crumb)
    
    resp <- curl::curl_fetch_memory(url, handle=h$ch)
    if (resp$status != 200) {
      warning(paste0("Could not find series '",identifiers[i],"'"))
      next
    }
    fr <- utils::read.csv(text=rawToChar(resp$content), na.strings="null")
    
    dates <- as.Date(fr$Date)
    fr <- fr[,-1]
    fr <- fr[,match(fields, valid.fields), drop=F]
    
    if (length(fields)==1)
      colnames(fr) <- identifiers[i]
    else
      colnames(fr) <- paste(identifiers[i], fields, sep=".")
    
    x <- xts(fr, dates)
    results[[identifiers[i]]] <- x
  }
  
  if (length(results) == 0)
    return(NULL)
  
  storenames <- sapply(results, names)
  results <- do.call(merge.xts, results)
  colnames(results) <- storenames
  results
}