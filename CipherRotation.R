inputX <- "DbCD-eF"
upperLeft <- 65
upperRight <- 90
lowerLeft <- 97
lowerRight <- 122

cipherRotationc <- function(input, N)
{
  internalConversion <- function(byte)
  {
    if (between(byte, upperLeft, upperRight))
      return(intToUtf8(byte + N))
    else
      - if (between(byte, lowerLeft, lowerRight))
        return(intToUtf8(byte + N))
    else
      return(intToUtf8(byte))
  }
  y <- input %>% utf8ToInt() %>% purrr::map(internalConversion)
  result<-paste(unlist(y), collapse='')
  result
}
cipherRotationc(inputX, 4)