# Helper functions to support GOE_SWTS

#create function to find the time difference and return in %H:%M:%S format
custTimeDiff <- function(time1, time2){
   a <- time2 - time1    
   units(a) <- "secs"
   a <- as.numeric(a)
   #Find hours (h)
   h <- a %/% (60*60)
   #reduce a by # of hours
   a <- a - (h * 60 * 60)
   #find minutes
   m <- a %/% 60 
   #reduce a by # of minutes
   a <- a - (m * 60)
   #find seconds
   s <- a
   s <- round(s, 0)
   #Put it all together
   if (h < 10) { h <- paste0("0", h)}
   if (m < 10) {m <- paste0("0", m)}
   if (s < 10) {s <- paste0("0", s)}
   a <- paste0(h, ":", m, ":", s)
   #return answer
   return(a)
} #/custTimeDiff
   

#
#create function to find the time difference and return in %H:%M:%S format
custTimeDiff2 <- function(time){
   ans <- c()
   for (i in 1:length(time)){
      a <- time[i] - time[1]    
      units(a) <- "secs"
      a <- as.numeric(a)
      #Find hours (h)
      h <- a %/% (60*60)
      #reduce a by # of hours
      a <- a - (h * 60 * 60)
      #find minutes
      m <- a %/% 60 
      #reduce a by # of minutes
      a <- a - (m * 60)
      #find seconds
      s <- a
      s <- round(s, 0)
      #Put it all together
      if (as.numeric(h) < 10) { h <- paste0("0", round(h, 0))}
      if (as.numeric(m) < 10) {m <- paste0("0", round(m, 0))}
      if (as.numeric(s) < 10) {s <- paste0("0", round(s, 0))}
      a <- paste0(h, ":", m, ":", s)
      ans <- c(ans, a)
   }
   
   #return answer
   return(ans)
} #/custTimeDiff
