#' voiceActivity
#' This function takes the Wave object and returns an array of the voice activity.
#' @importFrom tuneR melfcc
#' @importFrom stats kmeans
#' @export
#' @param x The input Wave object or a matrix obtained by melfcc()
#' @param minlen Minimum duration of segment (number of frames)
#' @param maxlen Maximum duration of segment (number of frames)
#' @param nclust Number of clusters, one of which should be silent
#' @param frameshift The frame shift (sec), 10ms by default
#' @return A vector of logical with length of number of frames. If the frame is the voice frame, the content of the vector is TRUE, otherwise FALSE.
#' @examples
#' \dontrun{
#' library(tuneR)
#' x <- readWave("speech.wav")
#' act <- voiceActivity(x)
#'}

voiceActivity <- function(x,simple=TRUE,minlen=50,maxlen=1000,nclust=4,frameshift=0.01) {
    if ("Wave" %in% class(x)) {
        xf <- melfcc(x,hoptime=frameshift,dither=T)
    } else {
        xf <- x
    }

    # Do clutering on the MFCC 
    cls <- kmeans(xf,nclust)
    pow <- c(0,0)
    for (cl in 1:nclust) {
        pow[cl] <- mean(xf[cls$cluster==cl,1])
    }

    # the cluster with the least power should be the silence
    i.silent <- which.min(pow)

    if (simple) {
        r <- vadeR_simple(cls,i.silent,minlen)
    } else {
        r <- vadeR_heavy(cls,i.silent,minlen,maxlen)
    }
    attr(r,"frameshift") <- frameshift
    r
}

vadeR_simple <- function(cls,i.silent,minlen) {
    dur <- rle(cls$cluster)
    nsegment <- length(dur$lengths)
    for (i in 1:nsegment) {
        if (dur$lengths[i] < minlen & dur$values[i] == i.silent) {
            if (i == 1) {
                dur$values[i] <- dur$values[i+1]
            } else if (dur$value[i] != dur$value[i-1]) {
                dur$values[i] <- dur$values[i-1]
            }
        }
    }
    seg.valid <- rep(TRUE,nsegment)
    j <- 1
    for (i in 1:nsegment) {
        if (dur$values[i] == i.silent) {
            seg.valid[j:(j+dur$lengths[i]-1)] <- FALSE
        } else {
            seg.valid[j:(j+dur$lengths[i]-1)] <- TRUE
        }
        j <- j+dur$lengths[i]
    }
    seg.valid
}

vadeR_heavy <- function(cls,i.silent,minlen,maxlen) {
    # Caluculate the optimum segmentation with restriction of 
    # minimum and maximum length of the segment
    # Find the segmentation where each segment satisfies the length constraint
    # and errors between the cluster decision and the segment decision 
    # is minimum

    # cluster decision: 1=silence  2=non-silence
    y <- as.integer(cls$cluster!=i.silent)+1

    # DP array to calculate minimum distance between y and the segmentation
    w <- array(Inf,dim=c(length(y),maxlen,2))
    b <- array(0,dim=c(length(y),2))

    w[1,1,1] <- abs(y[1]-1)
    w[1,1,2] <- abs(y[1]-2)
    b[1,1] <- 0
    b[1,2] <- 0

    for (i in 2:length(y)) {
       if (i > minlen) {
          for (v in 1:2) {
             err <- abs(y[i]-v)
             # 3-v means "The other one"; if v=2 then 3-v=1 and vice versa
             # m is the optimum length of the previous segment
             m <- which.min(w[i-1,minlen:maxlen,3-v])+minlen-1
             w[i,1,v] <- err+w[i-1,m,3-v]
             b[i,v] <- m
          }
       }
       for (v in 1:2) {
          err <- abs(y[i]-v)
          for (j in 2:maxlen) {
             w[i,j,v] <- w[i-1,j-1,v]+err
          }
       }
   }

   # backtrace
   j1 <- which.min(w[length(y),,1])
   j2 <- which.min(w[length(y),,2])
   if (w[length(y),j1,1] < w[length(y),j2,2]) {
      j <- j1
      v <- 1
   } else {
      j <- j2
      v <- 2
   }

   r <- rep(0,length(y))
   i <- length(y)
   while (i > 0) {
      r[i] <- v
      i <- i-1
      j <- j-1
      if (j == 0) {
         j <- b[i+1,v]
         v <- 3-v
      }
   }
   r <- r==2
}

