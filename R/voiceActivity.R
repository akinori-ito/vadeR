#' voiceActivity
#' This function takes the Wave object and returns an array of the voice activity.
#' @importFrom tuneR melfcc
#' @importFrom stats kmeans
#' @export
#' @param x The input Wave object
#' @param minlen Minimum duration of silence (number of frames)
#' @param nclust Number of clusters, one of which should be silent
#' @param frameshift The frame shift (sec), 10ms by default
#' @return A vector of logical with length of number of frames. If the frame is the voice frame, the content of the vector is TRUE, otherwise FALSE.
#' @examples
#' \dontrun{
#' library(tuneR)
#' x <- readWave("speech.wav")
#' act <- voiceActivity(x)
#'}

voiceActivity <- function(x,minlen=20,nclust=3,frameshift=0.01) {
    xf <- melfcc(x,hoptime=frameshift)
    cls <- kmeans(xf,nclust)
    pow <- c(0,0)
    for (cl in 1:nclust) {
        pow[cl] <- mean(xf[cls$cluster==cl,1])
    }
    i.silent <- which.min(pow)
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
#    plot(xf[,1],col=as.integer(seg.valid)+1)
    seg.valid
}

