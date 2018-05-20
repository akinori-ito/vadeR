#' voiceSegment
#' This function takes the Wave object or the result of voiceActivity() and returns a data frame of the voice segment.
#' @export
#' @param x The input Wave object or vector of logical
#' @param unit The unit of the result. "frame" (default) means the number of frames, and "time" means the duration time in second.
#' @param frameshift The frame shift (in second). This parameter is used only when x is a Wave object.
#' @param margin Margin to be added before and after a segment
#' @examples
#' \dontrun{
#' library(tuneR)
#' x <- readWave("speech.wav")
#' v <- voiceActivity(x)
#' seg <- voiceSegment(v)
#'}
voiceSegment <- function(x,unit="frame",frameshift=0.01,margin=0) {
    if (class(x) == "Wave") {
        x <- voiceActivity(x,frameshift=frameshift)
    }
    vle <- rle(as.vector(x))
    n <- length(vle$lengths)
    b <- 1
    f.begin <- c()
    f.end <- c()
    for (i in 1:n) {
        e <- b+vle$lengths[i]-1
        if (vle$values[i]) {
            f.begin <- c(f.begin,b)
            f.end <- c(f.end,e)
        }
        b <- e+1
    }
    res <- data.frame(begin=f.begin,end=f.end)
    if (unit == "time") {
        res$begin <- res$begin*attr(x,"frameshift")
        res$end <- res$end*attr(x,"frameshift")
    }
    res$begin <- res$begin-margin
    res$end <- res$end+margin
    res
}
