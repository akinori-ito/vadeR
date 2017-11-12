#' playVoice
#' This function takes the Wave object and plays only the speech parts, segment by segment, using tuneR::play() function.
#' @importFrom tuneR extractWave play
#' @export
#' @param x The input Wave object
#' @examples
#' \dontrun{
#' library(tuneR)
#' x <- readWave("speech.wav")
#' playVoice(x)
#'}
playVoice <- function(x) {
    v <- voiceActivity(x)
    vle <- rle(v)
    n <- length(vle$lengths)
    b <- 0
    for (i in 1:n) {
        e <- b+vle$lengths[i]
        if (vle$values[i]) {
            y <- extractWave(x,from=b/100,to=e/100,xunit="time")
            play(y)
        }
        b <- e+1
    }
    vle
}

