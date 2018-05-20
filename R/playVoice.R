#' playVoice
#' This function takes the Wave object and plays only the speech parts, segment by segment, using tuneR::play() function.
#' @importFrom tuneR extractWave play
#' @export
#' @param x The input Wave object
#' @param seg Segmentation obtained by voiceSegment(..,unit="time")
#' @examples
#' \dontrun{
#' library(tuneR)
#' x <- readWave("speech.wav")
#' playVoice(x)
#'}
playVoice <- function(x,seg=NA) {
    if (is.na(seg)) {
      seg <- voiceSegment(x,unit="time",margin=0.3)
    }
    for (i in 1:nrow(seg)) {
        y <- extractWave(x,from=seg$begin[i],to=seg$end[i],xunit="time")
        play(y)
    }
}

