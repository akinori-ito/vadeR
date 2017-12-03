# vadeR
A voice activity detetion function for R

This package provides the voice acitivity detection function for an Wave object.The return value is a vector of logical, each value of which indicates that the frame (10msec by default) is the voice part or not.

## Example
```R:
library(vadeR)
library(tuneR)

> x <- readWave("example.wav")
> v <- voiceActivity(x)
> v
  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
  [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
  [25] FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
  ...
  [289]  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
> voiceSegment(v)
  begin end
1    28 131
2   156 291
```



