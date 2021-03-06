  # Libraries 
    {
        library(signal)
        library(tuneR)
        library(seewave)
        library(fftw)
        library(rgl)
        library(rpanel)
        library(ggplot2)
    }
# change directory to file location 
dname <- choose.dir()
setwd(dname)

# constants 
len <- 2 ^ 13

# read in file 
wave <- readWave("file.wav")
{
# Time Domain Analysis 
 Dtime <- dfreq(wave, wl = len, ovlp = 90, fftw = T, plot = T, ylim = c(0, 1), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
    summary(as.factor(Dtime[, 2]))

# Frequency Domain Analysis 
    spec <- meanspec(wave, wl = len, ovlp = 90, fftw = T, col = "blue", flim = c(0, 1.1))
    peaks <- fpeaks(spec, amp = c(0.1, 0.1), freq = 9, threshold = 0.25, xlim = c(0, 3), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
}
