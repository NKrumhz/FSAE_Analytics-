library(genalg)
library(GenSA)
library(ggplot2)
# Constants
{
    Dout = 6        # outer diameter of pipe
    h = 0.05        # thickness of pipe
    Tmp = 520 # EGT Temp
    Din = Dout - (2*h)

    string <- vax <- vector(mode = "numeric", length = 3)
    #  c = sqrt(gamma * R * (Tmp + 459.67))    # in/s
    c = 19164       # in/s = speed of sound 
    tol = 9;
}

#Functions 
evaluation <- function(string = c()) {
returnVal = NA
    vax[1] <- pi / 4 * (Din ^ 2) * string[1]
    vax[2] <- string[2]
    vax[3] <- pi * (string[3] ^ 2)

    #cat("\n")
    if (length(string) == 3) {
        returnVal = abs(((c/(2*pi)) * sqrt(vax[3]/(vax[1]*vax[2]))) - 350) + (string[1] + string[2])^2 + abs(exp(vax[3]-6.2))
    }
    else {
        stop("Expecting a chromosome of length 3!")
    }
}

# Variables 
txt = c("Resonator length", "con.pipe length", "con.pipe radius")
lb = c( 0.1, 0.05, 0.45)
ub = c(5, 0.75, 1.405)

ga <- rbga(lb, ub, popSize = 500, iters = 750, evalFunc = evaluation, verbose = F, mutationChance = 0.1)
cat(summary(ga50))
plot(ga50)
grid()


