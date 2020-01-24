install.packages(c("devtools", "roxygen2"))  # installations can be `c`ombined
library("devtools")
library("roxygen2")
parentDirectory <- "F:/MEDSL/2018_orecincts2/competition"
setwd(parentDirectory)
create("medslCompetition")

setwd("./medslCompetition")
document()

setwd(parentDirectory)
install("medslCompetition")
?overlapfunction
?best_match
?matched_pop
overlapfunction()


###testing how to install pkgs from github 
install_github("https://github.mit.edu/MEDSL/competition", subdir = "medslCompetition")
# e40f5f2aa49156352629f346e5b59bec9027ae3e
#put agbove under the auth token pparam 
?install_github
git remote add origin https://github.com/jcuriel-unc/arealOverlap2
