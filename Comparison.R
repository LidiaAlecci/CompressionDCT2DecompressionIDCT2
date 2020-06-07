#DCT2 vs myDCT2
source("Script.R")

library(dtt)

library(microbenchmark)
library(reshape2)
library(ggplot2)

max_n = 100
dim=c(1,seq(5, max_n, 5))
ldim=length(dim)
results = data.frame(myDCT2=numeric(ldim),
                     DCT2= numeric(ldim),
                     id = dim)
for(i in 1:ldim){
  m=matrix(rexp(dim[i]*dim[i]), ncol=dim[i])
  results$myDCT2[i] = median(microbenchmark::microbenchmark(myDCT2(m), times=1)$time)
  results$DCT2[i] = median(microbenchmark::microbenchmark(dct(m, variant = 2, inverted=FALSE), times=1)$time)
}

results$myDCT2=1e-9*results$myDCT2#from nanoseconds to seconds
results$DCT2=1e-9*results$DCT2#from nanoseconds to seconds

resultsM = melt(results, id.vars=c("id"))

ggplot(resultsM, aes(x=value, y=id, color=variable)) + 
  geom_line() + scale_x_continuous(trans='log10') +
  xlab("time in seconds") + ylab("matrix dim")

