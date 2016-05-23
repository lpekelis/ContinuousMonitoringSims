#Type 1 error of Repeated Significance Testing (RST) simulations
#Author: Leo Pekelis
#Date: 2/5/2016

continuous_monitor_pval_power <- function(N=10000,alpha=.05) {
  #return: pvals and post-hoc power for two two-tailed z-test and 0 effect size
  
  #pvals
  xx = rnorm(N)
  xx = cumsum(xx) / sqrt(1:N)
  
  pn = 2 * pnorm( -1 * abs(xx) )
  #pnm = sapply(1:N,function(k){min(pn[1:k])})
  
  #post-hoc power
  za = qnorm(1 - alpha / 2)
  beta = pnorm(xx - za) + pnorm(-1*(za + xx))
  
  return(cbind(pn,beta))
}

B = 1000

pvals_1k = replicate(B,continuous_monitor_pval_power(),simplify=F)

save(pvals_1k,file="pvals_1k.Rdata")

is_rejected <- function(x,alpha=.05,beta=NA) {
  #alpha - type 1 error cutoff
  #beta - power cutoff
  #return: rejections based on different decision rules
  
  N = dim(x)[1]
  if(is.na(beta)) {
    sapply(1:N,function(k){min(x[1:k,1])}) <= alpha
  } else {
    y = (x[,1] <= alpha) * (x[,2] >= beta)
    sapply(1:N,function(k){max(y[1:k])})
  }
}

rej_a10 = sapply(pvals_1k,is_rejected,alpha=.1)
rej_a05 = sapply(pvals_1k,is_rejected,alpha=.05)
rej_a01 = sapply(pvals_1k,is_rejected,alpha=.01)
rej_power = sapply(pvals_1k,is_rejected,beta=.8)
save(rej_a10,rej_a05,rej_a01,rej_power,file="cont_monitoring_sims.Rdata")

#plot the results

library(ggplot2)
N = 10000
plot.dat = data.frame(vals = c(rowMeans(rej_a10),rowMeans(rej_a05),rowMeans(rej_a01),rowMeans(rej_power)),group=factor(rep(1:4,each=N)),Observations=rep(1:N,times=4))

avalues = c(.1,.05,.01,".05, Post-hoc power")
my.labs <- list(bquote(alpha==.(avalues[1])),bquote(alpha==.(avalues[2])),bquote(alpha==.(avalues[3])),bquote(alpha==.(avalues[4])))

p <- ggplot(plot.dat, aes(x=Observations,y=vals,color=group,group=group)) 
p <- p + geom_line(size=.8) + scale_color_discrete(name = "",labels=my.labs) + theme_bw(base_size = 12, base_family = "Helvetica")
p <- p + ylab("Type I Error") + ggtitle("") + xlab("Observations") + scale_y_continuous(limits=c(0,1),breaks=c(.05,.2,.5,.75,1))
p <- p + theme(panel.grid.major = element_line(size = .5, color = "grey"),axis.line = element_line(size=.7, color = "black"),legend.position = c(.75,.4),text = element_text(size=14))
p
Sys.sleep(2)
ggsave(file="continuous_monitoring_fixed_sims.png",height=7,width=14)
