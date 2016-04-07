##

coin_lik <- function(x, n) {
  require(dplyr)
  dat <- dplyr::data_frame(p=seq(0, 1, by=.01), lik=dbinom(x, size=n, p=p))
  p2 <- ggplot(dat, aes(x=p, y=lik)) + 
    geom_line() + 
    geom_hline(yintercept=1/8*max(dat$lik), linetype=2) +
    ggtitle(paste('N =', n, ', X =', x)) + 
    ylab('likelihood')
  (p2)
}