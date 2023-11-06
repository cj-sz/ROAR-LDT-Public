library(mirt)
library(dplyr)
library(ggplot2)
# load a response matrix where participants are sorted in ascending order of 
# ability and items are sorted in descending order of difficulty
df <- read.csv('https://raw.githubusercontent.com/yeatmanlab/ROAR-LDT-Public/main/Study2/data/LDT_alldata_wide_sorted_v2_newcodes.csv')

# Fit Rasch model to all data
m1 <- mirt(select(df, -subj), itemtype = 'Rasch', guess=0.5)
m1.coef <- as.data.frame(coef(m1,simplify=TRUE, IRTpars = TRUE))
f1 <- as.data.frame(fscores(m1))

# Make a new dataset to simulate early stopping where we NaN the n hardeds items
# for the n lowest ability participants
for (n in seq(1,120,10)) {
  df.es <- df
  df.es[1:round(n/2),1:n] <- NaN
  m1.es <- mirt(select(df.es, -subj), itemtype = 'Rasch', guess=0.5)
  m1.coef.es <- as.data.frame(coef(m1.es,simplify=TRUE, IRTpars = TRUE))
  # Merge early stopping item estimates with full estimates
  m1.coef.es <- rename(m1.coef.es,items.b.es = items.b)
  m1.coef <- mutate(m1.coef,items.b.es=m1.coef.es$items.b.es)
  
  g <- ggplot(m1.coef,aes(x=items.b, y=items.b.es)) +
    geom_point()+
    scale_x_continuous(limits=c(-4,6)) +
    scale_y_continuous(limits=c(-4,6)) +
    xlab('Full data model') +
    ylab(sprintf('%d hardest items NaNed for %d lowest theta',n, round(n/2))) +
    geom_abline(intercept=0,slope=1, color='red')
  print(g)
  ggsave(sprintf('Early stopping %d.pdf',n),g)

}

