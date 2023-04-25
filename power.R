library(pwr)
library(tidyverse)

samples <- 472

d <- tibble(freq = seq(from=0, to=0.025, by=0.0001)) %>%
  mutate(prob_at_least_one_seen = 1-pbinom(0, samples, prob=freq))

ggplot(d, aes(x=freq, y=prob_at_least_one_seen)) + 
  geom_line() +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  labs(title=paste0('Probability of observing at least one positive sample given ', samples, ' deer sampled as a function of variable underlying prevalence'),
       x='Potential underlying prevalence',
       y='Probability of detecting at least one positive sample')
ggsave('power.pdf', height=8, width=12)


d %>% filter(abs(prob_at_least_one_seen-0.8) < 0.01 ) # freq=0.0034
d %>% filter(abs(prob_at_least_one_seen-0.95) < 0.01 ) # freq=0.0064
d %>% filter(abs(prob_at_least_one_seen-0.99) < 0.001 ) # freq=0.0097
d %>% filter(abs(prob_at_least_one_seen-0.999) < 0.001 ) # freq=0.0137

## double check this works the way i think it does...
# coin flip (p=0.5): flip twice, p(successes <=0) = P(0,0) = 0.5*0.5=0.25; p(successes <=1) = P(0,0) + P(0,1) + P(1,0) = 3*0.25 = 0.75
pbinom(0, 2, prob=0.5)
pbinom(1, 2, prob=0.5)
pbinom(2, 2, prob=0.5)
# biased coin flip (p=0.6): flip twice, p(successes <=0) = P(0,0) = 0.4*0.4=0.16; p(successes <=1) = P(0,0) + P(0,1) + P(1,0) = 0.16 + (0.4*0.6) * 2 = 0.64
pbinom(0, 2, prob=0.6)
all.equal(pbinom(1, 2, prob=0.6),  dbinom(0, 2, prob=0.6) + dbinom(1, 2, prob=0.6))
pbinom(2, 2, prob=0.6)


nearest <- function(to_filter, filter_vals) {
  ## for each val in filter vals, make entry True in mask for first instance where to_filter exceeds val
  ## assumes already sorted
  seq_along(to_filter) %in% lapply(filter_vals, function(val) which.max(to_filter+0.001 >= val)) ## tolerance as 0.8 was matching to 0.81
}
nearest(seq(from=0, to=1, by=0.1), c(0.3, 0.5))


label_vals <- c(0.8, 0.95, 0.99)
ggplot(d, aes(x=freq, y=prob_at_least_one_seen)) + 
  geom_line() +
  geom_segment(aes(x=freq, y=prob_at_least_one_seen, xend=-Inf, yend=prob_at_least_one_seen), d %>% filter(nearest(prob_at_least_one_seen, label_vals)), lty=2, color='gray50') +
  geom_segment(aes(x=freq, y=prob_at_least_one_seen, xend=freq, yend=-Inf), d %>% filter(nearest(prob_at_least_one_seen, label_vals)), lty=2, color='gray50') +
  geom_text(aes(x=0, y=prob_at_least_one_seen, label=round(prob_at_least_one_seen, 2)), d %>% filter(nearest(prob_at_least_one_seen, label_vals)), vjust=1) +
  geom_text(aes(x=freq, y=0, label=scales::percent(freq)), d %>% filter(nearest(prob_at_least_one_seen, label_vals)), vjust=0) +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  labs(title=paste0('Probability of observing at least one positive sample given ', samples, ' deer sampled as a function of variable underlying prevalence'),
       x='Potential underlying prevalence',
       y='Probability of detecting at least one positive sample')
ggsave('power_labeled.pdf', height=8, width=12)
