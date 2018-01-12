###############
## code for vizualizing some of the National Observer Program data
###############

library(tidyverse)

nop_sum <- read_csv("NOP_funding and observer information by region and year.csv")
nop_sum$Year = as.factor(nop_sum$Year)

# funding by region stacked bar graph
f_by_r <- ggplot(nop_sum, aes(Year, Funding)) +
          geom_col(aes(fill = Region)) +
          ylab("Funding (USD)") +
          theme_bw()
f_by_r
ggsave("Preliminary figures/funding by region.pdf", f_by_r)

# Observers by region stacked bar graph
o_by_r <- ggplot(nop_sum, aes(Year, Observers)) +
  geom_col(aes(fill = Region)) +
  ylab("Total Observers") +
  theme_bw()
o_by_r
ggsave("Preliminary figures/observers by region.pdf", o_by_r)

# At-sea Days by region stacked bar graph
asd_by_r <- ggplot(nop_sum, aes(Year, `Actual Sea Days`)) +
  geom_col(aes(fill = Region)) +
  ylab("At-sea Days") +
  theme_bw()
asd_by_r
ggsave("Preliminary figures/at sea days by region.pdf", asd_by_r)
