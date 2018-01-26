###############
## code for vizualizing some of the National Observer Program data
###############

library(tidyverse)

nop_sum <- read_csv("NOP_funding and observer information by region and year.csv")
nop_sum$Year = as.factor(nop_sum$Year)


nop_sum_10_13 <- filter(nop_sum, Year %in%  c("2010", "2011", "2012", "2013"))

# funding by region stacked bar graph
f_by_r <- ggplot(nop_sum_10_13, aes(Year, Funding)) +
          geom_col(aes(fill = Region)) +
          ylab("Funding (USD)") +
          theme_bw()
f_by_r
ggsave("Preliminary figures/funding by region 2010-2013.pdf", f_by_r)

# Observers by region stacked bar graph
o_by_r <- ggplot(nop_sum_10_13, aes(Year, Observers)) +
  geom_col(aes(fill = Region)) +
  ylab("Total Observers") +
  theme_bw()
o_by_r
ggsave("Preliminary figures/observers by region 2010-2013.pdf", o_by_r)

# At-sea Days by region stacked bar graph
asd_by_r <- ggplot(nop_sum_10_13, aes(Year, `Actual Sea Days`)) +
  geom_col(aes(fill = Region)) +
  ylab("At-sea Days") +
  theme_bw()
asd_by_r
ggsave("Preliminary figures/at sea days by region 2010-2013.pdf", asd_by_r)



# funding by region stacked bar graph
f_by_r <- ggplot(nop_sum, aes(Year, Funding)) +
  geom_col(aes(fill = Region)) +
  ylab("Funding (USD)") +
  theme_bw()
f_by_r
ggsave("Preliminary figures/funding by region 2010-2016.pdf", f_by_r)

# Observers by region stacked bar graph
o_by_r <- ggplot(nop_sum, aes(Year, Observers)) +
  geom_col(aes(fill = Region)) +
  ylab("Total Observers") +
  theme_bw()
o_by_r
ggsave("Preliminary figures/observers by region 2010-2016.pdf", o_by_r)

# At-sea Days by region stacked bar graph
asd_by_r <- ggplot(nop_sum, aes(Year, `Actual Sea Days`)) +
  geom_col(aes(fill = Region)) +
  ylab("At-sea Days") +
  theme_bw()
asd_by_r
ggsave("Preliminary figures/at sea days by region 2010-2016.pdf", asd_by_r)
