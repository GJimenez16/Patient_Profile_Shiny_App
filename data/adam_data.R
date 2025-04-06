# Random adam datasets
data <- list(
  adsl = random.cdisc.data::cadsl,
  adae = random.cdisc.data::cadae,
  adlb = random.cdisc.data::cadlb %>% 
    mutate(LBTEST = stringr::str_remove(data$adlb$LBTEST,"Measurement")),
  advs = random.cdisc.data::cadvs
)
