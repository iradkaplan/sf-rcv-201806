library("rcv", lib.loc="~/R/win-library/3.5")

library("readr", lib.loc="~/R/win-library/3.5")

raw_ballot <- read_tsv(file="20180605_4_ballotimage.txt",col_names=FALSE)
raw_lookup <- read_tsv(file="20180605_4_masterlookup.txt", col_names=FALSE))

cleaned <- clean_ballot(ballot = raw_ballot, b_header = T, 
                        lookup = raw_lookup, l_header = T, 
                        format = "WinEDS")

results <- rcv_tally(cleaned, "Mayor")

knitr::kable(results)

d3_mayor <- rcv::make_d3list(results = results)
networkD3::sankeyNetwork(Links = d3_mayor$values, Nodes = d3_mayor$names,
                         Source = "source", Target = "target",
                         Value = "value", NodeID = "candidate", units = "voters",
                         fontSize = 12, nodeWidth = 20)