library("rcv")
library("readr")
library(tidyverse)
library("rjson")
library('scales')

input_ballot <- "data/20180612_ballotimage.txt"
input_lookup <- "data/20180612_masterlookup.txt"

raw_ballot <- read_tsv(file=input_ballot, col_names=FALSE)
raw_lookup <- read_tsv(file=input_lookup, col_names=FALSE)

cleaned <- clean_ballot(ballot = raw_ballot, b_header = T, 
                        lookup = raw_lookup, l_header = T, 
                        format = "WinEDS")
cleaned

can <- fromJSON(file = "cvr2/CandidateManifest.json")
candidates <- tibble(id = integer(), candidate = character())
for (item in can$List) {
  candidates <- add_row(candidates, 
                        id=item$Id,
                        # contest_id=item$ContestId,
                        candidate=item$Description )
}

con <- fromJSON(file = "cvr2/ContestManifest.json")
contests <- tibble(id = integer(), contest = character(), num_of_ranks = integer())
for (item in con$List) {
  contests <- add_row(contests, 
                      id=item$Id, 
                      contest=item$Description, 
                      num_of_ranks=item$NumOfRanks)
}
as_tibble(contests$List[[1]])
as_tibble(candidates$List[[1]])

# columns needed:
#   contest (name)
#   candidate (name)
#   pref_voter_id
#   vote_rank

cvr = fromJSON(file="cvr2/CvrExport.json")
# cleaned <- tibble(pref_voter_id = character(), contest = character(), candidate = character(), vote_rank = integer())
cleaned <- tibble(pref_voter_id = character(), contest_id = integer(), candidate_id = integer(), vote_rank = integer())
# old_cleaned <- cleaned
# cleaned <- as_tibble(cvr$Sessions[[1]]$Original)
total <- length(cvr$Sessions)
for (i in seq_along(cvr$Sessions)) {
# for (i in 1:1000) {
  session <-cvr$Sessions[[i]]
  # voter_id <- session$RecordId
  if ("Modified" %in% names(session)) {
    # print("using modified ballot")
    ballot <- session$Modified
  } else {
    ballot <- session$Original
  }
  for (contest in ballot$Contests) {
    # contest_id <- contest$Id
    # contest_name <- filter(contests, id == contest_id) %>% pull(name)
    if(contest$Id==339){
      for (mark in contest$Marks) {
        if (!mark$IsAmbiguous) {
          # candidate_name <- filter(candidates, id == mark$CandidateId) %>% pull(name)
          cleaned <- add_row(cleaned, pref_voter_id = i, contest_id = contest$Id, candidate_id = mark$CandidateId, vote_rank = mark$Rank)
          # cleaned <- add_row(cleaned, pref_voter_id = voter_id, contest = contest_name, candidate = candidate_name, vote_rank = mark$Rank)
        }
      }
    }
  }
  print(paste("Current progress: ", percent(i/total)))
}

test <- tibble(
  pref_voter_id = paste(cvr$Sessions), 
  contest_id = integer(), 
  candidate_id = integer(), 
  vote_rank = integer()
  )

# characterize
cleaned <- dplyr::left_join(cleaned, candidates, by = c("candidate_id" = "id")) %>%
  dplyr::left_join(contests, by = c("contest_id" = "id")) %>%
  dplyr::select(contest,
                pref_voter_id,
                vote_rank,
                candidate)

# cleaned_d5 <- cleaned

# num_higher_ranked_votes <- 
# dplyr::filter(cleaned, contest=="MAYOR") %>% dplyr::group_by(pref_voter_id) %>% count()

# rlj <- dplyr::filter(cleaned, candidate=="ROBERT L. JORDAN, JR.") %>% dplyr::select(pref_voter_id)

# for row in table
#   if nextrank != currentrank + 1 and nextvoter==thisvoter 
#     print (currentrank) 
#     print (nextrank)
#     print("---")
#   else if nextvoter != thisvoter
#     reset to 1

current_contest = "DISTRICT ATTORNEY"

rank <- 1
m<-dplyr::filter(cleaned, contest==current_contest)
for (row in 1:(nrow(m)-1)) {
  if (m$pref_voter_id[row+1] == m$pref_voter_id[row]
      && (
        !is.na(m$candidate[row]) &&
        m$vote_rank[row+1] != m$vote_rank[row] + 1
        || (
          is.na(m$candidate[row]) && !is.na(m$candidate[row+1])
        )
      )
  ) {
    print(m$vote_rank[row])
    print(m$vote_rank[row+1])
    print("---")
    m$candidate[row+1]=NA
    if(m$vote_rank[row] == m$vote_rank[row+1]) {m$candidate[row+1]=NA}
  } else if (m$pref_voter_id[row+1] != m$pref_voter_id[row]) {
    rank <- 1 # reset for the next voter
  } else {
    rank <- rank + 1 # increment for the next vote
  }
}

results <- rcv_tally(cleaned, current_contest)
results

knitr::kable(results)

old_results <- results

results

d3_mayor <- rcv::make_d3list(results = results)
networkD3::sankeyNetwork(Links = d3_mayor$values, Nodes = d3_mayor$names,
                         Source = "source", Target = "target",
                         Value = "value", NodeID = "candidate", units = "voters",
                         fontSize = 12, nodeWidth = 20)
