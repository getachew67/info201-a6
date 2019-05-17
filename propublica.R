#propublica
source("api-keys.R")
library(jsonlite)
library(dplyr)
library(knitr)
library(httr)
library(ggplot2)
library(lintr)
base_pub <- "https://api.propublica.org/congress/v1"
end_comp_pub <- "/members"
end_point_pub <- paste0(base_pub, end_comp_pub,
  "/house/",
  "OR", "/current.json")

response <- GET(end_point_pub, add_headers("X-API-KEY" = api_propublica))
body_pub <- content(response, "text")
results_pub <- fromJSON(body_pub)

flat_pub <- flatten(results_pub$results)

#gender plot

gender_stats <- flat_pub %>%
  group_by(gender) %>%
  tally()

gender_plot <- ggplot(gender_stats) +
  geom_col(mapping = aes(
    x = gender,
    y = n
  )) + coord_flip() +
  labs(
    y = "Number of Representatives",
    x = "Gender"
  )

#party plot
party_stats <- flat_pub %>%
  group_by(party) %>%
  tally()

party_plot <- ggplot(party_stats) +
  geom_col(mapping = aes(
    x = party,
    y = n
  )) +
  coord_flip() +
  labs(
    y = "Representatives by Party",
    x = "Party"
  )

#Last section
my_rep_uri <- paste0(base_pub, end_comp_pub, "/B001278.json")
my_response <- GET(my_rep_uri, add_headers("X-API-KEY" = api_propublica))
my_body <- content(my_response, "text")
my_results <- fromJSON(my_body)
my_flat <- flatten(my_results$results)

today <- as.Date(Sys.Date())
birthday <- as.Date(my_flat$date_of_birth)
rep_age <- round(as.numeric(today - birthday) / 365)

username <- my_flat$twitter_account
twitter <- paste0("https://twitter.com/", username)

#votes
vote_uri <- paste0(base_pub, end_comp_pub, "/B001278/votes.json")
vote_response <- GET(vote_uri, add_headers("X-API-KEY" = api_propublica))
vote_body <- content(vote_response, "text")
vote_results <- fromJSON(vote_body)
vote_flat <- flatten(vote_results$results)
vote_flat_v2 <- unlist(vote_flat)
vote_flat_v3 <- data.frame(vote_flat_v2)

vote_yes <- vote_flat_v3 %>%
  filter(vote_flat_v3 == "Yes") %>%
  nrow()
vote_no <- vote_flat_v3 %>%
  filter(vote_flat_v3 == "No") %>%
  nrow()

vote_total <- vote_yes + vote_no
vote_percent <- (vote_yes / vote_total) * 100
