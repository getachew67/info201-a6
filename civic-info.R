source("api-keys.R")
library(jsonlite)
library(dplyr)
library(knitr)
library(httr)

#base uri
base_civic <- "https://www.googleapis.com/civicinfo/v2"

#endpoint component
end_comp_civic <- "/representatives"

#query parameter
my_address <- "11619 SE Aerie Crescent Road, Happy Valley, OR 97086"

#endpoint
civic_uri <- paste0(base_civic, end_comp_civic)

#query list
query_params <- list("key" = api_civics, "address" = my_address)

#GET request and json conversion
civ_response <- GET(civic_uri, query = query_params)
civ_content <- content(civ_response, type = "text")
civ_results <- fromJSON(civ_content)

#parsing
or_officials <- civ_results$officials
or_offices <- civ_results$offices

#removing discrepancy in table
or_officials_v2 <- or_officials[-4, ]
rownames(or_officials_v2) <- 1:nrow(or_officials_v2)
#combine person with attributes
or_rep_df <- bind_cols(or_officials_v2, or_offices)

#replace NULL values w/in "emails" column to a string
or_rep_df$emails <- as.character(or_rep_df$emails)
or_rep_df$emails[or_rep_df$emails == "NULL"] <- "Not Available"
or_rep_df$emails <- as.factor(or_rep_df$emails)

#changing column names
or_rep_df_v2 <- or_rep_df %>%
  select(name, name1, party, emails, phones, photoUrl, urls) %>%
  rename(Name = name, Position = name1, Party = party, Email = emails,
         Phone = phones, Photo = photoUrl)

#changing photo url
or_rep_v3 <- flatten(or_rep_df_v2)
or_rep_v4 <- or_rep_v3 %>%
  mutate(Photo = ifelse(
    is.na(Photo), "Not Available", paste0("![](", Photo, ")")
  )) %>%
  mutate(Name = paste0("[", Name, "](", urls, ")"))

or_rep_v5 <- or_rep_v4 %>%
  select(Name, Position, Party, Email, Phone, Photo)