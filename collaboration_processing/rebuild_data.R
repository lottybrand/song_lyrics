library(tidyverse)
library(stringr)

billboard_artists <- read_csv("colls_billboard_format.csv")

billboard_data <- read_csv("../../output/data/billboard.csv") %>%
  mutate( ID_index=1:5100)
billboard_clustered <- read_csv("../data/billboard-authors-clustered.csv")
billboard_data$artist <- billboard_clustered$artist
billboard_data$artist <- str_replace(billboard_data$artist, "  ", " & ")

x <- inner_join(billboard_artists, billboard_data) %>%
  arrange(desc(-year), rank)

write_csv(x, "billboard_rebuilt.csv")

mxm_artists <- read_csv("colls_mxm_format.csv")

mxm_data <- read_csv("../../output/data/mxm.csv")
mxm_clustered <- read_csv("../data/xm.csv")
mxm_data$artist <- mxm_clustered$artist_ASCII

x <- inner_join(mxm_artists, mxm_data) 
write_csv(x,"mxm_rebuilt.csv")