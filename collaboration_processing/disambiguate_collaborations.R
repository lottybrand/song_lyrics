library(tidyverse)
library(stringr)

# LOAD ARTISTS:

mxm <- read_csv("../data/xm.csv") 
# artists are already processed by replacing unicode and by put all letters to
# lower, see this code:

#artistClusterXM <- read.csv("mxm-authors-clustered.csv")

#artist_ASCII <- c()
#for( i in 1:dim(artistClusterXM)[1]){
#  artist_ASCII[i] <- stringi::stri_unescape_unicode(artistClusterXM[i,])
#  print (i)
#}
#xm <- (data.frame(artist_ASCII))
#xm$artist_ASCII <- tolower(xm$artist_ASCII)
#write.csv(xm, file="xm.csv")

billboard <- read_csv("../data/billboard-authors-clustered.csv") 

#replace all double spaces with &
billboard$artist <- str_replace(billboard$artist, "  ", " & ")

# separators:
seps <-" featuring | feat. | feat | and | AND | & | with |,"

possible_colls_mxm <- str_subset(mxm$artist_ASCII, seps)
not_colls_mxm <- mxm$artist_ASCII[!mxm$artist_ASCII %in% possible_colls_mxm]

possible_colls_billboard <- str_subset(billboard$artist, seps)
not_colls_billboard <- billboard$artist[!billboard$artist %in% possible_colls_billboard]

unique_artists <- unique(c(not_colls_mxm, not_colls_billboard))


# BILLBOARD:
artist_out <- c()
artist_processed <- c()
# artist_status <- c() 

for( i in unique(billboard$artist) ){
  if( i %in% possible_colls_billboard ){
    aSplit <- strsplit(toString(i), seps)
    cArtists <- trimws(aSplit[[1]])
    if(sum(cArtists %in% unique_artists) > 0) {
    # if none of the artists is present in the list, it is a 
    # composite name of a single artist, otherwise
    # we just keep the artists we found in the list:
      for( j in cArtists){
        artist_out <- c(artist_out, i )
        artist_processed <- c(artist_processed, j)
     #   artist_status <- c(artist_status, j %in% unique_artists )
      }
    }  
    else{
      artist_out <- c(artist_out, i )
      artist_processed <- c(artist_processed, i)
   #   artist_status <- c(artist_status, TRUE)
    }
  }  
  else{
    artist_out <- c(artist_out, i )
    artist_processed <- c(artist_processed, i)
  #  artist_status <- c(artist_status, TRUE)
  }
}

colls_billboard_format <- data_frame(artist = artist_out, artist_processed = artist_processed)
                           #          ,artist_status = artist_status)
write_csv(colls_billboard_format, "colls_billboard_status.csv")

# MXM:
artist_out <- c()
artist_processed <- c()

for( i in unique(mxm$artist_ASCII)){
  if( i %in% possible_colls_mxm ){
    aSplit <- strsplit(toString(i), seps)
    cArtists <- trimws(aSplit[[1]])
    if(sum(cArtists %in% unique_artists) > 0) {
      # if none of the artists is present in the list, it is a 
      # composite name of a single artist, otherwise
      # we just keep the artists we found in the list:
      for( j in cArtists){
        artist_out <- c(artist_out, i )
        artist_processed <- c(artist_processed, j)
      }
    }  
    else{
      artist_out <- c(artist_out, i )
      artist_processed <- c(artist_processed, i)
    }
  }  
  else{
    artist_out <- c(artist_out, i )
    artist_processed <- c(artist_processed, i)
  }
}
colls_mxm_format <- data_frame(artist = artist_out, artist_processed = artist_processed)
write_csv(colls_mxm_format, "colls_mxm_format.csv")



##########################################################################
#########################################################################
### Idea for data organising: 

# artists <- c("drake","drake","drake","drake", "lil wayne", "beyonce", "beyonce","pearl jam", "metallica")
# song_title <- c("money", "cars", "status","bling","money","money","runTheWorld","garden","sandman" )
# lyrics <- c("aaaa", "bbbb","cccc" , "dddd","aaaa","aaaa","eeee","ffff","gggg")
# emotion <- c(187,345,560,607,156,187,156,198,713)
# genre <- c("hiphop","hiphop","hiphop","hiphop","hiphop","hiphop","pop","rock","metal")
# 
# LyricData <- data.frame(artists,song_title,lyrics,emotion,genre)
