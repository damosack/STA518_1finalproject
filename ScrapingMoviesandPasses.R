library(tidyverse)
library(lubridate)
library(rvest)
library(TMDb)
library(progress)


#assigning link and read link to objects for ease of use

bmlink <- "https://bechdeltest.com/?list=all"
bmpage <- read_html(bmlink)

#grabbing all movie titles and assigning to object

bechdelmovies <- bmpage %>% 
 html_elements(".movie a:nth-child(2)") %>% 
 html_text()


#grabbing ratings and assigning to object
#if movie == pass, rating == 1, else rating == 0;

bechdelratings <- bmpage %>%
 html_elements(".movie a:nth-child(1) img") %>%
 html_attr("src") %>%
 str_detect(., "nopass") %>% 
 as.numeric()

bechdelratings <- ifelse(bechdelratings == 0, 1, 0)

#assembling vectors for movies and ratings into tibble

MoviesTibble <- bind_rows(list(Movies = bechdelmovies,
                               "Pass (0 = Fail)" = bechdelratings))


#assigning my personal api_key to object of same name
api_key <- "cb02ca4d50e794825db1b7e261b553dd"


#creating a function to perform the same as TMDB::search_movie(), 
#but to return NA if an error is encountered or if no results from search

pb <- progress_estimated(length(MoviesTibble$Movies))
search_movie_function <- function(api_key, query, page) {
  pb$tick()$print()
  res <- try(search_movie(api_key, query, page),
                silent= TRUE) 
     if(class(res) == "try-error" || length(res$results) == 0) {
    nope <- tibble("title" = NA, "ID" = NA)
       return(nope)
} else {
    res$results %>% 
      as_tibble() %>%
      slice(1) %>%
      select("title" = title, "ID" = id)
    }
}



#using the functions to search the first 10 movies in MoviesTibble
#for ID and Title

queries100 <- map_df(MoviesTibble$Movies[1:100], 
                 ~search_movie_function(
                   api_key, query=., page=1))

#using the function to go through all titles in MoviesTibble$Movies

queries <- map_df(MoviesTibble$Movies, 
                  ~search_movie_function(
                    api_key, query=., page=1))

#adding TMDB query results to MoviesTibble and filtering where title is the 
#same for both bechdeltest.com and TMDB query results
#this removes all movies where the search_movie_function
#returned an error, or where the movie was likely
#not the same as the one on bechdelmovies.com

FullTibble <- (bind_rows(list(BMovieTitle = bechdelmovies, 
                          "Pass (0 = Fail)" = bechdelratings, 
                          TMDBtitle = queries$title, 
                         TMDBID = queries$ID))) %>%
 filter(BMovieTitle == TMDBtitle)

#writing FullTibble to CSV file to save myself from
#needing to repeat the function

write_csv(FullTibble, "Full.csv")


FullTibble <- (read_csv("Full.csv"))
FullTibble[1964, ]
ModTibble <- FullTibble[-c(1964), ]

#noticed that while testing my function below that even with a try-error catch
#entry FullTibble[1964, ] broke the function. Turns out this is a show
#not a movie so I removed point from data. Unclear why try-catch failed

allmovieinfo100base <- map(FullTibble$TMDBID[1], ~movie(api_key, id= . ))


#writing a modified movie function that gives progress, 
#and selects relevant variables, but otherwise performs similarly to movie()
#also making it so that if genres or runtime are empty vectors the function 
#returns NA values instead of breaking and halting progress

pg <- progress_bar$new(
  format = " downloading [:bar] :percent eta: :eta",
  total=7187, clear = F, width = 60)
 movie_att_function <- function(api_key, id) {
  pg$tick()
  r <- movie(api_key, id)
   if(length(r$genres) == 0 || length(r$runtime) == 0) {
    nope <- tibble("Title" = r$original_title,
                   "ID" = r$id,
                   "Budget" = NA,
                   "Language" = NA,
                   "Popularity" = NA,
                   "Year" = NA,
                   "Revenue" = NA,
                   "Runtime" = NA,
                   "Rating" = NA,
                   "Votes" = NA,
                   "Primary_Genre" = NA)
    return(nope)
      
    } else {
      
    return(tibble("Title" = r$original_title,
                    "ID"= r$id,
                    "Budget" = r$budget,
                    "Language" = r$original_language,
                    "Popularity" = r$popularity,
                    "Year" = r$release_date,
                    "Revenue" = r$revenue,
                    "Runtime" = r$runtime, 
                    "Rating" = r$vote_average, 
                    "Votes" = r$vote_count, 
                    "Primary_Genre" = r$genres$name[1]))
      
    }
  }


#testing my function on first 100 entries in FullTibble
#progress bar doesn't really work bc the function is too quick here

allmovieinfo100 <- map_df(ModTibble$TMDBID[1964],
                         ~movie_att_function(api_key, id=.))

ami <- map_df(ModTibble$TMDBID, 
                      ~movie_att_function(api_key, id= . ))

names(ModTibble) <- str_replace_all(names(ModTibble), " ", "_")
names(ModTibble) <- str_replace(names(ModTibble), "\\(", "_")
names(ModTibble) <- str_replace(names(ModTibble), "\\)", "_")
names(ModTibble) <- str_replace(names(ModTibble), "=", "")


MovieData1 <- (bind_rows(list(title = ModTibble$BMovieTitle, 
                          BT_score.1_is_Pass = ModTibble$Pass__0__Fail_, 
                           language = ami$Language,
                           year = ami$Year,
                           budget = ami$Budget,
                           revenue = ami$Revenue,
                           runtime = ami$Runtime,
                           popularity = ami$Popularity,
                           TMDB_rating = ami$Rating,
                           votes = ami$Votes,
                           primary_genre = ami$Primary_Genre,
                           TMDB_ID = ModTibble$TMDBID)))
 
write_csv(MovieData1, "MovieData1.csv")

MovieData1 <- read_csv("MovieData1.csv")

MovieData <- MovieData1 %>%
    filter(primary_genre != "Documentary" & primary_genre != "Music") %>% 
    filter(revenue != 0) %>%
    filter(budget != 0) %>%
    mutate(bechdel_pass = ifelse(BT_score.1_is_Pass == 0, "fail", "pass")) %>%
    mutate(spoken_language = language) %>%
    mutate(language = ifelse(language == 'en', 'English Language', 'Foreign Language')) %>%
    mutate(year = year(year)) %>%
    mutate(primary_genre = str_replace_all(primary_genre, "Science Fiction", "SciFi")) %>%
    mutate(logbudget = log(budget), logrevenue = log(revenue)) %>%
    select("title", "TMDB_ID", "bechdel_pass",
           "budget", "revenue", "primary_genre",
           "popularity", "year", "language",
           "runtime","TMDB_rating", "votes",
           "spoken_language", "logbudget", "logrevenue")
write_csv(MovieData, "MovieData.csv")

















