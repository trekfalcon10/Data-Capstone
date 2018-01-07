library(dplyr)
library(tidyr)
library(dummies)
library(ggplot2)
library(reshape2)
library(dmm)
library(pROC)

#Load for Mac
acad_awd <- read.csv("/Volumes/Backup_Chris/Dropbox/R_Projects/Movies/pictures.csv", stringsAsFactors = FALSE)
#Load for PC
#acad_awd <- read.csv("C:/Users/Christopher/Dropbox/R_Projects/Movies/pictures.csv", stringsAsFactors = FALSE)

View(acad_awd)

#Load for Mac
imdb <- read.csv("/Volumes/Backup_Chris/Dropbox/R_Projects/Movies/imdb_metadata.csv", stringsAsFactors = FALSE)
#Load for PC
#imdb <- read.csv("C:/Users/Christopher/Dropbox/R_Projects/Movies/imdb_metadata.csv", stringsAsFactors = FALSE)

View(imdb)

#Examine data
str(imdb)
str(acad_awd)

#Check range of years for imdb and acad_awd
min(imdb$title_year, na.rm = TRUE)
max(imdb$title_year, na.rm = TRUE)

min(acad_awd$year, na.rm = TRUE)
max(acad_awd$year, na.rm = TRUE)

#Rename movie_title column to match name in academy awards set
imdb <- rename(imdb, name = movie_title)

#Rename title_year column to match year in academy awards set
imdb <- rename(imdb, year = title_year)

#Rename imdb_score column to match rating in academy awards set
imdb <- rename(imdb, rating = imdb_score)

#Reorder titles to front for ease of viewing
imdb <- imdb %>% select(name, everything())

#Need to trim whitespace and remove question marks from titles in imdb set
imdb$name <- trimws(imdb$name)

#Remove question marks at the end of the names in imdb
imdb$name <- substr(imdb$name, 1, nchar(imdb$name)-1)


#Need to remove blank space from titles in acad_awd set
acad_awd$name <- trimws(acad_awd$name)

#Put movie titles in all-caps fr consistency
imdb$name <- toupper(imdb$name)
acad_awd$name <- toupper(acad_awd$name)

#Modify to get consistent title for Birdman in both datasets
imdb$name[695] <- "BIRDMAN"

#Change "-" to 1 in acad_awd nominations
acad_awd$nominations <- as.integer(acad_awd$nominations)
acad_awd[is.na(acad_awd$nominations), 3] <- 1

#Drop unneeded columns from acad_awd
drop_acad <- c("year", "rating", "duration", "genre1", "genre2", "release", "metacritic", "synopsis")
merge_acad_awd <- acad_awd[, !names(acad_awd) %in% drop_acad]

View(merge_acad_awd)

#Find column names in imdb that are na
colnames(imdb)[colSums(is.na(imdb)) > 0]  

#Impute mean to unknown budget and unknown gross, first on test set
money <- imdb[, c("budget", "gross")]

#Resolve na budget to mean budget
na_budget <- subset(money, is.na(money$budget))
na_budget <- as.data.frame(na_budget)
budget <- subset(money, !is.na(money$budget))
budget <- as.data.frame(budget)

na_budget$budget <- mean(budget$budget)

#Resolve na gross to mean gross
na_gross <- subset(money, is.na(money$gross))
gross <- subset(money, !is.na(money$gross))

na_gross$gross <- mean(gross$gross)

#Replace na values in money
money[which(is.na(money$budget)), 1] <- na_budget$budget
money[which(is.na(money$gross)), 2] <- na_gross$gross

#Apply budget and gross mean replacements to imdb set
imdb$budget <- money$budget
imdb$gross <- money$gross

#Apply average duration to NA durations

duration <- imdb[, c("name", "duration")]
na_duration <- duration[is.na(duration$duration), ]
num_duration <- duration[!is.na(duration$duration), ]

na_duration$duration <- round(mean(num_duration$duration))

duration[is.na(duration$duration), 2] <- na_duration$duration

imdb$duration <- duration$duration

#Test for duplicated names
imdb$name[duplicated(imdb$name)]

#Create test set of duplicated names from imdb
dupnames <- imdb[duplicated(imdb$name) | duplicated(imdb$name, fromLast = TRUE), ]

View(dupnames)


#Get distinct names only 
dupnames <- distinct(dupnames, name, .keep_all = TRUE)

View(dupnames)

duplicated(dupnames$name)

#Extract distinct movies names
imdb <- distinct(imdb, name, .keep_all = TRUE)

#Retest for duplicated names
imdb$name[duplicated(imdb$name)]

#Remove TV shows by subsetting to duration greater than 60 minutes
imdb <- subset(imdb, imdb$duration > 60)

#Remove blank names from imdb set
imdb <- subset(imdb, imdb$name != "")

View(imdb)

#Removed non-Academy Award nominated version of Ben-Hur
imdb <- subset(imdb, as.numeric(rownames(imdb)) != 1257)

View(imdb)

#Join imdb and acad_awd datasets
movies <- full_join(merge_acad_awd, imdb, by = "name")

View(movies)

#Get names of movies from acad_awd that are not in imdb that yield only na's for all other data 
#values post-merge

#Define negation of %in% 
`%!in%` = Negate(`%in%`)

#Get movie titles in acad_awd not in imdb
acad_awd$name[which(acad_awd$name %!in% imdb$name)]

#Get movie titles in acad_awd that are in imdb
acad_awd$name[which(acad_awd$name %in% imdb$name)]

#Find and remove titles now in movies that are not in imdb
movies$name[which(movies$name %!in% imdb$name)]
movies <- movies[which(movies$name %in% imdb$name), ]  

#Convert genre 1 and genre 2 columns to binaries in acad_awd

#Create binaries based on genre1 and genre2 in acad_award to match those in imdb

binary1 <- dummy(acad_awd$genre1)
binary2 <- dummy(acad_awd$genre2)

#Change names in columns to get rid of genre labels
for(i in 1:ncol(binary1)){
  colnames(binary1)[i] <- substr(colnames(binary1)[i],nchar("genre1")+1, nchar(colnames(binary1)[i]))
}

for(j in 1:ncol(binary2)){
  colnames(binary2)[j] <- substr(colnames(binary2)[j],nchar("genre2")+1, nchar(colnames(binary2)[j]))
}

#Convert binary datasets to dataframes  
binary1 <- as.data.frame(binary1)
binary2 <- as.data.frame(binary2)


#Unite binaries into one binary set
binaries <- cbind(binary1, binary2)

#Remove column of blanks (V1)
binaries <- binaries[,-9]

#Add duplicate-named columns together to avoid overwriting (find better way if possible)
binaries$Biography <- binaries$Biography + binaries$Biography.1
binaries$Comedy <- binaries$Comedy + binaries$Comedy.1
binaries$Crime <- binaries$Crime + binaries$Crime.1
binaries$Drama <- binaries$Drama + binaries$Drama.1
binaries$Musical <- binaries$Musical + binaries$Musical.1
binaries$Western <- binaries$Western + binaries$Western.1

#Remove duplicate columns
drops <- c("Biography.1", "Comedy.1", "Crime.1", "Drama.1", "Musical.1", "Western.1")
binaries <- binaries[, !names(binaries) %in% drops]

View(binaries)

#Add binaries to acad_award and remove genre1 and genre2 columns
merge2_acad_awd <- cbind(acad_awd, binaries)

drop_acad <- c("genre1", "genre2")
merge2_acad_awd <- merge2_acad_awd[, !names(merge2_acad_awd) %in% drop_acad]

View(merge2_acad_awd)

#Get rid of other extraneous columns in merge2
drop_acad2 <- c("release", "metacritic", "synopsis")
merge2_acad_awd <- merge2_acad_awd[, !names(merge2_acad_awd) %in% drop_acad2]

View(merge2_acad_awd)

#Convert merge2 set into only those movies not in imdb
merge2_acad_awd <- merge2_acad_awd[which(merge2_acad_awd$name %!in% imdb$name), ]

View(merge2_acad_awd)

#Do a second merge of into movies of the merge2 movies as a full join
movies2 <- full_join(merge2_acad_awd, movies)

View(movies2)

movies <- movies2

View(movies)


#Convert "unsuccessful" movies in same range with NA's for nominations to 0
movie_years_noms <- movies[movies$year < 2015 & movies$year > 1926, ]

movie_years_noms$nominations[is.na(movie_years_noms$nominations)] <- 0

#Remove NA rows
movie_years_noms <- movie_years_noms[!is.na(movie_years_noms$name), ]

#Get rid of NA's in other Genre columns
movie_years_noms$`Film-Noir`[is.na(movie_years_noms$`Film-Noir`)] <- 0
movie_years_noms$Fantasy[is.na(movie_years_noms$Fantasy)] <- 0
movie_years_noms$Sci.Fi[is.na(movie_years_noms$Sci.Fi)] <- 0
movie_years_noms$Thriller[is.na(movie_years_noms$Thriller)] <- 0
movie_years_noms$Documentary[is.na(movie_years_noms$Documentary)] <- 0
movie_years_noms$Horror[is.na(movie_years_noms$Horror)] <- 0
movie_years_noms$Animation[is.na(movie_years_noms$Animation)] <- 0

#Impute column means for numeric NA's

#Impute mean to unknown budget and unknown gross on joined set
money2 <- movie_years_noms[, c("budget", "gross")]

#Resolve na budget2 to mean budget
na_budget2 <- subset(money2, is.na(money2$budget))
na_budget2 <- as.data.frame(na_budget2)
budget2 <- subset(money2, !is.na(money2$budget))
budget2 <- as.data.frame(budget2)

na_budget2$budget <- mean(budget2$budget)

#Resolve na gross2 to mean gross
na_gross2 <- subset(money2, is.na(money2$gross))
gross2 <- subset(money2, !is.na(money2$gross))

na_gross2$gross <- mean(gross2$gross)

#Replace na values in money2
money2[which(is.na(money2$budget)), 1] <- na_budget2$budget
money2[which(is.na(money2$gross)), 2] <- na_gross2$gross

#Apply budget and gross mean replacements to joined set
movie_years_noms$budget <- money2$budget
movie_years_noms$gross <- money2$gross

#Impute "unknown" for character NA's

#Impute unknowns to NA and blank cast and crew columns
cast_crew <- movie_years_noms[, c("director_name", "actor_2_name", "actor_1_name", "actor_3_name", 
                                  "plot_keywords", "language", "country")]

#Director NA's
na_cast1 <- subset(cast_crew, is.na(cast_crew$director_name))

#Yields no blanks
blank_cast1 <- subset(cast_crew, cast_crew$director_name == "")

na_cast1$director_name <- "Unknown"


movie_years_noms$director_name[is.na(movie_years_noms$director_name)] <- na_cast1$director_name

#Actor 2 NA's
na_cast2 <- subset(cast_crew, is.na(cast_crew$actor_2_name))
blank_cast2 <- subset(cast_crew, cast_crew$actor_2_name == "")

na_cast2$actor_2_name <- "Unknown"
blank_cast2$actor_2_name <- "Unknown"

movie_years_noms$actor_2_name[is.na(movie_years_noms$actor_2_name)] <- na_cast2$actor_2_name
movie_years_noms$actor_2_name[movie_years_noms$actor_2_name == ""] <- blank_cast2$actor_2_name

#Actor 1 NA's
na_cast3 <- subset(cast_crew, is.na(cast_crew$actor_1_name))
blank_cast3 <- subset(cast_crew, cast_crew$actor_1_name == "")

na_cast3$actor_1_name <- "Unknown"
blank_cast3$actor_1_name <- "Unknown"

movie_years_noms$actor_1_name[is.na(movie_years_noms$actor_1_name)] <- na_cast3$actor_1_name
movie_years_noms$actor_1_name[movie_years_noms$actor_1_name == ""] <- blank_cast3$actor_1_name

#Actor 3 NA's
na_cast4 <- subset(cast_crew, is.na(cast_crew$actor_3_name))
blank_cast4 <- subset(cast_crew, cast_crew$actor_3_name == "")

na_cast4$actor_3_name <- "Unknown"
blank_cast4$actor_3_name <- "Unknown"

movie_years_noms$actor_3_name[is.na(movie_years_noms$actor_3_name)] <- na_cast4$actor_3_name
movie_years_noms$actor_3_name[movie_years_noms$actor_3_name == ""] <- blank_cast4$actor_3_name

#Plot Keywords NA's
na_cast5 <- subset(cast_crew, is.na(cast_crew$plot_keywords))
blank_cast5 <- subset(cast_crew, cast_crew$plot_keywords == "")

na_cast5$plot_keywords <- "Unknown"
blank_cast5$plot_keywords <- "Unknown"

movie_years_noms$plot_keywords[is.na(movie_years_noms$plot_keywords)] <- na_cast5$plot_keywords
movie_years_noms$plot_keywords[movie_years_noms$plot_keywords == ""] <- blank_cast5$plot_keywords

#Language NA's imputed to English--all were in acad_awds (so English) or had USA as country
na_cast6 <- subset(cast_crew, is.na(cast_crew$language))
blank_cast6 <- subset(cast_crew, cast_crew$language == "")

na_cast6$language <- "English"
blank_cast6$language <- "English"

movie_years_noms$language[is.na(movie_years_noms$language)] <- na_cast6$language
movie_years_noms$language[movie_years_noms$language == ""] <- blank_cast6$language

#Country NA's
na_cast7 <- subset(cast_crew, is.na(cast_crew$country))
blank_cast7 <- subset(cast_crew, cast_crew$country == "")

na_cast7$country <- "Unknown"
blank_cast7$country <- "Unknown"

movie_years_noms$country[is.na(movie_years_noms$country)] <- na_cast7$country
movie_years_noms$country[movie_years_noms$country == ""] <- blank_cast7$country

#Remove columns that are irrelevant (Facebook likes, imdb user ratings, etc.) since many of the nominated movies are old
train_movies <- movie_years_noms[, -c(21, 23, 27, 28, 30, 32, 33, 36, 38, 39, 46:52)]

#Remove invalid country name in train_movies
train_movies$country[train_movies$country == "Official site"] <- "Unknown"

View(train_movies)

#Subset new movies for prediction
new_movies <- movies[movies$year > 2014, ]

View(new_movies)

#Remove NA rows from new_movies
na_rows <- c("NA","NA.1", "NA.2", "NA.3", "NA.4", "NA.5", "NA.6", "NA.7", "NA.8", "NA.9", "NA.10", 
             "NA.11", "NA.12", "NA.13", "NA.14", "NA.15")

new_movies <-  new_movies[!rownames(new_movies) %in% na_rows, ]

View(new_movies)

#Remove NA's from Film Noir category
new_movies$`Film-Noir`[is.na(new_movies$`Film-Noir`)] <- 0


#Remove NA's from actor/plot/language columns
cast_newmov <- new_movies[, c("director_name", "actor_2_name", "actor_1_name", "actor_3_name", 
                                    "plot_keywords", "language", "country")]
#Director had no NA's or blanks
new_movies$director_name[is.na(new_movies$director_name)]
new_movies$director_name[new_movies$director_name == ""]

#Actor 2 blanks (no NA's)
new_movies$actor_2_name[is.na(new_movies$actor_2_name)]
new_movies$actor_2_name[new_movies$actor_2_name == ""]

blank_newcast1 <- subset(cast_newmov, cast_newmov$actor_2_name == "")

blank_newcast1$actor_2_name <- "Unknown"

new_movies$actor_2_name[new_movies$actor_2_name == ""] <- blank_newcast1$actor_2_name

#Actor 1 blanks (no NA's)
new_movies$actor_1_name[is.na(new_movies$actor_1_name)]
new_movies$actor_1_name[new_movies$actor_1_name == ""]

blank_newcast2 <- subset(cast_newmov, cast_newmov$actor_1_name == "")

blank_newcast2$actor_1_name <- "Unknown"

new_movies$actor_1_name[new_movies$actor_1_name == ""] <- blank_newcast2$actor_1_name

#Actor 3 blanks (no NA's)
new_movies$actor_3_name[is.na(new_movies$actor_3_name)]
new_movies$actor_3_name[new_movies$actor_3_name == ""]

blank_newcast3 <- subset(cast_newmov, cast_newmov$actor_3_name == "")

blank_newcast3$actor_3_name <- "Unknown"

new_movies$actor_3_name[new_movies$actor_3_name == ""] <- blank_newcast3$actor_3_name

#Plot Keyword blanks (no NA's)
new_movies$plot_keywords[is.na(new_movies$plot_keywords)]
new_movies$plot_keywords[new_movies$plot_keywords == ""]

blank_newcast4 <- subset(cast_newmov, cast_newmov$plot_keywords == "")

blank_newcast4$plot_keywords <- "Unknown"

new_movies$plot_keywords[new_movies$plot_keywords == ""] <- blank_newcast4$plot_keywords

#Language blanks (no NA's)
new_movies$language[is.na(new_movies$language)]
new_movies$language[new_movies$language == ""]

blank_newcast5 <- subset(cast_newmov, cast_newmov$language == "")

blank_newcast5$language <- "English"

new_movies$language[new_movies$language == ""] <- blank_newcast5$language

#Country had no NA's or blanks
new_movies$country[is.na(new_movies$country)]
new_movies$country[new_movies$country == ""]

#Remove irrelevant columns that were removed from training dataset
test_movies <- new_movies[, -c(21, 23, 27, 28, 30, 32, 33, 36, 38, 39, 46:52)]
View(test_movies)


#Write separated datasets to csv's
write.csv(train_movies, "train_movies.csv")

write.csv(test_movies, "test_movies.csv")



#---------------------------------------------

#Replace only specific NA's in a given column--does not work! 
#Forced to subset prematurely, by using movie_years_noms as the training set
movies[movies$year < 2015 & movies$year > 1926, 3] <- movie_years_noms$nominations

#______________________________________________

#Analyze train set

train_movies <- read.csv("train_movies.csv", stringsAsFactors = FALSE)

str(train_movies)
summary(train_movies)



#Subset train_movies to those that won nominations
won_movies <- subset(train_movies, train_movies$nominations > 0)
View(won_movies)

#Analyze won_movies set

str(won_movies)
summary(won_movies)

#Review language vs. country--primarily English, even in other countries
table(train_movies$language, train_movies$country)

#Review language vs. country in won_movies--all in English, even in other countries
table(won_movies$language, won_movies$country)

#Get average duration of total movies and won_movies along with median, range and variance for each
#Greater variance in won_movies, and greater average, but lower maximum
mean(train_movies$duration)
median(train_movies$duration)
range(train_movies$duration)
var(train_movies$duration)
sd(train_movies$duration)

mean(won_movies$duration)
median(won_movies$duration)
range(won_movies$duration)
var(won_movies$duration)
sd(won_movies$duration)

#Get average rating of total movies and won_movies along with median, range and variance for each
#Greater average and median, but lesser variance
mean(train_movies$rating)
median(train_movies$rating)
range(train_movies$rating)
var(train_movies$rating)
sd(train_movies$rating)

mean(won_movies$rating)
median(won_movies$rating)
range(won_movies$rating)
var(won_movies$rating)
sd(won_movies$rating)

#Get average gross and budget of total movies and won_movies along with median, range and variance for each

mean(train_movies$gross)
median(train_movies$gross)
range(train_movies$gross)
var(train_movies$gross)
sd(train_movies$gross)

mean(won_movies$gross)
median(won_movies$gross)
range(won_movies$gross)
var(won_movies$gross)
sd(won_movies$gross)

mean(train_movies$budget)
median(train_movies$budget)
range(train_movies$budget)
var(train_movies$budget)
sd(train_movies$budget)

mean(won_movies$budget)
median(won_movies$budget)
range(won_movies$budget)
var(won_movies$budget)
sd(won_movies$budget)


#Compare gross and budget in overall set--low positive correlation
#Almost all movies grossed more than budgeted
cor(train_movies$budget, train_movies$gross) 
ggplot(train_movies, aes((budget/10^6), (gross/10^6))) + geom_point() + 
  labs(title="Movies from 1927-2014", x="Budget in Millions of Dollars", 
       y="Gross in Millions of Dollars")

#Compare gross and budget in won_movies--higher positive correlation
#Higher budgeted movies tended to gross more here, but not always
cor(won_movies$budget, won_movies$gross) 
ggplot(won_movies, aes((budget/10^6), (gross/10^6))) + geom_point() + 
  labs(title="Oscar-Winning Movies", x="Budget in Millions of Dollars", 
       y="Gross in Millions of Dollars")

#Get totals for each genre in training set--Drama is most frequent, Film Noir and Westerns least frequent
genres1 <- train_movies[ , c("Action", "Adventure", "Biography", "Comedy", "Crime", "Drama", "Musical", "Western", 
                             "Family", "Film.Noir", "History", "Mystery", "Romance", "Sport", "War", "Fantasy", "Sci.Fi",
                             "Thriller", "Documentary", "Horror", "Animation")]
colSums(genres1)
max(colSums(genres1))
min(colSums(genres1[, -10]))


#Get totals for each genre in won_movies set--Drama is most frequent, 
#Film Noir, Sci Fi, Documentary, Animation, Fantasy, Horror and Mystery least frequent
genres2 <- won_movies[ , c("Action", "Adventure", "Biography", "Comedy", "Crime", "Drama", "Musical", "Western", 
                             "Family", "Film.Noir", "History", "Mystery", "Romance", "Sport", "War", "Fantasy", "Sci.Fi",
                             "Thriller", "Documentary", "Horror", "Animation")]
colSums(genres2)
max(colSums(genres2))
min(colSums(genres2[, -10]))


#Plot the genre totals in training set
 
genre_plot1 <- as.data.frame(colSums(genres1))
percent1 <- (genre_plot1$`colSums(genres1)` / nrow(train_movies)) * 100 
ggplot(genre_plot1, aes(x=rownames(genre_plot1), y=percent1)) + 
  geom_bar(stat = "identity", position = "dodge") + labs(x = "Genres", y = "Percent", title = "Movie Genres 1927-2014") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#Plot the genre totals in won_movies set

genre_plot2 <- as.data.frame(colSums(genres2))
percent2 <- (genre_plot2$`colSums(genres2)` / nrow(won_movies)) * 100
ggplot(genre_plot2, aes(x=rownames(genre_plot2), y=percent2)) + 
  geom_bar(stat = "identity", position = "dodge") + labs(x = "Genres", y = "Percent", title = "Oscar-Winning Movie Genres") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#Get most frequent director and top 95% of directors in training set
#Stephen Spielberg is most frequent director, 17 directors in top 99th percentile
director1 <- train_movies[, c("name", "director_name")]
director1 <- subset(director1, director1$director_name != "Unknown")
levels(as.factor(director1$director_name))
dir_count1 <- count(director1, director1$director_name)
dir_count1$`director1$director_name`[dir_count1$n == max(dir_count1$n)]
dir_count1$`director1$director_name`[dir_count1$n > quantile(dir_count1$n, .99)]

#Get most frequent directors in winning set (2 was the maximum frequency here, so only these directors are shown)
#Billy Wilder, Clint Eastwood, David Lean, Elia Kazan, Francis Ford Coppola, Frank Capra, 
#Fred Zinneman and Milos Forman were tied as most frequent directors.  
director2 <- won_movies[, c("name", "director_name")]
director2 <- subset(director2, director2$director_name != "Unknown")
levels(as.factor(director2$director_name))
dir_count2 <- count(director2, director2$director_name)
dir_count2$`director2$director_name`[dir_count2$n == max(dir_count2$n)]

#Get most frequent actor in training set and top 99% most frequent actors
#Robert De Niro is most frequent actor.  55 actors are in more than 18 films per person.
actors1 <- stack(list(act1 = train_movies$actor_1_name, act2 = train_movies$actor_2_name, act3 = train_movies$actor_3_name))
actors1 <- subset(actors1, actors1$values != "Unknown")
actor_count1 <- count(actors1, actors1$values)
actor_count1$`actors1$values`[actor_count1$n == max(actor_count1$n)]
actor_count1$`actors1$values`[actor_count1$n > quantile(actor_count1$n, .99)]

#Get most frequent actors in winning set
#Morgan Freeman is most frequent actor, although Robert De Niro is in the 90th percentile of actors
actors2 <- stack(list(act1 = won_movies$actor_1_name, act2 = won_movies$actor_2_name, act3 = won_movies$actor_3_name))
actors2 <- subset(actors2, actors2$values != "Unknown")
actor_count2 <- count(actors2, actors2$values)
actor_count2$`actors2$values`[actor_count2$n == max(actor_count2$n)]
actor_count2$`actors2$values`[actor_count2$n == quantile(actor_count2$n, .90)]


#Analysis of New Movies set (2015-2016)
new_movies <- read.csv("test_movies.csv", stringsAsFactors = FALSE)

#Analyze train set

str(new_movies)
summary(new_movies)

#Review language vs. country in new_movies--primarily in English, even in other countries
table(new_movies$language, new_movies$country)

#Get average duration of new movies along with median, range and variance for each
#Lower variance in new_movies, and lower average and maximum duration than older movies
mean(new_movies$duration)
median(new_movies$duration)
range(new_movies$duration)
var(new_movies$duration)
sd(new_movies$duration)

#Get average rating of new_movies along with median, range and variance for each
#Lower average and median than older movies, but also lesser variance with a higher minimum (2.2 vs. 1.6)
mean(new_movies$rating)
median(new_movies$rating)
range(new_movies$rating)
var(new_movies$rating)
sd(new_movies$rating)

#Get average gross and budget of new_movies along with median, range and variance for each
mean(new_movies$gross)
median(new_movies$gross)
range(new_movies$gross)
var(new_movies$gross)
sd(new_movies$gross)

mean(new_movies$budget)
median(new_movies$budget)
range(new_movies$budget)
var(new_movies$budget)
sd(new_movies$budget)


#Compare gross and budget in new_movies--higher positive correlation than older movies
#Higher budgeted movies tended to gross more here, but not always
cor(new_movies$budget, new_movies$gross) 
ggplot(new_movies, aes((budget/10^6), (gross/10^6))) + geom_point() + 
  labs(title="Movies from 2015-2016", x="Budget in Millions of Dollars", 
       y="Gross in Millions of Dollars")

#Get totals for each genre in new_movies set--Drama is most frequent, Film Noir and Documentaries least frequent
genres_new <- new_movies[ , c("Action", "Adventure", "Biography", "Comedy", "Crime", "Drama", "Musical", "Western", 
                             "Family", "Film.Noir", "History", "Mystery", "Romance", "Sport", "War", "Fantasy", "Sci.Fi",
                             "Thriller", "Documentary", "Horror", "Animation")]
colSums(genres_new)
max(colSums(genres_new))
min(colSums(genres_new[, -10]))

#Plot the genre totals in new_movies set
#Greater percentage of Sci-Fi and Action movies than in the past
genre_plot_new <- as.data.frame(colSums(genres_new))
percent_new <- (genre_plot_new$`colSums(genres_new)` / nrow(new_movies)) * 100 

ggplot(genre_plot_new, aes(x=rownames(genre_plot_new), y=percent_new)) + 
  geom_bar(stat = "identity", position = "dodge") + labs(x = "Genres", y = "Percent", title = "New Movies Genres") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#Get most frequent directors in new_movies set (2 was the maximum frequency here, so only these directors are shown)
#David O. Russell, James Wan, Jaume Collet-Serra, Patricia Riggen, Robert Schwenke,
#Roland Emmerich and Stephen Spielberg were tied as most frequent directors.  
director_new <- new_movies[, c("name", "director_name")]
director_new <- subset(director_new, director_new$director_name != "Unknown")
levels(as.factor(director_new$director_name))
dir_count_new <- count(director_new, director_new$director_name)
dir_count_new$`director_new$director_name`[dir_count_new$n == max(dir_count_new$n)]

#Get most frequent actor in training set and top 99% most frequent actors
#Robert De Niro and Chris Hemsworth are most frequent actors.  
#Top 99 percentile actors are Bradley Cooper, Chris Hemsworth, Johnny Depp, Robert De Niro,
#Scarlett Johansson and Tom Hardy
actors_new <- stack(list(act1 = new_movies$actor_1_name, act2 = new_movies$actor_2_name, act3 = new_movies$actor_3_name))
actors_new <- subset(actors_new, actors_new$values != "Unknown")
actor_count_new <- count(actors_new, actors_new$values)
actor_count_new$`actors_new$values`[actor_count_new$n == max(actor_count_new$n)]
actor_count_new$`actors_new$values`[actor_count_new$n > quantile(actor_count_new$n, .99)]

#-------------------------------------

#Regression analysis

#Subset whole dataset by percentages

#Read in existing cleaned datasets
train_movies <- read.csv("train_movies.csv", stringsAsFactors = TRUE)
new_movies <- read.csv("test_movies.csv", stringsAsFactors = TRUE)

#Combine previously separated datasets into unified dataset
all_movies <- bind_rows(train_movies, new_movies)

#Remove excess X column
all_movies <- all_movies[, -1]

#Change na's to "Unknown"
all_movies$nominations[is.na(all_movies$nominations)] <- "Unknown"

#Write unified dataset to csv for future use
write.csv(all_movies, "all_movies.csv")

#Read in all_movies--New starting point (Will need to remove excess columns again, so that line will have to be re-executed if starting from this point.)
all_movies <- read.csv("all_movies.csv", stringsAsFactors = TRUE)

#Set random number generator to ensure reproducibility
set.seed(123)

#Define negation of %in% 
`%!in%` = Negate(`%in%`)

#Define training set as 80% of total and test set as 20% of total
movies_train <- all_movies[sample(nrow(all_movies), size = .8 * nrow(all_movies)), ]

movies_test <- all_movies[which(all_movies$name %!in% movies_train$name), ]

#Remove unknowns from movies_train
movies_train <- movies_train[movies_train$nominations != "Unknown", ]

#Convert nominations into a binary Yes/No factor
#Unfactor nominations to do numeric test
movies_train$nominations <- unfactor(movies_train$nominations)

movies_train$nominations <- ifelse(movies_train$nominations > 0, movies_train$nominations <- 1, movies_train$nominations <- 0)

#Convert nominations back into a factor and give "Yes/No" levels
movies_train$nominations <- factor(movies_train$nominations, levels = c(0, 1), labels = c("No", "Yes"))


#Create logistic regression model
movies.mod1 <- glm(nominations ~ duration + rating + gross + language,
                  family = binomial(), data = movies_train)

summary(movies.mod1)

#Make Estimates more readable
movies.mod.tab1 <- coef(summary(movies.mod1))
movies.mod.tab1[, "Estimate"] <- exp(coef(movies.mod1))

movies.mod.tab1


#Remove language variable.  Language was not influential
movies.mod2 <- glm(nominations ~ duration + rating + gross,
                   family = binomial(), data = movies_train)

summary(movies.mod2)

#Make Estimates more readable
movies.mod.tab2 <- coef(summary(movies.mod2))
movies.mod.tab2[, "Estimate"] <- exp(coef(movies.mod2))

movies.mod.tab2

#Add budget variable
movies.mod3 <- glm(nominations ~ duration + rating + gross + budget,
                   family = binomial(), data = movies_train)

summary(movies.mod3)

#Make Estimates more readable
movies.mod.tab3 <- coef(summary(movies.mod3))
movies.mod.tab3[, "Estimate"] <- exp(coef(movies.mod3))

movies.mod.tab3


#Add budget-gross interaction term--only minimal improvement--still appears to be overfitting
movies.mod4 <- glm(nominations ~ duration + rating + gross + budget + gross:budget,
                   family = binomial(), data = movies_train)

summary(movies.mod4)

#Remove budget and add in all genre variables
movies.mod5 <- glm(nominations ~ duration + rating + gross + Action + Adventure +
                     Biography + Comedy + Crime + Drama + Musical + Western + 
                     Family + History + Mystery + Romance + Sport + War + Fantasy +
                     Sci.Fi + Thriller + Documentary + Horror + Animation,
                   family = binomial(), data = movies_train)

summary(movies.mod5)

#Make Estimates more readable
movies.mod.tab5 <- coef(summary(movies.mod5))
movies.mod.tab5[, "Estimate"] <- exp(coef(movies.mod5))

movies.mod.tab5


#All genre variables by themselves--Highest AIC, but still likely overfitting
movies.mod6 <- glm(nominations ~ Action + Adventure +
                     Biography + Comedy + Crime + Drama + Musical + Western + 
                     Family + History + Mystery + Romance + Sport + War + Fantasy +
                     Sci.Fi + Thriller + Documentary + Horror + Animation,
                   family = binomial(), data = movies_train)

summary(movies.mod6)

#Make Estimates more readable
movies.mod.tab6 <- coef(summary(movies.mod6))
movies.mod.tab6[, "Estimate"] <- exp(coef(movies.mod6))

movies.mod.tab6

#Add language variable back--not significant by itself but yields highest AIC with 
#least residual deviation

movies.mod7 <- glm(nominations ~ duration + rating + gross + language + Action + Adventure +
                     Biography + Comedy + Crime + Drama + Musical + Western + 
                     Family + History + Mystery + Romance + Sport + War + Fantasy +
                     Sci.Fi + Thriller + Documentary + Horror + Animation,
                   family = binomial(), data = movies_train)


summary(movies.mod7)


#Make Estimates more readable
movies.mod.tab7 <- coef(summary(movies.mod7))
movies.mod.tab7[, "Estimate"] <- exp(coef(movies.mod7))

movies.mod.tab7

#Attempt forward stepwise model selection
mod.base <- glm(nominations ~ 1, family = binomial(), data = movies_train)
fwd.model <- step(mod.base, direction = "forward", scope = (~ duration + rating + gross +
                                                              budget + language + country + 
                                                              Action + Adventure +
                                                              Biography + Comedy + Crime + Drama + Musical + Western + 
                                                              Family + History + Mystery + Romance + Sport + War + Fantasy +
                                                              Sci.Fi + Thriller + Documentary + Horror + Animation), trace = 0)

summary(fwd.model)

#Store forward model in fwd.model variable based on call shown in summary for ease of loading
fwd.model <- glm(formula = nominations ~ rating + Drama + country + Mystery + 
      Sci.Fi + duration + budget + gross + Animation + Romance + 
      Fantasy + Documentary + Horror + War + Family, family = binomial(), 
    data = movies_train)

summary(fwd.model)

#Make Estimates more readable
movies.mod.tab8 <- coef(summary(fwd.model))
movies.mod.tab8[, "Estimate"] <- exp(coef(fwd.model))

movies.mod.tab8

#Plot leading models--Model 7 appears to have more normal distribution
plot(movies.mod7)
plot(fwd.model)

#Compare residual deviations of both models--fwd model is somewhat better (but lower AIC)
anova(movies.mod7, fwd.model)

#Run predictions on both models on training set to evaluate models
pred_movies_train <- movies_train[, -3]

#Run predict function on both models
probs_train1 <- predict(movies.mod7, newdata = pred_movies_train, type = "response")
probs_train2 <- predict(fwd.model, newdata = pred_movies_train, type = "response")

pred1_train <- rep("No", 3604)
pred2_train <- rep("No", 3604)

pred1_train[probs_train1 > .5] <- "Yes"
pred2_train[probs_train2 > .5] <- "Yes"

#Slightly lower overall accuracy for first model (98.2% vs. 98.7%), with higher false negative and lower false positive in first model.
table(pred1_train, movies_train$nominations)
#Sensitivity is 19.2%, Specificity is 99.8%, False Negative is 80.8%, False Positive is 0.2%

#This model is likely superior--higher sensitivity with only slightly higher false positive rate.
table(pred2_train, movies_train$nominations)
# Sensitivity is 47.9%, Specificity is 99.7%, False Negative is 52.1%, False Positive is 0.3%

#Check ROC for first model
movies_train$prob1 <- probs_train1
train_ROC1 <- roc(nominations ~ prob1, data = movies_train)
plot.roc(train_ROC1, legacy.axes = TRUE, print.thres = TRUE, print.auc = TRUE)
auc(train_ROC1)

#Check ROC for second model--greater AUC here (.9713 vs. .9472)
movies_train$prob2 <- probs_train2
train_ROC2 <- roc(nominations ~ prob2, data = movies_train)
plot.roc(train_ROC2, legacy.axes = TRUE, print.thres = TRUE, print.auc = TRUE)
auc(train_ROC2)

#Try confusion matrix with optimal thresholds found by ROC curve for both models
#Reset for new confusion matrix
pred1_train <- rep("No", 3604)
pred2_train <- rep("No", 3604)

#Use optimal thresholds here
pred1_train[probs_train1 > .045] <- "Yes"
pred2_train[probs_train2 > .055] <- "Yes"

#Thresholds lowered overall accuracy rates by 6.5% and 3.8%, respectively. 
#Lower overall accuracy for first model (91.7% vs. 94.9%), with higher false negative and higher false positive in first model.
table(pred1_train, movies_train$nominations)
#Sensitivity is 83.6%, Specificity is 91.8%, False Negative is 16.4%, False Positive is 8.2%

#This model is likely superior--higher sensitivity with lower false positive rate.
table(pred2_train, movies_train$nominations)
# Sensitivity is 93.2%, Specificity is 95.0%, False Negative is 6.8%, False Positive is 5.0%

#-----------------------------------------

#Clean movies test as above, removing unknown nominations and changing to a binary ("Yes/No")
#Remove unknowns from movies_test
movies_test <- movies_test[movies_test$nominations != "Unknown", ]

#Convert nominations into a binary Yes/No factor
#Unfactor nominations to do numeric test
movies_test$nominations <- unfactor(movies_test$nominations)

movies_test$nominations <- ifelse(movies_test$nominations > 0, movies_test$nominations <- 1, movies_test$nominations <- 0)

#Convert nominations back into a factor and give "Yes/No" levels
movies_test$nominations <- factor(movies_test$nominations, levels = c(0, 1), labels = c("No", "Yes"))


#Make predictions based on leading models and evaulate

#Remove new languages Dzongkha, Maya and Romanian from Test set not in train set
#(only 3 movies, none were winners)
movies_test <- movies_test[movies_test$language != "Dzongkha" & 
                                               movies_test$language != "Maya" & 
                                               movies_test$language != "Romanian", ]
#Remove countries in test set not in train set (only 4 movies, none winners)

movies_test <- movies_test[movies_test$country != "Aruba" & 
                             movies_test$country != "Cambodia" & 
                             movies_test$country != "Libya" & 
                             movies_test$country != "Nigeria", ]

#Remove response variable from test set

pred_movies <- movies_test[, -3]

#Run predict function on both models
probs1 <- predict(movies.mod7, newdata = pred_movies, type = "response")
probs2 <- predict(fwd.model, newdata = pred_movies, type = "response")

pred1 <- rep("No", 887)
pred2 <- rep("No", 887)

pred1[probs1 > .5] <- "Yes"
pred2[probs2 > .5] <- "Yes"

#Same accuracy for both models (98%), but higher false negative and lower false positive in first model.
table(pred1, movies_test$nominations)
#Sensitivity is 7.1%, Specificity is 99.4%, False Negative is 92.9%, False Positive is 0.6%

#This model is likely superior
table(pred2, movies_test$nominations)
# Sensitivity is 35.7%, Specificity is 99.0%, False Negative is 64.3%, False Positive is 1.0%
 
#Check ROC for first model
movies_test$prob1 <- probs1
ROC1 <- roc(nominations ~ prob1, data = movies_test)
rocPlot1 <- plot.roc(ROC1, legacy.axes = TRUE, print.thres = TRUE, print.auc = TRUE)
auc(ROC1)

#Check ROC for second model--lower AUC here (.8528 vs. .8673)
movies_test$prob2 <- probs2
ROC2 <- roc(nominations ~ prob2, data = movies_test)
rocPlot2 <- plot.roc(ROC2, legacy.axes = TRUE, print.thres = TRUE, print.auc = TRUE)
auc(ROC2)

#Try confusion matrix with optimal thresholds found by ROC curve
#Reset for new confusion matrix
pred1 <- rep("No", 887)
pred2 <- rep("No", 887)

#Use optimal thresholds here
pred1[probs1 > .034] <- "Yes"
pred2[probs2 > .046] <- "Yes"

#Lowered overall accuracy for both models (88.6% vs. 92.3%, respectively), but lower false negative and higher false positive in first model, 
#and much higher sensitivities for both.
table(pred1, movies_test$nominations)
#Sensitivity is 85.7%, Specificity is 88.7%, False Negative is 14.3%, False Positive is 11.3%

#This model is therefore superior in most respects, 
#although the first model may be preferred where greater sensitivity is desired.
table(pred2, movies_test$nominations)
# Sensitivity is 71.4%, Specificity is 93.0%, False Negative is 28.6%, False Positive is 7.0%

#-----------------------------------------------------
#Predict probability of nominations based on specific values using model with greater sensitivity (Model 7)
#Create data frame for prediction with two values per variable
#First movie is Hellraiser; second movie is Seven Years in Tibet, both rated 7
View(pred_movies[pred_movies$rating == 7, ])

predDat <- data.frame(duration = c(86, 136), rating = c(7, 7), 
                      gross = c(14564027, 37901509), language = c("English", "English"), 
                      Action = c(0, 0), Adventure = c(0, 1), Biography = c(0, 1), 
                      Comedy = c(0, 0), Crime = c(0, 0), Drama = c(0, 1), Musical = c(0, 0), 
                      Western = c(0, 0), Family = c(0, 0), History = c(0, 1), Mystery = c(0, 0), 
                      Romance = c(0, 0), Sport = c(0, 0), War = c(0, 1), Fantasy = c(1, 0), 
                      Sci.Fi = c(0, 0), Thriller = c(0, 0), Documentary = c(0, 0), 
                      Horror = c(1, 0), Animation = c(0, 0))

predDat2 <- data.frame(duration = c(86, 136), rating = c(7, 7), 
                      gross = c(14564027, 37901509), country = c("UK", "USA"), budget = c(1e+06, 7e+04), 
                      Drama = c(0, 1), Family = c(0, 0), Mystery = c(0, 0),
                      Romance = c(0, 0), War = c(0, 1), Fantasy = c(1, 0), 
                      Sci.Fi = c(0, 0), Documentary = c(0, 0), 
                      Horror = c(1, 0), Animation = c(0, 0))

#Seven Years in Tibet has much higher likelihood of winning 
#(approximately 4% vs. less than 1 millionth of 1%), despite both movies 
#having imdb ratings of 7 (It didn't get an Oscar, but was nominated for many other
#awards, including a Golden Globe.) Hellraiser was nominated for a Saturn, but nothing
#as prestigious as a Golden Globe. Thus, imdb rating is not only deciding factor.

predict(movies.mod7, newdata = predDat, type = "response")

#Similar results produced by fwd_model
predict(fwd.model, newdata = predDat2, type = "response")

predResult <- pred_movies[pred_movies$name == "HELLRAISER" | pred_movies$name == "SEVEN YEARS IN TIBET", ]

#Get movies in same order as in predict
predResult <- predResult[c(2,1), ]

predResult <- cbind(predResult, predict(movies.mod7, newdata = predDat, type = "response", se.fit = TRUE))

#Change variable order to put prediction variables right after name
predResult <- predResult[, c(1, 35, 36, 2:34, 37)]
View(predResult)

#---------------------------
#Run again at Rating = 8
#Create data frame for prediction with two values per variable
#First movie is The Artist; second movie is Casino Royale, both rated 8
View(pred_movies[pred_movies$rating == 8, ])

pred_Dat <- data.frame(duration = c(100, 144), rating = c(8, 8), 
                      gross = c(44667095, 167007184), language = c("English", "English"), 
                      Action = c(0, 1), Adventure = c(0, 1), Biography = c(0, 0), 
                      Comedy = c(1, 0), Crime = c(0, 0), Drama = c(1, 0), Musical = c(0, 0), 
                      Western = c(0, 0), Family = c(0, 0), History = c(0, 0), Mystery = c(0, 0), 
                      Romance = c(1, 0), Sport = c(0, 0), War = c(0, 0), Fantasy = c(0, 0), 
                      Sci.Fi = c(0, 0), Thriller = c(0, 1), Documentary = c(0, 0), 
                      Horror = c(0, 0), Animation = c(0, 0))

pred_Dat2 <- data.frame(duration = c(100, 144), rating = c(8, 8), 
                       gross = c(44667095, 167007184), country = c("France", "UK"), budget = c(1.5e+07, 1.5e+08), 
                       Drama = c(1, 0), Family = c(0, 0), Mystery = c(0, 0), 
                       Romance = c(1, 0), War = c(0, 0), Fantasy = c(0, 0), 
                       Sci.Fi = c(0, 0), Documentary = c(0, 0), Horror = c(0, 0), Animation = c(0, 0))

#The Artist has much higher likelihood of winning 
#(approximately 14.5% vs. 3.8%), despite both movies 
#having imdb ratings of 8 (It won a Best Picture Oscar, and was nominated for many other
#awards, including a Golden Globe.) Casino Royale won a Saturn for Best Action/Adventure/Thriller film, a BAFTA award for Best Sound
#and was nominated for a number of other awards, but nothing as prestigious as an Oscar or a Golden Globe. Thus, imdb rating is not the only deciding factor.

predict(movies.mod7, newdata = pred_Dat, type = "response")

#fwd_model produced incorrect results because the model left out key genre variables
#(Action, Adventure, Thriller, Comedy).  It showed both films with very low probabilities
#of winning with Casino Royale at a much higher probability than The Artist 
#(approximately 1% vs. less than 1 millionth of 1%).
predict(fwd.model, newdata = pred_Dat2, type = "response")

pred_Result <- pred_movies[pred_movies$name == "THE ARTIST" | pred_movies$name == "CASINO ROYALE", ]

pred_Result <- cbind(pred_Result, predict(movies.mod7, newdata = pred_Dat, type = "response", se.fit = TRUE))

#Change variable order to put prediction variables right after name
pred_Result <- pred_Result[, c(1, 35, 36, 2:34, 37)]
View(pred_Result)

