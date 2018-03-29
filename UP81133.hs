--
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- UP811334
--
import Data.Char
import Data.List
import Text.Printf

--
-- Types
type Title = String
type Director = String
type Year = Int
type Likes = [String]
type Dislikes = [String]

-- Define Film type here
type Film = (Title, Director, Year, Likes, Dislikes)
type Database = [Film]

testDatabase :: [Film]
testDatabase = [("Blade Runner", "Ridley Scott", 1982, ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Dave"], ["Sam", "Olga", "Tim"]),
				("The Fly", "David Cronenberg", 1986, ["Garry", "Dave", "Zoe"], ["Kevin", "Emma", "Heidi", "Jo", "Kate"]),
				("Body Of Lies", "Ridley Scott", 2008, ["Garry", "Dave"], ["Bill", "Olga", "Tim", "Zoe", "Paula"]),
				("Avatar", "James Cameron", 2009, ["Dave", "Amy", "Liz"], ["Olga", "Tim", "Zoe", "Paula"]),
				("Titanic", "James Cameron", 1997, ["Zoe", "Emma", "Paula", "Liz", "Olga", "Dave"], ["Sam", "Wally", "Kate"]),
				("The Departed", "Martin Scorsese", 2006, ["Wally", "Liz", "Kevin", "Tim", "Emma"], ["Olga", "Dave", "Kate", "Zoe"]),
				("Aliens", "Ridley Scott", 1986, ["Dave", "Garry", "Liz", "Sam", "Wally", "Kate", "Zoe"], ["Tim", "Emma", "Jo", "Olga"]),
				("Kingdom Of Heaven", "Ridley Scott", 2005, ["Jo", "Wally", "Emma"], ["Tim", "Garry", "Ian", "Neal"]),
				("Alien: Covenant", "Ridley Scott", 2017, ["Kevin", "Tim"], ["Emma", "Jo", "Liz"]),
				("E.T. The Extra-Terrestrial", "Steven Spielberg", 1982, ["Dave", "Amy", "Garry", "Ian", "Neal"], ["Jenny", "Kate", "Emma", "Olga"]),
				("Bridge of Spies", "Steven Spielberg", 2015, ["Wally", "Sam", "Dave", "Neal"], ["Bill", "Garry", "Ian", "Kate"]),
				("Jaws", "Steven Spielberg", 1975, ["Jenny", "Emma", "Bill", "Neal"], ["Sam", "Ian", "Kate"]),
				("The Martian", "Ridley Scott", 2015, ["Wally", "Sam", "Dave", "Jo", "Jenny", "Kate", "Emma", "Olga"], ["Ian", "Neal", "Tim", "Liz"]),
				("The BFG", "Steven Spielberg", 2016, ["Sam", "Wally", "Dave", "Jo", "Kate"], ["Neal"]),
				("The Shawshank Redemption", "Frank Darabont", 1994, ["Dave", "Amy", "Bill", "Garry", "Ian", "Neal", "Kate", "Jenny", "Zoe", "Heidi"], ["Jo"]),
				("Gladiator", "Ridley Scott", 2000, ["Olga", "Neal", "Kate", "Garry"], ["Heidi", "Bill", "Sam", "Zoe"]),
				("The Green Mile", "Frank Darabont", 1999, ["Kevin", "Tim", "Emma", "Heidi"], ["Kate", "Jenny", "Zoe"]),
				("True Lies", "James Cameron", 1994, ["Sam", "Dave"], ["Emma", "Olga", "Jenny", "Zoe"]),
				("Super 8", "J J Abrams", 2011, ["Kevin", "Tim", "Emma", "Olga", "Heidi"], ["Wally", "Dave", "Jenny", "Zoe"]),
				("Minority Report", "Steven Spielberg", 2002, ["Kevin", "Kate", "Tim", "Emma", "Jenny", "Zoe"], ["Olga", "Heidi"]),
				("War Horse", "Steven Spielberg", 2011, ["Garry", "Bill", "Olga", "Jo", "Wally", "Emma", "Tim", "Kate", "Zoe"], ["Heidi", "Jenny", "Sam"]),
				("Silence", "Martin Scorsese", 2016, ["Wally", "Emma", "Tim", "Heidi", "Bill", "Jo"], ["Dave", "Olga"]),
				("The Terminal", "Steven Spielberg", 2004, ["Kate", "Dave", "Jo", "Wally", "Emma"], ["Heidi"]),
				("Star Wars: The Force Awakens", "J J Abrams", 2015, ["Emma", "Wally", "Zoe", "Kate", "Bill", "Dave", "Liz"], ["Olga", "Jo", "Neal"]),
				("Hugo", "Martin Scorsese", 2011, ["Wally", "Sam"], ["Kate", "Bill", "Dave"])]

--
--
--  Your functional code goes here
--
--
--call displayFilm and concat values for display
displayFilms :: Database -> String
displayFilms database = concat(map displayFilm database)

-- checks if two films are similar by title
sameFilm :: String -> Film -> Bool
sameFilm title (t, d, y, l, dl)
        | title == t = True
        | otherwise = False

-- search for a film by title
searchFilm :: String -> Database -> Database
searchFilm title database = filter (sameFilm title) database

--display film as string use map
displayFilmsAsString :: Database -> String
displayFilmsAsString database = concat(map displayFilmString database)

--display Film with title and Like/Dislike
displayFilmsAsLString :: Database -> String -> String
displayFilmsAsLString database lDlStr = concat(map (displayFilmLString lDlStr) database )

--display film with Rating
displayFilmsWithRating :: Database -> String
displayFilmsWithRating database = concat(map displayFilmRatingString database)

--display a Film  by multiline char
displayFilm :: Film -> String
displayFilm (title, director, year, likes, dislikes) = "\nTitle: " ++ title ++ "\nDirector: " ++ director ++ "\nYear: " ++ (show year) ++ "\nLikes: " ++ show (length likes)++ "" ++ "\nDislikes: "++ show (length dislikes) ++ "\n"

--film rating String with Formatted printf
displayFilmRatingString ::Film -> String
displayFilmRatingString (title, director, year, likes, dislikes) = printf "\n%-30s %-30s %-14d %-14d %-14d %14.1f" title director  year (length likes) (length dislikes) ((fromIntegral(length likes)/(fromIntegral(length likes)+fromIntegral(length dislikes))):: Float)

--return the rating of the film
getRatingOfFilm ::Film -> Float
getRatingOfFilm(title, director, year, likes, dislikes) = ((fromIntegral(length likes) / (fromIntegral(length likes) + fromIntegral(length dislikes))):: Float)

--display film string in formatted format
displayFilmString :: Film -> String
displayFilmString (title, director, year, likes, dislikes) = printf "\n%-30s %-30s %-14d %-14d %-14d" title director  year (length likes) (length dislikes)

--display film string with title and Like/Dislike
displayFilmLString :: String ->Film -> String
displayFilmLString lDlStr (title, director, year, likes, dislikes)= printf "\n%-30s %-30s " title (isLikeOrNot likes lDlStr)

--check that the director of a film is equal to passed value
hasDirector :: String -> Film -> Bool
hasDirector director (t, d, y, l, dl)
 | director == d = True
 | otherwise = False

-- check that the person likes or dislikes and return like if like
isLikeOrNot :: [String] -> String -> String
isLikeOrNot l lDlStr
 | elem lDlStr l = "Like"
 | otherwise = "Dislike"

--return rating of the film
getRating:: Database-> Float
getRating database = sum(map getRatingOfFilm database) / fromIntegral(length database)

--return film with rating > 0.75
hasRating:: Int -> Film -> Bool
hasRating rating (t, d, y, l, dl)
 | fromIntegral(length l) / (fromIntegral(length l) + fromIntegral(length dl)) > 0.75 = True
 | otherwise = False

-- return true if the person likes or dislikes list
likesOrDislikes :: String -> Film -> Bool
likesOrDislikes lDlStr (t, d, y, l, dl)
         | elem lDlStr l || elem lDlStr dl = True
		 | otherwise = False

-- return true if person likes or else False
doesLikes :: String -> Film -> Bool
doesLikes like (t, d, y, l, dl)
         | elem like l = True
		 | otherwise = False

-- return true if person dislikes or else False
isdislike :: String -> Film -> Bool
isdislike dislike (t, d, y, l, dl)
         | elem dislike dl = True
		 | otherwise = False

-- return true if year is between two years
betweenYears:: Int -> Int -> Film -> Bool
betweenYears minY maxY (t, d, y, l, dl)
         | y <= maxY && y >= minY = True
		 | otherwise = False

-- return greater then if first val is large else return false
ratingSort (t1, d1, y1, l1, dl1) (t2, d2, y2, l2, dl2)
         | (fromIntegral(length l1) / (fromIntegral(length l1) + fromIntegral(length dl1))) < (fromIntegral(length l2) / (fromIntegral(length l2) + fromIntegral(length dl2))) = GT
         | otherwise = LT

-- add new film
addFilm :: Film -> Database -> Database
addFilm film database = film : database

--display all films by a Director Name
displayFilmsByRidleyScott :: String -> Database -> String
displayFilmsByRidleyScott director database = displayFilmsAsString (filter (hasDirector director) database)

--display all films by a Director
displayAverageRating:: String -> Database ->String
displayAverageRating director database = printf "%3.2f" (getRating(filter (hasDirector director) database))

--display all films by by Rating
displayFilmsRatingGreater:: Int -> Database ->String
displayFilmsRatingGreater rating database = displayFilmsAsString (filter (hasRating rating) database)

--return titles and Like/Dislike
titlesRatedBy :: String -> Database -> String
titlesRatedBy lDlStr database = displayFilmsAsLString (filter (likesOrDislikes lDlStr) database) lDlStr

--Sorted Db with Rating
betweenYearsSortedByRating::Int -> Int -> Database -> Database
betweenYearsSortedByRating minYear maxYear database = sortBy ratingSort (filter (betweenYears minYear maxYear) database)

--add the person to like list
likedMovie :: String -> String -> Database -> Database
likedMovie like filmTitle [] = []
likedMovie like filmTitle ((t, d, y, l, dl) : xs)
	 | (filmTitle == t) &&  not(doesLikes like (t, d, y, l, dl)) = (t, d, y, like : l, dl) : likedMovie like filmTitle xs
		 | otherwise = (t, d, y, l, dl) : likedMovie like filmTitle xs

----add the person to dislike list
dislikedMovie :: String -> String -> Database -> Database
dislikedMovie dislike filmTitle [] = []
dislikedMovie dislike filmTitle ((t, d, y, l, dl) : xs)
	 | (filmTitle == t) &&  not(isdislike dislike (t, d, y, l, dl)) = (t, d, y, l, dislike : dl) : dislikedMovie dislike filmTitle xs
		 | otherwise = (t, d, y, l, dl) : dislikedMovie dislike filmTitle xs

-- Demo function to test basic functionality (without persistence - i.e.
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

demo :: Int -> IO ()
--demo 1  = putStrLn all films after adding 2018 film "Sherlock Gnomes"
--          directed by by "John Stevenson" to testDatabase
demo 1 = putStrLn (displayFilms(addFilm("Sherlock Gnomes","John Stevenson", 2018, [], []) testDatabase))
--demo 2  = putStrLn (filmsAsString testDatabase)
demo 2 = putStrLn (displayFilmsAsString testDatabase)
--demo 3  = putStrLn all films by "Ridley Scott"
demo 3 = putStrLn (displayFilmsByRidleyScott "Ridley Scott" testDatabase)
--demo 4  = putStrLn all films with website rating >= 75%
demo 4 = putStrLn (displayFilmsRatingGreater 75 testDatabase)
--demo 5  = putStrLn average website rating for "Ridley Scott"
demo 5 = putStrLn ("Average website rating for Ridley Scott : "++  displayAverageRating "Ridley Scott" testDatabase)
--demo 6  = putStrLn titles of films rated by "Emma" (with likes/dislikes)
demo 6 = putStrLn  (titlesRatedBy "Emma" testDatabase)
--demo 7  = putStrLn all films after "Emma" says she likes "Avatar"
demo 7 = putStrLn  (displayFilmsAsString(likedMovie "Emma" "Avatar" testDatabase))
--demo 71 = putStrLn all films after "Emma" says she likes "Titanic"
demo 71 = putStrLn  (displayFilmsAsString(likedMovie "Emma" "Titanic" testDatabase))
--demo 72 = putStrLn all films after "Emma" says she dislikes "Jaws"
demo 72 = putStrLn  (displayFilmsAsString(dislikedMovie "Emma" "Jaws" testDatabase))
--demo 8  = films between 2000 and 2006 inclusive sorted by website rating
demo 8 = putStrLn  (displayFilmsWithRating(betweenYearsSortedByRating 2000 2006 testDatabase))
--
--
-- Your user interface code goes here
--
--
main :: IO ()
main = do db <- readFile "films.txt"
          putStrLn "Enter your name: "
	  let database=read db:: [Film]
          userName <- getLine
	  database <- userInterface (userName, database)
          writeFile "films.txt" (show database)
          putStrLn "\n\nYour changes to the database has been successfull. :)"
userInterface :: (String, Database) -> IO Database
userInterface (userName, database) = do let info = (userName, database)
                                        let message1 = "Press Enter to go back to the main menu: "
                                        putStrLn "  Film Database  "
                                        putStrLn "1. - add a new film"
                                        putStrLn "2. - give all films in the database"
                                        putStrLn "3. - give all the films by a given director"
                                        putStrLn "4. - give all films that have a website rating of 75% or higher"
                                        putStrLn "5. - give the average website rating for the films of a given director"
                                        putStrLn "6. - give the titles of all films that a particular user has rated,along with how they have been rated (‘like’ or ‘dislike’) by that user"
                                        putStrLn "7. - allow a user to say they like or dislike a particular film"
                                        putStrLn "8. - give all the films released between two given years (inclusive), sorted in descending order of website rating"
                                        putStrLn "0. - Exit and update database"
                                        putStrLn "-----------------------------------------------------------------------------------------------------------------------------------------\n"
                                        putStr "Enter a number to perform an action or 0 to exit and save the database: "
                                        input <- getLine
                                        if input /= "0"
                                           then case input of
                                                     "1" -> do info <- selection 1 info
                                                               putStr message1
                                                               entry <- getLine
                                                               userInterface info
                                                     "2" -> do info <- selection 2 info
                                                               putStr message1
                                                               entry <- getLine
                                                               userInterface info
                                                     "3" -> do info <- selection 3 info
                                                               putStr message1
                                                               entry <- getLine
                                                               userInterface info
                                                     "4" -> do info <- selection 4 info
                                                               putStr message1
                                                               entry <- getLine
                                                               userInterface info
                                                     "5" -> do info <- selection 5 info
                                                               putStr message1
                                                               entry <- getLine
                                                               userInterface info
                                                     "6" -> do info <- selection 6 info
                                                               putStr message1
                                                               entry <- getLine
                                                               userInterface info
                                                     "7" -> do info <- selection 7 info
                                                               putStr message1
                                                               entry <- getLine
                                                               userInterface info
                                                     "8" -> do info <- selection 8 info
                                                               putStr message1
                                                               entry <- getLine
                                                               userInterface info
                                                     _ -> do putStrLn "You have entered an invalid number."
                                                             userInterface info
                                        else return (snd info)
selection 1 (userName, database) = do putStrLn "Add new Film  "
				      putStrLn "Enter film title or press enter to return to main menu"
				      title <- getLine
				      if title == ""
                                        then return(userName, database)
                                      else if searchFilm title database /= []
                                         then do putStrLn "Film already exists."
                                                 selection 1 (userName, database)
				      else do putStrLn "Enter Director of Film "
				              dir <- getLine
					      putStrLn "Enter Year of Film "
				              yea <- getLine
				      	      let year = read yea :: Int
                                              let newDb = addFilm (title, dir, year, [], []) database
                                              return (userName, newDb)
selection 2 (userName, database) = do putStrLn "  Display all films  "
                                      putStrLn (displayFilmsAsString database)
                                      return (userName, database)
selection 3 (userName, database) = do putStrLn "All films by a director"
				      putStrLn "Enter Director name "
				      director <- getLine
                                      putStrLn (displayFilmsByRidleyScott director database)
                                      return (userName, database)
selection 4 (userName, database) = do putStrLn "All films having rating greater then 75%"
                                      putStrLn (displayFilmsRatingGreater 75 database)
                                      return (userName, database)
selection 5 (userName, database) = do putStrLn "Average Website Rating for film of given a director"
				      putStrLn "Enter Director name "
				      director <- getLine
				      putStrLn ("Average website rating for director " ++ director ++ " : " ++  displayAverageRating director database)
                                      return (userName, database)
selection 7 (userName, database) = do putStrLn "Add Like/Dislike for a Movie"
				      putStrLn "Enter Film Title "
				      title <- getLine
				      putStrLn "Enter your name "
				      name <- getLine
				      putStrLn ("Enter like or dislike (Case Sensitive)")
				      choice <- getLine
				      if choice == "like"
                                         then do let newDB = likedMovie name title database
						 putStrLn "Movie liked"
					         return(userName, newDB)
				      else if choice == "dislike"
					 then do let newDB = dislikedMovie name title database
						 putStrLn "Movie disLiked"
					         return(userName, newDB)
				      else do putStrLn "Invalid Choice Entered"
					      return (userName, database)
selection 6 (userName, database) = do putStrLn "Titles rated by a Person"
				      putStrLn "Enter Person Name (Case Sensitive) "
			     	      name <- getLine
				      putStrLn  (titlesRatedBy name database)
				      return (userName, database)
selection 8 (userName, database) = do putStrLn "Films released between two given years (inclusive), sorted in descending order of website rating"
				      putStrLn "Enter Start Year "
				      year <- getLine
				      let firstYear = read year :: Int
				      putStrLn "Enter End Year "
				      year <- getLine
				      let lastYear = read year :: Int
				      putStrLn  (displayFilmsWithRating(betweenYearsSortedByRating firstYear lastYear database))
				      return (userName, database)
