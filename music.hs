import Data.List
import Data.Char


type Sales = (String, String,Int) -- Data type for each record
type Pairs = (String, Int) -- Used in function 4
tdata :: [Sales] --list to be used
tdata = [("Artist","song",1)]

-- Function 1 - record single sale of a track
recordSale ::[Sales] -> String -> String -> [Sales]
recordSale [] _ _ = []
recordSale ((artist,song,sale):list) singleArtist singleSong
      |artist/= singleArtist && song /= singlesong && list == [] = (singleArtist, singleSong,1):[] -- If not found due to empty list, add to list
      |artist /= singleArtist  || song /= singleSong && list /= []  = (artist,song,sale):recordSale list singleArtist singleSong -- not found, add to list
      |artist == singleArtist && song ==singleSong = (artist, song, sale +1):list -- If record is found, increment count of sales


--Function 2 - return the total number of sales of a track given an artist name and track title (this should return 0 if there is no matching track)
trackSale ::[Sales] -> String -> String -> Int
trackSale [] _ _ = 0 -- if no inputs, return 0
trackSale ((artist,song,sale):list) singleArtist singleSong
      |artist /= singleArtist || song /= singlesong = list singleArtist singleSong  -- traverse list until empty if not found"
      |otherwise = sale  -- output


-- Function 3 - remove a track from the record of sales given an artist name and track title
trackRemoval ::[Sales] -> String -> String -> [Sales]
trackRemoval [] _ _ = []
trackRemoval ((artist,song,sale):list) singleArtist singleSong
      |artist /= singleArtist || song /= singleSong = (artist,song,sale):trackRemoval list singleArtist singleSong
      |otherwise = trackRemoval list singleArtist singleSong



--Function 4 - return a list of pairs – (each including track name and sales figure) for a given artist
pairs ::[Sales] -> String -> [Pairs]
pairs [] _  = []
pairs ((artist,song,sale):list) singleArtist
      |artist /= singleArtist  = pairs list singleArtist
      |otherwise  = (song, sale):pairs list singleArtist


--Function 5 - give the average sales for a given artist (i.e. the average sales for the artist’s tracks)
aveSales ::[Sales] -> String -> Int ->Int ->Int
aveSales [] _ _ _=  0
aveSales ((artist,song,sale):list) singleArtist  count figures
    |artist == singleArtist = averageSales list singleArtist  (count + 1) (figures + sale)
    |list == [] && count >= 1 = (figures div count)
    |otherwise = averageSales list singleArtist  count figures


--Function 6 - remove a given artist and all the artist’s tracks from the record of sales
remArtist ::[Sales] -> String -> [Sales]
remArtist [] _ = []
remArtist ((artist,song,sale):list) singleArtist
      |artist /= singleArtist = (artist,song,sale):remArtist list singleArtist
      |otherwise = remArtist list singleArtist


-- Demos
demo1 = recordSale tData --"artist" "track"
demo2 =trackSale tData --"Artist" "track"
demo3 = trackRemoval tData --"artist" "track"
demo4 = pairs tData --"artist"
demo5 = aveSales  ttData -- "artist" 0 0
demo6 = remArtist testData -- "artist"
