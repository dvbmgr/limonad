module Limonad.Utils where
	
	-- |Joins the element
	join :: [a] -> [[a]] -> [a]
	join d = concat . intersperse d

	-- |Formats time
	formatTime :: Int -> String
	formatTime t 
		| e <= 0 = "just now"
		| e <= 1 = "a second" ++ a
		| e <= 59 = s e 1 " seconds" ++ a
		| e <= 119 = "a minute" ++ a
		| e <= 3540 = s e 60 " minutes" ++ a
		| e <= 7100 = "an hour" ++ a
		| e <= 82800 = s (e+99) (60*60) " hours" ++ a
		| e <= 172000 = "a day" ++ a
		| e <= 518400 = s (e+800) (60*60*24) "days" ++ a
		| e <= 1036800 = "a week" ++ a
		| otherwise = s (e+180000) (60*60*24*7) " weeks" ++ a
		where 
			e = abs t :: Float
			s m l = (++) (show $ round (m/l))
			a = if t > 0 then
					" ago"
				else 
					" in the future"