|| ==========================================================================================================
|| utils.strings
|| ==========================================================================================================

%export split startsWith toUpper toLower

str == [char]

|| ==========================================================================================================
|| split
|| > Splits a string into a list of string using a delimiter
|| > Requires startsWith
|| ==========================================================================================================

split :: str -> str -> [str]

split delim string
  = reverse (xsplit string [] [])
    where
    xsplit [] acc substring = substring : acc
    xsplit remaining acc substring
      = xsplit (drop (#delim) remaining) (substring : acc) [], if startsWith remaining delim
      = xsplit (tl remaining) acc (substring ++ [hd remaining]), otherwise

|| ==========================================================================================================
|| toUpper
|| ==========================================================================================================

toUpper = error "Not implemented exception"

|| ==========================================================================================================
|| toLower
|| ==========================================================================================================

toLower = error "Not implemented exception"

|| ==========================================================================================================
|| startsWith
|| > Returns a boolean value to determine whether an input string starts with another
|| > Note that I've defined startsWith [] any as False, and startsWith any [] as True.
|| ==========================================================================================================

startsWith :: str -> str -> bool
startsWith string matcher
  = xstartsWith string matcher
    where
    xstartsWith string [] = True
    xstartsWith []     any = False
    xstartsWith (x:xs) (x:ys) = xstartsWith xs ys
    xstartsWith (x:xs) (y:ys) = False