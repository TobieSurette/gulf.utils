clean.character <- function(x, ...){
   # Fix odd characters:
   x <- gsub('\xeb', " ", x)  
   x <- gsub('\xf8C', " ", x)
   x <- gsub('\xb0C', " ", x)
   x <- gsub('\xee', "i", x)  
   x <- gsub('\xfb', "u", x)  
   x <- gsub('\xce', "I", x) 
   x <- gsub('\xc9', "E", x) 
   x <- gsub('\xf4', "a", x) 
   x <- gsub('\xe0', "a", x) 
   x <- gsub('\xe9', "e", x)
   x <- gsub('\xe8', "e", x)  
   x <- gsub('\"+', " ", x)
   
   return(x)
}
