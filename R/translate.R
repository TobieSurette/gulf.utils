#' Translate Words
#'
#' @description Functions for translating common fishery and biological words and terms. These
#'              functions are intended for translating words and common short phrases only.
#'
#' @param x Character string(s).
#' @param plural Logical value specifying whether the plural form of a word is to be imposed.
#' @param feminine Logical value specifying whether the feminine form of a word is to be imposed.
#'
#' @examples
#' en2fr("male")
#' en2fr("crab")
#' en2fr("black")
#' en2fr("missing legs")
#' en2fr("fishing grids")
#' en2fr("survey")

#' @describeIn translate Translate from english to french.
#' @export
en2fr <- function(x, plural = FALSE, feminine = FALSE){
   # General:
   x <- gsub("crabs", "crabes", x)
   x <- gsub("crab", "crabe", x)
   x <- gsub(" males", " mâles", x)
   x <- gsub(" male", " mâle", x)
   x <- gsub("^males", "mâles", x)
   x <- gsub("^male", "mâle", x)
   x <- gsub("females", "femelles", x)
   x <- gsub("female", "femelle", x)

   # Fishery:
   x <- gsub("fleet", "flotille", x)
   x <- gsub("landings", "débarquements", x)
   x <- gsub("landing", "débarquement", x)
   x <- gsub("fishing", "pêche", x)
   x <- gsub("captain", "capitaine", x)
   x <- gsub("fisher", "pêcheur", x)
   x <- gsub("fishers", "pêcheurs", x)
   x <- gsub("fishing vessel", "bateau de pêche", x)

   # Science:
   x <- gsub("survey", "relevé", x)
   x <- gsub("surveys", "relevés", x)
   x <- gsub("stratified", "stratifié", x)
   x <- gsub("fishing grids", "quadrilatères de pêche", x)
   x <- gsub("grids", "quadrilatères", x)
   x <- gsub("grid", "quadrilatère", x)
   x <- gsub("logbook", "jounral de bord", x)

   # Sexual maturity and reproduction:
   x <- gsub("primiparous", "primipare", x)
   x <- gsub("multiparous", "primipare", x)
   x <- gsub("commercials", "commerciaux", x)
   x <- gsub("pubescent", "adolescent", x)
   x <- gsub("gonads", "gonades", x)
   x <- gsub("ovaries", "ovaires", x)
   x <- gsub("ovary", "ovaire", x)
   x <- gsub("testicles", "testicules", x)
   x <- gsub("testicle", "testicule", x)
   x <- gsub("eggs remaining", "oeufs restants", x)

   # Colours:
   x <- gsub("white", "blanc", x)
   x <- gsub("black", "noir", x)
   x <- gsub("brown", "brun", x)
   x <- gsub("light orange", "orange clair", x)
   x <- gsub("dark orange", "orange foncé", x)

   # Shell condition:
   x <- gsub("hard", "dur", x)
   x <- gsub("soft", "mou", x)
   x <- gsub("shell condition", "condition de carapace", x)
   x <- gsub("shell", "carapace", x)
   x <- gsub("missing legs", "pattes manquantes", x)
   x <- gsub("missing", "manquant", x)
   x <- gsub("leg", "patte", x)
   x <- gsub("appendage", "appendice", x)

   return(x)
}


