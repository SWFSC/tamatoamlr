#-------------------------------------------------------------------------------
pinniped.afs <- stringr::str_to_sentence("fur seal")
pinniped.phocid <- stringr::str_to_sentence(
  c("crabeater seal", "elephant seal", "leopard seal", "weddell seal")
)
pinniped.study <- stringr::str_to_sentence(
  c("fur seal", "elephant seal", "leopard seal", "weddell seal")
)

## code to prepare `pinniped.sp` dataset goes here
pinniped.sp <- c(pinniped.afs, pinniped.phocid)
# pinniped.sp.list <- as.list(pinniped.sp)
names(pinniped.sp) <- pinniped.sp
usethis::use_data(pinniped.sp, overwrite = TRUE)


## code to prepare `pinniped.sp.colors` dataset goes here
pinniped.sp.colors <- scales::hue_pal()(5)
names(pinniped.sp.colors) <- names(pinniped.sp)
usethis::use_data(pinniped.sp.colors, overwrite = TRUE)


## code to prepare `pinniped.phocid.sp` dataset goes here
pinniped.phocid.sp <- pinniped.sp[pinniped.sp %in% pinniped.phocid]
usethis::use_data(pinniped.phocid.sp, overwrite = TRUE)


## code to prepare `pinniped.sp.study` dataset goes here
pinniped.sp.study <- pinniped.sp[pinniped.sp %in% pinniped.study]
usethis::use_data(pinniped.sp.study, overwrite = TRUE)


## code to prepare `afs.study.beach.counts` dataset goes here
# Pup sum is added/handled in study beach census module
afs.study.beach.counts <- c(
  "pup_live_count", "pup_dead_count", "ad_female_count", "ad_male_sum",
  "juv_female_count", "juv_male_count", "juv_unk_count",
  "ad_male_count", "adult_male_terr_count",
  "adult_male_terr_wFem_count", "adult_male_terr_noFem_count",
  "adult_male_non_terr_count", "adult_male_unk_count", "ad_unk_count"
)

usethis::use_data(afs.study.beach.counts, overwrite = TRUE)



#-------------------------------------------------------------------------------
## prepare 'csphoc.core.locations'
library(dplyr)
library(odbc)
con <- odbc::dbConnect(odbc(), filedsn = "../dsn/amlr-pinniped-db-prod.dsn")
beaches <- tbl(con, "beaches") %>% collect()
csphoc.core.location.groups <- c(
  "Media Luna", "Punta Yuseff", "Larga", "Marko", "Daniel", "Modulo",
  "Hue", "Copi", "Maderas", "Cachorros", "Chungungo",
  "Ballena Sur", "Ballena Norte", "Bahamonde", "Nibaldo", "Roquerio",
  "Alcazar", "Pinochet de la Barra", "Papua", "Antartico",
  "Loberia", "Yamana", "Golondrina", "del Lobero", "Paulina",
  "Schiappacasse", "El Plastico", "Leopard Beach", "del Canal",
  "Aranda", "Remanso", "Golondrina-del Lobero", "Paulina-Aranda",
  "Cape Shirreff", "Copihue", "Peninsula Cerro Gajardo",
  "Punta Las Torres", "Penguin Colonies"
)
stopifnot(all(csphoc.core.location.groups %in% beaches$name))
# csphoc.core.location.groups[!(csphoc.core.location.groups %in% beaches$name)]
waldo::compare(tamatoamlr::csphoc.core.location.groups,
               csphoc.core.location.groups)

# Sanity check
z <- tbl(con, "vCensus_Phocid") %>%
  select(location_group) %>%
  collect() %>%
  distinct() %>%
  pull(1)
sort(unique(z[!(z %in% csphoc.core.location.groups)]))

usethis::use_data(csphoc.core.location.groups, overwrite = TRUE)

#-------------------------------------------------------------------------------
