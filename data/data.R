# Random adam datasets
data <- list(
  adsl = random.cdisc.data::cadsl,
  adae = random.cdisc.data::cadae,
  adlb = random.cdisc.data::cadlb %>% 
    mutate(LBTEST = stringr::str_remove(LBTEST,"Measurement")),
  advs = random.cdisc.data::cadvs
)


# MySQL database connection and data pull
db_user <- Sys.getenv("user")
db_password <- Sys.getenv("pwd")
db_name <- Sys.getenv("dbname")
db_host <- Sys.getenv("dbhost") # for local access
db_port <- 3306

# pool <- dbPool(
#   drv = RMySQL::MySQL(),
#   dbname = db_name,
#   host = db_host,
#   user = db_user,
#   password = db_password
# )
# 
# # con <-  dbConnect(MySQL(), 
# #                   user = db_user, 
# #                   password = db_password,
# #                   dbname = , 
# #                   host = db_host, 
# #                   port = db_port)
# 
# con <- poolCheckout(pool)
# on.exit(poolReturn(con), add = TRUE)
# 
# query <- paste0("SELECT * FROM eNotes;")
# enotes <- dbGetQuery(con, query)
enotes <- data.frame()
# on.exit(dbDisconnect(con))
