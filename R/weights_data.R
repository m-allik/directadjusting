# weights data
# single year for 2013 ESP

age <- 0:99
esp2013_21 <- c(1000, rep(4000/4, 4), rep(5500/5, 15), rep(6000/5, 10), rep(6500/5, 5), rep(7000/5, 20),
                rep(6500/5, 5), rep(6000/5, 5), rep(5500/5, 5), rep(5000/5, 5), rep(4000/5, 5),
                rep(2500/5, 5), rep(1500/5, 5), rep(800/5, 5), rep(200/5, 5))
sum(esp2013_21)

st_population <- data.table(age, esp2013_21)

usethis::use_data(st_population, internal = TRUE)
