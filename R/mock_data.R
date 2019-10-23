offsets <- rnorm(38, mean = 1000, sd = 100)
baseline <- 100
hrs_by_sex <- rep(1:2, each = 19)
hrs_by_ag <- rep(seq(0.1, 1.9, by = 0.1), times = 2)
counts <- rpois(38, baseline * hrs_by_sex * hrs_by_ag)
pop <- round(rnorm(38, mean = 100000, sd = 100))

my_data <- data.frame(counts, pop, age = 1:19, sex = hrs_by_sex)
my_data$age <- factor(my_data$age, levels = 1:19, labels = c("0-4", "5-9", "10-14", "15-16", "17-19", "20-24", "25-29",
                               "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
                               "65-69", "70-74", "75-79", "80-84", "85"), ordered = T)

usethis::use_data(my_data)

