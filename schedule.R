library(cronR)

script <- "/Users/conorkelly/Documents/Vaccine-Tracker/vaccine_tracker.R"

cmd <- cron_rscript(script)
cron_clear()
cron_add(command = cmd, frequency = 'hourly',
         id = 'pull_cdc', description = 'pull CDC data', tags = c('cdc', 'R'))