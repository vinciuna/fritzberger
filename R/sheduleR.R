
main.script <- here::here("R", "main_.R")
taskscheduleR::taskscheduler_create(taskname = "web_scrape_fritzberger", rscript = main.script,
                     schedule = "DAILY", starttime = "07:10", startdate = format(Sys.Date()+1, "%Y-%m-%d"))
# tasks <- taskscheduleR::taskscheduler_ls()
