# Vaccine-Tracker
Pulling CDC data on state vaccination progress. Three main R scripts:

* `initial_setup.R` adds vaccination totals for the US from Our World in Data, since the CDC does not provide historical counts. It was run only once at the beginning of the project.
* `vaccine_tracker.R` pulls a snapshot of vaccination data from the CDC's website and adds it to a local database of historical vaccination data. It also exports to Google Sheets where a Tableau Public dashboard is updated daily.
* `scrape_cvs.R` extracts a dataframe of vaccinations by state from a daily PDF for CVS' long-term care vaccination program.
* `schedule.R` initiates a local cron job to execute the main data collection script.
