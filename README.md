# Scheduling R to run from the command line
Last year, I built an interactive visualization in Tableau that allows users to [create spray charts for every ball hit into play since 2010](https://public.tableau.com/profile/billpetti#!/vizhome/SprayChartComparison/SprayandStrikeZonePlot). Here's a sample of what can be generated:

![alt text](https://github.com/BillPetti/Spray-Chart-Tool/blob/master/images/Tableau_screenshot.png?raw=true)

I use data generated by MLB's Game Day application (essentially, [PITCHf/x](http://www.sportvision.com/baseball/pitchfx®)) and scraped by [Baseball Heat Maps](http://www.baseballheatmaps.com) on a daily basis. It's a little too laborious to update the tool on a daily basis since there is currently no way to schedule Tableau Public to refresh it's data and republish, so during the season I committed to updating the tool every Monday morning.

To make life even easier, I wanted to schedule the database query required for updating the tool to run automatically and for the new data to be merged into the existing master data file. That way, all I needed to do was open Tableau and refresh the data manually. I use a Windows PC for this particular task, so I needed a method that leveraged Window's Task Scheduler to run R from the command line. I searched around and found [this great little tutorial](https://trinkerrstuff.wordpress.com/2015/02/11/scheduling-r-tasks-via-windows-task-scheduler/) from Tyler Rinker, but when I tried to execute it I kept running into issues where the query would fail and the script would terminate. After some searching, I found what I believe to be the answer.

Here's how to do it:

Write your Rscript
------------------
The first thing you need to do, obviously, is write and save the R code that you want to run. You can find my R script in [this repository](https://github.com/BillPetti/Spray-Chart-Tool), so I won't go through the entire script here. However, I want to highlight a few things.

First, and what I think is most important, you have to require the methods package at the very outset of your script.

``` r
require(methods)
```

Thanks to [this helpful commentator](http://stackoverflow.com/a/19468533/3987834), I learned that this is necessary because Rscript doesn't automatically load the methods package when running from the command line. 

I run a query that looks for any new batted ball data hit in the past seven days:

``` r
require(methods)
require(RMySQL)
require(dplyr)

con<-dbConnect(RMySQL::MySQL(), dbname="x", username = "xx", password = "xxx", host = "xxxx", port = 3306)

res<-dbGetQuery(con, "SELECT ... date BETWEEN (CURRENT_DATE() - INTERVAL 8 DAY) AND (CURRENT_DATE() - INTERVAL 1 DAY)")
```
The actually query is pretty long, so I've truncated it here. 

After my SQL query runs, I reformat some columns of data, recode a bunch of variables (like batted ball type, etc.), and then I bind the new data and merge it with the existing data file:

``` r
res2<-rbind(old_data, new_data)
```

Then the data is exported as a csv file and writes over the existing file, which is necessary for your Tableau visualization to not break when refreshing data.

Creat a Batch File
------------------
Second, in order to get your Rscript to run you need to create a batch file. The batch file essentially tells the command line to run the Rscript using R. It's a simple little line of code:

``` batch
@echo off 
"C:\Program Files\R\R-3.1.3\bin\R.exe" CMD BATCH "[whatever your file path is]\spraychartweeklyupdate.R"
```

Here, I simply include the file path to my version of R and then the file path for the Rscript I want executed.

Using the Task Scheduler
------------------------
Finally, we need to schedule the batch file to run. In my case, I wanted the data to update every Monday morning at 5:30 am.

Open up the task scheduler and on the righthand side, click on Create Basic Task: 

![alt text](https://github.com/BillPetti/Spray-Chart-Tool/blob/master/images/Task_Scheduler_1.JPG?raw=true)

Give the task a name and quick description. Next, tell the scheduler at what interval you want to run the task. In this case, I run it weekly on Mondays at 5:30 am. 

![alt text](https://github.com/BillPetti/Spray-Chart-Tool/blob/master/images/Task_Scheduler_2.JPG?raw=true)

Next, tell it that you want to Start a Program and find the file path to your batch file. Click Finish and you are done!
