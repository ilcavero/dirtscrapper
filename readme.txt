Club leaderboard scraper for the dirt 2 website.

To run:
1. In a terminal make sure you can run java -version, otherwise install the JVM
2. clone this repo, make it your current directory on a terminal
3. follow the instructions on postheader.txt or on credentials.txt
4. execute 'java -jar target/scala-2.13/dirtscrapper.jar 999 $OPTION' where 999 is your club id (if you don't know it open your club page and check in the url the number after /club/)
   for interactive mode, leave option empty
   if you know the championship and event id, use it them as option
   use auto as option to download the currently active event
   use autolast as option to download the previously active event
5. leaderboarddata.csv should now have been created in your dir