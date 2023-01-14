# College Basketball Free Throw Shooting

This project looks at the reliability of FT% in Division 1 Men's College Basketball. It explores how much a player's FT% in a given season reveals about their true talent for free throw shooting based on their number of free throw attempts. It also attempts to regress each player's observed FT% to the national average to approximate a player's free throw shooting skill.

Free Throw Shooting.R contains the code for loading all D1 Men's College Basketball play-by-play data from the 2017-18 season through the 2021-22 season. Also included is the code to read in roster data for each player, including their position, class, and conference. All the data loaded in this file are included as csv files in the Data folder.

Free Throw Shooting Report.Rmd contains the code for outputting a report for the analysis. The output is contained in the College Basketball Free Throw Shooting Report HTML file.

All_PbP_Stats.fst, All_FTs.fst, and PlayerSeasons.fst are datasets that result from running Free Throw Shooting.R. They are as follows:
- All_PbP_Stats.fst: All play-by-play records from 2017-18 to 2021-22.
- All_FTs.fst: All free throw attempts from 2017-18 to 2021-22.
- PlayerSeasons.csv: Free throw statistics and roster information for every player season from 2017-18 to 2021-22.
