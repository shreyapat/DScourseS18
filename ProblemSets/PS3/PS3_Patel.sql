-- ******************
-- Import data
-- ******************
-- PolicyID, statecode, county, eq_site_limit, hu_site_limit, fl_site_limit, tiv_2011, tiv_2012, eq_site_deductible
-- hu_448094,FL,CLAY COUNTY,1322376.3,1322376.3,1322376.3,1322376.3,1322376.3,1438163.57,0,0,0,0,30.063936,-81.707664,
.print ' '
.print 'Importing data'
-- First, create the table that the CSV will be stored in
CREATE TABLE "florida" (
	PolicyID CHAR(25),
	statecode CHAR(2),
	county CHAR(25),
	eq_site_limit INT,
	hu_site_limit INT,
  fl_site_limit INT,
	tiv_2011 INT,
  tiv_2012 INT,
	eq_site_deductible INT,
	hu_site_deductible INT,
	fl_site_deductible INT,
	fr_site_deductible INT,
	point_latitude INT,
	linee INT,
	construction INT,
);

-- Tell SQL to expect a CSV file by declaring CSV mode
.mode csv

-- Next, import the CSV following the directions on the sqlitetutorial website
.import FL_insurance_sample.csv florida

-- Drop the header row
DELETE FROM florida WHERE PolicyID = 'PolicyID';




-- ******************
-- View first 10 observations
-- ******************
.print ' '
.print 'View first 10 observations'
-- View first 10 observations
SELECT * FROM florida LIMIT 10;

-- View first 10 observations of only a set of variables (Season, Wscore, Lscore, Wloc)
-- don't need SELECT Season, Wscore, Lscore, Wloc FROM basketball LIMIT 10;




-- ******************
-- How many unique values of a certain variable?
-- ******************
.print ' '
.print 'Unique values'
-- Number of unique counties in the data (lists a number) below
--SELECT count(distinct county) from florida;
-- or lists each one in a separate line
SELECT DISTINCT county FROM florida;
-- or lists each one in a separate line with counts below
--SELECT florida, COUNT(*) FROM basketball GROUP BY Season;




-- ******************
-- Average margin of victory?
-- ******************
.print ' '
.print 'Property appreciation'
-- Create new column which is the Wscore-Lscore difference, then find the average of it
SELECT AVG(tiv_2012) FROM florida;
SELECT AVG(tiv_2011) FROM florida;
SELECT  AVG(tiv_2012-tiv_2011) FROM florida;


-- ******************
-- Distribution of categories
-- ******************
.print ' '
.print 'Categorical distribution'
-- Frequency table of NumOT
SELECT construction, COUNT(*) FROM florida GROUP BY construction;


-- ******************
-- Save as text file
-- ******************
.output florida.sqlite3
.dump


-- ProTip: You can execute this file from the Linux command line by issuing the following:
-- sqlite3 < SQLexample.sql > SQLexample.sqlog
