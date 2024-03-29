From: Douglas, David C <ddouglas@usgs.gov>
Sent: Thursday, May 14, 2020 5:08 PM
To: Martin, Kate H <kate_martin@fws.gov>; Christie, Katherine S (DFG) <katie.christie@alaska.gov>
Cc: Bradley, Catherine A <catherine_bradley@fws.gov>; Flint, Paul L <pflint@usgs.gov>; Graff, Nathan R <nathan_graff@fws.gov>; Rizzolo, Daniel J <daniel_rizzolo@fws.gov>
Subject: Re: [EXTERNAL] FW: quick question for you - are you around this afternoon?
 
Hi Kate,

Tuesday afternoon would work very well for me too.  I've limited experience setting up Teams, but I will attempt to invite you all to a meeting on Tuesday afternoon; 

does 1:30 PM sound OK?

In the meantime, Dan/Nathan, sign up for a NASA Earthdata account if you don't already have one:

https://urs.earthdata.nasa.gov/users/new

My goal will be to have one or both of you downloading and processing a subset of sea ice data; after that, it's just the same thing over and over.

I presume you have R installed.

You will need a free utility program called "wget".  It does not require Admin privileges (there is no installation), it's just a stand alone program.
You can get a Windows 10 binary here:  https://eternallybored.org/misc/wget/  Looks like the latest 64-bit EXE has had some SSL improvements made.
I'd probably get that one (v 1.20.3).
I store my wget.exe in a folder named:  C:\progs\wget\wget.exe

After you save a copy of wget.exe somewhere on your PC, create an empty file in that directory named exactly:   mycookies.txt

Then open a CMD (DOS) window, and CD into that directory with wget.exe and mycookies.txt, and issue the command below, AFTER you make changes specific for you (red font).

The first change defines a directory (must exist) on your PC where you want to store the raw sea ice data files (will be close to 4 Gb to download them all).
The second changes are to replace your Earthdata Username and Password

.\wget.exe -O 

D:\seaice\ssmi\bootstrap_v31\raw_nsidc_binary

\bs20181225.bin --http-user=

YOUR_EARTHDATA_USER

 --http-password=

YOUR_EARTHDATA_PASS 

--load-cookies mycookies.txt --save-cookies mycookies.txt --keep-session-cookies --no-check-certificate --auth-no-challenge -r --reject "index.html*" -np -e robots=off https://n5eil01u.ecs.nsidc.org/PM/NSIDC-0079.003/2018.12.25/bt_20181225_f17_v3.1_n.bin

Let me know if you successfully download one file using the command above.  If you meet with success, you're more than 90% of the way done.  The rest is just a couple of simple R scripts...

Dave

#**
.\wget.exe -O C:\nsdic_sea_ice\bs20181225.bin --http-user=drizzolo --http-password=P1ncheEarthdata! --load-cookies mycookies.txt --save-cookies mycookies.txt --keep-session-cookies --no-check-certificate --auth-no-challenge -r --reject "index.html*" -np -e robots=off https://n5eil01u.ecs.nsidc.org/PM/NSIDC-0079.003/2018.12.25/bt_20181225_f17_v3.1_n.bin



#*********************
From: Douglas, David C <ddouglas@usgs.gov>
Sent: Wednesday, May 13, 2020 7:46 AM
To: Martin, Kate H <kate_martin@fws.gov>; Christie, Katherine S (DFG) <katie.christie@alaska.gov>
Cc: Bradley, Catherine A <catherine_bradley@fws.gov>; Flint, Paul L <pflint@usgs.gov>
Subject: Re: [EXTERNAL] FW: quick question for you - are you around this afternoon?
 
Hi Katie, Kate, and Cat, (and Paul Flint, cc'd here so he has a copy of the attached update),

Rather than messing around with my old Unix system that is presently shutdown at my office, I decided to do everything from home from scratch.
I downloaded all the new Version-3 raw "Bootstrap algorithm" daily sea ice concentration (SIC) data files from NSIDC (NASA's Earthdata server).  I created a 'bat' script that used 'wget' in DOS to download all raw the files (1979-2018). It took about 5 hours (all automated), and consumed 3.9 Gb of disk space (~13,000 files,  ~275 Kb each).
I wrote a short R script that converted all the raw binary files to GeoTif (took about 1 hour, all automated). Consumed ~650 Mb.
I wrote a short R script that extracted the sea ice concentration values (Nov-Apr) at the spec eider winter area (took about 15 minutes, all automated)
I manipulated the extracted data (#3) into Excel format for your use, and to compare with values used in prior analyses.
Recall I said the SIC values in the new Version-3 processing would be expected to be slightly different from those used in prior analyses (early SIC processing versions). The attached Excel file not only contains the updated SIC values you are interested in (Column "J", min4_bs3), it also contains all the prior SIC values for comparison.  The new version-3 values with have "bs3" in the variable name, while the old values have "bs0" in the variable name.

The SIC value that has been used in eider models is the minimum SIC value among 4 pixels that span the core winter area (see the map in the second Excel worksheet). In the Excel file, that SIC value is in Column "J" for the new updated data (blue bold font), and Column "I" for the old values (red bold font).  You can browse around and see that the values are often slightly different, and that there is greater agreement among the more recent years.

The calculation of "number of days with persistent heavy ice cover" required a minimum SIC threshold (I don't recall offhand what that was... probably 95% or 90%). It raises a question if the same threshold will be robust with the new Version-3 SIC processing, especially during the early years where it appears there's a greater disparity between the old and new SIC values.

So, the attached Excel file gets you results through December 2018.

I contacted the National Snow and Ice Center, and they informed me that this "Bootstrap Algorithm" SIC time series will be updated through 2019 in about 2 months. Good!

I am happy to get you an update when 2019 is released; it will be very straight forward now that steps #1-3 have working scripts.

However, I'm also happy to show any or all of you how to implement steps #1-3 yourself.  I think a 20-30 minute "MS Teams" meeting of sorts would be sufficient for me to give you a walk-through tutorial.  I'm offering to empower you so when the next eider analysis is getting underway five years from now, you won't be slowed down because I had retired in the meantime.  Let me know if this is something you'd like to learn -- I have a fairly flexible schedule.

Best regards,

Dave