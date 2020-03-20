# Activity_Recognition
Machine Learning Course Project Q1 2020

In an attempt to allow others to run the code for this project I have organized it all into this 
git repository. If the wisdm-dataset file is added to a directory that contains the files and folders
above you should be able to run all the code provided. Note that in order to compile the latex pdf 
you must use the provided function 'mkdoc' with 'report.rnw' as the file name. 

When it comes to the font family used in visualisation.R and features_models.R, you must first add the 
font file provided under /fonts/ to your local machines font library. This can be done by navigating 
to C:/Windows/Fonts/ and dropping the file into this directory. 

It is also important that 
'''' 
windowsFonts(`LM Roman 10` = windowsFont('LM Roman 10'))
''''
is run before loading the neceessary libraries. If this is not done, it will be necessary to restart
RStudio!
