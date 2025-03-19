




rundir <- "C:/Users/malco/Dropbox/A_CodeR/test/"
filename="develo_of_ALpop.qmd"
title="The Package Development of ALSim"
author="Malcolm Haddon"
affiliation="IMAS, University of Tasmania"
department="IMAS-FA" 
city="Hobart"
DOCX=TRUE
HTML=TRUE
docref="wordreferencestyle.docx"


library(codeutils)
library(hplot)
library(qmdutils)


library(codeutils)
makeGitBook(rundir=rundir,filename=filename,
            author=author,affiliation=affiliation,
            department=department,city=city,DOCX=DOCX,HTML=HTML,
            docref=docref)







