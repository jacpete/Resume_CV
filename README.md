# Jacob Peterson's Resume and CV

This repo contains the source-code and results of my CV and Resume built with R using the [pagedown package](https://pagedown.rbind.io) and [Nick Strayer's template](https://github.com/nstrayer/cv).

The main files are:

- cv_Peterson.Rmd: Source template for the cv, contains a variable PDF_EXPORT in the header that changes styles for pdf vs html.
- cv_Peterson.html: The final output of the template when the header variable PDF_EXPORT is set to FALSE.
- cv_Peterson.pdf: The final exported pdf as rendered by Chrome on my computer.
- positions.csv: A csv with columns encoding the various fields needed for a position entry in the CV. A column section is also available so different sections know which rows to use.
- resume.Rmd: Source template for the resume.
- resume_Peterson.pdf: The final exported pdf as rendered by Chrome on my computer.
- CourseDescriptions: Short descriptions of topics covered in some of the more technical courses covered in my college career.
- css/: Directory containing the custom CSS files used to tweak the default 'resume' format from pagedown.
