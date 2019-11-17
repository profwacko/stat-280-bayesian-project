# Convert Joaq's Jupyter notebook to R Markdown
#install.packages("rmarkdown")
library(rmarkdown)
input <- "20191118_JOAQ_BART.ipynb"

rmarkdown:::convert_ipynb(input, output = xfun::with_ext(input, "Rmd"))

