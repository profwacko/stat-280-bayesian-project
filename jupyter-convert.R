# Convert Joaq's Jupyter notebook to R Markdown
#install.packages("rmarkdown")
library(rmarkdown)
input <- "20191117_EDA.ipynb"

R::convert_ipynb(input, output = xfun::with_ext(input, "Rmd"))
