all: index.html

index.html: ./index/index.Rmd
	R -e "rmarkdown::render('./index/index.Rmd', output_dir = '.')"