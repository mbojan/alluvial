default:

%.md: %.Rmd
	Rscript -e 'knitr::knit("$<", output="$@")'

%.html: %.md
	pandoc -f markdown_github -o $@ $<

	
default: README.md

preview: README.html

.PHONY: default preview
