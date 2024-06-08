export RSTUDIO_PANDOC=/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64

SOURCES=$(shell find . -name "*.Rmd")
TARGET = $(SOURCES:%.Rmd=%.pdf) $(SOURCES:%.Rmd=%.html) $(SOURCES:%.Rmd=%.nb.html) $(SOURCES:%.Rmd=%.docx)

%.docx %.nb.html %.html %.pdf: %.Rmd
	Rscript -e "Sys.setenv(RSTUDIO_PANDOC='$(RSTUDIO_PANDOC)'); rmarkdown::render('$<', output_format = 'all')"

default: $(TARGET)

clean:
	rm -rf $(TARGET)

check-pandoc:
	Rscript -e "cat('RSTUDIO_PANDOC:', Sys.getenv('RSTUDIO_PANDOC'), '\n')"
