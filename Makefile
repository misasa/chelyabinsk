PACKAGENAME = $(shell basename `pwd`)
VERSION = 1.0.8
TARGET = $(PACKAGENAME)_$(VERSION).tar.gz
RFILE = $(wildcard R/*.R)
RDOCFILE = $(wildcard man/*.Rd)
DATAFILE= $(wildcard inst/extdata/*.*)

$(TARGET): DESCRIPTION NAMESPACE $(RDOCFILE) $(RFILE) $(DATAFILE)
	Rscript -e "devtools::document()"
	R CMD check ../$(PACKAGENAME)
	R CMD build ../$(PACKAGENAME)

.PHONY: rdoc
rdoc:
	Rscript -e "devtools::document()"

.PHONY: test
test:
	R CMD check ../$(PACKAGENAME)

.PHONY: build
build:
	R CMD build ../$(PACKAGENAME)

.PHONY: install
install:
	R CMD install $(TARGET)

.PHONY: push
push:
	git pull; git commit -a -m "Revision by make"; git push
	ssh falcon@database.misasa.okayama-u.ac.jp "sudo Rscript -e \"devtools::install_github('misasa/chelyabinsk')\""
	ssh falcon@database.misasa.okayama-u.ac.jp "sudo /etc/init.d/shiny-server restart"
