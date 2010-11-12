#all: def .pdf
VIGNETTES := flu baseline chondrocytes FileIO introduction laser plotting
ZIPS := $(shell find -name "*.zip" -type f)
SRC := $(shell find pkg -type f)
#Vignettes: defs

# flu: defs Vignettes/flu/flu.pdf
# 	@echo $@
# 	cp Vignettes/flu/flu.Rnw  pkg/inst/doc/
# 	cp Vignettes/flu/flu.pdf  pkg/inst/doc/
all: vignettes data www DESCRIPTION check build
#		@echo all vignettes: $(VIGNETTES)

.FORCE: # always done, like .PHONY, but doesn't remove file

.INTERMEDIATE: $(foreach V,$(VIGNETTES),Vignettes/$(V)/$(V).tex)
#.SECONDARY: Vignettes/*.zip

# VIGNETTES in subdirs ##############################################################################
vignettes: $(VIGNETTES) $(filter "Vignettes/", $(ZIPS))
	@echo $(ZIPS)

# all vignettes depend on their pdf
$(VIGNETTES): $(foreach V,$(VIGNETTES),Vignettes/$(V)/$(V).pdf)

# single dependencies of particular vignettes
Vignettes/FileIO/FileIO.Rnw: Vignettes/FileIO/scan.txt.PerkinElmer.R
#	touch $@

# all vignette .Rnw files depend on vignettes.defs
$(foreach V,$(VIGNETTES),Vignettes/$(V)/$(V).Rnw): $(foreach V,$(VIGNETTES),Vignettes/$(V)/vignettes.defs) 
#	touch $@

#Vignettes/*.zip: FORCE

Vignettes/%.zip: `zip -sf $@`
	echo "$@ before" 
	cd $(dir $@) && zip -f -v $(notdir $@)
	echo "$@ done" 

Vignettes/flu/flu.Rnw: Vignettes/flu/scan.txt.PerkinElmer.R 
#	touch $@

Vignettes/flu/scan.txt.PerkinElmer.R: Vignettes/FileIO/scan.txt.PerkinElmer.R 
	@cp -av Vignettes/FileIO/scan.txt.PerkinElmer.R Vignettes/flu/

Vignettes/introduction/introduction.tex: Vignettes/introduction/strukturhyperspec.pdf
	touch $@
Vignettes/introduction/strukturhyperspec.pdf: Vignettes/introduction/strukturhyperspec.tex
	cd $(dir $<) && latexmk -pdf $(notdir $<) 
Vignettes/introduction/introduction.Rnw: Vignettes/introduction/functions.RData Vignettes/flu/vignettes.defs

Vignettes/%/vignettes.defs: Vignettes/vignettes.defs
	@cp -av $< $@ 

# for Rnw -> pdf conversion, first Sweave, then use latexmk which takes care of .bib etc.
%.pdf: %.Rnw
	cd $(dir $<) && R CMD Sweave $(notdir $<) && latexmk -pdf $(basename $(notdir $<)).tex

# data ##############################################################################################
instdoc: pkg/inst/doc/*.Rnw 
	@echo $?

data: pkg/data/*.rda

pkg/data/chondro.rda:     Vignettes/chondrocytes/chondro.rda 
	@cp -av $< $@
pkg/data/barbituates.rda: Vignettes/FileIO/barbituates.rda 
	@cp -av $< $@
pkg/data/flu.rda:         Vignettes/flu/flu.rda 
	@cp -av $< $@
pkg/data/paracetamol.rda: Vignettes/introduction/paracetamol.rda 
	@cp -av $< $@
pkg/data/laser.rda:       Vignettes/laser/laser.rda
	@cp -av $< $@

# dependencies of .rda files on .Rnw files: no need for rule in general, as .Rnws are sweaved anyways

# instdoc ###########################################################################################
pkg/inst/doc/vignettes.defs: Vignettes/vignettes.defs
	@cp -av $< $@ 
	@touch pkg/inst/doc/*.Rnw

pkg/inst/doc/baseline.Rnw: Vignettes/baseline/baseline.Rnw
	@cp -av $< $@

pkg/inst/doc/chondrocytes.pdf: Vignettes/chondrocytes/chondrocytes.pdf
	@cp -av $< $@

pkg/inst/doc/FileIO.pdf: Vignettes/FileIO/FileIO.pdf     
	@cp -av $< $@

pkg/inst/doc/flu.Rnw: Vignettes/flu/flu.Rnw pkg/inst/doc/rawdata/flu?.txt pkg/inst/doc/scan.txt.PerkinElmer.R 
	@cp -av $< $@
pkg/inst/doc/rawdata/flu%.txt: Vignettes/flu/rawdata/flu%.txt
	@cp -av $< $@
pkg/inst/doc/scan.txt.PerkinElmer.R: Vignettes/flu/scan.txt.PerkinElmer.R 
	@cp -av $< $@


pkg/inst/doc/introduction.Rnw: Vignettes/introduction/introduction.Rnw \
                               pkg/inst/doc/functions.RData \
                               pkg/inst/doc/introduction.bib \
                               pkg/inst/doc/strukturhyperspec.pdf
	@cp -av $< $@
pkg/inst/doc/functions.RData: Vignettes/introduction/functions.RData
	@cp -av $< $@
#pkg/inst/doc/hyperSpec-schema-annotated.pdf: Vignettes/introduction/hyperSpec-schema-annotated.pdf
pkg/inst/doc/introduction.bib: Vignettes/introduction/introduction.bib 
	@cp -av $< $@
pkg/inst/doc/strukturhyperspec.pdf: Vignettes/introduction/strukturhyperspec.pdf
	@cp -av $< $@


pkg/inst/doc/laser.Rnw: Vignettes/laser/laser.Rnw pkg/inst/doc/par3d.Rdata pkg/inst/doc/rawdata/laser.txt
	@cp -av $< $@
pkg/inst/doc/par3d.Rdata: Vignettes/laser/par3d.Rdata
	@cp -av $< $@
pkg/inst/doc/rawdata/laser.txt: Vignettes/laser/rawdata/laser.txt 
	@cp -av $< $@


# www
www: www/*.zip

www/%.pdf: Vignettes/%/%.pdf
	@cp -av $< $@

www/%.zip: Vignettes/%.zip #how do I do this only for the existing .zips? I.e. exclude hyperSpec-prebuilt.zip
	@cp -av $< $@

DESCRIPTION: $(shell find pkg -maxdepth 1 -daystart -not -ctime 0 -name "DESCRIPTION") #only if not modified today
	@echo update DESCRIPTION
	sed "s/\(^Version: .*-\)20[0-9][0-9][0-1][0-9][0-3][0-9]\(.*\)$$/\1`date +%Y%m%d`\2/" pkg/DESCRIPTION > .DESCRIPTION
	sed "s/\(^Date: .*\)20[0-9][0-9]-[0-1][0-9]-[0-3][0-9]\(.*\)$$/\1`date +%F`\2/" .DESCRIPTION > pkg/DESCRIPTION 
	rm .DESCRIPTION

build: DESCRIPTION $(SRC)
	R CMD build pkg

check: $(SRC)
	R CMD check pkg

checkfast: $(SRC)
	R CMD check --no-examples --no-tests --no-manual --no-vignettes pkg

checktest: $(SRC)
	R CMD check --no-manual --no-vignettes pkg

clean: FORCE
	@rm -f $(foreach V,$(VIGNETTES),Vignettes/$(V)/$(V).tex) 
	@rm -f $(foreach V,$(VIGNETTES),Vignettes/$(V)/*.aux) 
	@rm -f $(foreach V,$(VIGNETTES),Vignettes/$(V)/*.toc) 
	@rm -f $(foreach V,$(VIGNETTES),Vignettes/$(V)/*.log) 
	@rm -f $(foreach V,$(VIGNETTES),Vignettes/$(V)/*.out) 
	@rm -f $(foreach V,$(VIGNETTES),Vignettes/$(V)/*.bbl) 
	@rm -f $(foreach V,$(VIGNETTES),Vignettes/$(V)/*.blg) 
	@rm -f $(foreach V,$(VIGNETTES),Vignettes/$(V)/*.idx) 
	@rm -f $(foreach V,$(VIGNETTES),Vignettes/$(V)/*.ilg) 
	@rm -f $(foreach V,$(VIGNETTES),Vignettes/$(V)/*.ind) 
	@rm -f $(foreach V,$(VIGNETTES),Vignettes/$(V)/*.fdb_latexmk) 
	@rm -f $(foreach V,$(VIGNETTES),Vignettes/$(V)/Rplots.pdf) 
	@rm -f pkg/inst/doc/*.aux
	@rm -f pkg/inst/doc/*.log
	@rm -f pkg/inst/doc/*.toc
	@rm -f pkg/inst/doc/*.out
	@rm -f pkg/inst/doc/*.bbl
	@rm -f pkg/inst/doc/*.blg
	@rm -f pkg/inst/doc/*.idx 
	@rm -f pkg/inst/doc/*.ilg 
	@rm -f pkg/inst/doc/*.ind
	@rm -f pkg/inst/doc/Rplots.pdf
	@rm -f pkg/inst/doc/*.fdb_latexmk
#	@rm -f $(patsubst %.Rnw,%.tex,$(wildcard pkg/inst/doc/*.Rnw)) #should not be necessary
