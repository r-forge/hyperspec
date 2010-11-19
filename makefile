VIGNETTES := baseline chondrocytes FileIO flu introduction laser plotting

ZIPS := Vignettes/*.zip

SRC := pkg/R/*.R pkg/NAMESPACE 

all: vignettes data www DESCRIPTION build check
		@echo all vignettes: $(VIGNETTES)

zip: $(ZIPS)
		@echo all zips: $(ZIPS)

.FORCE: # always done, like .PHONY, but doesn't remove file

.INTERMEDIATE: $(foreach V,$(VIGNETTES),Vignettes/$(V)/$(V).tex)
#.SECONDARY: Vignettes/*.zip

# VIGNETTES in subdirs ##############################################################################
vignettes: $(VIGNETTES) $(ZIPS) pkg/inst/doc/vignettes.defs pkg/inst/doc/*

Vignettes/%/vignettes.defs: Vignettes/vignettes.defs
	@cp -av $< $@ 

# baseline ..........................................................................................
baseline:                                         Vignettes/baseline/baseline.pdf

Vignettes/baseline/baseline.pdf:                  Vignettes/baseline/baseline.tex 

Vignettes/baseline/baseline.tex:                  Vignettes/baseline/baseline.Rnw 

Vignettes/baseline/baseline.Rnw:                  Vignettes/baseline/vignettes.defs $(SRC)
	touch $@

# chondrocytes ......................................................................................
chondrocytes:                                     Vignettes/chondrocytes/chondrocytes.pdf Vignettes/chondrocytes.zip

Vignettes/chondrocytes/chondrocytes.pdf:          Vignettes/chondrocytes/chondrocytes.tex 	 \
                                                  Vignettes/chondrocytes/080606d-flip-ausw.pdf

Vignettes/chondrocytes/chondrocytes.tex:          Vignettes/chondrocytes/chondrocytes.Rnw

Vignettes/chondrocytes/chondrocytes.Rnw:          Vignettes/chondrocytes/vignettes.defs $(SRC)    \
                                                  Vignettes/chondrocytes/rawdata/* 
	touch $@

Vignettes/chondrocytes/*.rda:	                    Vignettes/chondrocytes/chondrocytes.Rnw
	cd $(dir $<) && R CMD Sweave $(notdir $<)

# FileIO ............................................................................................
FileIO:                                           Vignettes/FileIO/FileIO.pdf Vignettes/FileIO.zip

Vignettes/FileIO/FileIO.pdf:                      Vignettes/FileIO/FileIO.tex 			 
Vignettes/FileIO/FileIO.tex:                      Vignettes/FileIO/FileIO.Rnw

Vignettes/FileIO/FileIO.Rnw:                      Vignettes/FileIO/vignettes.defs $(SRC)    \
                                                  Vignettes/FileIO/scan.txt.PerkinElmer.R   \
                                                  Vignettes/FileIO/txt.t/Triazine\ 5_31.txt \
                                                  Vignettes/FileIO/ENVI/example2.img 		 \
                                                  Vignettes/FileIO/spc/*.SPC 					 \
                                                  Vignettes/FileIO/spc.Kaisermap/*.spc 		 \
                                                  Vignettes/FileIO/txt.Renishaw/* 
	touch $@

Vignettes/FileIO/*.rda:                           Vignettes/FileIO/FileIO.Rnw
	cd $(dir $<) && R CMD Sweave $(notdir $<)

# flu ...............................................................................................
flu:                                              Vignettes/flu/flu.pdf

Vignettes/flu/flu.pdf:          						  Vignettes/flu/flu.tex 	 

Vignettes/flu/flu.tex:          						  Vignettes/flu/flu.Rnw

Vignettes/flu/flu.Rnw:          						  Vignettes/flu/vignettes.defs $(SRC)    \
                                                  Vignettes/flu/rawdata/* \
                                                  Vignettes/flu/scan.txt.PerkinElmer.R
	touch $@

Vignettes/flu/scan.txt.PerkinElmer.R:             Vignettes/FileIO/scan.txt.PerkinElmer.R
	cp -av $< $@

Vignettes/flu/*.rda:  	                          Vignettes/flu/flu.Rnw
	cd $(dir $<) && R CMD Sweave $(notdir $<)

# introduction ......................................................................................
introduction:                                     Vignettes/introduction/introduction.pdf 

Vignettes/introduction/introduction.pdf:          Vignettes/introduction/introduction.tex 	 \
                                                  Vignettes/introduction/hyperSpec-schema-annotated.pdf \
                                                  Vignettes/introduction/strukturhyperspec.pdf \
                                                  Vignettes/introduction/hyperSpec-schema-annotated.pdf \
                                                  Vignettes/introduction/introduction.bib \
                                                  Vignettes/introduction/functions.RData 

Vignettes/introduction/introduction.tex:          Vignettes/introduction/introduction.Rnw

Vignettes/introduction/introduction.Rnw:          Vignettes/introduction/vignettes.defs $(SRC)    \
                                                  Vignettes/introduction/rawdata/* \
                                                  Vignettes/introduction/functions.RData
	touch $@

Vignettes/introduction/*.rda:	                    Vignettes/introduction/introduction.Rnw
	cd $(dir $<) && R CMD Sweave $(notdir $<)

# laser .............................................................................................
laser:                                     	  	  Vignettes/laser/laser.pdf 

Vignettes/laser/laser.pdf:          				  Vignettes/laser/laser.tex 	 

Vignettes/laser/laser.tex:          				  Vignettes/laser/laser.Rnw

Vignettes/laser/laser.Rnw:          				  Vignettes/laser/vignettes.defs $(SRC)    \
                                                  Vignettes/laser/rawdata/* \
                                                  Vignettes/laser/par3d.Rdata 
	touch $@

Vignettes/laser/*.rda:	                          Vignettes/laser/laser.Rnw
	cd $(dir $<) && R CMD Sweave $(notdir $<)

# plotting ..........................................................................................
plotting:                                     	  Vignettes/plotting/plotting.pdf 

Vignettes/plotting/plotting.pdf:          		  Vignettes/plotting/plotting.tex 	 

Vignettes/plotting/plotting.tex:          		  Vignettes/plotting/plotting.Rnw

Vignettes/plotting/plotting.Rnw:          		  Vignettes/plotting/vignettes.defs $(SRC)    \
                                                  Vignettes/plotting/par3d.Rdata 
	touch $@

Vignettes/plotting/par3d.Rdata:                   Vignettes/laser/par3d.Rdata
	cp -av $< $@

# zip files .........................................................................................

Vignettes/%.zip: .FORCE
	cd $(dir $@) && echo `zip -u $(notdir $@) || [ "$$?" -ne 12 ]` # error 12 is "nothing to do"

# general rules .....................................................................................
%.tex: %.Rnw
	cd $(dir $<) && R CMD Sweave $(notdir $<) 

%.pdf: %.tex
	cd $(dir $<) && latexmk -pdf $(basename $(notdir $<)).tex

# data ##############################################################################################
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

pkg/inst/doc/flu.Rnw: Vignettes/flu/flu.Rnw \
                      pkg/inst/doc/rawdata/flu?.txt \
                      pkg/inst/doc/scan.txt.PerkinElmer.R 
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

pkg/inst/doc/introduction.bib: Vignettes/introduction/introduction.bib 
	@cp -av $< $@

pkg/inst/doc/strukturhyperspec.pdf: Vignettes/introduction/strukturhyperspec.pdf
	@cp -av $< $@


pkg/inst/doc/laser.Rnw: Vignettes/laser/laser.Rnw \
                        pkg/inst/doc/par3d.Rdata \
                        pkg/inst/doc/rawdata/laser.txt
	@cp -av $< $@
pkg/inst/doc/par3d.Rdata: Vignettes/laser/par3d.Rdata
	@cp -av $< $@
pkg/inst/doc/rawdata/laser.txt: Vignettes/laser/rawdata/laser.txt 
	@cp -av $< $@


# www ###############################################################################################
www: www/*.zip www/*.pdf

www/%.pdf: Vignettes/%/%.pdf
	@cp -av $< $@

www/hyperSpec-prebuilt.zip: # built manually 
	touch $@

www/chondrocytes.zip: Vignettes/chondrocytes.zip
	@cp -av $< $@

www/FileIO.zip: Vignettes/FileIO.zip
	@cp -av $< $@


#####################################################################################################




DESCRIPTION: $(shell find pkg -maxdepth 1 -daystart -not -ctime 0 -name "DESCRIPTION") #only if not modified today
	@echo update DESCRIPTION
	sed "s/\(^Version: .*-\)20[0-9][0-9][0-1][0-9][0-3][0-9]\(.*\)$$/\1`date +%Y%m%d`\2/" pkg/DESCRIPTION > .DESCRIPTION
	sed "s/\(^Date: .*\)20[0-9][0-9]-[0-1][0-9]-[0-3][0-9]\(.*\)$$/\1`date +%F`\2/" .DESCRIPTION > pkg/DESCRIPTION 
	rm .DESCRIPTION

build: DESCRIPTION $(SRC) vignettes
	R CMD build pkg  && mv hyperSpec_*.tar.gz www/hyperSpec-prebuilt.tar.gz

devbuild: DESCRIPTION $(SRC) vignettes
	~/r-devel/bin/R CMD build pkg && mv hyperSpec_*.tar.gz www/hyperSpec-prebuilt-devel.tar.gz

winbuild: .FORCE
	cd www && ftp -n -d win-builder.r-project.org << EOT
user anonymous cbeleites@units.it
cd R-release
put hyperSpec-prebuilt.tar.gz
bye
EOT

windevbuild: .FORCE
	cd www && ftp -n -d win-builder.r-project.org << EOT
user anonymous cbeleites@units.it
cd R-devel
put hyperSpec-prebuilt.tar.gz
bye
EOT

check: $(SRC)
	R CMD check pkg

devcheck: $(SRC)
	~/r-devel/bin/R CMD check pkg

checkfast: $(SRC)
	R CMD check --no-examples --no-tests --no-manual --no-vignettes pkg

checktest: $(SRC)
	R CMD check --no-manual --no-vignettes pkg

clean: .FORCE
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
