VIGNETTES := baseline chondrocytes FileIO flu introduction laser plotting

ZIPS := Vignettes/*.zip

SRC := pkg/R/*.R pkg/NAMESPACE 
DOC := pkg/man/*.Rd
RNW := pkg/inst/doc/src/*.Rnw


all: vignettes data www DESCRIPTION build check
		@echo all vignettes: $(VIGNETTES)

zip: $(ZIPS)
		@echo all zips: $(ZIPS)

.FORCE: # always done, like .PHONY, but doesn't remove file

.INTERMEDIATE: $(foreach V,$(VIGNETTES),Vignettes/$(V)/$(V).tex)
#.SECONDARY: Vignettes/*.zip

# VIGNETTES in subdirs ##############################################################################
vignettes: $(VIGNETTES) $(ZIPS) pkg/inst/doc/src/vignettes.defs pkg/inst/doc/src/* pkg/inst/doc/pdf/*

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
                                                  Vignettes/chondrocytes/080606d-flip-ausw.jpg

Vignettes/chondrocytes/chondrocytes.tex:          Vignettes/chondrocytes/chondrocytes.Rnw

Vignettes/chondrocytes/chondrocytes.Rnw:          Vignettes/chondrocytes/vignettes.defs $(SRC)    \
                                                  Vignettes/chondrocytes/rawdata/* 
	touch $@

Vignettes/chondrocytes/*.rda:	                    Vignettes/chondrocytes/chondrocytes.Rnw
	cd $(dir $<) && R CMD Sweave $(notdir $<) && \


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

Vignettes/flu/flu:          						  Vignettes/flu/vignettes.defs $(SRC)    \
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
                                                  Vignettes/introduction/functions.RData
#                                                  Vignettes/introduction/rawdata/paracetamol.txt.gz 
	touch $@

#Vignettes/introduction/rawdata/paracetamol.txt.gz: Vignettes/FileIO/txt.Renishaw/paracetamol.txt.gz
#	@cp -av $< $@ 

Vignettes/introduction/*.rda:	                    Vignettes/introduction/introduction.Rnw
	cd $(dir $<) && R CMD Sweave $(notdir $<)

# laser .............................................................................................
laser:                                     	  	  Vignettes/laser/laser.pdf 

Vignettes/laser/laser.pdf:          				  Vignettes/laser/laser.tex 	 

Vignettes/laser/laser.tex:          				  Vignettes/laser/laser.Rnw

Vignettes/laser/laser.Rnw:          				  Vignettes/laser/vignettes.defs $(SRC)    \
                                                  Vignettes/laser/rawdata/laser.txt.gz

Vignettes/laser/rawdata/laser.txt.gz:             Vignettes/FileIO/txt.Renishaw/laser.txt.gz
	cp -av $< $@

Vignettes/laser/*.rda:	                          Vignettes/laser/laser.Rnw
	cd $(dir $<) && R CMD Sweave $(notdir $<)

# plotting ..........................................................................................
plotting:                                     	  Vignettes/plotting/plotting.pdf 

Vignettes/plotting/plotting.pdf:          		  Vignettes/plotting/plotting.tex 	 

Vignettes/plotting/plotting.tex:          		  Vignettes/plotting/plotting.Rnw

Vignettes/plotting/plotting.Rnw:          		  Vignettes/plotting/vignettes.defs $(SRC)    
#                                                  Vignettes/plotting/par3d.Rdata 
	touch $@

#Vignettes/plotting/par3d.Rdata:                   Vignettes/laser/par3d.Rdata
#	cp -av $< $@

# zip files .........................................................................................

Vignettes/%.zip: .FORCE
	cd $(dir $@) && echo `zip -u $(notdir $@) || [ "$$?" -ne 12 ]` # error 12 is "nothing to do"

# general rules .....................................................................................
%.tex: %.Rnw
	cd $(dir $<) && R CMD Sweave $(notdir $<) 

%.pdf: %.tex
#	cd $(dir $<) && rubber --pdf -s $(basename $(notdir $<)).tex
	cd $(dir $<) && latexmk -pdf $(basename $(notdir $<)).tex 
	gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/ebook -dNOPAUSE -dQUIET -dBATCH \
	-sOutputFile=tmp.pdf $@ && mv tmp.pdf $@

%.dvi: # should not happen!
	rm $@
# data ##############################################################################################
data: pkg/data/*.rda

pkg/data/chondro-internal.rda:     Vignettes/chondrocytes/chondro-internal.rda 
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
pkg/inst/doc/src/vignettes.defs: Vignettes/vignettes.defs
	@cp -av $< $@ 
	@touch pkg/inst/doc/src/*.Rnw

pkg/inst/doc/src/baseline.Rnw: Vignettes/baseline/baseline.Rnw 
	@cp -av $< $@

pkg/inst/doc/src/flu.Rnw: Vignettes/flu/flu.Rnw \
                      pkg/inst/doc/src/rawdata/flu?.txt \
                      pkg/inst/doc/src/scan.txt.PerkinElmer.R 
	@cp -av $< $@

pkg/inst/doc/src/rawdata/flu%.txt: Vignettes/flu/rawdata/flu%.txt
	@cp -av $< $@

pkg/inst/doc/src/scan.txt.PerkinElmer.R: Vignettes/flu/scan.txt.PerkinElmer.R 
	@cp -av $< $@

pkg/inst/doc/src/introduction.Rnw: Vignettes/introduction/introduction.Rnw \
                               pkg/inst/doc/src/functions.RData \
                               pkg/inst/doc/src/introduction.bib \
                               pkg/inst/doc/src/strukturhyperspec.pdf
#                               pkg/inst/doc/src/rawdata/paracetamol.txt.gz
	@cp -av $< $@

#pkg/inst/doc/src/rawdata/paracetamol.txt.gz: Vignettes/introduction/rawdata/paracetamol.txt.gz
#	@cp -av $< $@

pkg/inst/doc/src/functions.RData: Vignettes/introduction/functions.RData
	@cp -av $< $@

pkg/inst/doc/src/introduction.bib: Vignettes/introduction/introduction.bib 
	@cp -av $< $@

pkg/inst/doc/src/strukturhyperspec.pdf: Vignettes/introduction/strukturhyperspec.pdf
	@cp -av $< $@


pkg/inst/doc/src/laser.Rnw: Vignettes/laser/laser.Rnw \
                        pkg/inst/doc/src/rawdata/laser.txt.gz

pkg/inst/doc/src/rawdata/laser.txt.gz: Vignettes/laser/rawdata/laser.txt.gz
	@cp -av $< $@

pkg/inst/doc/pdf/chondrocytes.pdf: Vignettes/chondrocytes/chondrocytes.pdf
	@cp -av $< $@

pkg/inst/doc/pdf/FileIO.pdf: Vignettes/FileIO/FileIO.pdf     
	@cp -av $< $@

pkg/inst/doc/pdf/baseline.pdf: Vignettes/baseline/baseline.pdf     
	@cp -av $< $@

pkg/inst/doc/pdf/flu.pdf: Vignettes/flu/flu.pdf     
	@cp -av $< $@

pkg/inst/doc/pdf/introduction.pdf: Vignettes/introduction/introduction.pdf     
	@cp -av $< $@

pkg/inst/doc/pdf/laser.pdf: Vignettes/laser/laser.pdf     
	@cp -av $< $@

pkg/inst/doc/pdf/plotting.pdf: Vignettes/plotting/plotting.pdf     
	@cp -av $< $@

# www ###############################################################################################
www: www/*.zip www/*.pdf

www/baseline.pdf: Vignettes/baseline/baseline.pdf
	@cp -av $< $@
www/chondrocytes.pdf: Vignettes/chondrocytes/chondrocytes.pdf
	@cp -av $< $@
www/FileIO.pdf: Vignettes/FileIO/FileIO.pdf
	@cp -av $< $@
www/flu.pdf: Vignettes/flu/flu.pdf
	@cp -av $< $@
www/introduction.pdf: Vignettes/introduction/introduction.pdf
	@cp -av $< $@
www/laser.pdf: Vignettes/laser/laser.pdf
	@cp -av $< $@
www/plotting.pdf: Vignettes/plotting/plotting.pdf
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

build: DESCRIPTION $(SRC) $(RNW) $(DOC) vignettes
	rm -f hyperSpec_*.tar.gz
	R CMD build pkg  && mv hyperSpec_*.tar.gz www/hyperSpec-prebuilt.tar.gz

devbuild: DESCRIPTION $(SRC) $(RNW) $(DOC) vignettes
	rm -f hyperSpec_*.tar.gz
	~/r-devel/bin/R CMD build pkg && mv hyperSpec_*.tar.gz www/hyperSpec-prebuilt-devel.tar.gz

#winbuild: .FORCE
#	cd www && ftp -n -d win-builder.r-project.org << EOT
#user anonymous cbeleites@units.it
#cd R-release
#put hyperSpec-prebuilt.tar.gz
#bye
#EOT

#windevbuild: .FORCE
#	cd www && ftp -n -d win-builder.r-project.org << EOT
#user anonymous cbeleites@units.it
#cd R-devel
#put hyperSpec-prebuilt.tar.gz
#bye
#EOT

check: $(SRC) $(RNW) $(DOC) 
	R CMD check pkg

devcheck: $(SRC) $(RNW) $(DOC) 
	~/r-devel/bin/R CMD check pkg

checkfast: $(SRC)
	R CMD check --no-examples --no-tests --no-manual --no-vignettes pkg

checktest: $(SRC)
	R CMD check --no-manual --no-vignettes pkg

clean: .FORCE
	@rm -f $(foreach V,$(VIGNETTES),Vignettes/$(V)/$(V).tex) 
	@rm -f $(foreach V,$(VIGNETTES),Vignettes/$(V)/*.aux) 
	@rm -f $(foreach V,$(VIGNETTES),Vignettes/$(V)/*.dvi) 
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
	@rm -f pkg/inst/doc/*.dvi
	@rm -f pkg/inst/doc/*.log
	@rm -f pkg/inst/doc/*.toc
	@rm -f pkg/inst/doc/*.out
	@rm -f pkg/inst/doc/*.bbl
	@rm -f pkg/inst/doc/*.blg
	@rm -f pkg/inst/doc/*.idx 
	@rm -f pkg/inst/doc/*.ilg 
	@rm -f pkg/inst/doc/*.ind
	@rm -f pkg/inst/doc/Rplots.pdf
	@rm -f pkg/inst/doc/*/*.fdb_latexmk
	@rm -f pkg/inst/doc/*/*.aux
	@rm -f pkg/inst/doc/*/*.dvi
	@rm -f pkg/inst/doc/*/*.log
	@rm -f pkg/inst/doc/*/*.toc
	@rm -f pkg/inst/doc/*/*.out
	@rm -f pkg/inst/doc/*/*.bbl
	@rm -f pkg/inst/doc/*/*.blg
	@rm -f pkg/inst/doc/*/*.idx 
	@rm -f pkg/inst/doc/*/*.ilg 
	@rm -f pkg/inst/doc/*/*.ind
	@rm -f pkg/inst/doc/*/Rplots.pdf
	@rm -f pkg/inst/doc/*/*.fdb_latexmk#	
	@rm -f $(patsubst %.Rnw,%.tex,$(wildcard pkg/inst/doc/*/*.Rnw)) #should not be necessary
	@rm -rf pkg/inst/doc/auto
	@rm -rf pkg/inst/doc/*/auto


