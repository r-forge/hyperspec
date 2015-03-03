VIGNETTES := baseline chondro fileio flu introduction laser plotting

ZIPS := Vignettes/*.zip

MAN := pkg/hyperSpec/man/*.Rd
RNW := pkg/hyperSpec/vignettes/*.Rnw


all: vignettes doc data www DESCRIPTION roxy build check test
		@echo all vignettes: $(VIGNETTES)

zip: $(ZIPS)
		@echo all zips: $(ZIPS)

.FORCE: # always done, like .PHONY, but doesn't remove file

.INTERMEDIATE: $(foreach V,$(VIGNETTES),Vignettes/$(V)/$(V).tex)
#.SECONDARY: Vignettes/*.zip

# VIGNETTES in subdirs ##############################################################################
vignettes: $(VIGNETTES) $(ZIPS) pkg/hyperSpec/vignettes/vignettes.defs pkg/hyperSpec/vignettes/* pkg/hyperSpec/vignettes/*.pdf

Vignettes/%/vignettes.defs: Vignettes/vignettes.defs
	@cp -av $< $@ 

# baseline ..........................................................................................
baseline:                                         Vignettes/baseline/baseline.pdf

Vignettes/baseline/baseline.pdf:                  Vignettes/baseline/baseline.tex 

Vignettes/baseline/baseline.tex:                  Vignettes/baseline/baseline.Rnw 

Vignettes/baseline/baseline.Rnw:                  Vignettes/baseline/vignettes.defs $(SRC)
	touch $@

# chondro ......................................................................................
chondro:                                     Vignettes/chondro/chondro.pdf Vignettes/chondro.zip

Vignettes/chondro/chondro.pdf:          Vignettes/chondro/chondro.tex 	 \
                                                  Vignettes/chondro/080606d-flip-ausw.jpg

Vignettes/chondro/chondro.tex:          Vignettes/chondro/chondro.Rnw

Vignettes/chondro/chondro.Rnw:          Vignettes/chondro/vignettes.defs $(SRC)    \
                                                  Vignettes/chondro/rawdata/* 
	touch $@

Vignettes/chondro/*.rda:	                    Vignettes/chondro/chondro.Rnw
	cd $(dir $<) && Rscript -e "Sweave ('$(notdir $<)')" 


# fileio ............................................................................................
fileio:                                           Vignettes/fileio/fileio.pdf Vignettes/fileio.zip

Vignettes/fileio/fileio.pdf:                      Vignettes/fileio/fileio.tex 			 
Vignettes/fileio/fileio.tex:                      Vignettes/fileio/fileio.Rnw

Vignettes/fileio/fileio.Rnw:                      Vignettes/fileio/vignettes.defs $(SRC)    \
                                                  Vignettes/fileio/scan.txt.PerkinElmer.R   \
                                                  Vignettes/fileio/txt.t/Triazine\ 5_31.txt \
                                                  Vignettes/fileio/ENVI/example2.img 		 \
                                                  Vignettes/fileio/spc/*.SPC 					 \
                                                  Vignettes/fileio/spc.Kaisermap/*.spc 		 \
                                                  Vignettes/fileio/txt.Renishaw/* \
	                               					  Vignettes/fileio/txt.Renishaw/paracetamol.txt
	touch $@

Vignettes/fileio/*.rda:                           Vignettes/fileio/fileio.Rnw
	cd $(dir $<) && Rscript -e "Sweave ('$(notdir $<)')"

# flu ...............................................................................................
flu:                                              Vignettes/flu/flu.pdf

Vignettes/flu/flu.pdf:          						  Vignettes/flu/flu.tex 	 

Vignettes/flu/flu.tex:          						  Vignettes/flu/flu.Rnw

Vignettes/flu/flu.Rnw:       						  	  Vignettes/flu/vignettes.defs $(SRC)    \
                                                  Vignettes/flu/rawdata/* \
                                                  Vignettes/flu/scan.txt.PerkinElmer.R
	touch $@

Vignettes/flu/scan.txt.PerkinElmer.R:             Vignettes/fileio/scan.txt.PerkinElmer.R
	cp -av $< $@

Vignettes/flu/*.rda:  	                          Vignettes/flu/flu.Rnw
	cd $(dir $<) && Rscript -e "Sweave ('$(notdir $<)')"

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

#Vignettes/introduction/rawdata/paracetamol.txt.gz: Vignettes/fileio/txt.Renishaw/paracetamol.txt.gz
#	@cp -av $< $@ 

#Vignettes/fileio/paracetamol.rda:	        Vignettes/fileio/fileio.Rnw
#	cd $(dir $<) && Rscript -e "Sweave ('$(notdir $<)')"

# laser .............................................................................................
laser:                                     	  	  Vignettes/laser/laser.pdf 

Vignettes/laser/laser.pdf:          				  Vignettes/laser/laser.tex 	 

Vignettes/laser/laser.tex:          				  Vignettes/laser/laser.Rnw 
	cd $(dir $<) && echo '.rgl = TRUE; Sweave ("'$(notdir $<'"))' | R --no-save # warum ))?
#	cd $(dir $<) && R --rgl CMD Sweave $(notdir $<) 

Vignettes/laser/fig-3D.png:						  Vignettes/laser/laser.tex

Vignettes/laser/laser.Rnw:          				  Vignettes/laser/vignettes.defs $(SRC)    \
                                                  Vignettes/laser/rawdata/laser.txt.gz

Vignettes/laser/rawdata/laser.txt.gz:             Vignettes/fileio/txt.Renishaw/laser.txt.gz
	cp -av $< $@

Vignettes/laser/*.rda:	                          Vignettes/laser/laser.Rnw
	cd $(dir $<) && echo '.rgl = TRUE; Sweave ("'$(notdir $<'"))' | R --no-save # warum ))?

# plotting ..........................................................................................
plotting:                                     	  Vignettes/plotting/plotting.pdf 

Vignettes/plotting/plotting.pdf:          		  Vignettes/plotting/plotting.tex 	 

Vignettes/plotting/plotting.tex:          		  Vignettes/plotting/plotting.Rnw
	cd $(dir $<) && echo '.rgl = TRUE; Sweave ("'$(notdir $<'"))' | R --no-save # warum ))?	

Vignettes/plotting/fig-3D.png:					  Vignettes/plotting/plotting.tex

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
	cd $(dir $<) && Rscript -e "Sweave ('$(notdir $<)')"

%.pdf: %.tex
#	cd $(dir $<) && rubber --pdf -s $(basename $(notdir $<)).tex
	cd $(dir $<) && latexmk -pdf $(basename $(notdir $<)).tex 
	gs -sDEVICE=pdfwrite -dPDFSETTINGS=/screen -dAutoRotatePages=/None \
	-dDownsampleColorImages=false -dNOPAUSE -dQUIET -dBATCH -sOutputFile=tmp.pdf $@ && qpdf tmp.pdf $@

%.dvi: # should not happen!
	rm $@
# data ##############################################################################################
data: pkg/hyperSpec/data/*.rda pkg/hyperSpec/R/sysdata.rda

pkg/hyperSpec/data/barbiturates.rda: Vignettes/fileio/barbiturates.rda 
	@cp -av $< $@
pkg/hyperSpec/data/flu.rda:         Vignettes/flu/flu.rda 
	@cp -av $< $@
pkg/hyperSpec/data/paracetamol.rda: Vignettes/fileio/paracetamol.rda 
	@cp -av $< $@
pkg/hyperSpec/data/laser.rda:       Vignettes/laser/laser.rda
	@cp -av $< $@

pkg/hyperSpec/R/sysdata.rda:	Vignettes/chondro/chondro-internal.rda	
	@cp -av $< $@

# instdoc ###########################################################################################
doc: $(foreach V,$(VIGNETTES), pkg/hyperSpec/vignettes/$(V).Rnw)

pkg/hyperSpec/inst/doc/vignettes.defs: Vignettes/vignettes.defs
	@cp -av $< $@ 
	@touch pkg/hyperSpec/vignettes/*.Rnw

pkg/hyperSpec/vignettes/vignettes.defs: Vignettes/vignettes.defs
	@cp -av $< $@ 
	@touch pkg/hyperSpec/vignettes/*.Rnw

pkg/hyperSpec/vignettes/baseline.Rnw: Vignettes/baseline/baseline.Rnw 
	@cp -av $< $@

pkg/hyperSpec/vignettes/flu.Rnw: Vignettes/flu/flu.Rnw \
                      pkg/hyperSpec/vignettes/rawdata/flu1.txt \
                      pkg/hyperSpec/vignettes/rawdata/flu2.txt \
                      pkg/hyperSpec/vignettes/rawdata/flu3.txt \
                      pkg/hyperSpec/vignettes/rawdata/flu4.txt \
                      pkg/hyperSpec/vignettes/rawdata/flu5.txt \
                      pkg/hyperSpec/vignettes/rawdata/flu6.txt \
                      pkg/hyperSpec/vignettes/scan.txt.PerkinElmer.R 
	@cp -av $< $@

pkg/hyperSpec/vignettes/rawdata/flu%.txt: Vignettes/flu/rawdata/flu%.txt
	@cp -av $< $@

pkg/hyperSpec/vignettes/scan.txt.PerkinElmer.R: Vignettes/flu/scan.txt.PerkinElmer.R 
	@cp -av $< $@

pkg/hyperSpec/vignettes/introduction.Rnw: Vignettes/introduction/introduction.Rnw \
                               pkg/hyperSpec/vignettes/functions.RData \
                               pkg/hyperSpec/vignettes/introduction.bib \
                               pkg/hyperSpec/vignettes/strukturhyperspec.pdf
	@cp -av $< $@

#pkg/hyperSpec/vignettes/rawdata/paracetamol.txt.gz: Vignettes/introduction/rawdata/paracetamol.txt.gz
#	@cp -av $< $@

pkg/hyperSpec/vignettes/functions.RData: Vignettes/introduction/functions.RData
	@cp -av $< $@

pkg/hyperSpec/vignettes/introduction.bib: Vignettes/introduction/introduction.bib 
	@cp -av $< $@

pkg/hyperSpec/vignettes/strukturhyperspec.pdf: Vignettes/introduction/strukturhyperspec.pdf
	@cp -av $< $@


pkg/hyperSpec/vignettes/laser.Rnw: Vignettes/laser/laser.Rnw \
                        pkg/hyperSpec/vignettes/rawdata/laser.txt.gz \
								pkg/hyperSpec/vignettes/fig-3D.png
	@cp -av $< $@

pkg/hyperSpec/vignettes/rawdata/laser.txt.gz: Vignettes/laser/rawdata/laser.txt.gz
	@cp -av $< $@

pkg/hyperSpec/vignettes/fig-3D.png: Vignettes/laser/fig-3D.png
	@cp -av $< $@

pkg/hyperSpec/vignettes/chondro.Rnw: pkg/hyperSpec/vignettes/chondro.pdf # no mistake: this is a dummy .Rnw
	touch $@

pkg/hyperSpec/vignettes/chondro.pdf: Vignettes/chondro/chondro.pdf
	@cp -av $< $@

pkg/hyperSpec/vignettes/fileio.Rnw: pkg/hyperSpec/vignettes/fileio.pdf # no mistake: this is a dummy .Rnw
	touch $@

pkg/hyperSpec/vignettes/fileio.pdf: Vignettes/fileio/fileio.pdf     
	@cp -av $< $@

pkg/hyperSpec/vignettes/plotting.Rnw: Vignettes/plotting/plotting.Rnw \
									pkg/hyperSpec/vignettes/fig-3D.png 
	@cp -av $< $@

# www ###############################################################################################
www: jekyll/blob/chondro.zip jekyll/blob/fileio.zip \
     jekyll/blob/baseline.pdf jekyll/blob/chondro.pdf jekyll/blob/fileio.pdf jekyll/blob/flu.pdf \
     jekyll/blob/introduction.pdf jekyll/blob/laser.pdf jekyll/blob/plotting.pdf \
     jekyll/blob/hyperSpec-prebuilt.tar.gz
	cd jekyll && jekyll

jekyll/blob/baseline.pdf: Vignettes/baseline/baseline.pdf
	@cp -av $< $@
jekyll/blob/chondro.pdf: Vignettes/chondro/chondro.pdf
	@cp -av $< $@
jekyll/blob/fileio.pdf: Vignettes/fileio/fileio.pdf
	@cp -av $< $@
jekyll/blob/flu.pdf: Vignettes/flu/flu.pdf
	@cp -av $< $@
jekyll/blob/introduction.pdf: Vignettes/introduction/introduction.pdf
	@cp -av $< $@
jekyll/blob/laser.pdf: Vignettes/laser/laser.pdf
	@cp -av $< $@
jekyll/blob/plotting.pdf: Vignettes/plotting/plotting.pdf
	@cp -av $< $@

jekyll/blob/hyperSpec-prebuilt.zip: # built manually 
	touch $@
jekyll/blob/hyperSpec-prebuilt.tar.gz: build
	@cp -av $< $@

jekyll/blob/chondro.zip: Vignettes/chondro.zip
	@cp -av $< $@

jekyll/blob/fileio.zip: Vignettes/fileio.zip
	@cp -av $< $@


#####################################################################################################




DESCRIPTION: $(shell find pkg/hyperSpec -maxdepth 1 -daystart -not -ctime 0 -name "DESCRIPTION") #only if not modified today
	@echo update DESCRIPTION
	sed "s/\(^Version: .*-\)20[0-9][0-9][0-1][0-9][0-3][0-9]\(.*\)$$/\1`date +%Y%m%d`\2/" pkg/hyperSpec/DESCRIPTION > .DESCRIPTION
	sed "s/\(^Date: .*\)20[0-9][0-9]-[0-1][0-9]-[0-3][0-9]\(.*\)$$/\1`date +%F`\2/" .DESCRIPTION > pkg/hyperSpec/DESCRIPTION 
	rm .DESCRIPTION

roxy: clean DESCRIPTION pkg/hyperSpec/R/*.R 
	Rscript --vanilla -e "library (roxygen2); roxygenize ('pkg/hyperSpec')" 


build: DESCRIPTION $(SRC) vignettes $(RNW) $(MAN) data roxy install
	rm -f hyperSpec_*.tar.gz
	R CMD build pkg/hyperSpec --compact-vignettes=both 
	gunzip hyperSpec_*.tar.gz
	tar -xvf hyperSpec_*.tar 
	rm -f hyperSpec/vignettes/*.aux 
	rm -f hyperSpec/vignettes/*.log 
	rm -f hyperSpec/vignettes/*.out 
	rm -f hyperSpec/vignettes/*.toc 
	rm -f hyperSpec/vignettes/Rplots.pdf
	tar -czf `ls hyperSpec_*.tar`.gz hyperSpec 
	rm hyperSpec_*.tar
  rm -rf hyperSpec/
	cp hyperSpec_*.tar.gz jekyll/blob/hyperSpec-prebuilt.tar.gz

devbuild: DESCRIPTION $(SRC) vignettes $(RNW) $(MAN) data roxy install
	rm -f hyperSpec_*.tar.gz
	~/r-devel/bin/R CMD build pkg/hyperSpec --compact-vignettes=both && cp hyperSpec_*.tar.gz jekyll/blob/hyperSpec-prebuilt-devel.tar.gz

buildfast: DESCRIPTION roxy
	rm -f hyperSpec_*.tar.gz
	R CMD build --no-build-vignettes pkg/hyperSpec

check: build
	R CMD check hyperSpec_*.tar.gz --no-build-vignettes --timings

devcheck: devbuild
	~/r-devel/bin/R CMD check hyperSpec_*.tar.gz

checkfast: $(SRC)
	R CMD check --no-examples --no-tests --no-manual --no-vignettes pkg/hyperSpec

checktest: $(SRC)
	R CMD check --no-manual --no-vignettes pkg/hyperSpec

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
	@rm -f $(foreach V,$(VIGNETTES),Vignettes/$(V)/*.fls) 
	@rm -f $(foreach V,$(VIGNETTES),Vignettes/$(V)/*.fdb_latexmk) 
	@rm -f $(foreach V,$(VIGNETTES),Vignettes/$(V)/Rplots.pdf) 
	@rm -f pkg/hyperSpec/vignettes/*.aux
	@rm -f pkg/hyperSpec/vignettes/*.dvi
	@rm -f pkg/hyperSpec/vignettes/*.log
	@rm -f pkg/hyperSpec/vignettes/*.toc
	@rm -f pkg/hyperSpec/vignettes/*.out
	@rm -f pkg/hyperSpec/vignettes/*.bbl
	@rm -f pkg/hyperSpec/vignettes/*.blg
	@rm -f pkg/hyperSpec/vignettes/*.idx 
	@rm -f pkg/hyperSpec/vignettes/*.ilg 
	@rm -f pkg/hyperSpec/vignettes/*.ind
	@rm -f pkg/hyperSpec/vignettes/*.fls
	@rm -f pkg/hyperSpec/vignettes/Rplots.pdf
	@rm -f pkg/hyperSpec/vignettes/*.fdb_latexmk
#	@rm -f pkg/hyperSpec/vignettes/*/*.aux
#	@rm -f pkg/hyperSpec/vignettes/*/*.dvi
#	@rm -f pkg/hyperSpec/vignettes/*/*.log
#	@rm -f pkg/hyperSpec/vignettes/*/*.toc
#	@rm -f pkg/hyperSpec/vignettes/*/*.out
#	@rm -f pkg/hyperSpec/vignettes/*/*.bbl
#	@rm -f pkg/hyperSpec/vignettes/*/*.blg
#	@rm -f pkg/hyperSpec/vignettes/*/*.idx 
#	@rm -f pkg/hyperSpec/vignettes/*/*.ilg 
#	@rm -f pkg/hyperSpec/vignettes/*/*.ind
	@rm -f pkg/hyperSpec/vignettes/*.bak
	@rm -f pkg/hyperSpec/vignettes/*~
#	@rm -f pkg/hyperSpec/vignettes/fig/*	
	@rm -f pkg/hyperSpec/vignettes/*/Rplots.pdf
#	@rm -f pkg/hyperSpec/vignettes/*/*.fdb_latexmk#	
	@rm -f $(patsubst %.Rnw,%.tex,$(wildcard pkg/hyperSpec/vignettes/*/*.Rnw)) #should not be necessary
	@rm -rf pkg/hyperSpec/vignettes/auto
	@rm -rf pkg/hyperSpec/vignettes/*/auto
	@find -name ".Rhistory" -delete
	@find -name "*~" -delete


install:
		R CMD INSTALL pkg/hyperSpec	

test: install
	Rscript --vanilla -e "library (hyperSpec); hy.unittest ()"
