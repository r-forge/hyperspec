VIGNETTES := baseline chondro fileio flu introduction laser plotting

ZIPS := Vignettes/*.zip

SRC := pkg/R/*.R pkg/NAMESPACE 
MAN := pkg/man/*.Rd
RNW := pkg/inst/doc/*.Rnw


all: vignettes doc data www DESCRIPTION build check
		@echo all vignettes: $(VIGNETTES)

zip: $(ZIPS)
		@echo all zips: $(ZIPS)

.FORCE: # always done, like .PHONY, but doesn't remove file

.INTERMEDIATE: $(foreach V,$(VIGNETTES),Vignettes/$(V)/$(V).tex)
#.SECONDARY: Vignettes/*.zip

# VIGNETTES in subdirs ##############################################################################
vignettes: $(VIGNETTES) $(ZIPS) pkg/inst/doc/vignettes.defs pkg/inst/doc/* pkg/inst/doc/*.pdf

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
	cd $(dir $<) && R CMD Sweave $(notdir $<) && \


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
	cd $(dir $<) && R CMD Sweave $(notdir $<)

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

#Vignettes/introduction/rawdata/paracetamol.txt.gz: Vignettes/fileio/txt.Renishaw/paracetamol.txt.gz
#	@cp -av $< $@ 

#Vignettes/fileio/paracetamol.rda:	        Vignettes/fileio/fileio.Rnw
#	cd $(dir $<) && R CMD Sweave $(notdir $<) 

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
	cd $(dir $<) && R CMD Sweave $(notdir $<) 

%.pdf: %.tex
#	cd $(dir $<) && rubber --pdf -s $(basename $(notdir $<)).tex
	cd $(dir $<) && latexmk -pdf $(basename $(notdir $<)).tex 
	gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/screen -dAutoRotatePages=/None \
	-dDownsampleColorImages=false -dNOPAUSE -dQUIET -dBATCH -sOutputFile=tmp.pdf $@ && mv tmp.pdf $@

%.dvi: # should not happen!
	rm $@
# data ##############################################################################################
data: pkg/data/*.rda pkg/R/sysdata.rda

pkg/data/barbiturates.rda: Vignettes/fileio/barbiturates.rda 
	@cp -av $< $@
pkg/data/flu.rda:         Vignettes/flu/flu.rda 
	@cp -av $< $@
pkg/data/paracetamol.rda: Vignettes/fileio/paracetamol.rda 
	@cp -av $< $@
pkg/data/laser.rda:       Vignettes/laser/laser.rda
	@cp -av $< $@

pkg/R/sysdata.rda:	Vignettes/chondro/chondro-internal.rda	
	@cp -av $< $@

# instdoc ###########################################################################################
doc: $(foreach V,$(VIGNETTES), pkg/inst/doc/$(V).Rnw)

pkg/inst/doc/vignettes.defs: Vignettes/vignettes.defs
	@cp -av $< $@ 
	@touch pkg/inst/doc/*.Rnw

pkg/inst/doc/baseline.Rnw: Vignettes/baseline/baseline.Rnw 
	@cp -av $< $@

pkg/inst/doc/flu.Rnw: Vignettes/flu/flu.Rnw \
                      pkg/inst/doc/rawdata/flu1.txt \
                      pkg/inst/doc/rawdata/flu2.txt \
                      pkg/inst/doc/rawdata/flu3.txt \
                      pkg/inst/doc/rawdata/flu4.txt \
                      pkg/inst/doc/rawdata/flu5.txt \
                      pkg/inst/doc/rawdata/flu6.txt \
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

#pkg/inst/doc/rawdata/paracetamol.txt.gz: Vignettes/introduction/rawdata/paracetamol.txt.gz
#	@cp -av $< $@

pkg/inst/doc/functions.RData: Vignettes/introduction/functions.RData
	@cp -av $< $@

pkg/inst/doc/introduction.bib: Vignettes/introduction/introduction.bib 
	@cp -av $< $@

pkg/inst/doc/strukturhyperspec.pdf: Vignettes/introduction/strukturhyperspec.pdf
	@cp -av $< $@


pkg/inst/doc/laser.Rnw: Vignettes/laser/laser.Rnw \
                        pkg/inst/doc/rawdata/laser.txt.gz \
								pkg/inst/doc/fig-3D.png
	@cp -av $< $@

pkg/inst/doc/rawdata/laser.txt.gz: Vignettes/laser/rawdata/laser.txt.gz
	@cp -av $< $@

pkg/inst/doc/fig-3D.png: Vignettes/laser/fig-3D.png
	@cp -av $< $@

pkg/inst/doc/chondro.Rnw: pkg/inst/doc/chondro.pdf # no mistake: this is a dummy .Rnw
	touch $@

pkg/inst/doc/chondro.pdf: Vignettes/chondro/chondro.pdf
	@cp -av $< $@

pkg/inst/doc/fileio.Rnw: pkg/inst/doc/fileio.pdf # no mistake: this is a dummy .Rnw
	touch $@

pkg/inst/doc/fileio.pdf: Vignettes/fileio/fileio.pdf     
	@cp -av $< $@

pkg/inst/doc/plotting.Rnw: Vignettes/plotting/plotting.Rnw \
									pkg/inst/doc/fig-3D.png 
	@cp -av $< $@

#pkg/inst/doc/baseline.pdf: Vignettes/baseline/baseline.pdf     
#	@cp -av $< $@

#pkg/inst/doc/flu.pdf: Vignettes/flu/flu.pdf     
#	@cp -av $< $@

#pkg/inst/doc/introduction.pdf: Vignettes/introduction/introduction.pdf     
#	@cp -av $< $@

#pkg/inst/doc/laser.pdf: Vignettes/laser/laser.pdf     
#	@cp -av $< $@

#pkg/inst/doc/plotting.pdf: Vignettes/plotting/plotting.pdf     
#	@cp -av $< $@

# www ###############################################################################################
www: www/*.zip www/*.pdf

www/baseline.pdf: Vignettes/baseline/baseline.pdf
	@cp -av $< $@
www/chondro.pdf: Vignettes/chondro/chondro.pdf
	@cp -av $< $@
www/fileio.pdf: Vignettes/fileio/fileio.pdf
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

www/chondro.zip: Vignettes/chondro.zip
	@cp -av $< $@

www/fileio.zip: Vignettes/fileio.zip
	@cp -av $< $@


#####################################################################################################




DESCRIPTION: $(shell find pkg -maxdepth 1 -daystart -not -ctime 0 -name "DESCRIPTION") #only if not modified today
	@echo update DESCRIPTION
	sed "s/\(^Version: .*-\)20[0-9][0-9][0-1][0-9][0-3][0-9]\(.*\)$$/\1`date +%Y%m%d`\2/" pkg/DESCRIPTION > .DESCRIPTION
	sed "s/\(^Date: .*\)20[0-9][0-9]-[0-1][0-9]-[0-3][0-9]\(.*\)$$/\1`date +%F`\2/" .DESCRIPTION > pkg/DESCRIPTION 
	rm .DESCRIPTION

build: DESCRIPTION $(SRC) vignettes $(RNW) $(MAN) data
	rm -f hyperSpec_*.tar.gz
	R CMD build pkg  && cp hyperSpec_*.tar.gz www/hyperSpec-prebuilt.tar.gz

devbuild: DESCRIPTION $(SRC) vignettes $(RNW) $(MAN) data
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

check: DESCRIPTION $(SRC) vignettes $(RNW) $(MAN) data
	R CMD check pkg

devcheck: DESCRIPTION $(SRC) vignettes $(RNW) $(MAN) data
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
	@rm -f pkg/inst/doc/*.fdb_latexmk
#	@rm -f pkg/inst/doc/*/*.aux
#	@rm -f pkg/inst/doc/*/*.dvi
#	@rm -f pkg/inst/doc/*/*.log
#	@rm -f pkg/inst/doc/*/*.toc
#	@rm -f pkg/inst/doc/*/*.out
#	@rm -f pkg/inst/doc/*/*.bbl
#	@rm -f pkg/inst/doc/*/*.blg
#	@rm -f pkg/inst/doc/*/*.idx 
#	@rm -f pkg/inst/doc/*/*.ilg 
#	@rm -f pkg/inst/doc/*/*.ind
	@rm -f pkg/inst/doc/*.bak
	@rm -f pkg/inst/doc/*~
	@rm -f pkg/inst/doc/fig/*	
	@rm -f pkg/inst/doc/*/Rplots.pdf
#	@rm -f pkg/inst/doc/*/*.fdb_latexmk#	
	@rm -f $(patsubst %.Rnw,%.tex,$(wildcard pkg/inst/doc/*/*.Rnw)) #should not be necessary
	@rm -rf pkg/inst/doc/auto
	@rm -rf pkg/inst/doc/*/auto


