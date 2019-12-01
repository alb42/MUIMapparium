CPU=$(shell "fpc" -iTP)
OS=$(shell "fpc" -iTO)
MYFPC=fpc
ifeq ($(OS),linux)
MYFPC=fpc4aros.sh
CPU=i386
OS=aros
endif
UNITSDIR=units/$(CPU)-$(OS)

MUIMapp: $(UNITSDIR) MUIMapparium.pas
	@echo Create units in $<
	@$(MYFPC) -g- -FU$< MUIMapparium.pas
	@echo Units created in $<
	

$(UNITSDIR):
	@echo Create units dir $@
	@makedir $@

locale: ctfile catalog source

ctfile:
	@flexcat locale/MUIMapparium.cd Catalogs/deutsch.ct NEWCTFILE Catalogs/deutsch.ct

catalog:
	@flexcat locale/MUIMapparium.cd Catalogs/deutsch.ct CATALOG Catalogs/deutsch/MUIMapparium.catalog
source:
	@flexcat locale/MUIMapparium.cd MUIMappariumlocale.pas=locale/FPCUnit.sd

all: ctfile locale MUIMapp
	@echo done.

clean:
	@delete $(UNITSDIR)/#?.ppu
	@delete $(UNITSDIR)/#?.o  
