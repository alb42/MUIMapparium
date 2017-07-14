CPU=$(shell "fpc" -iTP)
OS=$(shell "fpc" -iTO)
UNITSDIR=units/$(CPU)-$(OS)

MUIMapp: $(UNITSDIR) MUIMapparium.pas
	@echo Create units in $<
	@fpc -g- -FU$< MUIMapparium.pas

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

clean:
	@delete $(UNITSDIR)/#?.ppu
	@delete $(UNITSDIR)/#?.o  
