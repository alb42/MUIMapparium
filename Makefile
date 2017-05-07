CPU=$(shell "fpc" -iTP)
OS=$(shell "fpc" -iTO)
UNITSDIR=units/$(CPU)-$(OS)
DUMMY=$(shell makedir $(UNITSDIR))
MUIMapp:
	echo Create units dir $(DUMMY)
	fpc -FU$(UNITSDIR) MUIMapparium.pas

locale: catalog source

ctfile:
	flexcat locale/MUIMapparium.cd Catalogs/english.ct NEWCTFILE Catalogs/english.ct
	flexcat locale/MUIMapparium.cd Catalogs/deutsch.ct NEWCTFILE Catalogs/deutsch.ct
catalog:
	flexcat locale/MUIMapparium.cd Catalogs/english.ct CATALOG Catalogs/english/MUIMapparium.catalog
	flexcat locale/MUIMapparium.cd Catalogs/deutsch.ct CATALOG Catalogs/deutsch/MUIMapparium.catalog
source:
	flexcat locale/MUIMapparium.cd MUIMappariumlocale.pas=locale/FPCUnit.sd
all: ctfile locale MUIMapp
