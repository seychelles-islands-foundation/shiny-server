all: r_data clean

# wildcard rule
%.csv: %.xlsx
	/usr/local/bin/xlsx2csv -s 1 -f "%d-%m-%Y %H:%M:%S" $< $@ 
	
# data folders
data = /mnt/fileserver/sifapps/subsistence_fishing/app_data/
ex = /mnt/fileserver/sifapps/subsistence_fishing/db_export_tables/
app = /srv/shiny-server/fishing/
csv = $(data)csv/

#SOURCES = tblBoat.xlsx tblFamily.xlsx tblFishCaught.xlsx tblFishingMethod.xlsx tblFishUse.xlsx tblFoodSecurityZone.xlsx tblSession.xlsx tblSpecies.xlsx tblTrip.xlsx
#OBJECTS = $(SOURCES:.xlsx=.csv)

src = $(wildcard $(ex)*.xlsx)
obj = $(src:.xlsx=.csv)

r_data: $(data)fishing_global.rds

$(data)fishing_global.rds: $(obj) $(app)code/01_read_tables.R
	/usr/bin/Rscript $(app)code/01_read_tables.R $< $@

.PHONY: clean

clean:
	/bin/rm ~*
#$(patsubst %,$(csv)%,$(OBJECTS)): $(patsubst %,$(ex)%,$(SOURCES))
#	xlsx2csv $< $@


