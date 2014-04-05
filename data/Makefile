
unemployment = unemployment/SL.UEM.TOTL.ZS_Indicator_en_csv_v2.csv
inflation = inflation/FP.CPI.TOTL.ZG_Indicator_en_csv_v2.csv

all: $(unemployment) $(inflation)

$(unemployment): unemployment unemployment.zip
	unzip -d $^
	touch $(unemployment)

$(inflation): inflation inflation.zip
	unzip -d $^
	touch $(inflation)


unemployment:
	mkdir unemployment

inflation:
	mkdir inflation

unemployment.zip:
	wget -O $@ http://api.worldbank.org/v2/en/indicator/sl.uem.totl.zs?downloadformat=csv

inflation.zip:
	wget -O $@ http://api.worldbank.org/v2/en/indicator/fp.cpi.totl.zg?downloadformat=csv

