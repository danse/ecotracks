
unemployment = unemployment/SL.UEM.TOTL.ZS_Indicator_en_csv_v2.csv
inflation = inflation/FP.CPI.TOTL.ZG_Indicator_en_csv_v2.csv

index: conversion
	ls converted > index

conversion: $(unemployment) $(inflation) convert
	./convert $(unemployment) $(inflation)

convert: convert.hs
	ghc $<

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

