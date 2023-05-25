#####
#11/05/23
#Code to gather the information from US and Brasil from the original sources
#to develop the regresion relating the production cost, yield and price. 
#The final regresions were made in Stata.
#Code developed by Sioux Melo. sfmelol@gmail.com
#####


library(readxl)
library(dplyr)
require(foreign)
#wd<-"C:/Users/MeloL001/OneDrive - Universiteit Utrecht/2023/May"
wd<-"C:/Users/sfmel/OneDrive - Universiteit Utrecht/2023/May"
#####
##US analysis

CostCrop <-c()
cropfiles<-list.files(path=paste0(wd,"/USDA"), pattern=NULL, all.files=FALSE,
    full.names=FALSE)

setwd(paste0(wd,"/USDA"))

	
for(i in cropfiles){

DataSet1 <- read_excel(i, sheet = "Data sheet (machine readable)", col_names = TRUE)
CostCrop <-rbind(CostCrop,DataSet1)

}
setwd(wd)

write.table(CostCrop,"CostCrop.txt",sep=";",row.names=FALSE)

data1<-c("Soybeans","Peanuts")
data2<-c("Soybean","Peanut")

for(i in 1:2){
	CostCrop <- data.frame(lapply(CostCrop, function(x) {
	                  gsub(data1[i], data2[i], x)
		}))
}

CostCrop$crop_names_helga_up1 <-CostCrop$Commodity
idCropHe<- read_excel("HelgaCropsComplete.xlsx", sheet = "Advance", col_names = TRUE)
CostCrop1 <- merge(idCropHe,CostCrop,  by="crop_names_helga_up1")
CostCrop1$Value<-as.numeric(CostCrop1$Value)
##Year
CostCrop2<-CostCrop1[CostCrop1$Year >= 2010 & CostCrop1$Year <= 2021,]
##Yield
Yield<-CostCrop2[CostCrop2$Item == "Yield",]
translate<- read_excel("HelgaCropsComplete.xlsx", sheet = "toTons", col_names = TRUE)
Yield1<-merge(Yield,translate,  by="crop_names_helga_up1", all.x=TRUE)
Yield1$Value<-as.numeric(Yield1$Value)
Yield1$ValueEq<-as.numeric(Yield1$ValueEq)
Yield1$Yield<-Yield1$Value*2.4710538147/Yield1$ValueEq
YieldP<-data.frame(Yield1$crop_names_helga_up1, Yield1$Country, Yield1$Region, Yield1$Year, Yield1$Yield)
colnames(YieldP)<-c("crop_names_helga_up1", "Country", "Region", "Year", "Yield")
#Cost
Cost<-CostCrop2[CostCrop2$Item == "Total, costs listed",]
Cost1<-merge(Cost,translate,  by="crop_names_helga_up1", all.x=TRUE)
Cost1$Value<-as.numeric(Cost1$Value)
Cost1$ValueEq<-as.numeric(Cost1$ValueEq)
Cost1$Cost<-Cost1$Value*2.4710538147
CostP<-data.frame(Cost1$crop_names_helga_up1, Cost1$Country, Cost1$Region, Cost1$Year, Cost1$Cost)
colnames(CostP)<-c("crop_names_helga_up1", "Country", "Region", "Year", "Cost")
#Price
Price<-CostCrop2[CostCrop2$Item == "Price",]
Price1<-merge(Price,translate,  by="crop_names_helga_up1", all.x=TRUE)
Price1$Value<-as.numeric(Price1$Value)
Price1$ValueEq<-as.numeric(Price1$ValueEq)
Price1$Price<-Price1$Value*Price1$ValueEq
PriceP<-data.frame(Price1$crop_names_helga_up1, Price1$Country, Price1$Region, Price1$Year, Price1$Price)
colnames(PriceP)<-c("crop_names_helga_up1", "Country", "Region", "Year", "Price")
##US Database
US<-merge(YieldP,CostP, by=c("crop_names_helga_up1", "Country", "Region", "Year"))
US$Yield<-as.numeric(US$Yield)
US$Cost<-as.numeric(US$Cost)
US1<-merge(US,PriceP, by=c("crop_names_helga_up1", "Country", "Region", "Year"))
US1$Yield<-as.numeric(US1$Yield)
US1$Cost<-as.numeric(US1$Cost)
US1$Price<-as.numeric(US1$Price)
US1$Product<-US1$crop_names_helga_up1
US1$DoubleCountry<-1

write.table(US1,"USData.csv",sep=";",row.names=FALSE, dec=".")
Countrydupl<-unique(US1$crop_names_helga_up1)

#####
##BR analysis

RawData<-read.table(file="CustoProducao.txt", header=TRUE, sep=";", dec=".", quote="")
idCropHe<- read_excel("HelgaCropsComplete.xlsx", sheet = "EquivalenceBR", col_names = TRUE)
Crops2018_raw <- merge(RawData, idCropHe, by="produto")

##Yield
Yield_avr <-aggregate(list(vlr_custo_variavel_ha=Crops2018_raw$vlr_custo_variavel_ha, vlr_custo_fixo_ha=Crops2018_raw$vlr_custo_fixo_ha,
						   vlr_custo_variavel_unidade=Crops2018_raw$vlr_custo_variavel_unidade, vlr_custo_fixo_unidade=Crops2018_raw$vlr_custo_fixo_unidade,
						   vlr_renda_fator_ha=Crops2018_raw$vlr_renda_fator_ha, vlr_renda_fator_unidade=Crops2018_raw$vlr_renda_fator_unidade), 
	by = list(produto=Crops2018_raw$produto ,crop_names_helga_up1=Crops2018_raw$crop_names_helga_up1, uf=Crops2018_raw$uf, unidade_comercializacao=Crops2018_raw$unidade_comercializacao, ano=Crops2018_raw$ano), FUN=mean)
Yield <-as.data.frame(Yield_avr)	
vector_trans<-as.data.frame(unique(Yield$unidade_comercializacao))

weighEq<-vector_trans
weighEq[,2]<-0

le<-length(vector_trans)

for (i in 1:9){
	if ( weighEq[i,1]=="10 kg"){
	weighEq[i,2]=0.01
	} else if (weighEq[i,1]=="15 kg"){
	weighEq[i,2]=0.015
	} else if (weighEq[i,1]=="24 kg"){
	weighEq[i,2]=0.024
	} else if (weighEq[i,1]=="25 kg"){
	weighEq[i,2]=0.025
	} else if (weighEq[i,1]=="50 kg"){
	weighEq[i,2]=0.05
	} else if (weighEq[i,1]=="60 kg"){
	weighEq[i,2]=0.060
	} else if (weighEq[i,1]=="hl"){
	weighEq[i,2]=0.055
	} else if (weighEq[i,1]=="kg"){
	weighEq[i,2]=0.001
	} else {
	weighEq[i,2]=1
	} 
 }
 
colnames(weighEq)<-c( "unidade_comercializacao","valueW")
Yield1<-merge(Yield,weighEq, by="unidade_comercializacao")
Yield1$Country<-"Brasil"
Yield1$Yield<-((Yield1$vlr_custo_variavel_ha+Yield1$vlr_custo_fixo_ha+Yield1$vlr_renda_fator_ha)*Yield1$valueW)/(Yield1$vlr_custo_variavel_unidade+Yield1$vlr_custo_fixo_unidade+Yield1$vlr_renda_fator_unidade)
YieldP<-data.frame(Yield1$produto, Yield1$crop_names_helga_up1 ,Yield1$Country, Yield1$uf, Yield1$Yield, Yield1$ano )
colnames(YieldP)<-c( "Product","crop_names_helga_up1","Country", "Region","Yield", "Year" )
##Cost
Cost_avr<- Yield_avr
Exchange<- read_excel("HelgaCropsComplete.xlsx", sheet = "USD_RBR", col_names = TRUE)
Cost1<-merge(Cost_avr,Exchange, by="ano")
Cost1$Cost<-(Cost1$vlr_custo_variavel_ha+Cost1$vlr_custo_fixo_ha)/Cost1$Exchange
Cost1$Country<-"Brasil"
CostP<-data.frame(Cost1$produto, Cost1$crop_names_helga_up1 ,Cost1$Country, Cost1$uf, Cost1$Cost, Cost1$ano )
colnames(CostP)<-c( "Product","crop_names_helga_up1","Country", "Region","Cost", "Year" )
##Price
Price_avr<- Yield1
Exchange<- read_excel("HelgaCropsComplete.xlsx", sheet = "USD_RBR", col_names = TRUE)
Price1<-merge(Price_avr,Exchange, by="ano")
Price1$Price<-(Price1$vlr_custo_variavel_unidade+Price1$vlr_custo_fixo_unidade+Price1$vlr_renda_fator_unidade)/(Price1$Exchange*Price1$valueW)
Price1$Country<-"Brasil"
PriceP<-data.frame(Price1$produto, Price1$crop_names_helga_up1 ,Price1$Country, Price1$uf, Price1$Price, Price1$ano )
colnames(PriceP)<-c( "Product","crop_names_helga_up1","Country", "Region","Price", "Year" )

##BR Database
BR<-merge(YieldP,CostP, by=c("Product","crop_names_helga_up1", "Country", "Region", "Year"))
BR$Yield<-as.numeric(BR$Yield)
BR$Cost<-as.numeric(BR$Cost)
BR1<-merge(BR,PriceP, by=c("Product","crop_names_helga_up1", "Country", "Region", "Year"))
BR1$Yield<-as.numeric(BR1$Yield)
BR1$Cost<-as.numeric(BR1$Cost)
BR1$Price<-as.numeric(BR1$Price)

BR1$DoubleCountry<-0
for (i in Countrydupl){
	BR1$DoubleCountry[which(BR1$crop_names_helga_up1 == i)] <- 1
}


write.table(BR1,"BRData.csv",sep=";",row.names=FALSE, dec=".")

#######
#Total merge
#######

Total<-rbind(US1,BR1)
IdHelga<-read_excel("HelgaCropsComplete.xlsx", sheet = "CropId", col_names = TRUE)
Total1<-merge(Total,IdHelga, by="crop_names_helga_up1")
write.table(Total1,"ProcessedData.csv",sep=";",row.names=FALSE, dec=".")
write.dta(Total1, "ProcessedData.dta")