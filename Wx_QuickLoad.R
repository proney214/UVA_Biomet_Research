library(readxl)
CHO_Final_Weather <- read_excel("~/Desktop/CHO.Final.Weather.xlsx")
CHO_Final_Weather  <- CHO_Final_Weather[1:5844,]
EMV_Final_Weather <- read_excel("~/Desktop/EMV.Final.Weather.xlsx")
EZF_Final_Weather <- read_excel("~/Desktop/EZF.Final.Weather.xlsx")
IAD_Final_Weather <- read_excel("~/Desktop/IAD.Final.Weather.xlsx")
LYH_Final_Weather <- read_excel("~/Desktop/LYH.Final.Weather.xlsx")
OKV_Final_Weather <- read_excel("~/Desktop/OKV.Final.Weather.xlsx")
ORF_Final_Weather <- read_excel("~/Desktop/ORF.Final.Weather.xlsx")
PHF_Final_Weather <- read_excel("~/Desktop/PHF.Final.Weather.xlsx")
RIC_Final_Weather <- read_excel("~/Desktop/RIC.Final.Weather.xlsx")
ROA_Final_Weather <- read_excel("~/Desktop/ROA.Final.Weather.xlsx")
SHD_Final_Weather <- read_excel("~/Desktop/SHD.Final.Weather.xlsx")
VJI_Final_Weather <- read_excel("~/Desktop/VJI.Final.Weather.xlsx")

BlackMortVA <- CHO_Final_Weather$Black + EMV_Final_Weather$Black + EZF_Final_Weather$Black + IAD_Final_Weather$Black + LYH_Final_Weather$Black+OKV_Final_Weather$Black+ORF_Final_Weather$Black+PHF_Final_Weather$Black+RIC_Final_Weather$Black+ROA_Final_Weather$Black+SHD_Final_Weather$Black+VJI_Final_Weather$Black
WhiteMortVA <- CHO_Final_Weather$White + EMV_Final_Weather$White + EZF_Final_Weather$White + IAD_Final_Weather$White + LYH_Final_Weather$White+OKV_Final_Weather$White+ORF_Final_Weather$White+PHF_Final_Weather$White+RIC_Final_Weather$White+ROA_Final_Weather$White+SHD_Final_Weather$White+VJI_Final_Weather$White
MortMortVA <- CHO_Final_Weather$Mort + EMV_Final_Weather$Mort + EZF_Final_Weather$Mort + IAD_Final_Weather$Mort + LYH_Final_Weather$Mort+OKV_Final_Weather$Mort+ORF_Final_Weather$Mort+PHF_Final_Weather$Mort+RIC_Final_Weather$Mort+ROA_Final_Weather$Mort+SHD_Final_Weather$Mort+VJI_Final_Weather$Mort

sum(BlackMortVA)
sum(WhiteMortVA)
sum(MortMortVA)

MortMortVA <- CHO_Final_Weather$Mort + EMV_Final_Weather$Mort + EZF_Final_Weather$Mort + IAD_Final_Weather$Mort + LYH_Final_Weather$Mort+OKV_Final_Weather$Mort+ORF_Final_Weather$Mort+PHF_Final_Weather$Mort+RIC_Final_Weather$Mort+ROA_Final_Weather$Mort+SHD_Final_Weather$Mort+VJI_Final_Weather$Mort
MortMDC01VA <- CHO_Final_Weather$MDC.01 + EMV_Final_Weather$MDC.01 + EZF_Final_Weather$MDC.01 + IAD_Final_Weather$MDC.01 + LYH_Final_Weather$MDC.01+OKV_Final_Weather$MDC.01+ORF_Final_Weather$MDC.01+PHF_Final_Weather$MDC.01+RIC_Final_Weather$MDC.01+ROA_Final_Weather$MDC.01+SHD_Final_Weather$MDC.01+VJI_Final_Weather$MDC.01
MortMDC04VA <- CHO_Final_Weather$MDC.04 + EMV_Final_Weather$MDC.04 + EZF_Final_Weather$MDC.04 + IAD_Final_Weather$MDC.04 + LYH_Final_Weather$MDC.04+OKV_Final_Weather$MDC.04+ORF_Final_Weather$MDC.04+PHF_Final_Weather$MDC.04+RIC_Final_Weather$MDC.04+ROA_Final_Weather$MDC.04+SHD_Final_Weather$MDC.04+VJI_Final_Weather$MDC.04
MortMDC05VA <- CHO_Final_Weather$MDC.05 + EMV_Final_Weather$MDC.05 + EZF_Final_Weather$MDC.05 + IAD_Final_Weather$MDC.05 + LYH_Final_Weather$MDC.05+OKV_Final_Weather$MDC.05+ORF_Final_Weather$MDC.05+PHF_Final_Weather$MDC.05+RIC_Final_Weather$MDC.05+ROA_Final_Weather$MDC.05+SHD_Final_Weather$MDC.05+VJI_Final_Weather$MDC.05
MortMDC19VA <- CHO_Final_Weather$MDC.19 + EMV_Final_Weather$MDC.19 + EZF_Final_Weather$MDC.19 + IAD_Final_Weather$MDC.19 + LYH_Final_Weather$MDC.19+OKV_Final_Weather$MDC.19+ORF_Final_Weather$MDC.19+PHF_Final_Weather$MDC.19+RIC_Final_Weather$MDC.19+ROA_Final_Weather$MDC.19+SHD_Final_Weather$MDC.19+VJI_Final_Weather$MDC.19
MortNonBillVA <- CHO_Final_Weather$Non.billable.ICD + EMV_Final_Weather$Non.billable.ICD + EZF_Final_Weather$Non.billable.ICD + IAD_Final_Weather$Non.billable.ICD + LYH_Final_Weather$Non.billable.ICD+OKV_Final_Weather$Non.billable.ICD+ORF_Final_Weather$Non.billable.ICD+PHF_Final_Weather$Non.billable.ICD+RIC_Final_Weather$Non.billable.ICD+ROA_Final_Weather$Non.billable.ICD+SHD_Final_Weather$Non.billable.ICD+VJI_Final_Weather$Non.billable.ICD

sum(MortMDC01VA)
sum(MortMDC04VA)
sum(MortMDC05VA)
sum(MortMDC19VA)
sum(MortNonBillVA)
