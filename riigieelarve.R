library(readxl)
eelarve=read_excel("./andmed/2016.aasta_riigieelarve_seadus.xlsx", skip=2)
sheets=excel_sheets("./andmed/2016.aasta_riigieelarve_seadus.xlsx")
#lapply sheedid listi
eelarve=lapply(sheets,function(y){read_excel(sheet=y,
  "./andmed/2016.aasta_riigieelarve_seadus.xlsx")})
#üheks dataframeks
eelarve=data.table::rbindlist(eelarve, fill = T)