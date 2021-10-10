* Run script for health coverage project

*This research project seeks to estimate the long-term effects of having access 
*to a health facility during the first years of life. We employ a 
*difference-in-differences strategy with data from SUSALUD establishments 
*and the Demographic and Family Health Survey.

* CONTENTS
	* Set folder globals and schemes
	* Clean renipress data
	* Clean ENDES dataset
	* Construct main dataset
	* Descriptive stats
	* Diff-in-diff regression
	* Health mechanism regression

* User path --------------------------------------------------------------------

	global datawork ""

* Folder globals ---------------------------------------------------------------

	global data_source1		"$datawork/datasets/data_source1"
	global data_source2		"$datawork/datasets/data_source2"
	global data_source3		"$datawork/datasets/data_source3"
	global data_construct	"$datawork/datasets/data_constructed"
	global scripts			"$datawork/scripts"
	global results			"$datawork/results"
   
* Scheme and packages ----------------------------------------------------------
   
	*ssc install importsav
	*ssc install grc1leg
	*ssc install drdid
	*ssc install csdid
	*ssc install reghdfe
   
	set scheme white_tableau
	set matsize 5000

* Clean renipress data ---------------------------------------------------------

* Load data
import excel "$data_source1/raw/USLRC20210101231308_xp.xls", sheet("Listado de Establecimientos") firstrow clear

* Drop private institutions
drop if Institución == "PRIVADO"
* Keep Cusco department
keep if Departamento == "CUSCO"

* Gen creation year
gen year= real(substr( IniciodeActividad ,7,4))

* Gen province and district ID
gen id_prov = substr(UBIGEO ,1,4)
gen id_dis = UBIGEO
destring id_dis, replace

tempfile renipress
save `renipress'

** Distrito Analysis

use `renipress', clear

* We keep ID district and year
keep id_dis year
sort id_dis year

* We drop duplicates in years where more than one hospital was created in a district
duplicates report id_dis year 
duplicates drop id_dis year, force
tsset id_dis year

** We drop districts that had a health center before 1965
gen temp_variable = 1 if year < 1965
bysort id_dis: carryforward temp_variable, gen(district_not_considered)

drop if district_not_considered == 1
drop temp_variable district_not_considered

** We drop districts created after 1990
drop if year > 1990

** We drop duplicade districts
duplicates drop id_dis, force

* We generate treatment indicator for the selected districts
gen temp_treat = 1
gen temp_year = year
tsfill, full
bysort id_dis: carryforward temp_treat, gen(treatment)
bysort id_dis: carryforward temp_year, gen(first_treat)

replace treatment = 0 if treatment ==.
replace first_treat = 0 if first_treat ==.

drop temp_treat temp_year

bysort id_dis (year) : replace treatment = year[1] if first_treat == 1


save "$data_source1/clean/data_renipress_clean.dta", replace

* Clean ENDES dataset ----------------------------------------------------------
	
forval date = 2018/2019 {

	* RECH5 Contains information on women's anemia
	importsav "Endes/74_peso_talla/RECH5_`date'.SAV"
	rename *, lower
	
	*rename id1 year
	rename ha1 edad
	rename ha2 peso_kg
	rename ha3 tabla_kg
	rename ha40 indice_masa_corporal
	rename ha56 hemoglobina_ajustado
	rename ha57 anemia
	rename ha66 nivel_educativo
	
	keep hhid edad peso_kg tabla_kg indice_masa_corporal hemoglobina_ajustado anemia nivel_educativo
	
	tempfile endes_rech5_`date'
	save `endes_rech5_`date''
	
	* RECH0 Contains location information
	importsav "Endes/64_caracteristicas_hogar/RECH0_`date'.SAV"
	rename *, lower
	gen year = `date'
	rename ubigeo id_dis
	destring id_dis, replace
	rename hv024 region
	
	keep hhid year id_dis region

	tempfile endes_rech0_`date'
	save `endes_rech0_`date''

	use `endes_rech0_`date''
	merge 1:m hhid using `endes_rech5_`date'', nogen keep(matched)
	
	tempfile endes_`date'
	save `endes_`date''
	}

* Append datasets
use `endes_2018'
append using `endes_2019'

* We gen year of birth
rename year year_survey
gen year = year_survey - edad

save "$data_source3/clean/endes_idperso_clean.dta", replace

* Construct main dataset -------------------------------------------------------
use "$data_source2/clean/enaho_iddis_clean.dta", clear

** Realizamos merge
merge 1:1 year id_dis using "$data_source1/clean/data_renipress_clean.dta", keep(using matched)

* Descriptive stats ------------------------------------------------------------

** Summary tables

use "$data_construct/data_enaho_idperso.dta", clear
   
   estpost summarize  p301a ing_total experiencia rural p207
   esttab using "$results/tables/summarytable2.rtf", ///
   nonumber noobs label replace ///
   cells("count mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") 
   unicode convertfile "$results/tables/summarytable2.rtf" "$results/tables/summarytable2_utf8.rtf", srcencoding(UTF-8) dstencoding(macintosh) replace

	* Treatment-control
	eststo clear
	estpost tabstat  p301a ing_total experiencia rural p207, by(treatment)  ///
	statistics(mean) columns(statistics)
	esttab . using "$results/tables/summarytable1.rtf",  replace main(mean)
   unicode convertfile "$results/tables/summarytable1.rtf" "$results/tables/summarytable1_utf8.rtf", srcencoding(UTF-8) dstencoding(macintosh) replace
   
use "$data_construct/data_endes_idperso.dta", clear

   estpost summarize  indice_masa_corporal anemia hemoglobina_ajustado
   esttab using "$results/tables/summarytable3.rtf", ///
   nonumber noobs label replace ///
   cells("count mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") 
   unicode convertfile "$results/tables/summarytable3.rtf" "$results/tables/summarytable3_utf8.rtf", srcencoding(UTF-8) dstencoding(macintosh) replace

* Diff-in-diff regression --------------------------------------------------------
   
** Main regression table

	use "$data_construct/data_enaho_idperso.dta", clear

	eststo clear
	* Education
	reghdfe p301a treatment p207, absorb(year id_dis id_dis#c.trend) vce( cluster id_dis)
		eststo
	   quietly estadd local fixed_dis "Si", replace
	   quietly estadd local fixed_year "Si", replace
	   sum p301a if treatment == 0
	   quietly estadd local mean_treatment = r(mean), replace

	* Income
	reghdfe ing_total treatment p207, absorb(year id_dis id_dis#year_survey)  vce( cluster id_prov)
		eststo
	   quietly estadd local fixed_dis "Si", replace
	   quietly estadd local fixed_year "Si", replace
	   sum ing_total if treatment == 0
	   quietly estadd local mean_treatment = r(mean), replace

	
	* Employment
	reghdfe experiencia treatment p207, absorb(year id_dis id_dis#c.trend)  vce( cluster id_dis)
		eststo
		quietly estadd local fixed_dis "Si", replace
		quietly estadd local fixed_year "Si", replace
		sum experiencia if treatment == 0
		quietly estadd local mean_treatment = r(mean), replace

   esttab , keep(treatment) ///
   star(* 0.10 ** 0.05 *** 0.01) ///
   label	///
   stats(mean_treatment fixed_dis fixed_year N, label("Promedio pre-tratamiento" "Tendencia tiempo-distrito" "Efectos fijo año" "Observaciones")) ///
   title("Tabla de regresión") ///
   mtitles ("Educación" "Ingresos" "Duración empleo") ///
   nonotes addnotes( "Nota. * p<0.10, ** p<0.05, *** p<0.01. Errores robustos en paréntesis") ///
   compress replace

   unicode convertfile "$results/tables/main_table.rtf" "$results/tables/main_table_utf8.rtf", srcencoding(UTF-8) dstencoding(macintosh) replace
   
   
** Heterogeneous effects
   
   *** Women
   preserve
   keep if p207 == 1
   eststo clear
   * Education
	reghdfe p301a treatment p207, absorb(year id_dis id_dis#c.trend) vce( cluster id_dis)
		eststo
		sum p301a if treatment == 0
		quietly estadd local mean_treatment = r(mean), replace

	* Income
	reghdfe ing_total treatment p207, absorb(year id_dis id_dis#year_survey)  vce( cluster id_prov)
		eststo
	   sum ing_total if treatment == 0
	   quietly estadd local mean_treatment = r(mean), replace
	
	* Employment
	reghdfe experiencia treatment p207, absorb(year id_dis id_dis#c.trend)  vce( cluster id_dis)
		eststo
		sum experiencia if treatment == 0
		quietly estadd local mean_treatment = r(mean), replace
	
   esttab using "$results/tables/heffects_women.rtf", keep(treatment) ///
   star(* 0.10 ** 0.05 *** 0.01) ///
   label	///
   stats(mean_treatment  N, label("Promedio pre-tratamiento" "Observaciones")) ///
   title("Tabla de regresión") ///
   mtitles ("Educación" "Ingresos" "Duración empleo") ///
   nonotes addnotes( "Nota. * p<0.10, ** p<0.05, *** p<0.01. Errores robustos en paréntesis") ///
   compress replace

   unicode convertfile "$results/tables/heffects_women.rtf" "$results/tables/heffects_women_utf8.rtf", srcencoding(UTF-8) dstencoding(macintosh) replace
   
* Health mechanism regression --------------------------------------------------

use "$data_construct/data_endes_idperso.dta", clear

	eststo clear

	reghdfe indice_masa_corporal treatment , absorb(id_dis year)
	eststo
	   quietly estadd local fixed_dis "Si", replace
	   quietly estadd local fixed_year "Si", replace
	   sum indice_masa_corporal if treatment == 0
	   estadd local mean_treatment = r(mean), replace

	reghdfe anemia treatment , absorb(id_dis year ) 
	eststo
	   quietly estadd local fixed_dis "Si", replace
	   quietly estadd local fixed_year "Si", replace
	   sum anemia if treatment == 0
	   estadd local mean_treatment = r(mean), replace

	reghdfe hemoglobina_ajustado treatment , absorb(id_dis year ) 
	eststo
	   quietly estadd local fixed_dis "Si", replace
	   quietly estadd local fixed_year "Si", replace
	   sum hemoglobina_ajustado if treatment == 0
	   estadd local mean_treatment = r(mean), replace

   esttab using "$results/tables/mechanism.rtf", keep(treatment) ///
   star(* 0.10 ** 0.05 *** 0.01) ///
   label	///
   stats(mean_treatment fixed_dis fixed_year N, label("Promedio pre-tratamiento" "Efectos fijos distrito" "Efectos fijo año" "Observaciones")) ///
   title("Tabla de regresión") ///
   mtitles ("Índice Masa Corporal" "Anemia" "Hemoglobina") ///
   nonotes addnotes( "Nota. * p<0.10, ** p<0.05, *** p<0.01. Errores robustos en paréntesis") ///
   compress replace

   unicode convertfile "$results/tables/mechanism.rtf" "$results/tables/mechanism_utf8.rtf", srcencoding(UTF-8) dstencoding(macintosh) replace
