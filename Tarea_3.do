clear

* CAMBIAR FILEPATH
cd "C:\Users\Rafael\Google Drive\MAESTRÍA ITAM\Econometría Aplicada\Tareas\Tarea 3"

use Stove.dta


/* ############################
	1. ESTADÍSTICA DESCRIPTIVA
   ############################ */

estpost sum
esttab using descr.tex, cells("count mean sd min max") replace

tab stove region, freq

/* ############################
	2. COMPOSICIÓN X REGIÓN
   ############################ */
 

 tab stove region, col nofreq
 
 
/* ############################
	3. LOGIT MULTINOMIAL stove - region
   ############################ */
 
mlogit region, base(1)

mlogit   stove i.region, base(1)
estimates store modelo
predict r1 r2 r3 r4 r5

esttab modelo using p3.tex, se label nobaselevels replace


/* ############################
	4. LOGIT MULTINOMIAL completo
   ############################ */
   
list in 1/20,clean
   
rename *_gc *1
rename *_gr *2
rename *_ec *3
rename *_er *4
rename *_hp *5

* para cada individuo i (idcase) creamos 5 observaciones, con el precio de cada estufa (ic,oc) y la variable que nos indica el precio para esa estufa es option
reshape long ic oc, i(idcase) j(option)
list in 1/20,clean

*choice es una variable indicadora de que estufa compro el id i
gen choice = 0
replace choice=1 if stove==option
label define stove_type 1 "gc" 2 "gr" 3 "ec" 4 "er" 5 "hp"
label values option stove_type
list in 1/20,clean


asclogit choice ic oc, case(idcase) alternatives(option) casevars(age num_people income i.region)   
estimates store modelo2
esttab modelo2 using p4.tex,  not label nobaselevels replace
predict pr

/* ############################
	5. P5
   ############################ */
* siguiente comando genera el efecto parcial del individuo promedio
estat mfx, varlist(num_people) at(mean)


/* ############################
	5. P6
   ############################ */
ssc install fitstat
asclogit choice ic oc, case(idcase) alternatives(option) casevars(age num_people income i.region)

*forma equivalente :O   
clogit choice ic oc i.option##c.age i.option##c.num_people i.option##c.income i.option##i.region , group(idcase)
fitstat
  
   