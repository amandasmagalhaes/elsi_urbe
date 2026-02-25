*zona - 1 urbano
* ci1 - 1 - proxy
*gen filtro = 1 if zona==1 & ci1==0
gen filtro2= 1 if zona==1 
tab filtro2

***criar score (sintomas depressivos)

tab1 r2 r3 r4 r5 r6 r7 r8 r9
recode r2 r3 r4 r5 r6 r7 r8 r9 (8 9=.)
recode r5 (0=1) (1=0), gen (r5invet)
recode r7 (0=1) (1=0), gen (r7invet)
egen score_dep1=rowtotal(r2 r3 r4 r5invet r6 r7invet r8 r9)

***excuir missing
replace score_dep1=. if r1==.
tab score_dep1
gen missingescore=r2+r3+r4+r5invet+r6+r7invet+r8+r9
replace score_dep1=. if score_dep1 <4 & missingescore==.
replace score_dep1=. if score_dep1 <4 & (r2==.| r3==.| r4==.| r5==.| r6==.| r7==.| r8==.| r9==.)

***classificar depressão
recode score_dep1 (0/3=0 "não") (4/8=1 "sim"), gen (sint_dep)
tab sint_dep
tab sint_dep if filtro2==1, miss

**prevalência depressão
svy, subpop (if filtro2==1): tab sint_dep, col percent ci



egen  sum_w2=sum(peso_calibrado_n) if filtro2==1, by( setor )
egen  sum_wsq2=sum(peso_calibrado_n^2) if filtro2==1, by( setor )
generate wt2= peso_calibrado_n*sum_w2/sum_wsq2

melogit sint_dep [pweight = wt2] if filtro2==1 || setor:, eform

egen stdarvorep = std( arvorep )
egen stdbueirop = std( bueirop )
egen stdcalçadap = std( calçadap )
egen stdesgotop = std( esgotop )
egen stdiluminaçãop = std( iluminaçãop )
egen stdlixop = std( lixop )
egen stdmeiofiop = std( meiofiop )
egen stdpavimentaçãop = std( pavimentaçãop )
egen stdrampap = std( rampap )


melogit sint_dep desordemp [pweight = wt2] if filtro2==1 || setor:, eform
melogit sint_dep stdarvorep [pweight = wt2] if filtro2==1 || setor:, eform
melogit sint_dep stdbueirop [pweight = wt2] if filtro2==1 || setor:, eform
melogit sint_dep stdcalçadap [pweight = wt2] if filtro2==1 || setor:, eform
melogit sint_dep stdesgotop [pweight = wt2] if filtro2==1 || setor:, eform
melogit sint_dep stdiluminaçãop [pweight = wt2] if filtro2==1 || setor:, eform
melogit sint_dep stdlixop [pweight = wt2] if filtro2==1 || setor:, eform
melogit sint_dep stdmeiofiop [pweight = wt2] if filtro2==1 || setor:, eform
melogit sint_dep stdpavimentaçãop [pweight = wt2] if filtro2==1 || setor:, eform
melogit sint_dep stdrampap [pweight = wt2] if filtro2==1 || setor:, eform