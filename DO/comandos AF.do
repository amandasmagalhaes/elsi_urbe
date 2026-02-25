* Onda 1
recode l5 (9 = .), gen (cam_dias)
recode l6 (8888 = .)(9999 =.), gen (cam_tempo)
gen caminhada=cam_dias*cam_tempo
replace caminhada = 0 if cam_dias==0
recode caminhada (10/149=0 "inativo") (150/max=1 "ativo"), gen(caminhada_cat)

recode l7 (9 = .), gen (mod_dias)
recode l8 (8888 = .)(9999 =.), gen (mod_tempo)
gen moderada=mod_dias*mod_tempo
replace moderada = 0 if mod_dias==0

recode l9 (9 = .), gen (vig_dias)
recode l10 (8888 = .)(9999 =.), gen (vig_tempo)
gen vigorosa = vig_dias * vig_tempo
replace vigorosa = 0 if vig_dias==0

gen vigorosa2 = 2* vigorosa
egen minutos_semana = rowtotal (caminhada moderada vigorosa2)
sum minutos_semana
replace minutos_semana = . if caminhada==.& moderada==.& vigorosa2==.
sum minutos_semana

recode minutos_semana (0/149 = 0 "inativo") (150/max = 1 "ativo"), gen (AF_global)
tab AF_global




* Onda 2 e Onda 3
recode l5 (9 = .), gen (cam_dias)
recode l6_1 (88 = .)(99 =.), gen (cam_tempo_h)
recode l6_2 (88 = .)(99 =.), gen (cam_tempo_m)
gen cam_tempo = ((cam_tempo_h * 60) + cam_tempo_m)
replace cam_tempo = . if cam_tempo <10 & cam_dias>0 
gen caminhada=cam_dias*cam_tempo
replace caminhada = 0 if cam_dias==0
recode caminhada (10/149=0 "inativo") (150/max=1 "ativo"), gen(caminhada_cat)

recode l7 (9 = .), gen (mod_dias)
recode l8_1 (88 = .)(99 =.), gen (moderada_tempo_h)
recode l8_2 (88 = .)(99 =.), gen (moderada_tempo_m)
gen mod_tempo = ((moderada_tempo_h * 60) + moderada_tempo_m)
replace mod_tempo = . if mod_tempo <10 & cam_dias>0 
gen moderada=mod_dias*mod_tempo
replace moderada = 0 if mod_dias==0

recode l9 (9 = .), gen (vig_dias)
recode l10_1 (88 = .)(99 =.), gen (vigorosa_tempo_h)
recode l10_2 (88 = .)(99 =.), gen (vigorosa_tempo_m)
gen  vig_tempo = ((vigorosa_tempo_h * 60) + vigorosa_tempo_m) 
replace vig_tempo = . if vig_tempo <10 & vig_dias>0 
gen vigorosa = vig_dias * vig_tempo
replace vigorosa = 0 if vig_dias==0

gen vigorosa2 = 2* vigorosa
egen minutos_semana = rowtotal (caminhada moderada vigorosa2)
sum minutos_semana
replace minutos_semana = . if caminhada==.& moderada==.& vigorosa2==.
sum minutos_semana

recode minutos_semana (0/149 = 0 "inativo") (150/max = 1 "ativo"), gen (AF_global)
tab AF_global




**** Análise de associação
*As variáveis exploratórias, selecionadas conforme literatura 9,14,15 , incluíram: idade (em anos), sexo (masculino, feminino), escolaridade em anos de estudo (< 4; quatro a sete; oito ou mais), estado conjugal (não casado; casado ou em união estável), cor da pele (não branca; branca), número de doenças crônicas (nenhuma; uma; duas ou mais), número de consultas médicas nos 12 meses anteriores à entrevista (nenhuma; uma ou duas; três ou mais) e se o entrevistado conhecia ou participava de algum programa público de estímulo à prática de atividade física (não conhece; conhece, mas não participa; conhece e participa), avaliada pelas perguntas: “O(A) sr.(a) conhece algum programa público no seu município de estímulo à prática de atividade física?” e “O(A) sr.(a) participa desse programa?”. As doenças crônicas foram autorreferidas pelo participante e incluíram o diagnóstico médico para hipertensão arterial, diabetes mellitus, doenças coronarianas (infarto, angina e insuficiência cardíaca), acidente vascular encefálico, doença pulmonar crônica, artrite, depressão, câncer e insuficiência renal.

********************************
*** SOCIODEMOGRÁFICAS ***
tab sexo
svy: tab sexo, percent

sum idade
svy: mean idade
estat sd

recode idade (50/59=1 "50-59") (60/69=2 "60-69") (70/79=3 "70-79") (80/max=4 "80+"), gen (faixaetaria)
tab faixaetaria
svy: tab faixaetaria, percent

tab e22
recode e22 (1/5=1 "0 a 4") ( 6/9=2 "5 a 8") (10/13=3 "9 a 11") (14/18=4 "12+") (99=.),  gen (escolaridade)
tab escolaridade
svy: tab escolaridade, percent

recode e22 (1/4=1 "<4") ( 5/8=2 "4 a 7") (9/18=3 "8+") (99=.),  gen (escolaridade2)
tab escolaridade2
svy: tab escolaridade2, percent

sum rendadompc
xtile rendadompc2corr = rendadompc [pweight = peso_calibrado], nquantiles(3)
tab rendadompc2corr

tab e7
recode e7 (1 3 4 = 1 "Não casado") (2 = 2 "casado/união estável"), gen (est_civil)
svy: tab est_civil, percent

tab e9
recode e9 (1=0 "Branca") (2/5=1 "Não branca") (9=.), gen(cor_raca)
svy: tab cor_raca, percent

***doenças crônicas
recode n28 (0 =0 "Não") ( 1=1 "Sim") (9=.) (2=.),  gen (HA)
recode n35 (0 =0 "Não") ( 1=1 "Sim") (9=.) (2=.),  gen (DM)
*recode n44 (0 =0 "Não") ( 1=1 "Sim") (9=.),  gen (COLESTEROL)
recode n46 (0 =0 "Não") ( 1=1 "Sim") (9=.),  gen (IAM)
recode n48 (0 =0 "Não") ( 1=1 "Sim") (9=.),  gen (ANG)
recode n50 (0 =0 "Não") ( 1=1 "Sim") (9=.),  gen (IC)
recode n52 (0 =0 "Não") ( 1=1 "Sim") (9=.),  gen (AVC)
recode n54 (0 =0 "Não") ( 1=1 "Sim") (9=.),  gen (ASMA)
recode n55 (0 =0 "Não") ( 1=1 "Sim") (9=.),  gen (DPOC)
recode n56 (0 =0 "Não") ( 1=1 "Sim") (9=.),  gen (ARTRITE)
*recode n57 (0 =0 "Não") ( 1=1 "Sim") (9=.),  gen (OSTEOPOROSE)
*recode n58 (0 =0 "Não") ( 1=1 "Sim") (9=.),  gen (COLUNA)
recode n59 (0 =0 "Não") ( 1=1 "Sim") (9=.),  gen (DEPRESSAO)
recode n60 (0 =0 "Não") ( 1=1 "Sim") (9=.),  gen (CANCER)
recode n61 (0 =0 "Não") ( 1=1 "Sim") (9=.),  gen (IRC)
*recode n62 (0 =0 "Não") ( 1=1 "Sim") (9=.),  gen (PARKISON)
*recode n63 (0 =0 "Não") ( 1=1 "Sim") (9=.),  gen (ALZHMEIER)

egen n_doenças = rowtotal(HA-IRC), miss
tab n_doenças

recode n_doenças (0=0 "Nenhuma") (1=1 "1") (2/max=2 "2+"), gen (dcnt)
tab dcnt

* consultas 12 meses
tab u6
recode u6 (0=0 "Nenhuma") (1 2=1 "1 a 2") (3/120=2 "3+") (999=.), gen(consultas)
tab consultas

*programa público
tab1 l13 l14
recode l13 (0 =0 "Não") ( 1=1 "Sim") (9=.),  gen (l13_1)
recode l14 (0 =0 "Não") ( 1=1 "Sim") (8=.) (9=.),  gen (l14_c)

gen programa= l13_1
replace programa = 2 if l14_c==1
label define programa 0 "Não conhece" 1 "Conhece, mas não participa" 2 "Conhece e participa"
label values programa programa
tab programa


*****************************************************************
*** Estudo da linha de base *******
egen miss = rowmiss(AF_global sexo idade escolaridade2 est_civil cor_raca dcnt consultas programa)
tab miss

svy, subpop(if miss==0): tab AF_global, percent ci



svy, subpop(if miss==0): tab AF_global, percent ci
svy, subpop(if miss==0): tab sexo AF_global, percent ci col
svy, subpop(if miss==0): tab faixaetaria AF_global, percent ci col
svy, subpop(if miss==0): tab escolaridade2 AF_global, percent ci col
svy, subpop(if miss==0): tab cor_raca AF_global, percent ci col
svy, subpop(if miss==0): tab est_civil AF_global, percent ci col
svy, subpop(if miss==0): tab dcnt AF_global, percent ci col
svy, subpop(if miss==0): tab consultas AF_global, percent ci col
svy, subpop(if miss==0): tab programa AF_global, percent ci col

******************************************************************
**** Análise de associação AF - Onda 3 ***************************


*** acamado e cadeirante
tab1 p1a p2a
tab p1a AF_global
tab p2a AF_global

*bloco comportamento de saúde - proxy
tab l39
tab l39 AF_global

gen filtro =1 if p1a==0 & p2a==0 & l39 ==0
tab filtro, miss

svy, subpop(if filtro==1): tab AF_global, percent ci
svy, subpop(if filtro==1): tab sexo AF_global, percent ci col
svy, subpop(if filtro==1): tab faixaetaria AF_global, percent ci col
svy, subpop(if filtro==1): tab escolaridade2 AF_global, percent ci col
svy, subpop(if filtro==1): tab cor_raca AF_global, percent ci col
svy, subpop(if filtro==1): tab est_civil AF_global, percent ci col
svy, subpop(if filtro==1): tab dcnt AF_global, percent ci col
svy, subpop(if filtro==1): tab consultas AF_global, percent ci col
svy, subpop(if filtro==1): tab programa AF_global, percent ci col

* frutas e hortaliças consumo regular
tab1 l15 l19
recode  l15 (9=.), gen (l15c)
recode  l19 (9=.), gen (l19c)
tab1 l15c l19c
gen consregFH = l15c + l19c
tab consregFH
recode  consregFH (0/4=0 "<5") (5/max=1 ">=5") (9=.), gen (regularFH)
tab regularFH
svy: tab regularFH, percent










*** testes


*Onda 2
recode l5 (9 = .), gen (cam_dias)
recode l6_1 (88 = .)(99 =.), gen (cam_tempo_h)
recode l6_2 (88 = .)(99 =.), gen (cam_tempo_m)
sum cam_tempo_h cam_tempo_m if cam_dias>0
gen cam_tempo = ((l6_1 * 60) + l6_2) if cam_dias>0
replace cam_tempo = . if cam_tempo <10 & cam_dias>0 
replace cam_tempo = . if cam_tempo_h==. & cam_tempo_m==. & cam_dias>0
replace cam_tempo = cam_tempo_h if cam_tempo_m==. & cam_dias>0 & cam_tempo==.
replace cam_tempo = cam_tempo_m if cam_tempo_m==. & cam_dias>0 & cam_tempo==.
sum cam_tempo cam_tempo_h cam_tempo_m if cam_dias>0
*list cam_tempo_h cam_tempo_m if cam_tempo==. & cam_dias>0
gen caminhada=cam_dias*cam_tempo
replace caminhada = 0 if cam_dias==0
recode caminhada (10/149=0 "inativo") (150/max=1 "ativo"), gen(caminhada_cat)
tab caminhada_cat
tab caminhada_cat, miss

recode l7 (9 = .), gen (mod_dias)
recode l8_1 (88 = .)(99 =.), gen (moderada_tempo_h)
recode l8_2 (88 = .)(99 =.), gen (moderada_tempo_m)
sum moderada_tempo_h moderada_tempo_m if mod_dias>0
gen mod_tempo = ((l8_1 * 60) + l8_2) if mod_dias>0
replace mod_tempo = . if mod_tempo <10 & mod_dias>0 
replace mod_tempo = . if moderada_tempo_h==. & moderada_tempo_m==. & mod_dias>0
replace mod_tempo = moderada_tempo_h if moderada_tempo_m==. & mod_dias>0 & mod_tempo==.
replace mod_tempo = moderada_tempo_m if moderada_tempo_m==. & mod_dias>0 & mod_tempo==.
sum mod_temp moderada_tempo_h moderada_tempo_m if mod_dias>0
*list moderada_tempo_h moderada_tempo_m if mod_tempo==. & mod_dias>0
gen moderada=mod_dias*mod_tempo
replace moderada = 0 if mod_dias==0
sum moderada


recode l9 (9 = .), gen (vig_dias)
recode l10_1 (88 = .)(99 =.), gen (vigorosa_tempo_h)
recode l10_2 (88 = .)(99 =.), gen (vigorosa_tempo_m)
sum vigorosa_tempo_h vigorosa_tempo_m if vig_dias>0
gen  vig_tempo = ((l10_1 * 60) + l10_2) if vig_dias>0
replace vig_tempo = . if  vig_tempo <10 & vig_dias>0 
replace vig_tempo = . if vigorosa_tempo_h==. & vigorosa_tempo_m==. & vig_dias>0
replace vig_tempo = vigorosa_tempo_h if vigorosa_tempo_m==. & vig_dias>0 &  vig_tempo==.
replace vig_tempo = vigorosa_tempo_m if vigorosa_tempo_m==. & vig_dias>0 &  vig_tempo==.
sum vig_tempo vigorosa_tempo_h vigorosa_tempo_m if vig_dias>0
*list vigorosa_tempo_h vigorosa_tempo_m if  vig_tempo==. & vig_dias>0
gen vigorosa=vig_dias* vig_tempo
replace vigorosa = 0 if vig_dias==0
sum vigorosa
gen vigorosa2 = 2* vigorosa

egen minutos_semana = rowtotal (caminhada moderada vigorosa2)
sum minutos_semana
replace minutos_semana = . if caminhada==.& moderada==.& vigorosa2==.
sum minutos_semana

recode minutos_semana (0/149 = 0 "inativo") (150/max = 1 "ativo"), gen (AF_global)

