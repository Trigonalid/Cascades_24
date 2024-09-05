Cascading diversity

3) "Enemy free space"      - Vytvořit metaweb ze všech záznamů interakcí
                           - random vybrat interakce z metaweb na velikost lokálních sítí
                           - porovnat kompartmenty mezi subsamplovanýma sítěma a původníma
                           - porovnáním získáme informaci o tom, jaká je šance, že potenciálně můžeme získat danou interakci na dané lokalitě.



I have a dataset loaded in R, which contains information about locality, host plant, caterpillar and parasitoid. 
Hlavní cíl: zjistit, jaký je průměrný počet interakcí mezi housenkou a parazitoidem na dané lokalitě a rostlině -> 

1)	For each locality, count number of plant species, caterpillar and parasitoid species, their abundances as well,  and save the numbers into the table.
2)	Count number of caterpillar species for each plant at every locality.
3)	I need to randomly pick data from whole dataset. Amount of data will be according to the number of caterpillars per each host plant at each locality. It is important that for each plant the records will be picked from the whole dataset, but only those which are connected with the same host plant.
4)	Data saves into the table.
5)	From the randomly picked table, count the number of caterpillars and parasitoids, and count mean of parasitoids per host caterpillar.
6)	Do this random subsampling 999 times.
7)	Save the results into the table, not list.
8)	For each locality and host plant, count the unique links between caterpillar and parasitoid, and save the results.









---------------
Ondra TODO:
DIVERZITA
1) Subsample Ohu1 to size of Ohu2 - náhodně vyber řádky z datasetu Ohu1  na počet záznamů Ohu2 - 999x repl. -> To udělat jak pro parazitoidy (Par_remove=0), tak pro housenky (cat_remove=0). Získaná náhodná společenstva porovnat s původní hodnotou Ohu1, Ohu2 a průměrem sub_Ohu1 pomocí indexů Sorensen (potřeba předělat abundance na presence/absence druhy), Chao-Sorensen (ponechat abundance), Bray-Curtis (ponechat abundance).

DISSIMILARITY OF FW INTERACTIONS
2) Subsampling potravních sítí caterpillar-plant na velikost parazitoid-caterpillar pro každou lokalitu (např. pomocí velikosti dané food-web na dané lokalitě).
     - spočítat Network dissimilarity (beta diversity) and its components (S, OS, WN, ST, ST.l, ST.h, ST.lh) - betalinkr_multi(webs2array(...) 
     - získáme tabulku s jednotlivými hodnotami
     - provedeme 999x, a výsledné hodnoty se zprůměrují pro každý z těch komponent zvlášť
     - porovnáme průměrné hodnoty z subsamplovaných sítí s původními hodnotami (celkově to bude 36 vs 36 hodnot (zignorujeme odchylky z těch replikací).
     - získáme informaci o tom, co se stane, když změnšíme dataset CAT-PLANT na velikost PAR-CAT, a zjistíme, jestli pozorovaný rozdíl (větší dissimilarita interakcí v sítích PAR-CAT oproti CAT-      PLANT) je způsobena sampling effortem nebo ne.
     - subsamplované datasety porovnáme pomocí mantel testu, a zjistíme, jak se jednotlivé kompartmenty liší se vzdáleností (uvažovali jsme dva způsoby i) vzít celkový průměr ze subsampled a udělat jeden mantel test, NEBO ii) udělat 999x mantel test pro každou subsamplovanou síť






------------


!!! Pro analyzy najít systém, kterým jsme schopni porovnat body - FRAMEWORK na další analýzy

1)  Subsampling Ohu1 na velikost Ohu2 ->  otestovat beta diverzitu - Sorensen, Chao-Sorensen, Bray-Curtis s ostatními lokalitami
2)  Porovnat tyto tři datasety statisticky (Caterpillars, Parasitoids, Subsampled caterpillars) pro všechny tři indexy
______
3)  Subsampling datasetu Caterpillar-Plant na velikost Parasitoid-Caterpillar -> porovnat Network dissimilarity (beta diversity) and its components (S, OS, WN, ST, ST.l, ST.h, ST.lh)
4)  Enemy free-space: Aim: Jaká je šance, že chybí na dané lokalitě možná interakce získaná z metaweb? Aim: Jaká je šance, že se chybí na dané lokalitě vyskytovat možná interakce získaná z metaweb?
     - housenka může být parazitovaná vícero druhy parazitoidů -> zajímá nás, jak často tam toho svého parazitoida nemá -> resampling metaweb na lokality zvlášť, a tím získáme informaci o tom, jaká je šance, že daná realizovaná interakce je nebo není přítomna?
     - 


  


More details (with some comments)
1) Subsample Ohu 1 to size of Ohu 2 -

- Aim: Resample bigger Ohu 1 to the size of Ohu 2, and describe the difference -> description of two datasets at the same locality - Does the sampling has the effect?
Statisticky rámec, ve kterém můžeme porovnávat reálné komunity s reasamplovanyma 
- rezignace na vztah se vzdáleností, pokud to provnáme v rámci modelu 
- ((popřípadě použít vzdálenost jako covariate -> porovnání parazitoidní betadiversity mezi Ohu1 a Ohu2 resampled - potřeba to kvantifikovat))

Otestovat beta diverzitu - Sorensen, Chao-Sorensen, Bray-Curtis

2)  Food-Web interactions

A) Role of sampling effect - different sample size
Aim of subsampling -> find out if the difference between parasitoid-caterpillar and caterpillar-plant interactions in dissimilarity is not caused by sampling error (we have more records of caterpillars than parasitoids (~ 10x more cats))
1) Subsampling data from Metaweb according to sample size at a particular locality (random 99x - for now is enough)
2) Subsample data for  Caterpillar-Plant to Parasitoid-Caterpillar.
3) For each subsampled dataset calculate the Network dissimilarity (beta diversity) and its components (S, OS, WN, ST, ST.l, ST.h, ST.lh)
4) Compare subsampled datasets with the original data
5) Make a figure where we compare all

comments
- keep all data with identified specimens only

Hostitelská specializace, beta diversity a beta-diversity v hostitelské specializaci (protože rewiring nepracuje v rámci Metaweb)
Porovnání metaweb s lokálníma sítěma

Co tím získáme?
1) Density dependence - napadají parazitoidi ty hojné druhy
2) Globální preference je 50 40 10
3) lokální je 70 10 20
4) Is there density dependence in which host is being attacked at each site? Preferují se ty hojné druhy housenek víc než ty vzácné?



B) Role of host specificity in beta diversity of interactions
Aim: Describe the role of host specificity in the observed phenomenon

1) Separate parasitoids to generalists and specialists (without singletons and doubletons)
2) Calculate the network dissimilarity
3) Compare with original data

C) Host specificity of parasitoids
Aim: What is the apparent competition of parasitoids in a particular area
Q: Jak často napadají housenky své hostitele, pokud jsou přítomni v dané lokalitě, aneb jaká je realizovaná specializace na dané lokalitě





D) Ohu resampling
Aim: Resample bigger Ohu 1 to the size of Ohu 2, and describe the difference -> description of two datasets at the same locality - Does the sampling has the effect?
Statisticky rámec, ve kterém můžeme porovnávat reálné komunity s reasamplovanejma - rezignace na vztah se vzdáleností, pokud to provnáme v rámci modelu - popřípadě použít vzdálenost jako covariate -> porovnání parazitoidní betadiversity mezi Ohu1 a Ohu2 resampled - potřeba to kvantifikovat

E) Enemy free-space
Aim: Jaká je šance, že se chybí na dané lokalitě vyskytovat možná interakce získaná z metaweb?
Enemy free space - housenka může být parazitovaná vícero druhy parazitoidů -> zajímá nás, jak často tam toho svého parazitoida nemá -> resampling metaweb na lokality zvlášť, a tím získáme informaci o tom, jaká je šance, že daná realizovaná interakce je nebo není přítomna.

To znamená, že získáme informaci o tom, jak často ten parazitoid na dané lokalitě není, přestože dle resamplingu z metaweb by se dalo očekávat, že tam bude.
Dataset je založený na parazitovaných housenkách 
- hostitelská specializace na urovni metaweb je zachovaná, nedochází k přehazování parazitoidů
- resampling na velikost lokality

- 







