* FINAL COMMANDS FOR PRIMO, BINDER, MALTZMAN 
* "WHO CONSENTS? COMPETING PIVOTS IN FEDERAL JUDICIAL SELECTION"
* American Journal of Political Science, Vol. 52, No. 3, July 2008, Pp. 471Ð489

* DO FILE FINALIZED FEBRUARY 12, 2008

* To run appellate court estimates, use ajps2008appellate.dta  (formerly, newprimo2007final.dta)
* To run district court estimates, use ajps2008district.dta (formerly districtdata80109clean.dta)

*TABLE 1 is summary of pivotal politics models

*TABLE 2 SUMMARY STATISTICS
*NOTE: REPEAT FOR BOTH DISTRICT AND APPELLATE COURTS WITH SEPARATE DATA FILES

su failed if congress >93 & stateid ~=55
su failed if congress >93 & congress <101 & stateid~=55
su failed if congress >100 & stateid ~=55
su mediangridlock weakmajgridlock purefilibustzone purebluezone committeezone bsfilibustzone majbluezone majfilizone majbluefili commpartyzone commfilizone commbluezone commfilibluezone commpartyfilizone commpartybluezone newfullmodelzone divided if congress>93 & stateid ~=55


*TABLE 3 NOMINEE FAILURE RATES BY CONGRESS
*NOTE: REPEAT FOR BOTH DISTRICT AND APPELLATE COURTS WITH SEPARATE DATA FILES

table congress if stateid ~=55 & congress >93, c(mean failed count success)



*TABLES 4-6 ARE FOR COURTS OF APPEALS
*TABLES 7-9 ARE FOR DISTRICT COURTS
*RERUN CODE ON APPELLATE DATA SET AND THEN DISTRICT DATA SET


*TABLE 4 AND 7 - LOGIT ESTIMATES 1975-2006, NO DC CIRCUIT NOMINEES
*NOTE THERE ARE 17 MODELS (16 GRIDLOCK ZONES + DIVIDED GOVERNMENT)
*NOTE TO GET PRE INSERT pred if stateid ~=55 & congress > 93AFTER THE FITSTAT LINE

logit failed mediangridlock if stateid ~=55 & congress > 93, robust cluster (congress)fitstat
logit failed weakmajgridlock if stateid ~=55 & congress > 93, robust cluster (congress)
fitstat
logit failed purebluezone  if stateid ~=55 & congress > 93, robust cluster (congress)
fitstat
logit failed purefilibustzone if stateid ~=55 & congress > 93, robust cluster (congress)fitstat
logit failed bsfilibustzone  if stateid ~=55 & congress > 93, robust cluster (congress)fitstat
logit failed majbluezone if stateid ~=55 & congress > 93, robust cluster (congress)fitstat
logit failed majfilizone if stateid ~=55 & congress > 93, robust cluster (congress)fitstat
logit failed majbluefili if stateid ~=55 & congress > 93, robust cluster (congress)fitstat
logit failed divided if stateid ~=55 & congress > 93, robust cluster (congress)fitstat
logit failed committeezone if stateid ~=55 & congress >93, robust cluster (congress)
fitstat
logit failed commfilizone if stateid ~=55 & congress >93, robust cluster (congress)
fitstat
logit failed commbluezone if stateid ~=55 & congress >93, robust cluster (congress)
fitstat
logit failed commpartyzone if stateid ~=55 & congress >93, robust cluster (congress)
fitstat
logit failed commpartyfilizone if stateid ~=55 & congress >93, robust cluster (congress)
fitstat
logit failed commpartybluezone if stateid ~=55 & congress >93, robust cluster (congress)
fitstat
logit failed commfilibluezone if stateid ~=55 & congress >93, robust cluster (congress)
fitstat
logit failed newfullmodelzone if stateid ~=55 & congress >93, robust cluster (congress)
fitstat

*TABLE 4 CHANGE IN PROBABILITY OF REJECTION

su majfilizone if congress>93 & stateid ~=55, detail
*mean majfilizone = .746 and std = .184

logit failed majfilizone if stateid ~=55 & congress >93, robust cluster (congress)
mfx compute
mfx compute, at (.562)
mfx compute, at (.930)

su mediangridlock if congress>93 & stateid ~=55, detail
*mean mediangridlock = .534 and std = .132

logit failed mediangridlock if stateid ~=55 & congress > 93, robust cluster (congress)
mfx compute
mfx compute, at (.402)
mfx compute, at (.666)

su newfullmodelzone if congress>93 & stateid ~=55, detail
*mean newfullmodelzone = .873 and std = .200

logit failed newfullmodelzone if stateid ~=55 & congress >93, robust cluster (congress)
mfx compute
mfx compute, at (.673)
mfx compute, at (1.073)


su purebluezone if congress>93 & stateid ~=55, detail
*mean purebluezone = .776 and std = .239

logit failed purebluezone  if stateid ~=55 & congress > 93, robust cluster (congress)
mfx compute
mfx compute, at (.537)
mfx compute, at (1.015)



*TABLE 5 AND TABLE 8
*TIME PERIOD: 1975-1988  (FORD --> REAGAN)

logit failed mediangridlock   if stateid ~=55 & congress > 93 & congress <101, robust cluster (congress)fitstat
logit failed weakmajgridlock  if stateid ~=55 & congress > 93 & congress <101 , robust cluster (congress)
fitstat
logit failed purebluezone  if stateid ~=55 & congress > 93 & congress <101, robust cluster (congress)
fitstat
logit failed purefilibustzone  if stateid ~=55 & congress > 93 & congress <101, robust cluster (congress)fitstat
logit failed bsfilibustzone   if stateid ~=55 & congress > 93 & congress <101, robust cluster (congress)fitstatlogit failed majbluezone if stateid ~=55 & congress > 93 & congress <101, robust cluster (congress)fitstat
logit failed majfilizone  if stateid ~=55 & congress > 93 & congress <101, robust cluster (congress)fitstat
logit failed majbluefili if stateid ~=55 & congress > 93 & congress < 101, robust cluster (congress)fitstat
logit failed divided if stateid ~=55 & congress > 93 & congress <101, robust cluster (congress)fitstat
logit failed committeezone if stateid ~=55 & congress >93 & congress <101, robust cluster (congress)
fitstat
logit failed commfilizone if stateid ~=55 & congress >93 & congress <101, robust cluster (congress)
fitstat
logit failed commbluezone if stateid ~=55 & congress >93 & congress <101, robust cluster (congress)
fitstat
logit failed commpartyzone if stateid ~=55 & congress >93 & congress <101, robust cluster (congress)
fitstat
logit failed commpartyfilizone if stateid ~=55 & congress >93 & congress <101, robust cluster (congress)
fitstat
logit failed commpartybluezone if stateid ~=55 & congress >93 & congress <101, robust cluster (congress)
fitstat
logit failed commfilibluezone if stateid ~=55 & congress >93 & congress <101, robust cluster (congress)
fitstat
logit failed newfullmodelzone if stateid ~=55 & congress >93 & congress <101, robust cluster (congress)
fitstat


*TABLE 6 AND TABLE 9
*TIME PERIOD: 1989-2006 (BUSH I --> BUSH II)


logit failed mediangridlock   if stateid ~=55 & congress > 100, robust cluster (congress)fitstat
logit failed weakmajgridlock  if stateid ~=55 & congress > 100 , robust cluster (congress)
fitstat
logit failed purebluezone  if stateid ~=55 & congress > 100, robust cluster (congress)
fitstat
logit failed purefilibustzone  if stateid ~=55 & congress > 100, robust cluster (congress)fitstat
logit failed bsfilibustzone   if stateid ~=55 & congress > 100, robust cluster (congress)fitstat
logit failed majbluezone if stateid ~=55 & congress > 100, robust cluster (congress)fitstat
logit failed majfilizone  if stateid ~=55 & congress > 100, robust cluster (congress)fitstat
logit failed majbluefili if stateid ~=55 & congress > 100, robust cluster (congress)fitstat
logit failed divided if stateid ~=55 & congress > 100, robust cluster (congress)fitstat
logit failed committeezone if stateid ~=55 & congress >100, robust cluster (congress)
fitstat
logit failed commfilizone if stateid ~=55 & congress >100, robust cluster (congress)
fitstat
logit failed commbluezone if stateid ~=55 & congress >100, robust cluster (congress)
fitstat
logit failed commpartyzone if stateid ~=55 & congress >100, robust cluster (congress)
fitstat
logit failed commpartyfilizone if stateid ~=55 & congress >100, robust cluster (congress)
fitstat
logit failed commpartybluezone if stateid ~=55 & congress >100, robust cluster (congress)
fitstat
logit failed commfilibluezone if stateid ~=55 & congress >100, robust cluster (congress)
fitstat
logit failed newfullmodelzone if stateid ~=55 & congress >100, robust cluster (congress)
fitstat




*FOR FOOTNOTE NOTING LACK OF CHANGE WHEN DC APPELLATE NOMINEES INCLUDED (NO BLUE SLIP MODELS)

logit failed mediangridlock if congress > 93, robust cluster (congress)fitstat
logit failed weakmajgridlock if congress > 93, robust cluster (congress)
fitstat
logit failed purefilibustzone if congress > 93, robust cluster (congress)fitstat
logit failed majfilizone if congress > 93, robust cluster (congress)fitstat
logit failed divided if congress > 93, robust cluster (congress)fitstat
logit failed committeezone if congress >93, robust cluster (congress)
fitstat
logit failed commfilizone if  congress >93, robust cluster (congress)
fitstat
logit failed commpartyzone if congress >93, robust cluster (congress)
fitstat
logit failed commpartyfilizone if congress >93, robust cluster (congress)
fitstat


*FOR FOOTNOTE NOTING LACK OF CHANGE WHEN CONTROLLING FOR NOMINEE QUALITY 
logit failed mediangridlock wellqual if stateid ~=55 & congress > 93, robust cluster (congress)fitstat
logit failed weakmajgridlock wellqual if stateid ~=55 & congress > 93, robust cluster (congress)
fitstat
logit failed purebluezone  wellqual if stateid ~=55 & congress > 93, robust cluster (congress)
fitstat
logit failed purefilibustzone wellqual if stateid ~=55 & congress > 93, robust cluster (congress)fitstat
logit failed bsfilibustzone  wellqual if stateid ~=55 & congress > 93, robust cluster (congress)fitstatlogit failed majbluezone wellqual if stateid ~=55 & congress > 93, robust cluster (congress)fitstat
logit failed majfilizone wellqual if stateid ~=55 & congress > 93, robust cluster (congress)fitstat
logit failed majbluefili wellqual if stateid ~=55 & congress > 93, robust cluster (congress)fitstat
logit failed divided wellqual if stateid ~=55 & congress > 93, robust cluster (congress)fitstat
logit failed committeezone wellqual if stateid ~=55 & congress >93, robust cluster (congress)
fitstat
logit failed commfilizone wellqual if stateid ~=55 & congress >93, robust cluster (congress)
fitstat
logit failed commbluezone wellqual if stateid ~=55 & congress >93, robust cluster (congress)
fitstat
logit failed commpartyzone wellqual if stateid ~=55 & congress >93, robust cluster (congress)
fitstat
logit failed commpartyfilizone wellqual if stateid ~=55 & congress >93, robust cluster (congress)
fitstat
logit failed commpartybluezone wellqual if stateid ~=55 & congress >93, robust cluster (congress)
fitstat
logit failed commfilibluezone wellqual if stateid ~=55 & congress >93, robust cluster (congress)
fitstat
logit failed newfullmodelzone wellqual if stateid ~=55 & congress >93, robust cluster (congress)
fitstat
