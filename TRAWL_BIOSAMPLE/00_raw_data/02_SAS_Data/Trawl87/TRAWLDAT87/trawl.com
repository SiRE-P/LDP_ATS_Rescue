$set def [lep.juvenile.TRAWL87]
$!sas trawlchk.sas
$sas trawltab.sas
$pur trawl*.*
