%LET            yy = 87 ;      	* Only place to change year ;

LIBNAME         io       "[LEP.JUVENILE.TRAWL87]" ;
LIBNAME         library  "[LEP.FMT]" ;

OPTIONS         OBS=MAX  NODATE  ERRORABEND  LS=132  PS=130 ;  TITLE ;
TITLE3         "EAU 19&yy Trawl Database" ;
FOOTNOTE       "Dataset : TRAWL&yy..SSD &SYSDATE" ;

DATA name ;
     FILE      "trwl&yy.pc.dat" ;
     SET        io.trawl&yy ;
*    IF         system = 35 | system = 38 | system = 43 ;  * Get only NASS 93 stuff ;
     IF         system  THEN  sysname = PUT ( system, sysfmt. ) ;
     IF         fspecies  THEN  fspname = PUT ( fspecies, fspfmt. ) ;
*    IF         _N_=1  THEN PUT 'System Species   Date Trawl Depth  Fish Length '
                'StdWt  Scale' ;
*    PUT        system 6. fspecies 6. ' '  date DATE7. ' ' trwlnmbr 9. depth 6. 
		fishnmbr 6. length 6.  stdwght 6.2  scale 6. ;
     RUN ;

PROC TABULATE   DATA = name ;
     CLASS      sysname date fspname trwlnmbr fishnmbr ;
     VAR        length stdwght ;
     TABLES     sysname='', date * fspname='Species',
                trwlnmbr='Trawl' * N='' * F=COMMA6. ALL='Total'*N=''*F=COMMA6. /
                RTS = 35  MISSTEXT=' '  ROW=FLOAT  CONDENSE ;
     TABLES     sysname='', date * fspname='Species',
                ( length='Length (mm)'  stdwght='Std Weight (g)' ) *
                ( MEAN='Mean'*F=6.1  STD='Std Dev'*F=7.1 
                MIN='Min'*F=5.1  Max='Max'*F=5.1 ) /
                RTS = 35  MISSTEXT=' '  ROW=FLOAT  CONDENSE ;
     TITLE4    "19&yy Trawl Sample Data" ;
     FORMAT     date DATE7. ;
     RUN ;

PROC FREQ       DATA = io.trawl&yy ;
     BY         system date trwlnmbr fspecies ;
     TABLES     fishnmbr / NOPRINT  OUT = dupchk ;
     RUN ;

DATA duperr ;
     SET        dupchk ;
     IF         count > 1 ;
     IF         system  THEN  sysname = PUT ( system, sysfmt. ) ;
     IF         fspecies  THEN  fspname = PUT ( fspecies, fspfmt. ) ;
     RUN ;

PROC TABULATE   DATA = duperr ;
     CLASS      sysname date fspname trwlnmbr fishnmbr ;
     VAR        count ;
     TABLES     sysname='', date * fspname='Species' * fishnmbr,
                trwlnmbr='Trawl' * count=''*SUM='' * F=COMMA7. /
                RTS = 37  MISSTEXT=' '  ROW=FLOAT  CONDENSE ;
     TITLE4    "Frequency Count of Duplicate Fish Records" ;
     RUN ;

DATA dupkey ;
     KEEP       system sysname i date trwlnmbr fspecies count ;
     SET        duperr ;
     DO         i = 1 to count ;
                OUTPUT ;
                END ;
     RUN ;

PROC SORT       DATA = dupkey ;  BY  system date trwlnmbr fspecies ;  RUN ;

DATA skim ;
     MERGE      dupkey ( IN = dup )  io.trawl&yy ;
     BY         system date trwlnmbr fspecies ;
     IF         dup ;
     RUN ;

PROC SORT ;     BY  system date trwlnmbr fspecies fishnmbr ;  RUN ;

DATA drop ;
     MERGE      duperr ( IN = dup )  skim ;
     BY         system date trwlnmbr fspecies fishnmbr ;  
     IF         dup ; 
     RUN ;
                
PROC TABULATE   DATA = drop ;
     CLASS      sysname date fspname trwlnmbr fishnmbr i ;
     VAR        length stdwght ;
     TABLES     sysname='', date * fspname='Species' * fishnmbr,
                trwlnmbr='Trawl' * i='Duplicate No.' * 
                stdwght = '' * SUM='' * F=7.2 /
                RTS = 37  MISSTEXT=' '  ROW=FLOAT  CONDENSE ;
     TITLE4    "Comparative Standard Weights (g) of Duplicate Fish Records" ;
     RUN ;

