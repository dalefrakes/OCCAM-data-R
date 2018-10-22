C
C
C *****************************************************************************
C
C
C                   CONFIRMATORY STRUCTURE MODELLING PROGRAM 3.0
C
C                BY KLAUS KRIPPENDORFF (UNIVERSITY OF PENNSYLVANIA)
C
C                                  JULY 21, 1979
C                        3rd MAJOR REVISION DECEMBER 4, 1988
cc                     Minor revisions 2-10-89 - DA [PSU-SYSC]
C
c References to this program should also include a reference to 
c
c Krippendorff, Klaus. Information Theory: Structural Models for Qualitative 
c  Data. Series: Quantitative Applications in the Social c Sciences, 
c  Paper # 62, Sage Publications, Beverly Hills, California, 1986.
c  (ISBN 0-8039-2132-2, paperback)
c
C *****************************************************************************
C
C                                USERS INSTRUCTIONS
C
C ONE PARAMETER LINE:
C                   COL 1     =*
C                   COL 2-3   =M; NUMBER OF VARIABLES IN DATA: 1 ... 10
C                   COL 4     DATA INPUT TYPE
C                             =A M-TUPLES AFTER FREQUENCIED OR PROBABILITIES
C                             =B M-TUPLES BEFORE FREQUENCIES OR PROBABILITIES
C                             =W M-TUPLES WITHOUT FREQUENCIES OR PROBABILITIES
C                   COL 5-8   =N; SAMPLE SIZE (OPTIONAL).
C                             OVERRIDES FREQUENCIES IN DATA UNLESS OMITTED.
C                             AFFECTS L**2 AND DATA PRINTOUT ONLY.
C                   COL 9-80  NAME OF THE DATA
C
C ONE FORMAT LINE FOR LOCATING M-TUPLES AND FREQUENCIES OR PROBABILITIES.
C                   THE FIRST FIELD MUST BE AN "A1"
C                             FOR READING SOMETHING OTHER THAN DATA.
C                   M-TUPLES MUST BE SPECIFIED AS CHARACTERS, IN A-TERMS,
C                   FREQUENCIES OR PROBABILITIES AS REAL NUMBERS, IN F-TERMS.
C
C ANY NUMBER OF LINES FOR DATA ACCORDING TO PRECEEDING FORMAT STATEMENT.
C                             THE FIRST COLUMN MAY NOT BE USED FOR DATA
C                             AND CONTAIN NEITHER "*" NOR "@".
C
C ONE SWITCH        COL 1     =* IF MODEL SPECIFICATIONS FOLLOW IN THIS FILE.
C                             =@ IF MODEL SPEC'S ARE ENTERED INTERACTIVELY.
C                   COL 2-80  =  BLANK
C
C ANY NUMBER OF LINES, ONE FOR EACH MODEL:
C                   COL 1     =@ SWITCHES TO INTERACTIVE INPUT MODE.
C                             =  BLANK CONTINUES FILE INPUT MODE.
C                   COL 2     =1 ENTERS MODEL-GENERATED DATA INTO A FILE.
C                             =  BLANK DOES NOT.
C                   COL 3     =1 SHOWS DATA AND MODEL-GENERATED DATA ON SCREEN.
C                          =  BLANK DOES NOT.
C                   COL 4     =N SETS THE NUMBER OF ITERATIONS TO 2**N.
C                             ITERATIONS ARE REQUIRED WHEN MODELS CONTAIN
C                             LOOPS, E.G. AB:AC:BC DOES, AB:AC:AD DOES NOT.
C                             =  BLANK SETS THIS NUMBER TO 2**4=16 ITERATIONS.
C                             >4 MAY BE NEEDED ONLY IF DATA CONTAIN AN UNUSUAL
C                             CONFIGURATION AND NUMBER OF ZERO FREQUENCIES
C                             OR HIGHER PPROXIMATIONS ARE DESIRED.
C                             HIGHER VALUES INCREASE COMPUTATION.
C                   COL 5-52  AN EXPRESSION OF THE FORM  (K1:K2:...) .
C                             EACH Ki CONSISTS OF A STRING OF LETTERS,
C                             DEFINING A COMPONENT BY ITS VARIABLES.
C                             EG. (ABC:BDE:AE) = 3 COMPONENTS IN 5 VARIABLES.
C                             STRINGS MUST NOT EQUAL OR BE CONTAINED IN OTHER
C                             STRINGS.  LETTERS MUST BE FROM THE FIRST
C                             10 LETTERS OF THE ENGLISH ALPHABET: A THROUGH J.
C                   COL 53-80 ANY NAME ASSOCIATED WITH THE MODEL
C
C ONE SWITCH        COL 1     =* IF NEXT LINE CONTAINS PARAMETERS OF OTHER DATA
C                             =@ IF DATA ARE TO BE EXPLORED INTERACTIVELY.
C                             AFTER WHICH MODELLING OF SUBSEQUENT DATA MAY
C                             RESUME.
C                             =Q QUIT, TERMINATE.
C                   COL 2-80  =  BLANK
C
C
C LIMITATIONS:      NO MORE THAN  10 VARIABLES
C                                 10 VALUES IN EACH VARIABLE
C                                 10 COMPONENTS IN STRUCTURE SPECIFICATIONS
C                               5000 TOTAL NUMBER OF CELLS IN DATA SPACE
C                                    NOTE: THIS NUMBER CAN EASILY BE EXPANDED
C                                    BY CHANGING THE DIMENSION FOR P(XXXX,2)
C                                    AND LIMIT = XXXX
C
C
C     *************************************************************************
C
C
C                              SOURCE PROGRAM
C
C
      INTEGER*2 NC(10), LIST(11,11), NVARC(11), ID(10), NW(50), NL(11)
      REAL P(5000,2), SUM
      CHARACTER*2 AST(48), AT(10), STAR, A, VLIST(10), SPACE(10,9),
     1 CODE(10,10), FMT(80), INTYPE, NAME(72)
      character filename*20,punchname*24,dummy*24
cc      CHARACTER*20 FILENAME, PUNCHNAME, DUMMY
      LOGICAL L(50,10), LS, LLLL, INTER, ILLEGT
      DATA AT/'A','B','C','D','E','F','G','H','I','J'/,
     1DUMMY/'                        '/
      LIMIT = 5000
C
C READ NAME OF FILES
C
      print '(/a/)','    CONFIRMATORY STUCTURAL MODELLING PROGRAM 3.0'
      print *,' KLAUS KRIPPENDORFF  (UNIVERSITY OF PENNSYLVANIA)'
      print '(//a)','    ENTER THE NAME OF THE DATA FILE: '
      read '(a20)',filename
      call outfile(filename,punchname)
      open (1,file=PUNCHNAME)
      rewind (1)
      WRITE(1,2)
    2 FORMAT(////80('*')/80('*')//17X,'CONFIRMATORY STUCTURAL MODELLIN'
     1'G PROGRAM 3.0'//13X,'BY  KLAUS KRIPPENDORFF  (UNIVERSITY OF '
     2'PENNSYLVANIA)'//80('*')//)
cc     2'PENNSYLVANIA)'//80('*')//' ENTER THE '
cc     3'NAME OF THE DATA FILE:',/)
cc      READ(1,3) FILENAME
cc    3 FORMAT(A20)
      OPEN(5,FILE=FILENAME,STATUS='OLD')
      rewind (5)
cc      WRITE(1,4)
cc    4 FORMAT(/' ENTER A FILENAME TO KEEP MODEL-GENERATED DATA'
cc     1' OR PRESS RETURN.'/)
cc      READ(1,3) PUNCHNAME
cc      IF(PUNCHNAME.NE.DUMMY) OPEN(6,FILE=PUNCHNAME,STATUS='NEW')
C
C READ PARAMETERS
C
    1 READ(5,10,END=3000) STAR, NVAR, INTYPE, NSAMPL, NAME
   10 FORMAT(A1,I2,A1,I4,72A1)
      IF(STAR.EQ.'Q'.OR.STAR.EQ.'q') GO TO 3000
      IF(STAR.NE.'*') GO TO 1
      READ(5,20) FMT
   20 FORMAT(80A1)
      IF(FMT(1).EQ.'*'.OR.FMT(1).EQ.'@') GO TO 1
      IF(FMT(1).EQ.'Q'.OR.FMT(1).EQ.'q') GO TO 3000
      DO 25 IV = 1,10
   25 NC(IV) = 0
      LLLL = .FALSE.
      NVK = 0
      SUM = 0.
      IF(NVAR.GT.10) then
         WRITE(1,1060) NVAR
         write(*,1060) nvar
         nvar=10
      endif
cc      IF(NVAR.GT.10) NVAR = 10
C
C READ DATA
C
   30 IF(INTYPE.EQ.'A') READ(5,FMT) STAR, PX, (VLIST(IV),IV=1,NVAR)
      IF(INTYPE.EQ.'B') READ(5,FMT) STAR, (VLIST(IV),IV=1,NVAR), PX
      IF(INTYPE.EQ.'W') READ(5,FMT) STAR, (VLIST(IV),IV=1,NVAR)
      IF(LLLL) GO TO 110
C     the following is a preliminary reading to establish the space needed
      IF(STAR.EQ.'*'.OR.STAR.EQ.'@') GO TO 60
      IF(STAR.EQ.'Q'.OR.STAR.EQ.'q') GO TO 60
      NVK = NVK + 1
      DO 50 IV = 1,NVAR
      NV = NC(IV)
      DO 40 J = 1,NV
      IF(CODE(IV,J).EQ.VLIST(IV)) GO TO 50
   40 CONTINUE
      NC(IV) = NC(IV) + 1
      CODE(IV,NC(IV)) = VLIST(IV)
   50 CONTINUE
      IF(INTYPE.EQ.'A'.OR.INTYPE.EQ.'B') SUM = SUM + PX
      IF(INTYPE.EQ.'W') SUM = SUM + 1.
      GO TO 30
   60 LLLL = .TRUE.
      IC = -1
      NL(1) = 1
      DO 70 IV = 1,10
      IF(NC(IV).LE.0) NC(IV) = 1
      NL(IV+1) = NL(IV)*NC(IV)
   70 IC = IC + NL(IV)
      NN = NL(11)
      DO 80 I = 1,NN
   80 P(I,1) = 0.
      WRITE(1,90) NAME, FILENAME, PUNCHNAME, NVAR, NN,
     1(NC(I),I=1,NVAR)
      write(*,90) name, filename, punchname, nvar, nn,
     >(nc(i),i=1,nvar)
   90 FORMAT(//80('*')//1X,72A1,//
     1' FILENAMES: INPUT= ',A20,' MODEL-GENERATED= ',A20,//
     2I3,' VARIABLES, USING',I8,'CELLS AS   ',I2,9(' X',I2))
cc The original program wrote SUM as a real in format F8.4 which 
cc usually overflowed.  SUM is now written as an integer, following line. 
      IF((INTYPE.EQ.'A'.OR.INTYPE.EQ.'B').AND.
     1(SUM.LT.0.9999.OR.SUM.GT.1.0001)) WRITE(1,92) nint(SUM)
   92 FORMAT(/,' DATA ENTRIES ADD TO',i6,' AND ARE ADJUSTED TO'
     1' SUM TO 1.0')
      IF(NSAMPL.LE.0) NSAMPL = SUM + .5
      IF(NN.GT.LIMIT) then
         WRITE(1,1020) NN, LIMIT
         write(*,1020) nn, limit
         goto 1
      endif
cc      IF(NN.GT.LIMIT) GO TO 1
      DO 100 I = 1,NVK + 1
  100 BACKSPACE 5
      GO TO 30
C     the following actually stores the data
  110 IF(STAR.EQ.'*'.OR.STAR.EQ.'@') GO TO 200
      IF(STAR.EQ.'Q'.OR.STAR.EQ.'q') GO TO 3000
      INDEX = 1
      DO 130 IV = 1,NVAR
      NV = NC(IV)
      DO 120 J = 1,NV
      IN = J - 1
      IF(CODE(IV,J).EQ.VLIST(IV)) GO TO 130
  120 CONTINUE
  130 INDEX = INDEX + IN*NL(IV)
      IF(INTYPE.EQ.'A'.OR.INTYPE.EQ.'B')
     1 P(INDEX,1) = P(INDEX,1) + PX/SUM
      IF(INTYPE.EQ.'W') P(INDEX,1) = P(INDEX,1) + 1./SUM
      GO TO 30
C
C READ MODEL SPECIFICATIONS
C
  200 INTER = .TRUE.
  201 IF(STAR.EQ.'*') INTER = .FALSE.
      if(inter) write(*,211)
cc      IF(INTER) WRITE(1,211)
  211 FORMAT(/80('*')//' ENTER MODEL: COL 1     =  BLANK'
     1/14X,'COL 2     =1 ENTERS MODEL-GENERATED DATA INTO FILE, BLANK'
     3' DOESN''T.'/14X,'COL 3     =1 SHOWS MODEL-GENERATED DATA, BLANK'
     4' DOESN''T.'/14X,'COL 4     =N FOR 2**N ITERATIONS, BLANK IS '
     5'DEFAULT = 1 ITERATION.'
     6/,14X,'COL 5-52  =(K1:K2:...)'//
     7' SWITCH MODE: COL 1     =* SWITCHES INPUT MODE BACK TO FILE.'
     8//' OR QUIT:     COL 1     =Q'//)
ccccc:  Rather than duplicate the next line, the
ccccc:  unit # was changed from 1 to *. [DA.1-15-89]      
      IF(INTER) READ(*,210,END=3000) STAR, IPUNCH, IPRINT,
     1NITER, AST, (NAME(J),J=1,28)
      IF(STAR.EQ.'*') INTER = .FALSE.
      IF(.NOT.INTER) READ(5,210,END=3000) STAR, IPUNCH, IPRINT,
     1NITER, AST, (NAME(J),J=1,28)
  210 FORMAT(A1,3I1,76A1)
      IF(STAR.EQ.'*') GO TO 1
      IF(STAR.EQ.'@') INTER = .TRUE.
      IF(STAR.EQ.'Q'.OR.STAR.EQ.'q') GO TO 3000
      IF(PUNCHNAME.EQ.DUMMY.OR.IPUNCH.LE.0) GO TO 214
CCCCC      write(1,212) nvar, nsampl, ast, (name(j),j=1,24)
cc      WRITE(6,212) NVAR, NSAMPL, AST, (NAME(J),J=1,24)
  212 FORMAT('*',I2,'B',I4,72A1)
cc      WRITE(6,213) NVAR
CCCCC      write(1,213) nvar
  213 FORMAT('(A1,',I2,'A1,2X,F12.10)')
  214 DO 220 K=1,50
      DO 220 I=1,10
  220 L(K,I) = .FALSE.
      ILLEGT = .FALSE.
C
C COMPUTE NUMBER OF COMPONENTS  NCOMP, AND REPRESENT THEM BY  L(NCOMP*NVAR)
C
      NCOMP = 1
      LLLL = .FALSE.
      LS = .FALSE.
      M1 = 1
      DO 250 M=1,48
      A = AST(M)
      AST(M1) = A
      IF(A.EQ.'('.OR.A.EQ.' '.OR.A.EQ.')') GO TO 250
      IF(.NOT.LS.AND.A.EQ.':') GO TO 250
      IF(A.NE.':') GO TO 230
      M1 = M1 + 1
      LS = .FALSE.
      NCOMP = NCOMP + 1
  230 DO 240 IV=1,NVAR
      IF(A.NE.AT(IV)) GO TO 240
      IF(L(NCOMP,IV)) ILLEGT = .TRUE.
      L(NCOMP,IV) = .TRUE.
      LLLL = .TRUE.
      LS = .TRUE.
      M1 = M1 + 1
      GO TO 250
  240 CONTINUE
      IF(A.NE.':') ILLEGT = .TRUE.
  250 CONTINUE
  253 IF(AST(M1).NE.':') GO TO 254
      M1 = M1 + 1
      GO TO 253
  254 DO 255 IV = M1,48
  255 AST(IV) = ' '
      IF(.NOT.LLLL) NCOMP = 0
C
C ELIMINATE REDUNDANT COMPONENTS
C
      DO 500 K = 1,NCOMP
      NW(K) = 0
      DO 500 IV = 1,NVAR
  500 IF(L(K,IV)) NW(K) = NW(K) + 1
      K = 0
  510 K = K + 1
  515 IF(K.GE.NCOMP) GO TO 550
      K1 = K
  520 K1 = K1 + 1
  525 IF(K1.GT.NCOMP) GO TO 510
      NVK = 0
      DO 530 IV = 1,NVAR
  530 IF(L(K,IV).AND.L(K1,IV)) NVK = NVK + 1
      K2 = 0
      IF(NVK.EQ.NW(K)) K2 = K
      IF(NVK.EQ.NW(K1)) K2 = K1
      IF(K2.LE.0) GO TO 520
      ILLEGT = .TRUE.
      NCOMP = NCOMP - 1
      DO 540 I = K2, NCOMP
      DO 540 IV = 1,NVAR
      L(I,IV) = L(I+1,IV)
      IF(I.EQ.NCOMP) L(I+1,IV) = .FALSE.
  540 NW(I) = NW(I+1)
      IF(K1.EQ.K2) GO TO 525
      IF(K.EQ.K2) GO TO 515
  550 IF(NCOMP.LE.10) GO TO 900
      WRITE(1,1050) AST
      write(*,1050) ast
      GO TO 201
C
C TEST FOR LOOPS IN MODEL
C
  900 DO 901 K = 1,11
      DO 901 IV = 1,11
  901 LIST(K,IV) = 0
      DO 905 K = 1,NCOMP
      DO 905 IV = 1,NVAR
      IF(.NOT.L(K,IV)) GO TO 905
      LIST(K,IV) = LIST(K,IV) + 1
      LIST(K,11) = LIST(K,11) + 1
      LIST(11,IV)=LIST(11,IV) + 1
      LIST(11,11)=LIST(11,11) + 1
  905 CONTINUE
  906 ICHANGE = 0
C     eliminate unique variables from components
      DO 920 IV = 1,NVAR
      IF(LIST(11,IV).NE.1) GO TO 920
      ICHANGE = 1
      DO 910 K =1,11
      LIST(K,11) = LIST(K,11) - LIST(K,IV)
  910 LIST(K,IV) = 0
  920 CONTINUE
C     eliminate redundant (contained) components from LIST
      DO 960 K = 1,NCOMP-1
      IF(LIST(K,11).LE.0) GO TO 960
      DO 950 K1 = K+1,NCOMP
      IF(LIST(K1,11).LE.0) GO TO 950
      NVK = 0
      DO 930 IV = 1,NVAR
      IF(LIST(K,IV).EQ.0.OR.LIST(K1,IV).EQ.0) GO TO 930
      NVK = NVK + 1
  930 CONTINUE
      K2 = 0
      IF(NVK.EQ.LIST(K,11)) K2 = K
      IF(NVK.EQ.LIST(K1,11)) K2 = K1
      IF(K2.LE.0) GO TO 950
      ICHANGE = 1
      DO 940 IV = 1,11
      LIST(11,IV) = LIST(11,IV) - LIST(K2,IV)
  940 LIST(K2,IV) = 0
  950 CONTINUE
  960 CONTINUE
      IF(ICHANGE.GE.1) GO TO 906
      IF(NITER.LT.4.AND.LIST(11,11).GT.0) NITER = 4
      IF(LIST(11,11).LE.0) NITER = 0
      DO 970 K = 1,11
      DO 970 IV = 1,11
  970 LIST(K,IV) = 0
C
C CREATE LIST OF VARIABLES IN EACH COMPONENT
  260 DO 290 K=1,NCOMP
      N1 = 0
      N2 = NVAR + 1
      N3 = 11
      DO 285 IV=1,10
      IF(IV.GT.NVAR) GO TO 280
      IF(.NOT.L(K,IV)) GO TO 275
      N1 = N1 + 1
      IX = N1
      GO TO 285
  275 N2 = N2 - 1
      IX = N2
      GO TO 285
  280 N3 = N3 - 1
      IX = N3
  285 LIST(K,IX) = IV
  290 NVARC(K) = N1
      NITER = 2**NITER
      IF(ILLEGT) then
         WRITE(1,1040) AST
         write(*,1040) ast
      endif
      IF(.NOT.ILLEGT) GO TO 298
      IX = 0
      DO 292 K = 1,NCOMP
      DO 291 IV = 1,NVAR
      IF(.NOT.L(K,IV)) GO TO 291
      IX = IX + 1
      AST(IX) = AT(IV)
  291 CONTINUE
      IX = IX + 1
      AST(IX) = ':'
  292 CONTINUE
      DO 293 IV = IX,48
  293 AST(IV) = ' '
  298 WRITE(1,299) (NAME(I),I=1,28), AST, NITER
      write(*,299) (name(i),i=1,28), ast, niter
  299 FORMAT(/,80('*'),//,1X,28A1,//,' MODEL:',7X,48A1
     1,I6,' ITERATIONS')
C
C ITERATIVELY COMPUTE EXPECTED PROBABILITIES IN MODEL
C
  300 LS = .TRUE.
      FO = 1./FLOAT(NN)
      DO  301 I=1,NN
  301 P(I,2) = FO
      DO 410 IT=1,NITER
      DO 410 K=1,NCOMP
      NV = NVARC(K)
      IN = LIST(K,1)
      N1 = NL(IN)
      M1 = NL(IN+1)
      IN = LIST(K,2)
      N2 = NL(IN)
      M2 = NL(IN+1)
      IN = LIST(K,3)
      N3 = NL(IN)
      M3 = NL(IN+1)
      IN = LIST(K,4)
      N4 = NL(IN)
      M4 = NL(IN+1)
      IN = LIST(K,5)
      N5 = NL(IN)
      M5 = NL(IN+1)
      IN = LIST(K,6)
      N6 = NL(IN)
      M6 = NL(IN+1)
      IN = LIST(K,7)
      N7 = NL(IN)
      M7 = NL(IN+1)
      IN = LIST(K,8)
      N8 = NL(IN)
      M8 = NL(IN+1)
      IN = LIST(K,9)
      N9 = NL(IN)
      M9 = NL(IN+1)
      IN = LIST(K,10)
      N10 = NL(IN)
      M10 = NL(IN+1)
      DO 410 I1=N1,M1,N1
      IF(NV.NE.1) GO TO 302
      PSUM = 0.
      WSUM = 0.
  302 DO 400 I2=N2,M2,N2
      IF(NV.NE.2) GO TO 303
      PSUM = 0.
      WSUM = 0.
  303 DO 390 I3=N3,M3,N3
      IF(NV.NE.3) GO TO 304
      PSUM = 0.
      WSUM = 0.
  304 DO 380 I4=N4,M4,N4
      IF(NV.NE.4) GO TO 305
      PSUM = 0.
      WSUM = 0.
  305 DO 370 I5=N5,M5,N5
      IF(NV.NE.5) GO TO 306
      PSUM = 0.
      WSUM = 0.
  306 DO 360 I6=N6,M6,N6
      IF(NV.NE.6) GO TO 307
      PSUM = 0.
      WSUM = 0.
  307 DO 350 I7=N7,M7,N7
      IF(NV.NE.7) GO TO 308
      PSUM = 0.
      WSUM = 0.
  308 DO 340 I8=N8,M8,N8
      IF(NV.NE.8) GO TO 309
      PSUM = 0.
      WSUM = 0.
  309 DO 330 I9=N9,M9,N9
      IF(NV.NE.9) GO TO 310
      PSUM = 0.
      WSUM = 0.
  310 DO 320 I10=N10,M10,N10
      INDEX = I1 + I2 + I3 + I4 + I5 + I6 + I7 + I8 + I9 + I10 - IC
      IF(LS) GO TO 315
      P(INDEX,2) = P(INDEX,2)*PSUM/WSUM
      GO TO 320
  315 PSUM = PSUM + P(INDEX,1)
      WSUM = WSUM + P(INDEX,2)
      IF(NV.EQ.10) P(INDEX,2) = P(INDEX,1)
  320 CONTINUE
      IF(NV.NE.9.OR.WSUM.LE.0.) GO TO 330
      LS = .NOT.LS
      IF(.NOT.LS) GO TO 310
  330 CONTINUE
      IF(NV.NE.8.OR.WSUM.LE.0.) GO TO 340
      LS = .NOT.LS
      IF(.NOT.LS) GO TO 309
  340 CONTINUE
      IF(NV.NE.7.OR.WSUM.LE.0.) GO TO 350
      LS = .NOT.LS
      IF(.NOT.LS) GO TO 308
  350 CONTINUE
      IF(NV.NE.6.OR.WSUM.LE.0.) GO TO 360
      LS = .NOT.LS
      IF(.NOT.LS) GO TO 307
  360 CONTINUE
      IF(NV.NE.5.OR.WSUM.LE.0.) GO TO 370
      LS = .NOT.LS
      IF(.NOT.LS) GO TO 306
  370 CONTINUE
      IF(NV.NE.4.OR.WSUM.LE.0.) GO TO 380
      LS = .NOT.LS
      IF(.NOT.LS) GO TO 305
  380 CONTINUE
      IF(NV.NE.3.OR.WSUM.LE.0.) GO TO 390
      LS = .NOT.LS
      IF(.NOT.LS) GO TO 304
  390 CONTINUE
      IF(NV.NE.2.OR.WSUM.LE.0.) GO TO 400
      LS = .NOT.LS
      IF(.NOT.LS) GO TO 303
  400 CONTINUE
      IF(NV.NE.1.OR.WSUM.LE.0.) GO TO 410
      LS = .NOT.LS
      IF(.NOT.LS) GO TO 302
  410 CONTINUE
C
C PRINT INPUT AND MODEL-GENERATED DATA
C
  600 if(iprint+ipunch.le.0) go to 620
cc  The GOTO in the following line
cc  caused a jump over essential code.
cc  600 IF(IPRINT+IPUNCH.LE.0) GO TO 700
      if(iprint.gt.0) write(*,630)
      if(ipunch.gt.0) write(1,630)
cc      IF(IPRINT.LE.0) GO TO 620
cc      WRITE(1,630)
  630 FORMAT(/' DATA: INPUT AND GENERATED',3X,
     1'N OBSERVED',3X,'P OBSERVED  P GENERATED',4X,'M-TUPLES',/)
  620 FN = NSAMPL
      N1 = NC(1)
      DO 660 I1=1,N1
      ID(1) = I1 - 1
      N2 = NC(2)
      DO 660 M2=1,N2
      ID(2) = M2 - 1
      I2 = M2*NL(2)
      N3 = NC(3)
      DO 660 M3=1,N3
      ID(3) = M3 - 1
      I3 = M3*NL(3)
      N4 = NC(4)
      DO 660 M4=1,N4
      ID(4) = M4 - 1
      I4 = M4*NL(4)
      N5 = NC(5)
      DO 660 M5=1,N5
      ID(5) = M5 - 1
      I5 = M5*NL(5)
      N6 = NC(6)
      DO 660 M6=1,N6
      ID(6) = M6 - 1
      I6 = M6*NL(6)
      N7 = NC(7)
      DO 660 M7=1,N7
      ID(7) = M7 - 1
      I7 = M7*NL(7)
      N8 = NC(8)
      DO 660 M8=1,N8
      ID(8) = M8 - 1
      I8 = M8*NL(8)
      N9 = NC(9)
      DO 660 M9=1,N9
      ID(9) = M9 - 1
      I9 = M9*NL(9)
      N10 = NC(10)
      DO 660 M10=1,N10
      ID(10) = M10 - 1
      I10 = M10*NL(10)
      INDEX = I1 + I2 + I3 + I4 + I5 + I6 + I7 + I8 + I9 + I10 - IC
      IF(P(INDEX,2).LT.0.0000001) GO TO 660
      NF = FN*P(INDEX,1) + 0.5
      if(iprint.gt.0) write(*,650) nf, p(index,1),
     > p(index,2), (id(i),i=1,nvar)
c      IF(IPRINT.GT.0) WRITE(1,650) NF, P(INDEX,1),
cc     1 P(INDEX,2), (ID(I),I=1,NVAR)
  650 FORMAT(21X,I16,2F13.4,6X,10I1)
cc      IF(PUNCHNAME.EQ.DUMMY) GO TO 660
      if(ipunch.gt.0) write(1,650) nf, p(index,1),
     > p(index,2), (id(i),i=1,nvar)
cc  ??? The next line writes unlabelled data
cc  to the results file.  Delete for now.
cc  IF(IPUNCH.GT.0) WRITE(6,655) ID, P(INDEX,2)
  655 FORMAT(1X,10I1,2X,F12.10)
  660 CONTINUE
      write(*,670) nsampl
      WRITE(1,670) NSAMPL
  670 FORMAT(14X,'SAMPLE SIZE=',I11)
cc      IF(PUNCHNAME.NE.DUMMY.AND.IPUNCH.GT.0) WRITE(6,11)
   11 FORMAT('@')
C
C COMPUTE ENTROPIES, TRANSMISSIONS, ETC.
C
  700 HMAX = -ALOG(FO)/ALOG(2.)
      HDATA = 0.
      HSTRC = 0.
      DO 720 I=1,NN
      PX = P(I,1)
      IF(PX.LE.0.) GO TO 720
      HDATA = HDATA - PX*ALOG(PX)
      HSTRC = HSTRC - PX*ALOG(P(I,2))
  720 CONTINUE
      HDATA = HDATA/ALOG(2.)
      TOTAL = HMAX - HDATA
      HSTRC = HSTRC/ALOG(2.)
      TSTRC = HMAX - HSTRC
      TLOSS = HSTRC - HDATA
      PERCT = 100.*TSTRC/TOTAL
      PERLS = 100. - PERCT
      KKK = 100000.*TOTAL
      TOTAL = FLOAT(KKK)/100000.
      KKK = 100000.*TSTRC
      TSTRC = FLOAT(KKK)/100000.
      KKK = 100000.*TLOSS
      TLOSS = FLOAT(KKK)/100000.
      CHITL = FN*1.3863*TOTAL
      CHIST = FN*1.3863*TSTRC
      CHILS = FN*1.3863*TLOSS
C
C COMPUTE DEGREES OF FREEDOM
C
      IDFTL = NN - 1
      IDFST = 0
      IF(NITER.EQ.1) GO TO 2000
C     compute DF of models with loops
      MCOMP = NCOMP
      DO 790 K = 1,MCOMP
      NW(K) = 0
      DO 790 IV = 1,NVAR
  790 IF(L(K,IV)) NW(K) = NW(K) + 1
C     compute DF for the component K=1 on top of the list L
  800 IF(MCOMP.LE.0) GO TO 1000
      IDF = 1
      NVK = 0
      DO 810 IV = 1,NVAR
      IF(.NOT.L(1,IV)) GO TO 810
      IDF = IDF*(NC(IV) - 1)
      NVK = NVK + 1
      VLIST(NVK) = AT(IV)
  810 CONTINUE
      IDFST = IDFST + IDF
C     eliminate top component by moving all others one up
      MCOMP = MCOMP-1
      IF(MCOMP.LE.0) GO TO 830
      DO 820 K = 1,MCOMP
      DO 820 IV = 1,NVAR
      L(K,IV) = L(K+1,IV)
  820 NW(K) = NW(K+1)
  830 IF(NVK.LE.1) GO TO 800
C     remove one interaction by decomposing the top component
C     and enter the resulting components in a temporary workSPACE
      DO 840 I1 = 1,NVK-1
      DO 840 I2 = 1,NVK
      I = (I1*NVK + I2 - 2)/(NVK - 1)
      J = (I1-1)*NVK + I2 - (NVK-1)*(I-1)
  840 SPACE(I,J) = VLIST(I2)
C     add newly created components to LIST
      KMIN = MCOMP + 1
      MCOMP = MCOMP + NVK
      DO 855 K = KMIN,MCOMP
      DO 845 IV = 1,NVAR
  845 L(K,IV) = .FALSE.
      NW(K) = NVK - 1
      I = K - KMIN + 1
      DO 850 J = 1,NVK-1
      DO 850 IV = 1,NVAR
  850 IF(SPACE(I,J).EQ.AT(IV)) L(K,IV) = .TRUE.
  855 CONTINUE
C     identify redundant component K2 (either = to K or K1)
      K = 0
  860 K = K + 1
  865 IF(K.GE.MCOMP) GO TO 800
      K1 = K
  870 K1 = K1 + 1
  875 IF(K1.GT.MCOMP) GO TO 860
      NVK = 0
      DO 880 IV = 1,NVAR
  880 IF(L(K,IV).AND.L(K1,IV)) NVK = NVK + 1
      K2 = 0
      IF(NVK.EQ.NW(K)) K2 = K
      IF(NVK.EQ.NW(K1)) K2 = K1
      IF(K2.LE.0) GO TO 870
C     remove redundant component K2 from LIST and moving all others up
      MCOMP = MCOMP - 1
      IF(K2.GT.MCOMP) GO TO 860
C     REMOVE REDUNDANT COMPONENT FROM LIST
      DO 890 I = K2,MCOMP
      DO 890 IV = 1,NVAR
      L(I,IV) = L(I+1,IV)
  890 NW(I) = NW(I+1)
C     this goes back till all components are exhausted and no variables
C     are left at which point the DF should be in IDFST
      IF(K2.EQ.K1) GO TO 875
      GO TO 865
C     compute DF for models without loops
 2000 DO 2200 IS = 1,NCOMP
      IN = 0
      ID(1) = 0
 2010 ID(1) = ID(1) + 1
      IF(ID(1).GT.NCOMP+1-IS) GO TO 2130
      IF(IS.EQ.1) GO TO 2100
      ID(2) = ID(1)
 2020 ID(2) = ID(2) + 1
      IF(ID(2).GT.NCOMP+2-IS) GO TO 2010
      IF(IS.EQ.2) GO TO 2100
      ID(3) = ID(2)
 2030 ID(3) = ID(3) + 1
      IF(ID(3).GT.NCOMP+3-IS) GO TO 2020
      IF(IS.EQ.3) GO TO 2100
      ID(4) = ID(3)
 2040 ID(4) = ID(4) + 1
      IF(ID(4).GT.NCOMP+4-IS) GO TO 2030
      IF(IS.EQ.4) GO TO 2100
      ID(5) = ID(4)
 2050 ID(5) = ID(5) + 1
      IF(ID(5).GT.NCOMP+5-IS) GO TO 2040
      IF(IS.EQ.5) GO TO 2100
      ID(6) = ID(5)
 2060 ID(6) = ID(6) + 1
      IF(ID(6).GT.NCOMP+6-IS) GO TO 2050
      IF(IS.EQ.6) GO TO 2100
      ID(7) = ID(6)
 2070 ID(7) = ID(7) + 1
      IF(ID(7).GT.NCOMP+7-IS) GO TO 2060
      IF(IS.EQ.7) GO TO 2100
      ID(8) = ID(7)
 2080 ID(8) = ID(8) + 1
      IF(ID(8).GT.NCOMP+8-IS) GO TO 2070
      IF(IS.EQ.8) GO TO 2100
      ID(9) = ID(8)
 2090 ID(9) = ID(9) + 1
      IF(ID(9).GT.NCOMP+9-IS) GO TO 2080
      IF(IS.EQ.9) GO TO 2100
      ID(10) = 10
 2100 IX = 1
      DO 2120 IV = 1,NVAR
      DO 2110 K = 1,IS
      IF(.NOT.L(ID(K),IV)) GO TO 2120
 2110 CONTINUE
      IX = IX * NC(IV)
 2120 CONTINUE
      IF((IS/2)*2.LT.IS) IDFST = IDFST + IX - 1
      IF((IS/2)*2.EQ.IS) IDFST = IDFST - IX + 1
      IN = IN + IX
      GO TO (2010,2020,2030,2040,2050,2060,2070,2080,2090,2130) IS
 2130 IF(IN.EQ.0) GO TO 1000
 2200 CONTINUE
 1000 IDFLS = IDFTL - IDFST
C
C PRINT FINAL ACCOUNTS    (ERROR MESSAGES)
C
      WRITE(1,1010) TLOSS, PERLS, CHILS, IDFLS, TSTRC, PERCT,
     1 CHIST, IDFST
      write(*,1010) tloss, perls, chils, idfls, tstrc, perct,
     > chist, idfst
 1010 FORMAT(/' ACCOUNTS:',18X,'INFORMATION',7X,'o/o',9X,'L**2',
     17X,'DF'/14X,'ERROR:',F17.4,F13.2,F13.4,I8,/14X,'IN MODEL:'
     2F14.4,F13.2,F13.4,I8)
 1020 FORMAT(/,'*******ABORT*******',I4,' = NUMBER OF CELLS'
     1' REQUESTED EXCEEDS LIMIT =',I6//)
 1040 FORMAT(/80('*')/
     1/'******WARNING****** ILLEGITIMATE CHARACTERS OR'
     2' REDUNDANCIES IN MODEL SPECIFICATIONS:',/20X,48A1)
 1050 FORMAT(/'*******ABORT******* NUMBER OF COMPONENTS IN MODEL'
     1' SPECIFICATIONS EXCEED 10',/20X,48A1,//)
 1060 FORMAT(/'******WARNING******',I4,' = SPECIFIED NUMBER OF'
     1' VARIABLES EXCEEDS 10',/,20X,'ONLY THE FIRST 10 ARE ENTERED'//)
      GO TO 201
 3000 CLOSE(5,STATUS='KEEP')
      close (1)
cc      IF(PUNCHNAME.NE.DUMMY) CLOSE(6,STATUS='KEEP')
      STOP
      END
C
C     ******************************************************************
c
      subroutine outfile(filename,punchfile)
c
c        Creates the filename for output by replacing the 
c        extension on the data/commands filename with .CST
c
      character filename*(*),punchfile*(*),ext*4,bl,dot
      data EXT,BL,DOT/'.CST',' ','.'/
c
c     Parse FILENAME into the filespec ([drive:] [path] filename)
c     and its extension, if any.  Ignore the old extension, append
c     the new EXTension and copy to PUNCHFILE.
c
      loc=index(filename,DOT)-1
      if (loc.eq.-1) then
c        No extension found; first find
c        the length of the input filespec. . .
         lng=len(filename)+1
    1    lng=lng-1
         if (filename(lng:lng).eq.BL) goto 1
c        . . .then create the output filename:
         punchfile=filename(:lng)//EXT
      else
c        Extension found; create the output filename:
         punchfile=filename(:loc)//EXT
      endif
c
      return
      end
