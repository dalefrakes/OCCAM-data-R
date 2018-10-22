C
C     ******************************************************************
C
C
C                   A SPECTRAL ANALYSIS OF RELATIONS  2.0
C
C                          BY KLAUS KRIPPENDORFF
C                        UNIVERSITY OF PENNSYLVANIA
C
C                               JULY 20, 1978
C                    2nd MAJOR REVISION NOVEMBER 18, 1988
cc                 Minor revisions 2-8-89 by DA [PSU-SYSC]
C
C     ******************************************************************
C
C
C                            USERS INSTRUCTIONS
C
C
C     1ST LINE     COL 1     BLANK
C                  COL 2-3   NUMBER  M  OF VARIABLES IN DATA
C                  COL 4     DATA INPUT TYPE
C                            =A M-TUPLES AFTER FREQUENCIES OR PROBABILITIES
C                            =B M-TUPLES BEFORE FREQUENCIES OR PROBABILITIES
C                            =W M-TUPLES WITHOUT FREQUENCIES OR PROBABILITIES.
C                  COL 5-8   SAMPLE SIZE (OPTIONAL), AFFECTS CHI**2 APPROX. ONLY
C                  COL 9-80  NAME OF DATA OR JOB
C
C     2ND LINE     COL 1-80  FORTRAN FORMAT, CONSISTENT WITH ABOVE INPUT TYPE.
C                            FREQUENCIES OR PROBABILITIES IN F-FORMATE CODE
C                            M-TUPLES AS M FIELDS IN A-FORMAT CODE.
C                            THE FIRST FIELD MUST BE "A1" AND IS USED FOR
C                            READING SOMETHING OTHER THAN DATA.
C
C     DATA LINES   ONE FOR EACH M-TUPLE
C                            IN FORMAT CONSISTENT WITH ABOVE LINES.
C                            THE FIRST COLUMN MUST BE BLANK.
C                            ANYTHING IN THERE TERMINATES THE READING OF DATA.
C                            (This rather unusual requirement is to make data
C                            input compatible with CONSTRUCT, the confirmatory
C                            structural modelling program, making use of this
C                            column for other purposes).
C
C
C     ******************************************************************
C
C
C                              SOURCE PROGRAM
C
C
      REAL*4 FREQ( 100), H(1023)
      LOGICAL*1 INDEX( 100)
      INTEGER DF, SET(10), LIST(1023,10), ORDINL(1023), UNIQ(1023),
     1 NNN(1023)
      CHARACTER FILENAME*40, FMT(80), DATA( 100,10), INTYPE, STAR,
     1 AT(10),punchfile*44
      DATA AT/'A','B','C','D','E','F','G','H','I','J'/
C
C     READ PARAMETERS OF THE PROGRAM
C
C     M = NUMBER OF VARIABLES                   = OR < THAN 20 BY STATEMENT 607
      MLIMIT = 10
C     N = NUMBER OF DATA POINTS (UNIQUE M-TUPLES)
      NLIMIT = 100
C
C     EXEPTING M=<20, OTHER LIMITS CAN BE EXPANDED BY DIMENSION STATEMENTS
C
C     SPACE REQUIRED FOR SET:                    M
C                    FOR FREQ, INDEX:            N
C                    FOR DATA:                  N*M
C                    FOR H, ORDINL, UNIQ, NNN: 2**M - 1
C                    FOR LIST:               (2**M-1)*M
C
      print '(8x,a/)',' A SPECTRAL ANALYSIS OF RELATIONS 2.0'
      print *, '   KLAUS KRIPPENDORFF (UNIVERSITY OF PENNSYLVANIA)'
      print '(//a)', '    ENTER THE NAME OF THE DATA FILE: '
      read '(a40)', filename
      call outfile(filename,punchfile)
      open (1,file=punchfile)
      rewind (1)
      WRITE(1,100)
  100 FORMAT(///,80('*'),/,80('*'),
     1//22X,'A SPECTRAL ANALYSIS OF RELATIONS 2.0'
     2//14X,'BY  KLAUS KRIPPENDORFF  (UNIVERSITY OF PENNSYLVANIA)'
     3//80('*'))
cc     3//80('*'),
cc     4//'ENTER THE NAME OF THE DATA FILE:'
cc     5 11X,'(FOR INSTRUCTIONS SEE SOURCE PROGRAM)',/)
cc      READ(1,1) FILENAME
cc    1 FORMAT(A40)
      OPEN(5,FILE=FILENAME,STATUS='OLD')
      rewind (5)
      READ(5,101,END=1001) M, INTYPE, NSAMPL, (FMT(J),J=1,72)
  101 FORMAT(1X,I2,A1,I4,72A1)
      WRITE(1,102) (FMT(J),J=1,72), M, NSAMPL
      write(*,102) (fmt(j),j=1,72), m, nsampl
  102 format(80('*')
cc  102 FORMAT(/,80('*')
     1             ///,18X,'TITLE:  ',72A1,
     3             ///,17X,'NUMBER OF VARIABLES =',I6,
     4              //,14X,'USER-GIVEN SAMPLE SIZE =',I6)
      IF(M.GT.MLIMIT) GO TO 1003
      CSAMPL = NSAMPL
      CSAMPL = CSAMPL*1.3863
      READ(5,103,END=1005) FMT
  103 FORMAT(80A1)
      WRITE(1,104) FMT
  104 FORMAT(/24X,'INPUT FORMAT :  ',80A1)
      IF(INTYPE.NE.'A'.AND.INTYPE.NE.'B') GO TO 150
C
C     READ M-TUPLES WITH ASSIGNED FREQUENCIES OR PROBABILITIES
C
cc The following READ block over-counts m-tuples by 1.
cc The quick fix below is to change the initial values 
cc of FN and NN to -1.0 and -1 respectively.
cc  120 FN = 0.
  120 fn=-1.
cc      NN = 0
      nn=-1  
      DO 124 K=1,NLIMIT+1
  122 IF(INTYPE.EQ.'A') READ(5,FMT,END=125) STAR, FREQ(K),
     1 (DATA(K,J),J=1,M)
      IF(INTYPE.EQ.'B') READ(5,FMT,END=125) STAR, (DATA(K,J),J=1,M),
     1 FREQ(K)
      IF(FREQ(K).LE.0.) GO TO 122
      IF(STAR.NE.' ') GO TO 125
      FN = FN + FREQ(K)
      NN = NN+1
  124 CONTINUE
      GO TO 1009
  125 WRITE(1,126) NN
      write(*,126) nn
  126 FORMAT(/,11X,'NUMBER OF UNIQUE M-TUPLES =',I6)
      IF(FN.GT.1.01.OR.FN.LT..99) then
cc The original code wrote the sample size FN as a real in
cc format F11.4 but was often subject to rounding error;
cc hence the change to integer reporting below.
         WRITE(1,127) nint(FN)
         write(*,127) nint(fn)
      endif
  127 format(/t11,' GIVEN FREQUENCIES OR PROBABILITIES SUM TO :',i7)
cc  127 FORMAT(/,20('*'),'  GIVEN FREQUENCIES OR PROBABILITIES SUM TO :'
cc     1,F11.4)
      IF(NN.EQ.0) GO TO 1007
      IF(NSAMPL.EQ.0.AND.(FN.GT.1.01.OR.FN.LT..99)) CSAMPL=FN*1.3863
      GO TO 190
C
C     READ M-TUPLES WITHOUT  FREQUENCIES ASSIGNED
C
  150 READ(5,FMT,END=1007) STAR, (DATA(1,J),J=1,M)
      IF(STAR.NE.' ') GO TO 1007
      FREQ(1) = 1.
      N = 1
      NN = 1
  151 READ(5,FMT,END=156) STAR, (DATA(NN+1,J),J=1,M)
      IF(STAR.NE.' ') GO TO 156
      N = N + 1
  152 DO 155 K=1,NN
  153 DO 154 J=1,M
      IF(DATA(NN+1,J).NE.DATA(K,J)) GO TO 155
  154 CONTINUE
      FREQ(K) = FREQ(K) + 1.
      GO TO 151
  155 CONTINUE
      NN = NN + 1
      IF(NN.GT.NLIMIT) GO TO 1009
      FREQ(NN) = 1.
      GO TO 151
  156 IF(N.EQ.1) GO TO 1007
      FN = N
      WRITE(1,157) NN, N
      write(*,157) nn, n
  157 FORMAT(/,11X,'NUMBER OF UNIQUE M-TUPLES =',I6,
     1        //,12X,'TOTAL NUMBER OF M-TUPLES =',I6)
  190 WRITE(1,191)
  191 FORMAT(//,80('*'))
C
C     CREATE SETS OF SUBSETS OF VARIABLES
C
  200 IEND = 2**M - 1
      ALG2 = ALOG(2.)
      DO 224 I=1,IEND
      FI = I
      K = ALOG(FI+0.9)/ALG2
      NNN(I) = I
      IRESID = I
      JORD = 0
      DO 223 J = M-K,M
      K = IRESID - 2**(M-J)
c     IF(K) 223, 221, 221
c     replaced by the following (MZ, 5/02/08):
      if (K.lt.0) go to 223
      if (K.ge.0) go to 221
  221 IRESID = K
      JORD = JORD + 1
      SET(JORD) = M-J+1
  223 CONTINUE
  226 ORDINL(I) = JORD
      NDEX = 0
      DO 225 JJ=1,JORD
      IX = SET(JORD-JJ+1)
      NDEX = NDEX + IX*M**(JORD-JJ)
  225 LIST(I,JJ) = IX
      UNIQ(I) = NDEX
  224 CONTINUE
      DO 240 I = 1, IEND-1
      DO 240 I1 = I, IEND
      IF(UNIQ(I).LT.UNIQ(I1)) GO TO 240
      IX = UNIQ(I)
      UNIQ(I) = UNIQ(I1)
      UNIQ(I1) = IX
      IX = NNN(I)
      NNN(I) = NNN(I1)
      NNN(I1) = IX
  240 CONTINUE
C
C     COMPUTE FREQUENCIES, ENTROPIES AND UNIQUE M-TUPLES
C
  500 DO 505 I=1,IEND
      DO 501 K=1,NN
  501 INDEX(K) = .FALSE.
      UNIQ(I) = 0
      ENLOG = 0.
      JORD = ORDINL(I)
      DO 504 K = 1,NN-1
      IF(INDEX(K)) GO TO 504
      FRE = FREQ(K)
      DO 503 KOMP = K+1, NN
      IF(INDEX(KOMP)) GO TO 503
      DO 502 JJ=1,JORD
      J = LIST(I,JJ)
      IF(DATA(K,J).NE.DATA(KOMP,J)) GO TO 503
  502 CONTINUE
      FRE = FRE + FREQ(KOMP)
      INDEX(KOMP) = .TRUE.
  503 CONTINUE
      UNIQ(I) = UNIQ(I) + 1
      ENLOG = ENLOG + FRE*ALOG(FRE)
  504 CONTINUE
      IF(.NOT.INDEX(NN)) UNIQ(I) = UNIQ(I) + 1
      IF(.NOT.INDEX(NN)) ENLOG = ENLOG + FREQ(NN)*ALOG(FREQ(NN))
      IF(FN.GT.0) H(I) = (FN*ALOG(FN) - ENLOG)/ALG2/FN
      IF(H(I).LT.0.) H(I) = -H(I)
  505 CONTINUE
      WRITE(1,506)
  506 FORMAT(///,'RESULTS:'//'#UNIQ M   H',9X,'T',7X,'CHI**2 '
     1'   DF(T)',5X,'Q',6X,'VARIABLES',/)
C
C     COMPUTE  T, MAXIMUM LIKELIHOOD APPROXIMATION TO CHI**2, DF, Q
C
  600 DO 609 IX=1,IEND
      I = NNN(IX)
      DF = 1
      NDF = 0
      T = -H(I)
      Q = -H(I)
      JORD = ORDINL(I)
      DO 601 JJ=1,JORD
      J = LIST(I,JJ)
      NIQ = UNIQ(2**(J-1))
      DF = DF*NIQ
      NDF = NDF + NIQ - 1
  601 T = T + H(2**(J-1))
      DF = DF - 1 - NDF
      CHI = 0.
c     IF(JORD-1) 608, 608, 602
c     replaced by the following (MZ, 5/02/08):
      if (JORD-1.le.0) go to 608
      if (JORD-1.gt.0) go to 602
  602 DO 606 II=1,I-1
      JJORD = ORDINL(II)
c     IF(JORD-JJORD) 606, 606, 603
c     replaced by the following (MZ, 5/02/08):
      if (JORD-JJORD.le.0) go to 606
      if (JORD-JJORD.gt.0) go to 603
  603 J = 0
      DO 605 JJ=1,JJORD
  604 J = J + 1
      IF(J.GT.JORD) GO TO 606
c     IF(LIST(I,J)-LIST(II,JJ)) 604, 605, 606
c     replaced by the following (MZ, 5/02/08):
      IF(LIST(I,J)-LIST(II,JJ).lt.0) go to 604
      IF(LIST(I,J)-LIST(II,JJ).eq.0) go to 605
      IF(LIST(I,J)-LIST(II,JJ).gt.0) go to 606
  605 CONTINUE
      Q = Q - (-1.)**(JORD-JJORD)*H(II)
  606 CONTINUE
      CHI = CSAMPL*T
  608 WRITE(1,607) UNIQ(I),H(I),T,CHI,DF,Q,(AT(LIST(I,J)),J=1,JORD)
  607 FORMAT(I4,2F10.4,F10.2,I7,F11.4,5X,20A2)
  609 CONTINUE
      GO TO 2000
C
C     WRITE ERROR MESSAGES AND TERMINATE
C
 1001 WRITE(1,1002)
      write(*,1002)
 1002 FORMAT(/,20('*'),'  NO PARAMETER CARDS ENCOUNTERED IN DATA  ',
     1 20('*'))
      GO TO 2000
 1003 WRITE(1,1004) MLIMIT
      write(*,1004) mlimit
 1004 FORMAT(/,20('*'),'  SPECIFIED NUMBER OF VARIABLES EXCEEDS CURREN
     1T LIMIT OF',I4,2X,20('*'))
      GO TO 2000
 1005 WRITE(1,1006)
      write(*,1006)
 1006 FORMAT(/,20('*'),'  NO FORMAT STATEMENT ENCOUNTERED  ',20('*'))
      GO TO 2000
 1007 WRITE(1,1008)
      write(*,1008)
 1008 FORMAT(/,20('*'),'  NO DATA ENCOUNTERED  ',20('*'))
      GO TO 2000
 1009 WRITE(1,1010) NLIMIT
      write(*,1010) nlimit
 1010 FORMAT(/,20('*'),'  NUMBER OF UNIQUE M-TUPLES EXCEEDS CURRENT LI
     1MIT OF',I6,2X,20('*'))
 2000 WRITE(1,2001)
      write(*,2001)
 2001 FORMAT(///,80('*'),///)
      close (1)
      STOP
      END
C
C     *******************************************************************
c
      subroutine outfile(filename,punchfile)
c
c        Creates the filename for output by replacing the 
c        extension on the data/commands filename with .SPC
c
      character filename*(*),punchfile*(*),ext*4,bl,dot
      data EXT,BL,DOT/'.spc',' ','.'/
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
