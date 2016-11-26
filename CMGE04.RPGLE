     H ActGrp('CMG') DftActGrp(*No)
     H Option(*SrcStmt:*NoDebugIO)
     h bnddir('CMGZB01')

     F**************************************************************************
     F* Copyright. . . . . . Chorus Software Limited 1999                      *
     F* Program Name . . . . CMGE04                                            *
     F* Program Date . . . . 24/11/99                                          *
      //@@TITLE: Contract Enquiry
     F* Release level. . . . 400.4                                       *
     F**************************************************************************
      * NOTES:
      *      PARAMETER Z#LR:
      *                  1 = CLOSE PROGRAM
      *                  2 = RETURN LEAVING PROGRAM OPEN
      *                      ANY OTHER VALUE WILL RETURN WHEN
      *                      REQUESTED AND CLOSE
      **************************************************************************
     FCMGE04DF  CF   E             WORKSTN
     F                                     INFDS(STATDS)

      // Contract header details
     FCMGCH00   IF   E           K DISK
BS01 FCMGCHS00  IF   E           K DISK
     FCMGCHT00  IF   E           K DISK
     FCMGCHU00  IF   E           K DISK
     FCMGCHV00  IF   E           K DISK

     FCMGLC00   IF   E           K DISK
     FCMGLCX00  IF   E           K DISK

      //FCMGRC00   IF   E           K DISK

      **************************************************************************
      * Declare fields
      **************************************************************************

     d gEntryMode      s              5i 0
     d gExitStatus     s              5i 0

      //========================================================================
      // Declare constants
      //========================================================================

      /copy CMGCONST
      /copy STDCONST

      //========================================================================
      // Declare prototypes
      //========================================================================

     d orderDetails...
     d                 pr             5i 0
     d   pContract                    7p 0 const


      **************************************************************************
      * Tab processing definitions
      **************************************************************************
     D C#NumTabCodes   c                   10
     D C#Head1         c                   'H1'
     D C#Head2         c                   'H2'
     D C#HeadAnal      c                   'HA'
     D C#ItemList      c                   'IL'
     D C#FiltItem      c                   'FI'
     D C#IniAll        c                   'INIALL'
     D C#PrcTab        c                   'PRCTAB'
     D C#UpdRem        c                   'UPDREM'
     D C#ExitProg      c                   'EXITPROG'
     D C#EndAll        c                   'ENDALL'
     D @TabProc        s               n
     D Z#TabCode       s              2a
     D Z#TabCodes      s                   Like(Z#TabCode) Dim(C#NumTabCodes)
     D Z#TabFunc       s             10a
     D Z#FromTabCode   s                   Like(Z#TabCode)
     D ZzTabCodeNum    s              3s 0
     D Z5TabCode       s                   Like(Z#TabCode)
     D Z5TabCodes      s                   Like(Z5TabCode) Dim(C#NumTabCodes)
     D Z5TabFunc       s             10a
     D Z5FromTabCode   s                   Like(Z5TabCode)
      **************************************************************************
     D CON             S              7  0 DIM(12)
     D SCA             S             10    DIM(20)                              Analysis Hdngs
     D CHA             S              4    DIM(20)                              Analysis Codes
     D CTC             S              1    DIM(20)
     D CTD             S             20    DIM(20)
     D*
     D                 DS
     D  WHA                    1     80
     D                                     DIM(20)                              Analysis Codes
     D  WHA01                  1      4
     D  WHA02                  5      8
     D  WHA03                  9     12
     D  WHA04                 13     16
     D  WHA05                 17     20
     D  WHA06                 21     24
     D  WHA07                 25     28
     D  WHA08                 29     32
     D  WHA09                 33     36
     D  WHA10                 37     40
     D  WHA11                 41     44
     D  WHA12                 45     48
     D  WHA13                 49     52
     D  WHA14                 53     56
     D  WHA15                 57     60
     D  WHA16                 61     64
     D  WHA17                 65     68
     D  WHA18                 69     72
     D  WHA19                 73     76
     D  WHA20                 77     80
     D                 DS
     D  ZCA                    1    200
     D                                     DIM(20)                              Analysis Hdngs
     D  ZCA01                  1     10
     D  ZCA02                 11     20
     D  ZCA03                 21     30
     D  ZCA04                 31     40
     D  ZCA05                 41     50
     D  ZCA06                 51     60
     D  ZCA07                 61     70
     D  ZCA08                 71     80
     D  ZCA09                 81     90
     D  ZCA10                 91    100
     D  ZCA11                101    110
     D  ZCA12                111    120
     D  ZCA13                121    130
     D  ZCA14                131    140
     D  ZCA15                141    150
     D  ZCA16                151    160
     D  ZCA17                161    170
     D  ZCA18                171    180
     D  ZCA19                181    190
     D  ZCA20                191    200
     D                 DS
     D  ADA                    1    300
     D                                     DIM(20)                              Analysis Detls
     D  ADA01                  1     15
     D  ADA02                 16     30
     D  ADA03                 31     45
     D  ADA04                 46     60
     D  ADA05                 61     75
     D  ADA06                 76     90
     D  ADA07                 91    105
     D  ADA08                106    120
     D  ADA09                121    135
     D  ADA10                136    150
     D  ADA11                151    165
     D  ADA12                166    180
     D  ADA13                181    195
     D  ADA14                196    210
     D  ADA15                211    225
     D  ADA16                226    240
     D  ADA17                241    255
     D  ADA18                256    270
     D  ADA19                271    285
     D  ADA20                286    300
     D STATDS          DS
     D  ROWCOL               370    371B 0
     D  SFLRRN               378    379B 0
     D                SDS
     D  ZZWSID               244    253
     D  ZZUSER               254    263
      *
     D QLDA           UDS
     D  XXMODL                 1      3
     D  XXDATG                 4      7
     D CDTA           UDS
     D  XXPLIB                 1     10
     D  XXDLIB                11     20
     D  XXXLIB                21     30
     D XCMDCD         UDS
     D  DSNAME                 1     40
     D  DSADD1                41     75
     D  DSADD2                76    110
     D  DSADD3               111    145
     D  DSADD4               146    180
     D  DSPOCD               181    190
     D  DSCONT               191    210
     D  DSTELP               211    225
     D  DSFAXN               226    240
     D  DTAARA              1000   1000
 ISL  /COPY CMGE04I
     I**************************************************************************
     ICMGLC
     I              LCCT01                      CTC(1)
     I              LCCT02                      CTC(2)
     I              LCCT03                      CTC(3)
     I              LCCT04                      CTC(4)
     I              LCCT05                      CTC(5)
     I              LCCT06                      CTC(6)
     I              LCCT07                      CTC(7)
     I              LCCT08                      CTC(8)
     I              LCCT09                      CTC(9)
     I              LCCT10                      CTC(10)
      *
     I              LCCD01                      CTD(1)
     I              LCCD02                      CTD(2)
     I              LCCD03                      CTD(3)
     I              LCCD04                      CTD(4)
     I              LCCD05                      CTD(5)
     I              LCCD06                      CTD(6)
     I              LCCD07                      CTD(7)
     I              LCCD08                      CTD(8)
     I              LCCD09                      CTD(9)
     I              LCCD10                      CTD(10)
     ICMGLCX
     I              LCCT11                      CTC(11)
     I              LCCT12                      CTC(12)
     I              LCCT13                      CTC(13)
     I              LCCT14                      CTC(14)
     I              LCCT15                      CTC(15)
     I              LCCT16                      CTC(16)
     I              LCCT17                      CTC(17)
     I              LCCT18                      CTC(18)
     I              LCCT19                      CTC(19)
     I              LCCT20                      CTC(20)
      *
     I              LCCD11                      CTD(11)
     I              LCCD12                      CTD(12)
     I              LCCD13                      CTD(13)
     I              LCCD14                      CTD(14)
     I              LCCD15                      CTD(15)
     I              LCCD16                      CTD(16)
     I              LCCD17                      CTD(17)
     I              LCCD18                      CTD(18)
     I              LCCD19                      CTD(19)
     I              LCCD20                      CTD(20)
      *
     I*
     ICMGCH
     I              CHA01                       CHA(1)
     I              CHA02                       CHA(2)
     I              CHA03                       CHA(3)
     I              CHA04                       CHA(4)
     I              CHA05                       CHA(5)
     I              CHA06                       CHA(6)
     I              CHA07                       CHA(7)
     I              CHA08                       CHA(8)
     I              CHA09                       CHA(9)
     I              CHA10                       CHA(10)
     I              CHA11                       CHA(11)
     I              CHA12                       CHA(12)
     I              CHA13                       CHA(13)
     I              CHA14                       CHA(14)
     I              CHA15                       CHA(15)
     I              CHA16                       CHA(16)
     I              CHA17                       CHA(17)
     I              CHA18                       CHA(18)
     I              CHA19                       CHA(19)
     I              CHA20                       CHA(20)
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    Z#CONT            7
     C                   PARM                    Z#LR              1
     C                   Parm                    Z#TabCodes
     C                   Parm                    Z#TabCode
     C                   Parm                    Z#TabFunc
     C                   Parm                    Z#FromTabCode
     C*
     C* LAST RECORD REQUESTED
     C     Z#LR          IFEQ      '1'
     C                   SETON                                        LR
 ISL C     @@EO5F        IFEQ      '5'
 ISL C                   EXSR      SBR012
 ISL C                   ENDIF
     C                   RETURN
     C                   ENDIF
     C*
     C                   Eval      @TabProc = Z#TabFunc <> *Blanks
     C*
     C                   Select
     C* Non-tab (normal) processing initialisation, then proceed to
     C* screen 1
     C                   When      Z#TabFunc = *Blanks
     C*
     C                   EXSR      FSTCYC
     C*
     C                   EXSR      GETSC1
     C*
     C* Tab processing initialisation, then end program
     C                   When      Z#TabFunc = C#IniAll
     C*
     C                   Exsr      FstCyc
     C                   Goto      T#EndProgram
     C*
     C* Header [sales] analysis function - proceed to screen 3
     C                   When      Z#TabFunc = C#HeadAnal
     C*
     C                   Exsr      GetSc3
     C*
     C* Process a particular tab
     C                   When      Z#TabFunc = C#PrcTab
     C*
     C                   Select
     C*    Header 1 - proceed to screen 1
     C                   When      Z#TabCode = C#Head1
     C                   Exsr      GetSc1
     C*    Header 2 - proceed to screen 2
     C                   When      Z#TabCode = C#Head2
     C                   Exsr      GetSc2
     C*    Header [sales] analysis - proceed to screen 3
     C                   When      Z#TabCode = C#HeadAnal
     C                   Exsr      GetSc3
     C*    Unknown tab - don't know how to proceed
     C                   Other
     C                   Eval      Z#TabFunc = C#EndAll
     C                   Eval      Z#TabCode = *Blanks
     C                   Eval      Z#FromTabCode = *Blanks
     C                   Goto      T#EndProgram
     C                   Endsl
     C*
     C* Remaining contract update
     C                   When      Z#TabFunc = C#UpdRem
     C*
     C                   Eval      Screen = *Zero
     C*
     C* Unknown function - don't know how to proceed
     C                   Other
     C*
     C                   Eval      Z#TabFunc = C#EndAll
     C                   Eval      Z#TabCode = *Blanks
     C                   Eval      Z#FromTabCode = *Blanks
     C                   Goto      T#EndProgram
     C*
     C                   Endsl
     C*
     C* Show tab details on screens as necessary
     C                   Eval      *In79 = *Off
     C                   Eval      ZzTcav = *Blanks
     C                   If        @TabProc
     C                   Eval      *In79 = *On
     C                   Eval      ZzTcav = Z#TabCodes(1)
     C                   Eval      ZzTabCodeNum = 2
     C                   Dow       ZzTabCodeNum <= %Elem(Z#TabCodes)
     C                             And Z#TabCodes(ZzTabCodeNum) <> *Blanks
     C                   Eval      ZzTcav = %Trimr(ZzTcav) + ' '
     C                                    + Z#TabCodes(ZzTabCodeNum)
     C                   Eval      ZzTabCodeNum = ZzTabCodeNum + 1
     C                   Enddo
     C                   Endif
     C*
     C* Show screens as necessary
     C                   Eval      *In78 = *Off
     C                   If        Screen = *Zero
     C                   Eval      *In78 = *On
     C                   Endif
     C*
     C     *IN78         DOwEQ     '0'
     C     SCREEN        CASEQ     1             PROSC1
     C     SCREEN        CASEQ     2             PROSC2
     C     SCREEN        CASEQ     3             PROSC3
     C*
     C                   END
     C                   END
     C*
     C* Remaining contract update as necessary
     C                   If        Z#TabFunc = *Blanks
     C                             Or Z#TabFunc = C#UpdRem
     C*
     C                   MOVE      'CE'          Z#ACTN
     C                   EXSR      CAUDIT
     C*
     C                   Endif
     C*
     C     T#EndProgram  Tag
     C*
     C     Z#LR          IFNE      '2'
     C                   SETON                                        LR
 ISL C     @@EO5F        IFEQ      '5'
 ISL C                   EXSR      SBR012
 ISL C                   ENDIF
     C                   ENDIF
     C                   RETURN
     C**************************************************************************
     C     GETSC1        BEGSR
     C**************************************************************************
     C*
     C                   Z-ADD     1             SCREEN            3 0
     C                   SETOFF                                       46
     C                   MOVEL     *BLANKS       WHRCOD
     C                   MOVEL     *BLANKS       WHDESC

     C                   Clear     *All          CMGCH
     C                   Clear     *All          CMGCHS
     C                   Clear     *All          CMGCHT
     C                   Clear     *All          CMGCHU
     C                   Clear     *All          CMGCHV

     C     WHCNTN        CHAIN(N)  CMGCH00                            10
     C     WHCNTN        CHAIN(N)  CMGCHS00
     C     WHCNTN        CHAIN(N)  CMGCHT00
     C     WHCNTN        CHAIN(N)  CMGCHU00
     C     WHCNTN        CHAIN(N)  CMGCHV00

     C     *IN10         IFEQ      *OFF
     C*
     C* LIVE CONTRACT
     C*
     C     CHSTAT        IFEQ      'L'
     C                   SETON                                        32
     C                   ELSE
     C                   SETOFF                                       32
     C                   END
     C*
     C* SETUP SCREEN STATUS
     C*
     C     CHSTAT        IFEQ      'E'
     C                   MOVEL     'ENQUIRY '    WMODE
     C                   ENDIF
     C     CHSTAT        IFEQ      'L'
     C                   MOVEL     'LIVE    '    WMODE
     C                   ENDIF
     C     CHSTAT        IFEQ      'C'
     C                   MOVEL     'COMPLETE'    WMODE
     C                   ENDIF
     C     CHSTAT        IFEQ      'R'
     C                   MOVEL     'REJECTED'    WMODE
     C                   MOVEL     CHRCOD        WHRCOD
     C     WHRCOD        CHAIN     CMGRC00                            21
     C   21              MOVEL     *BLANKS       WHDESC
     C  N21              MOVEL     RCDESC        WHDESC
     C                   SETON                                        46
     C                   ENDIF
     C     CHSTAT        IFEQ      'D'
     C                   MOVEL     'DELETED '    WMODE
     C                   ENDIF
     C*
     C* SCREEN 1
     C                   MOVEL     CHTYPE        WHTYPE
     C                   EXSR      GETTYP
     C                   MOVEL     CHCCOD        WHCCOD
     C                   MOVEL     CHCSUB        WHCSUB
 ISL  *
 ISL C     @@EO5F        IFEQ      '5'
 ISL C                   MOVE      *BLANK        @@5U01          001
 ISL C                   EXSR      SBR001
 ISL C                   MOVE      *OFF          *IN22
 ISL C     @@5P04        IFEQ      '1'
 ISL C     @@5P04        OREQ      '9'
 ISL C     X@IND1        IFEQ      '1'
 ISL C                   MOVE      *ON           *IN22
 ISL C                   ENDIF
 ISL C                   ENDIF
 ISL C                   ELSE
 ISL C                   EXSR      SBR003
 ISL C                   MOVE      *OFF          *IN22
 ISL C     @@EP04        IFNE      *BLANKS
 ISL C                   MOVE      *ON           *IN22
 ISL C                   ENDIF
 ISL C                   ENDIF
 ISL  *
     C     *IN22         IFEQ      *OFF
     C     CDSTOP        ANDEQ     'Y'
     C                   SETON                                        95
     C                   END
      *
      * PROSPECT CODE
      *
     C*                   MOVEL     CHPRCD        WHPRCD
      *
      * IF PROSPECT HAS BEEN SPECIFIED AND MDB IS ACTIVE THEN
      * USE THE VALIDATION ROUTINE TO RETRIEVE PROSPECT DETAILS
      *
     C*                   MOVEL     *BLANKS       ZZNAME           40
     C*                   MOVEL     *BLANKS       ZZADD1           35
     C*                   MOVEL     *BLANKS       ZZADD2           35
     C*                   MOVEL     *BLANKS       ZZADD3           35
     C*                   MOVEL     *BLANKS       ZZADD4           35
     C*                   MOVEL     *BLANKS       ZZPOCD           10
     C*                   MOVEL     *BLANKS       ZZCONT           20
     C*                   MOVEL     *BLANKS       ZZTELP           15
     C*                   MOVEL     *BLANKS       ZZFAXN           15
     C*                   MOVEL     *OFF          @RMDBD            1
      *
     C*     @MDB          IFEQ      *ON
     C*     WHPRCD        ANDNE     *BLANKS
     C*                   CALL      'XCMGCEPL'
     C*                   PARM      '1'           X#MODE            1
     C*                   PARM      WHPRCD        X#PRCD            8
     C*                   PARM      XXDATG        X#DATG            4
     C*                   PARM      XXPLIB        X#PLIB           10
     C*                   PARM      *BLANKS       X#ERRF            1
     C*     X#ERRF        IFNE      '1'
     C*     *LOCK         IN        XCMDCD
     C*                   MOVEL     DSNAME        ZZNAME
     C*                   MOVEL     DSADD1        ZZADD1
     C*                   MOVEL     DSADD2        ZZADD2
     C*                   MOVEL     DSADD3        ZZADD3
     C*                   MOVEL     DSADD4        ZZADD4
     C*                   MOVEL     DSPOCD        ZZPOCD
     C*                   MOVEL     DSCONT        ZZCONT
     C*                   MOVEL     DSTELP        ZZTELP
     C*                   MOVEL     DSFAXN        ZZFAXN
     C*                   MOVEL     *ON           @RMDBD
     C*                   OUT       XCMDCD
     C*                   ENDIF
     C*                   ENDIF
      *
     C                   MOVEL     CHCNAM        WHCNAM
     C                   MOVEL     CHCAD1        WHCAD1           35
     C                   MOVEL     CHCAD2        WHCAD2           35
     C                   MOVEL     CHCAD3        WHCAD3           35
     C                   MOVEL     CHCAD4        WHCAD4           35
     C                   MOVEL     CHCPCD        WHCPCD           10

      /free

        // build new single address line
        whcadd = %trim(whcad1);
        if ( whcad2 <> '' );
          whcadd = %trim(whcadd) + ', ' + %trim(whcad2);
        endif;
        if ( whcad3 <> '' );
          whcadd = %trim(whcadd) + ', ' + %trim(whcad3);
        endif;
        if ( whcad4 <> '' );
          whcadd = %trim(whcadd) + ', ' + %trim(whcad4);
        endif;
        if ( whcpcd <> '' );
          whcadd = %trim(whcadd) + ', ' + %trim(whcpcd);
        endif;

      /end-free

     C                   MOVEL     CHCCON        WHCCON
     C                   MOVEL     CHCTEL        WHCTEL
      *
      * IF THE CUSTOMER CODE HAS NOT BEEN SPECIFIED BUT THE PROSPECT
      * CODE HAS; AND THE CONTRACT ADDRESS, CONTACT, TEL AND FAX
      * DETAILS ARE CURRENTLY BLANK - THEN USE THE DETAILS RETRIEVED
      * FROM THE PROSPECT IF AVAILABLE.
      *
     C*     WHCCOD        IFEQ      *BLANKS
     C*     WHCNAM        ANDEQ     *BLANKS
     C*     WHCAD1        ANDEQ     *BLANKS
     C*     WHCAD2        ANDEQ     *BLANKS
     C*     WHCAD3        ANDEQ     *BLANKS
     C*     WHCAD4        ANDEQ     *BLANKS
     C*     WHCPCD        ANDEQ     *BLANKS
     C*     WHCCON        ANDEQ     *BLANKS
     C*     WHCTEL        ANDEQ     *BLANKS
     C*     @RMDBD        ANDEQ     *ON
     C*                   MOVEL     ZZNAME        WHCNAM
     C*                   MOVEL     ZZADD1        WHCAD1
     C*                   MOVEL     ZZADD2        WHCAD2
     C*                   MOVEL     ZZADD3        WHCAD3
     C*                   MOVEL     ZZADD4        WHCAD4
     C*                   MOVEL     ZZPOCD        WHCPCD
     C*                   MOVEL     ZZCONT        WHCCON
     C*                   MOVEL     ZZTELP        WHCTEL
     C*                   ENDIF
      *
     C                   MOVEL     CHSTWN        WHSTWN
     C                   MOVEL     CHSNAM        WHSNAM
     C                   MOVEL     CHSAD1        WHSAD1
     C                   MOVEL     CHSAD2        WHSAD2
     C                   MOVEL     CHSAD3        WHSAD3
     C                   MOVEL     CHSAD4        WHSAD4
     C                   MOVEL     CHSPCD        WHSPCD
     C                   MOVEL     CHSCON        WHSCON
     C                   MOVEL     CHSTEL        WHSTEL
     C                   MOVEL     CHSEML        WHSEML
     C                   CALL      'XXFDAT'
     C                   PARM                    CHESDT
     C                   PARM                    WHESDT
     C                   CALL      'XXFDAT'
     C                   PARM                    CHEEDT
     C                   PARM                    WHEEDT
     C*
     C* SCREEN 2
     C                   MOVEL     CHONRD        WHONRD
     C                   MOVEL     CHMPDF        WHMPDF
     C*                   MOVEL     CHPRQD        WHPRQD
     C                   MOVEL     CHPRES        WHPRES
      // Permit expiry date
     C                   CALL      'XXFDAT'
     C                   PARM                    CHPEDT
     C                   PARM                    WHPEDT
     C*                   MOVEL     CHPODL        WHPODL
      // Invoice once complete
     C                   Eval      WHIONC = CHIONC
      // Invoice at movement level
     C*                   Eval      WHINVL = CHINVL
     C                   MOVEL     CHONRQ        WHONRQ
     C                   CALL      'XXFDAT'
     C                   PARM                    CHSDAT
     C                   PARM                    WHSDAT
     C     CHSDAT        IFNE      0
     C                   SETON                                        41
     C                   END
     C                   CALL      'XXFDAT'
     C                   PARM                    CHEDAT
     C                   PARM                    WHEDAT
     C     CHEDAT        IFNE      0
     C                   SETON                                        42
     C                   END
     C*
     C                   END
     C*
     C*
     C                   ENDSR
     C*****************************************************************
     C     PROSC1        BEGSR                                                              .
     C**************************************************************************
     C     TOPSC1        TAG
     C*
     C                   Eval      ZzTcod = *Blanks
     C*
     C                   EXFMT     CMGE0401
     C*
     C                   SETOFF                                       90
     C                   MOVE      '9999'        MIC
     C                   Movel(p)  'XAPMSG'      Msgf
     C*
     C* F3=Exit
     C* F12=PREVIOUS
     C*
     C     *INKC         IFEQ      '1'
     C     *INKL         OREQ      '1'
     C                   Exsr      Sc1Exit
     C                   GOTO      ENDSC1
     C                   END
     C*                                                                   SOPM44
     C* F17=CONTRACT TRACKING
     C*
     C     *INKR         IFEQ      '1'
     C                   MOVE      Z#CONT        ZZCNTN            7
     C                   CALL      'CMGE28'
     C                   PARM                    ZZCNTN
     C                   GOTO      TOPSC1
     C                   END

      * F19=Order numbers
     C                   If        ( *INKT )
     C                   Eval      gExitStatus = orderDetails( whcntn )
     C                   If        ( gExitStatus = CMGI48_EXIT_EXIT )
     C                   Exsr      Sc1Exit
     C                   Goto      ENDSC1
     C                   Endif
     C                   If        ( gExitStatus = CMGI48_EXIT_NORMAL or
     C                               gExitStatus = CMGI48_EXIT_PREVIOUS )
     C                   Goto      TOPSC1
     C                   Endif
     C                   If        ( gExitStatus = CMGI48_EXIT_SHOW_CMGI05 )
     C                   Eval      ZzTcod = C#FiltItem
     C                   Endif
     C                   Endif

     C*
     C* F15=PROSPECT DETAILS
     C*
     C*     *INKP         IFEQ      '1'
     C*     @MDB          ANDEQ     *ON
     C*     WHPRCD        ANDNE     *BLANKS
     C*                   MOVE      '3'           ZZMODE            1
     C*                   MOVE      WHPRCD        ZZPRCD            8
     C*                   MOVE      XXDATG        ZZDATG            4
     C*                   MOVE      XXPLIB        ZZPLIB           10
     C*                   MOVE      *BLANKS       ZZERR             1
     C*                   CALL      'XCMGCECL'
     C*                   PARM                    ZZMODE
     C*                   PARM                    ZZPRCD
     C*                   PARM                    ZZDATG
     C*                   PARM                    ZZPLIB
     C*                   PARM                    ZZERR
     C*                   GOTO      TOPSC1
     C*                   ENDIF
     C*
     C* F20=SALES ANALYSIS
     C*
     C     *INKU         IFEQ      *ON
     C     @TabProc      Andeq     *Off
     C                   EXSR      GETSC3
     C                   MOVE      '1'           @FROM             1
     C                   GOTO      ENDSC1
     C                   END
      *
      * F10=Project Statement Enquiry
      *
     c                   if        *inkj
     c                   call      'CMGE18'
     c                   parm      whcntn        p#contract        7 0
     c                   parm      *blanks       p#kfey            1
     c                   goto      topsc1
     c                   endif
     C*
     C* TAB CODE MUST BE VALID, IF ENTERED
     C*
     C* Ignore current tab code
     C                   If        ZzTcod <> *Blanks
     C                             And ZzTcod = C#Head1
     C                   Goto      TopSc1
     C                   Endif
     C*
     C                   If        ZzTcod <> *Blanks
     C                             and ZzTcod <> C#FiltItem
     C                   Eval      *In21 = *Off
     C     ZzTcod        Lookup    Z#TabCodes                             21
     C                   If        Not *In21
     C                   Eval      Msgf = 'CHSMSG'
     C                   Eval      Mic = '0324'
     C                   Eval      *In90 = *On
     C                   Goto      TopSc1
     C                   Endif
     C                   Endif
     C*
     C* Tab processing ...
     C                   If        @TabProc
     C                   Select
     C* ... for sales analysis function
     C                   When      *InKU
     C                   Eval      Z#TabFunc = C#HeadAnal
     C                   Eval      Z#TabCode = *Blanks
     C*
     C                   Other
     C                   Eval      Z#TabFunc = C#PrcTab
     C* ... for valid tab code entered
     C                   If        ZzTcod <> *Blanks
     C                   Eval      Z#TabCode = ZzTcod
     C* ... for next logical screen
     C                   Else
     C                   Eval      Z#TabCode = C#Head2
     C                   Endif
     C                   Endsl
     C                   Eval      Z#FromTabCode = C#Head1
     C                   Eval      *In78 = *On
     C*
     C* Normal processing - next logical screen
     C                   Else
     C*
     C                   EXSR      GETSC2
     C*
     C                   Endif
     C*
     C     ENDSC1        ENDSR
     C*****************************************************************
     C     Sc1Exit       Begsr

     C*
     C* Exit tab processing
     C                   If        @TabProc
     C                   Eval      Z#TabFunc = C#ExitProg
     C                   Eval      Z#TabCode = *Blanks
     C                   Eval      Z#FromTabCode = C#Head1
     C                   Endif
     C*
     C                   SETON                                        78

     C                   endsr
     C**************************************************************************
     C     GETSC2        BEGSR
     C**************************************************************************
     C*
     C                   Z-ADD     2             SCREEN            3 0
 ISL  *
 ISL C***  @@EO5F        IFEQ      '5'
 ISL C                   MOVE      *BLANK        @@5U01          001
 ISL C                   EXSR      SBR004
 ISL C                   MOVE      *OFF          *IN22
 ISL C     @@5P04        IFEQ      '1'
 ISL C     @@5P04        OREQ      '9'
 ISL C     X@IND1        IFEQ      '1'
 ISL C                   MOVE      *ON           *IN22
 ISL C                   ENDIF
 ISL C                   ENDIF
 ISL C***                ENDIF
     C*
     C                   MOVE      CHSWMR        WHSWMR
     C                   MOVE      CHSWMC        WHSWMC
     C                   MOVEL     CHHWPC        WHHWPC
     C                   CALL      'XXFDAT'
     C                   PARM                    CHHWPX
     C                   PARM                    WHHWPX
      *
     C                   ENDSR
     C*****************************************************************
     C     PROSC2        BEGSR                                                              .
     C**************************************************************************
     C*
     C     TOPSC2        TAG
      *
     C                   Eval      ZzTcod = *Blanks
      *
      * IS THE SCRATCH PAD ACTIVE FOR THIS CUSTOMER ?
     C                   MOVE      'CMG'         Z#MODL            3            MODULE CODE
     C                   MOVE      WHCCOD        Z#CD06            6
     C                   MOVE      WHCSUB        Z#CD03            3
     C                   MOVE      *BLANKS       Z#CD02            2
     C                   MOVE      *BLANKS       Z#CD25           25
     C                   MOVE      *BLANKS       Z#CD41            4
     C                   MOVE      *BLANKS       Z#CD42            4
     C                   MOVE      *BLANKS       Z#CD43            4
     C                   MOVE      *BLANKS       Z#CD44            4
     C                   MOVE      *BLANKS       Z#CD45            4
     C                   MOVE      *BLANKS       Z#CD07            7
     C                   MOVE      *OFF          Z#TEXT            1            RETURN FLAG
     C                   MOVE      *BLANKS       Z#RTYP            4            RETURN TYPE
     C                   CALL      'XAPA74CL'
     C                   PARM                    Z#MODL            3            MODULE CODE
     C                   PARM                    Z#CD06            6            )
     C                   PARM                    Z#CD03            3            )
     C                   PARM                    Z#CD02            2            )
     C                   PARM                    Z#CD25           25            ) INTERCHANGEABLE
     C                   PARM                    Z#CD41            4            ) CODES
     C                   PARM                    Z#CD42            4            ) DEPENDANT ON
     C                   PARM                    Z#CD43            4            ) WHICH MODULE
     C                   PARM                    Z#CD44            4            )
     C                   PARM                    Z#CD45            4            )
     C                   PARM                    Z#CD07            7            )
     C                   PARM                    Z#TEXT            1            RETURN FLAG
     C                   PARM                    Z#RTYP            4            RETURN TYPE
      *
      * DISPLAY 'SCRATCH PAD ACTIVE' MESSAGE IF SO.
     C     Z#TEXT        IFEQ      *ON
     C     MIC           ANDEQ     '9999'
     C                   MOVE      '0660'        MIC
     C                   ENDIF
     C*
     C                   EXFMT     CMGE0402
     C*
     C                   SETOFF                                       90
     C                   MOVE      '9999'        MIC
     C                   Movel(p)  'XAPMSG'      Msgf
     C*
     C* F3=EXIT
     C*
     C     *INKC         IFEQ      '1'
     C     Z#FKEY        OREQ      'C'
     C                   Exsr      Sc2Exit
     C                   GOTO      ENDSC2
     C                   END
     C*
     C* F12=PREVIOUS
     C*
     C     *INKL         IFEQ      *ON
     C*
     C* Tab processing for previous logical screen
     C                   If        @TabProc
     C                   Eval      Z#TabFunc = C#PrcTab
     C                   Eval      Z#TabCode = C#Head1
     C                   Eval      Z#FromTabCode = C#Head2
     C                   Eval      *In78 = *On
     C*
     C* Normal processing - previous logical screen
     C                   Else
     C                   EXSR      GETSC1
     C                   Endif
     C*
     C                   GOTO      ENDSC2
     C                   END
     C*
     C*
     C* F17=CONTRACT TRACKING
     C*
     C     *INKR         IFEQ      '1'
     C                   MOVE      Z#CONT        ZZCNTN            7
     C                   CALL      'CMGE28'
     C                   PARM                    ZZCNTN
     C                   GOTO      TOPSC2
     C                   END
     C*
     C* F18=NOTES (SCRATCH PAD)
     C*
     C     *INKS         IFEQ      '1'
     C                   MOVE      Z#CONT        ZZCNTN            7
     C                   CALL      'CMGM80'
     C                   PARM                    ZZCNTN
     C                   GOTO      TOPSC2
     C                   END


      * F19=Order numbers
     C                   If        ( *INKT )
     C                   Eval      gExitStatus = orderDetails( whcntn )
     C                   If        ( gExitStatus = CMGI48_EXIT_EXIT )
     C                   Exsr      Sc2Exit
     C                   Goto      ENDSC2
     C                   Endif
     C                   If        ( gExitStatus = CMGI48_EXIT_NORMAL or
     C                               gExitStatus = CMGI48_EXIT_PREVIOUS )
     C                   Goto      TOPSC2
     C                   Endif
     C                   If        ( gExitStatus = CMGI48_EXIT_SHOW_CMGI05 )
     C                   Eval      ZzTcod = C#FiltItem
     C                   Endif
     C                   Endif


     C*
     C* F20=SALES ANALYSIS
     C*
     C     *INKU         IFEQ      *ON
     C     @TabProc      Andeq     *Off
     C                   EXSR      GETSC3
     C                   MOVE      '2'           @FROM             1
     C                   GOTO      ENDSC2
     C                   END
      *
      * F10=Project Statement Enquiry
      *
     c                   if        *inkj
     c                   call      'CMGE18'
     c                   parm      whcntn        p#contract        7 0
     c                   parm      *blanks       p#kfey            1
     c                   goto      topsc2
     c                   endif
     C*
     C* TAB CODE MUST BE VALID, IF ENTERED
     C*
     C* Ignore current tab code
     C                   If        ZzTcod <> *Blanks
     C                             And ZzTcod = C#Head2
     C                   Goto      TopSc2
     C                   Endif
     C*
     C                   If        ZzTcod <> *Blanks
     C                             and ZzTcod <> C#FiltItem
     C                   Eval      *In21 = *Off
     C     ZzTcod        Lookup    Z#TabCodes                             21
     C                   If        Not *In21
     C                   Eval      Msgf = 'CHSMSG'
     C                   Eval      Mic = '0324'
     C                   Eval      *In90 = *On
     C                   Goto      TopSc2
     C                   Endif
     C                   Endif
      *
     C                   Select
      * Tab Processing for sales analysis function
     C                   When      @TabProc
     C                             And *InKU
     C                   Eval      Z#TabFunc = C#HeadAnal
     C                   Eval      Z#TabCode = *Blanks
     C                   Eval      Z#FromTabCode = C#Head2
     C                   Eval      *In78 = *On
      *
      * Tab processing for valid tab code entered
     C                   When      @TabProc
     C                             And ZzTcod <> *Blanks
     C                   Eval      Z#TabFunc = C#PrcTab
     C                   Eval      Z#TabCode = ZzTcod
     C                   Eval      Z#FromTabCode = C#Head2
     C                   Eval      *In78 = *On
      *
     C                   Other
     C*
     C* GET WASTEFILE HEADER SCREEN
     C*
     C     Z#FKEY        IFNE      'L'
     C                   MOVE      *BLANKS       Z#FKEY            1
     C**                 EXSR      GETSC4
      *
      * Tab processing for next logical screen
     C                   If        @TabProc
     C                   Eval      Z#TabFunc = C#PrcTab
     C                   Eval      Z#TabCode = C#ItemList
     C                   Eval      Z#FromTabCode = C#Head2
     C                   Eval      *In78 = *On
      *
      * Normal processing - next logical screen
     C                   Else
     C                   EXSR      ITMPRG
     C                   Endif
      *
     C                   ELSE
     C                   MOVE      *BLANKS       Z#FKEY
     C                   ENDIF
      *
     C                   Endsl
     C*
     C*
     C     ENDSC2        ENDSR
     C*****************************************************************
     C     Sc2Exit       Begsr

     C*
     C* Exit tab processing
     C                   If        @TabProc
     C                   Eval      Z#TabFunc = C#ExitProg
     C                   Eval      Z#TabCode = *Blanks
     C                   Eval      Z#FromTabCode = C#Head2
     C                   Endif
     C*
     C                   SETON                                        78

     C                   endsr
     C**************************************************************************
     C     ITMPRG        BEGSR
     C**************************************************************************
     C*
     C                   MOVE      CHCNTN        Z5CODE
     C                   MOVE      ' '           EXIT
     C                   Eval      gEntryMode = CMGE05_MODE_NORMAL
     C                   Move      gEntryMode    gEntryMode
     C                   CALL      'CMGE05CL'
     C                   PARM                    Z5CODE            7
     C                   PARM                    EXIT              1
     C                   Parm      *Blanks       Z5Lr              1
     C                   Parm      *Blanks       Z5TabCodes
     C                   Parm      *Blanks       Z5TabCode
     C                   Parm      *Blanks       Z5TabFunc
     C                   Parm      *Blanks       Z5FromTabCode
     C                   Parm      *Blanks       Z5ItemNo          3
     C                   Parm      *Blanks       Z5MoveNo          5
     C                   Parm                    pEntryMode        5
     C     EXIT          IFEQ      'X'
     C                   SETON                                        78
     C                   END
     C*
     C                   ENDSR
     C**************************************************************************
     C     GETSC3        BEGSR
     C**************************************************************************
     C*
     C                   Z-ADD     3             SCREEN            3 0
     C*
     C                   MOVEA     CHA           WHA
     C*
     C* DESCRIPTIONS
     C*
     C                   DO        20            X                 3 0
     C                   MOVE      *BLANKS       ADA(X)
     C     SCA(X)        IFEQ      *BLANKS
     C                   ITER
     C                   END
     C     WHA(X)        IFNE      *BLANKS
     C                   Z-ADD     X             ANTYPE
     C                   MOVE      WHA(X)        ANCODE
 ISL  *
 ISL C     @@EO5F        IFEQ      '5'
 ISL C                   MOVE      *BLANK        @@5U01          001
 ISL C                   EXSR      SBR006
 ISL C                   MOVE      *OFF          *IN21
 ISL C     @@5P04        IFEQ      '1'
 ISL C     @@5P04        OREQ      '9'
 ISL C     X@IND1        IFEQ      '1'
 ISL C                   MOVE      *ON           *IN21
 ISL C                   ENDIF
 ISL C                   ENDIF
 ISL C                   ELSE
 ISL C                   EXSR      SBR008
 ISL C                   MOVE      *OFF          *IN21
 ISL C     @@EP04        IFNE      *BLANKS
 ISL C                   MOVE      *ON           *IN21
 ISL C                   ENDIF
 ISL C                   ENDIF
 ISL  *
     C  N21              MOVEL     MDDESC        ADA(X)
     C                   END
     C                   ENDDO
      *
      *
     C                   ENDSR
     C*****************************************************************
     C     PROSC3        BEGSR                                                              .
     C**************************************************************************
     C     TOPSC3        TAG
     C*
     C                   Eval      ZzTcod = *Blanks
     C*
     C                   EXFMT     CMGE0403
     C*
     C                   SETOFF                                       90
     C                   MOVE      '9999'        MIC
     C                   Movel(p)  'XAPMSG'      Msgf
     C*
     C* F3=EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   Exsr      Sc3Exit
     C                   GOTO      ENDSC3
     C                   END


      * F19=Order numbers
     C                   If        ( *INKT )
     C                   Eval      gExitStatus = orderDetails( whcntn )
     C                   If        ( gExitStatus = CMGI48_EXIT_EXIT )
     C                   Exsr      Sc3Exit
     C                   Goto      ENDSC3
     C                   Endif
     C                   If        ( gExitStatus = CMGI48_EXIT_NORMAL or
     C                               gExitStatus = CMGI48_EXIT_PREVIOUS )
     C                   Goto      TOPSC3
     C                   Endif
     C                   If        ( gExitStatus = CMGI48_EXIT_SHOW_CMGI05 )
     C                   Eval      ZzTcod = C#FiltItem
     C                   Endif
     C                   Endif


     C*
     C* TAB CODE MUST BE VALID, IF ENTERED
     C*
     C* Ignore current tab code
     C                   If        ZzTcod <> *Blanks
     C                             And ZzTcod = C#HeadAnal
     C                   Goto      TopSc3
     C                   Endif
     C*
     C                   If        ZzTcod <> *Blanks
     C                             and ZzTcod <> C#FiltItem
     C                   Eval      *In21 = *Off
     C     ZzTcod        Lookup    Z#TabCodes                             21
     C                   If        Not *In21
     C                   Eval      Msgf = 'CHSMSG'
     C                   Eval      Mic = '0324'
     C                   Eval      *In90 = *On
     C                   Goto      TopSc3
     C                   Endif
     C                   Endif
      *
      * Tab processing for valid tab code entered
     C                   If        @TabProc
     C                             And ZzTcod <> *Blanks
     C                   Eval      Z#TabFunc = C#PrcTab
     C                   Eval      Z#TabCode = ZzTcod
     C                   Eval      Z#FromTabCode = C#HeadAnal
     C                   Eval      *In78 = *On
     C                   Else
     C*
     C* F12=PREVIOUS
      *
      * Tab processing for previous logical screen
     C                   If        @TabProc
     C                   If        Z#TabFunc = C#HeadAnal
     C                   Eval      Z#TabCode = Z#FromTabCode
     C                   Else
     C                   Eval      Z#TabCode = C#Head2
     C                   Endif
     C                   Eval      Z#TabFunc = C#PrcTab
     C                   Eval      Z#FromTabCode = C#HeadAnal
     C                   Eval      *In78 = *On
      *
      * Normal processing - previous logical screen
     C                   Else
     C*
     C     @FROM         IFEQ      '1'
     C                   EXSR      GETSC1
     C                   END
     C*
     C     @FROM         IFEQ      '2'
     C                   EXSR      GETSC2
     C                   END
     C*
     C                   Endif
     C                   Endif
     C*
     C     ENDSC3        ENDSR
     C*****************************************************************
     C     Sc3Exit       Begsr

     C*
     C* Exit tab processing
     C                   If        @TabProc
     C                   Eval      Z#TabFunc = C#ExitProg
     C                   Eval      Z#TabCode = *Blanks
     C                   Eval      Z#FromTabCode = C#HeadAnal
     C                   Endif
     C*
     C                   SETON                                        78

     C                   endsr
     C*****************************************************************
     C     GETTYP        BEGSR
     C*****************************************************************
     C*
     C                   Z-ADD     1             X1                2 0
     C     WHTYPE        LOOKUP    CTC(X1)                                26
     C     *IN26         IFEQ      *ON
     C                   MOVEL     CTD(X1)       WHTDSC
     C                   ELSE
     C                   MOVEL     *BLANKS       WHTDSC
     C                   ENDIF
     C*
     C                   ENDSR
     C**************************************************************************
     C     CAUDIT        BEGSR
     C**************************************************************************
     C*
     C* CONTRACT AUDIT TRAIL
     C*
     C                   MOVE      CHCNTN        Z#CNUM            7
     C                   CALL      'CMGA28  '
     C                   PARM                    Z#CNUM
     C                   PARM      *ZEROS        Z#INUM            3
     C                   PARM                    Z#ACTN            2
     C                   PARM      *BLANKS       Z#TXT           120
     C*
     C*
     C                   ENDSR
      **************************************************************************
     C     FSTCYC        BEGSR
      **************************************************************************
 ISL  /COPY CMGE04B
      *
      *
      * SET ALL INDICATORS OFF
      *
     C                   MOVE      *OFF          *IN
     C*
     C     0             CHAIN     CMGLC00                            21
     C     0             CHAIN(N)  CMGLCX00                           21
 ISL  *
 ISL C     @@EO5F        IFEQ      '5'
 ISL C                   MOVE      *BLANK        @@5U01          001
 ISL C                   EXSR      SBR009
 ISL C                   MOVE      *OFF          *IN21
 ISL C     @@5P04        IFEQ      '1'
 ISL C     @@5P04        OREQ      '9'
 ISL C     X@IND1        IFEQ      '1'
 ISL C                   MOVE      *ON           *IN21
 ISL C                   ENDIF
 ISL C                   ENDIF
 ISL C                   ELSE
 ISL C                   EXSR      SBR011
 ISL C                   MOVE      *OFF          *IN21
 ISL C     @@EP04        IFNE      *BLANKS
 ISL C                   MOVE      *ON           *IN21
 ISL C                   ENDIF
 ISL C                   ENDIF
 ISL  *
     C*
     C                   MOVEA     SCA           ZCA
     C                   MOVE      '1'           @FROM             1
     C*
     C                   MOVE      Z#CONT        WHCNTN            7 0
     C*
     C                   Move      *Blanks       Msgf
     C                   Movel     'XAPMSG'      Msgf
     C                   MOVE      '9999'        MIC               4
     C*
     C                   SETOFF                                       90
      *
      * SETUP MDB DATA AREA
      *
     C     *DTAARA       DEFINE                  XCMDCD
     C                   IN        XCMDCD
     C                   UNLOCK    XCMDCD
      *
      * SET UP *LDA
      *
     C     *DTAARA       DEFINE    *LDA          QLDA
     C                   IN        QLDA
      *
      * SET UP CHORUSDTA
      *
     C     *DTAARA       DEFINE    CHORUSDTA     CDTA
     C                   IN        CDTA
     C                   UNLOCK    CDTA
      *
      * ... is MDB active?
      *
     C     *LIKE         DEFINE    *INLR         @MDB
     C*                   CALL      'CMGMDBCL'
     C*                   PARM                    @MDB
      * Turn off link to MDB database
     C                   MOVE      *OFF          @MDB
     C*
     C                   ENDSR
      *
     C**************************************************************************
     C*
     C     ANKEY         KLIST
     C                   KFLD                    ANTYPE            2 0
     C                   KFLD                    ANCODE            4
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CHCCOD
     C                   KFLD                    CHCSUB
     C*
 ISL  /COPY CMGE04C

      //========================================================================
      // F19=Order details processing
      // - A wrapper around CMGI48
      //========================================================================

     p orderDetails...
     p                 b

     d orderDetails...
     d                 pi             5i 0
     d   pContract                    7p 0 const

      // Customer order details
     d CMGI48          pr                  extpgm('CMGI48')
     d   pContract                    7p 0 const
     d   pExitStatus                  5i 0
     d   pCHONRQ                      1a   const options(*nopass:*omit)
     d   pEntryMode                   5i 0 const options(*nopass:*omit)

     d wExitStatus     s              5i 0

      /free

       // Show customer order details panel (CMGI48)
       CMGI48( pContract : wExitStatus : *omit : CMGI48_MODE_VIEW );
       return wExitStatus;

      /end-free


     p orderDetails...
     p                 e