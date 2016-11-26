     H ActGrp('CMG') DftActGrp(*No)                                                                 
     H BndDir('CMGSTDGEN')                                                                          
     H Option(*SrcStmt:*NoDebugIO)                                                                  
     h bnddir('CMGZLCT')                                                                            
     h bnddir('CMGZB01')                                                                            
     h bnddir('XAPZB01')                                                                            
                                                                                                    
     F**************************************************************************                    
     F* Copyright. . . . . . Chorus Software Limited 1999                      *                    
     F* Program Name . . . . CMGE05                                            *                    
     F* Program Date . . . . 24/11/99                                          *                    
     F* Title. . . . . . . . Contract Items & Movements                        *                    
     F*                      (WASTEFILE & WASTEFILE TECH)                      *                    
     F* Release level. . . . 400.4                                       *                          
     F**************************************************************************                    
      * NOTES:                                                                                      
      *      PARAMETER Z#LR:                                                                  
      *                  1 = CLOSE PROGRAM                                                          
      *                  2 = RETURN LEAVING PROGRAM OPEN                                            
      *                      ANY OTHER VALUE WILL RETURN WHEN                                       
      *                      REQUESTED AND CLOSE                                                    
      **************************************************************************                    
     FCMGE05DF  CF   E             WORKSTN                                                          
     F                                     SFILE(CMGE051S:SFRCD1)                                   
     F                                     SFILE(CMGE053S:SFRCD3)                                   
     F                                     SFILE(CMGE055S:SFRCD5)                                   
     F                                     INFDS(STATDS)                                            
     FCMGCH00   IF   E           K DISK                                                             
     FCMGCW00   IF   E           K DISK                                                             
     F**CMGCWS00  IF   E           K DISK                                                           
     FCMGCW01   IF   E           K DISK                                                             
     F                                     RENAME(CMGCW:CMGCW1)                                     
     FCMGMV00   IF   E           K DISK                                                             
     FCMGMVW00  IF   E           K DISK                                                             
     FCMGMV13   IF   E           K DISK                                                             
     F                                     RENAME(CMGMV:CMGMV1)                                     
     FCMGLC00   IF   E           K DISK                                                             
     FCMGLCX00  IF   E           K DISK                                                             
     FCMGMC00   IF   E           K DISK                                                             
     FIIMIM00   IF   E           K DISK                                                             
     FCMGAC00   IF   E           K DISK                                                             
     FCMGAN00   IF   E           K DISK                                                             
     FCMGCWZ00  IF   E           K DISK                                                             
BS01 FCMGCWY00  IF   E           K DISK                                                             
     FCMGCWW00  IF   E           K DISK                                                             
     FCMGCWV00  IF   E           K DISK                                                             
     FCMGCWR00  IF   E           K DISK                                                             
     FCMGDS00   IF   E           K DISK                                                             
     FCMGCF00   IF A E           K DISK                                                             
      *                                                                                             
     FBPLSDS00  IF   E           K DISK                                                             
                                                                                                    
      * Item definition details                                                                     
     fcmgid00   if   e           k disk                                                             
                                                                                                    
      * Unit code details                                                                           
     fcmgun00   if   e           k disk                                                             
                                                                                                    
      **************************************************************************                    
      * Declare fields                                                                              
      **************************************************************************                    
                                                                                                    
     d pEntryMode      s              5a                                                            
     d gEntryMode      s              5i 0                                                          
     d gUseFilterList  s               n                                                            
     d gItemAccepted   s               n                                                            
                                                                                                    
                                                                                                    
      ****************************************************************                              
      * Named indicators                                                                            
      ****************************************************************                              
                                                                                                    
     d PtrIndicators   s               *   Inz(%Addr(*In))                                          
     d Indicators      ds                  Based(PtrIndicators)                                     
     d   MaintainDSCD         91     91n                                                            
     d   MaintainESTW         92     92n                                                            
     d   MaintainAPPL         93     93n                                                            
     d   MaintainEWCN         94     94n                                                            
                                                                                                    
      **************************************************************************                    
      * Declare constants                                                                           
      **************************************************************************                    
                                                                                                    
      /copy CMGCONST                                                                                
      /copy STDCONST                                                                                
                                                                                                    
      * Default user defined labels                                                                 
     d cDftLbl1        c                   'Weight Included'                                        
     d cDftLbl2        c                   'Rate/Tonne'                                             
     d cDftLbl3        c                   'Rental Details'                                         
     d cDftLbl4        c                   'Estimated Weight'                                       
     d cDftLbl5        c                   'Estimated Lifts'                                        
                                                                                                    
     d cLbl1Suffix     c                   ' Included'                                              
     d cLbl2Prefix     c                   'Rate/'                                                  
     d cLbl3Suffix     c                   ' Details'                                               
     d cLbl4Prefix     c                   'Estimated '                                             
     d cLbl5Prefix     c                   'Estimated '                                             
                                                                                                    
      **************************************************************************                    
      * Declare prototypes                                                                          
      **************************************************************************                    
                                                                                                    
      // Customer order number details                                                              
     d ContractOrders  pr                  extpgm('CMGI48')                                         
     d  pContract                     7p 0 const                                                    
     d  pFKey                         1a                                                            
     d  pCHONRQ                       1a   const options(*nopass:*omit)                             
     d  pMode                         5i 0 const options(*nopass:*omit)                             
                                                                                                    
      /copy CMGPR02                                                                                 
      /copy CMGZLCTC                                                                                
      /copy CMGZLCWC                                                                                
      /copy CMGZLCOC                                                                                
      /copy CMGZLMVC                                                                                
      /copy XAPZLUSC                                                                                
                                                                                                    
     d onFilterList...                                                                              
     d                 pr              n                                                            
     d   pContract                    7p 0 const                                                    
     d   pItem                        3p 0 const                                                    
                                                                                                    
     d getFilterListDescription...                                                                  
     d                 pr            40a                                                            
                                                                                                    
     d orderDetails...                                                                              
     d                 pr             5i 0                                                          
     d   pContract                    7p 0 const                                                    
                                                                                                    
      **************************************************************************                    
      * Declare work fields                                                                         
      **************************************************************************                    
                                                                                                    
     d today           s              7p 0                                                          
     d gExitStatus     s              5i 0                                                          
                                                                                                    
      **************************************************************************                    
      * Tab processing definitions                                                                  
      **************************************************************************                    
     D C#NumTabCodes   c                   10                                                       
     D C#Head2         c                   'H2'                                                     
     D C#ItemList      c                   'IL'                                                     
     D C#FiltItem      c                   'FI'                                                     
     D C#ItemDetl      c                   'ID'                                                     
     D C#MoveList      c                   'ML'                                                     
     D C#IniAll        c                   'INIALL'                                                 
     D C#IniTab        c                   'INITAB'                                                 
     D C#PrcTab        c                   'PRCTAB'                                                 
     D C#EnqItem       c                   'ENQITEM'                                                
     D C#ClsItem       c                   'CLSITEM'                                                
     D C#EnqMove       c                   'ENQMOVE'                                                
     D C#UpdRem        c                   'UPDREM'                                                 
     D C#ExitProg      c                   'EXITPROG'                                               
     D C#EndAll        c                   'ENDALL'                                                 
     D @TabProc        s               n                                                            
     D Z#TabCode       s              2a                                                            
     D Z#TabCodes      s                   Like(Z#TabCode) Dim(C#NumTabCodes)                       
     D Z#TabFunc       s             10a                                                            
     D Z#FromTabCode   s                   Like(Z#TabCode)                                          
     D Z#ItemNo        s              3a                                                            
     D Z#MoveNo        s              5a                                                            
     D ZzTabCodeNum    s              3s 0                                                          
     D ZzItemNoOpen    s                   Like(WwItem)                                             
     D ZzMoveNoOpen    s                   Like(S5Mvno)                                             
     D Z6TabCode       s                   Like(Z#TabCode)                                          
     D Z6TabCodes      s                   Like(Z6TabCode) Dim(C#NumTabCodes)                       
     D Z6TabFunc       s             10a                                                            
     D Z6FromTabCode   s                   Like(Z6TabCode)                                          
     D**************************************************************************                    
     D ZwItem          s                   Like(WwItem)                                             
      *                                                                                             
     D SEL             S              1    DIM(30)                                                  
     D CON             S              7  0 DIM(12)                                                  
     D SCA             S             10    DIM(20)                              Analysis Hdngs      
     D ZCA             S             10    DIM(20)                              Analysis Hdngs      
     D CHA             S              4    DIM(20)                              Analysis Codes      
     D MVA             S              4    DIM(20)                              Analysis Codes      
     D WMA             S              4    DIM(20)                              Analysis Codes      
     D MDA             S             15    DIM(20)                              Analysis Detls      
     D CTC             S              1    DIM(20)                                                  
     D CTD             S             20    DIM(20)                                                  
     D DIN             S             10    DIM(25)                              DOC INDEX NAME      
     D DIE             S             20    DIM(25)                              DOC INDEX ENTRY     
      *                                                                                             
      *                                                                                             
     D STATDS          DS                                                                           
     D  ROWCOL               370    371B 0                                                          
     D  SFLRRN               378    379B 0                                                          
     D                SDS                                                                           
     D  ZZWSID               244    253                                                             
     D  ZZUSER               254    263                                                             
     D CANC            C                   CONST('*CANCELLED*')                                     
      *                                                                                             
     D C#CARE          C                   CONST('CARE ')                                           
     D C#BOOK          C                   CONST('BOOKREFNO ')                                      
     D C#SCAN          C                   CONST('*SCANNED* ')                                      
                                                                                                    
     D LDA            UDS                                                                           
     D  LDMODL                 1      3                                                             
     D  LDDATG                 4      7                                                             
     D  DATFMT                 9     11                                                             
                                                                                                    
     d @7selected      s               n                                                            
     d margin          s             15p 4                                                          
                                                                                                    
     I**************************************************************************                    
 ISL  /COPY CMGE05I                                                                                 
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
     C*****************************************************************                             
     C     *ENTRY        PLIST                                                                      
     C                   PARM                    Z#CONT            7                                
     C                   PARM                    Z#EXIT            1                                
     C                   Parm                    Z#Lr              1                                
     C                   Parm                    Z#TabCodes                                         
     C                   Parm                    Z#TabCode                                          
     C                   Parm                    Z#TabFunc                                          
     C                   Parm                    Z#FromTabCode                                      
     C                   Parm                    Z#ItemNo                                           
     C                   Parm                    Z#MoveNo                                           
     C                   Parm                    pEntryMode                                         
     C*                                                                                             
     C* LAST RECORD REQUESTED                                                                       
     C     Z#Lr          Ifeq      '1'                                                              
     C                   Seton                                        LR                            
     C     @@EO5F        Ifeq      '5'                                                              
     C                   Exsr      Sbr014                                                           
     C                   Endif                                                                      
     C                   Return                                                                     
     C                   Endif                                                                      
     C*                                                                                             
     C     *Dtaara       Define    *Lda          Lda                                                
     C                   In        Lda                                                              
     C                   Move      LDMODL        Save_Mod          3                                
     C*                                                                                             
     C                   Eval      @TabProc = Z#TabFunc <> *Blanks                                  
                                                                                                    
                                                                                                    
      * Deal with 'entry mode' parameter; converting from character                                 
      * to a numeric field                                                                          
     C                   Move      pEntryMode    gEntryMode                                         
                                                                                                    
                                                                                                    
      * Check whether a specific list of contract items should be shown                             
     C                   Eval      gUseFilterList = false                                           
     C                   Eval      *IN33 = false                                                    
     C                   If        ( gEntryMode = CMGI05_MODE_USE_LIST )                            
     C                   Eval      gUseFilterList = true                                            
     C                   Eval      *IN33 = true                                                     
     C                   Eval      S1FDES = getFilterListDescription()                              
     C                   Endif                                                                      
                                                                                                    
                                                                                                    
     C                   Select                                                                     
     C* Non-tab (normal) processing initialisation, then proceed to                                 
     C* screen 1                                                                                    
     C                   When      Z#TabFunc = *Blanks                                              
     C*                                                                                             
     C                   EXSR      FSTCYC                                                           
     C*                                                                                             
     C                   EXSR      SETSF1                                                           
     C*                                                                                             
     C* Tab processing initialisation, then end program                                             
     C                   When      Z#TabFunc = C#IniAll                                             
     C*                                                                                             
     C                   Exsr      FstCyc                                                           
     C                   Goto      T#EndProgram                                                     
     C*                                                                                             
     C* Initialise a particular tab, then end program                                               
     C                   When      Z#TabFunc = C#IniTab                                             
     C*                                                                                             
     C                   Select                                                                     
     C*    Item detail - initialise screen 2                                                        
     C                   When      Z#TabCode = C#ItemDetl                                           
     C                   Move      Z#ItemNo      WwItem                                             
     C                   Clear     *All          CmgCw                                              
     C     CwKey3        Chain(n)  CmgCw00                                                          
     C                   Exsr      GetSc2                                                           
     C*    Movement list - initialise screen 5                                                      
     C                   When      Z#TabCode = C#MoveList                                           
     C                   Exsr      SetSf5                                                           
     C*    Unknown tab - don't know what to initialise                                              
     C                   Other                                                                      
     C                   Eval      Z#TabFunc = C#EndAll                                             
     C                   Eval      Z#TabCode = *Blanks                                              
     C                   Eval      Z#FromTabCode = *Blanks                                          
     C                   Endsl                                                                      
     C*                                                                                             
     C                   Goto      T#EndProgram                                                     
     C*                                                                                             
     C* Process a particular tab                                                                    
     C                   When      Z#TabFunc = C#PrcTab                                             
     C*                                                                                             
     C                   Select                                                                     
     C*    Item list - proceed to screen 1                                                          
     C                   When      Z#TabCode = C#ItemList                                           
     C                   Exsr      SetSf1                                                           
     C*    Item detail - proceed to screen 2                                                        
     C                   When      Z#TabCode = C#ItemDetl                                           
     C                   Eval      Screen = 2                                                       
     C*    Movement list - proceed to screen 5                                                      
     C                   When      Z#TabCode = C#MoveList                                           
     C***                Exsr      SetSf5                                                           
     C******             Exsr      SetSf5                                                           
     C                   Eval      Screen = 5                                                       
     C*    Unknown tab - don't know how to proceed                                                  
     C                   Other                                                                      
     C                   Eval      Z#TabFunc = C#EndAll                                             
     C                   Eval      Z#TabCode = *Blanks                                              
     C                   Eval      Z#FromTabCode = *Blanks                                          
     C                   Goto      T#EndProgram                                                     
     C                   Endsl                                                                      
     C*                                                                                             
     C* Close current item, then end program                                                        
     C                   When      Z#TabFunc = C#ClsItem                                            
     C*                                                                                             
     C                   Goto      T#EndProgram                                                     
     C*                                                                                             
     C* Remaining contract items update                                                             
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
     C                   Eval      ZzItemNoOpen = *Zero                                             
     C                   Eval      ZzMoveNoOpen = *Zero                                             
     C                   If        @TabProc                                                         
     C                   If        Z#ItemNo <> *Blanks                                              
     C                   Move      Z#ItemNo      ZzItemNoOpen                                       
     C                   Endif                                                                      
     C                   If        Z#MoveNo <> *Blanks                                              
     C                   Move      Z#MoveNo      ZzMoveNoOpen                                       
     C                   Endif                                                                      
     C                   Endif                                                                      
     C*                                                                                             
     C                   Eval      Z#ItemNo = *Blanks                                               
     C                   Eval      Z#MoveNo = *Blanks                                               
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
     C***  *INLR         DOUEQ     '1'                                                              
     C     *IN78         DOwEQ     '0'                                                              
     C     SCREEN        CASEQ     1             PROSC1                                             
     C     SCREEN        CASEQ     2             PROSC2                                             
     C     SCREEN        CASEQ     3             PROSC3                                             
     C     SCREEN        CASEQ     5             PROSC5                                             
     C*                                                                                             
     C                   END                                                                        
     C                   END                                                                        
     C*                                                                                             
     C* Remaining contract items update as necessary                                                
     C                   If        Z#TabFunc = *Blanks                                              
     C                             Or Z#TabFunc = C#UpdRem                                          
     C*                                                                                             
     C                   MOVE      'CE'          Z#ACTN                                             
     C                   EXSR      CAUDIT                                                           
     C*                                                                                             
                                                                                                    
      * Check if any new movemets havebeen created. If so then create                               
      * confirmations                                                                               
      *                                                                                             
     C     *LOVAL        SETLL     CMGCF00                                                          
     C     *ON           DOUEQ     *OFF                                                             
     C                   READ      CMGCF00                                                          
     C                   IF        %EOF(CMGCF00)                                                    
     C                   LEAVE                                                                      
     C                   ENDIF                                                                      
     C                   MOVE      CFCNTN        Z#A12C                                             
     C                   CALL      'CMGA12'                                                         
     C                   PARM                    Z#A12C            7                                
     C                   PARM      *BLANKS       Z#A12I            3                                
     C                   PARM      *BLANKS       Z#A12D            7                                
     C                   PARM      *BLANKS       Z#A12F            1                                
     C                   PARM      'Y'           Z#A12A            1                                
     C                   LEAVE                                                                      
     C                                                                                              
     C                   ENDDO                                                                      
     C                                                                                              
     C                   Endif                                                                      
     C                                                                                              
     C     T#EndProgram  Tag                                                                        
     C                                                                                              
 ISL C                   IN        LDA                                                              
 ISL C                   MOVE      Save_Mod      LDMODL                                             
 ISL C                   OUT       LDA                                                              
                                                                                                    
     C     Z#Lr          Ifne      '2'                                                              
     C                   Seton                                        LR                            
     C     @@EO5F        Ifeq      '5'                                                              
     C                   Exsr      Sbr014                                                           
     C                   Endif                                                                      
     C                   Endif                                                                      
     c                   Return                                                                     
      *****************************************************************                             
     C     SETSF1        BEGSR                                                                      
      *****************************************************************                             
     C                   SETOFF                                       808182                        
     C                   SETOFF                                       838889                        
     C                   WRITE     CMGE051C                                                         
      *                                                                                             
      * SETUP FIRST PAGE OF SUBFILE                                                                 
      *                                                                                             
     C                   Z-ADD     0             SFRCD1            4 0                              
     C                   Z-ADD     0             BTITEM                                             
     C  N86WHCNTN        SETLL     CMGCW00                                                          
     C   86CWKEY4        SETLL     CMGCW01                                                          
     C                   EXSR      GETSF1                                                           
      *                                                                                             
     C     X@ACPT        COMP      'X'                                    50                        
     C     *IN50         IFEQ      *ON                                                              
     C     SFRCD1        ANDEQ     0                                                                
     C                   SETON                                        85                            
     C                   EXSR      GETSF1                                                           
     C                   ENDIF                                                                      
      *                                                                                             
      * CHECK THERE IS AT LEAST ONE RECORD IN THE SUBFILE                                           
      *                                                                                             
     C     SFRCD1        IFEQ      0                                                                
     C                   SETOFF                                       80                            
     C                   ELSE                                                                       
     C                   SETON                                        80                            
     C                   ENDIF                                                                      
     C                   SETON                                        81                            
      *                                                                                             
     C                   ENDSR                                                                      
      **************************************************************************                    
     C     GETSF1        BEGSR                                                                      
      **************************************************************************                    
     C                   SETOFF                                       8283                          
     C                   Z-ADD     1             SCREEN            3 0                              
     C                   MOVE      *BLANKS       X@ACPT            1                                
      *                                                                                             
     C  N86CWKEY6        SETGT     CMGCW00                                                          
     C   86CWKEY8        SETGT     CMGCW01                                                          
      *                                                                                             
     C     *IN88         DOUEQ     *ON                                                              
      *                                                                                             
     C  N86WHCNTN        READE     CMGCW00                                88                        
     C   86CWKEY4        READE     CMGCW01                                88                        
      *                                                                                             
     C     *IN88         IFEQ      '0'                                          *IF 2               
                                                                                                    
                                                                                                    
      * Use the filter list? If so and this item is NOT on the                                      
      * list then skip this record                                                                  
     C                   If        ( gUseFilterList and                                             
     C                               not onFilterList( cwcntn : cwitem ) )                          
     C                   Iter                                                                       
     C                   Endif                                                                      
                                                                                                    
      *                                                                                             
      * SHOW COMPLETED RECORDS ???                                                                  
      *                                                                                             
     C     *IN48         IFEQ      *OFF                                                             
     C     CWSUSP        ANDEQ     'C'                                                              
     C     *IN33         ANDEQ     *OFF                                                             
     C                   ITER                                                                       
     C                   ENDIF                                                                      
      *                                                                                             
      * SHOW ACCEPTED RECORDS ???                                                                   
      *                                                                                             
     C                   IF        CWLINA = ' '  and                                                
     C                             CWDELF <> 'D' and                                                
     C                             CWSUSP <> 'C'                                                    
     C                   MOVE      'X'           X@ACPT                                             
     C     *IN85         IFEQ      *OFF                                                             
     C     *IN33         ANDEQ     *OFF                                                             
     C                   ITER                                                                       
     C                   ENDIF                                                                      
     C                   ENDIF                                                                      
      *                                                                                             
     C                   ADD       1             SFRCD1                                             
     C                   MOVEL     ' '           S1SLCT                                             
     C                   MOVEL     CWITEM        S1ITEM                                             
     C                   MOVE      CWSUSP        S1SUSP                                             
      *                                                                                             
     C                   MOVE(P)   WHCNTN        Z1CONT            7                                
     C                   MOVE(P)   S1ITEM        Z1ITEM            3                                
     C                   MOVE      *BLANKS       Z1SKIP           11                                
     C                   CALL      'CMGA44'                                                         
     C                   PARM                    Z1CONT                                             
     C                   PARM                    Z1ITEM                                             
     C                   PARM                    Z1SKIP                                             
     C                   MOVE      Z1SKIP        S1SKIP                                             
      *                                                                                             
      * COLOUR THE SUBFILE BASED ON THE NUMBER OF SKIPS AND IF THE ITEM                             
      * EXCEEDS THE MAXIUM ITEM VALUE                                                               
      *                                                                                             
     C                   EXSR      COLOUR                                                           
     C                   MOVEL     CWITEM        BTITEM                                             
     C                   MOVEL     CWPROD        S1PROD                                             
     C*                                                                                             
     C                   MOVE      *BLANKS       S1DESC                                             
     C     CWPROD        CHAIN     IIMIM00                            21                            
     C     *IN21         IFEQ      *OFF                                         *IF 2               
     C**N86              MOVEL     IMSDSC        S1DESC                                             
     C***86              MOVEL     IMDESC        S1DESC                                             
     C                   MOVEL     IMDESC        S1DESC                                             
     C                   END                                                                        
     C*                                                                                             
     C                   MOVE      CWSUPL        S1SUPL                                             
     C                   MOVE      *BLANKS       S1SDSC                                             
     C     *IN86         IFEQ      *OFF                                         *IF 2               
     C                   CALL      'CMGA52'                                                         
     C                   PARM      CWCNTN        ZACNTN            7 0                              
     C                   PARM      CWITEM        ZAITEM            3 0                              
     C                   PARM      TODAY         ZADATE            7 0                              
     C                   PARM      *ZEROS        ZALINE            3 0                              
     C                   PARM      *ZEROS        ZAACTF            7 0                              
     C                   PARM      *BLANKS       ZASUPL            6                                
     C                   PARM      *BLANKS       ZACONT           20                                
     C                   PARM      *BLANKS       ZATELP           15                                
     C                   PARM      *BLANKS       ZAFAX            15                                
     C                   PARM      *BLANKS       ZACEOF            1                                
     C                   PARM      *ZEROS        ZACBAS           11 2                              
     C                   PARM      *ZEROS        ZACWIN           11 2                              
     C                   PARM      *ZEROS        ZACRPT           11 2                              
     C                   PARM      *ZEROS        ZACLWI           11 2                              
     C                   PARM      *ZEROS        ZACLRT           11 2                              
     C                   PARM      *ZEROS        ZACRVL           11 2                              
     C                   PARM      *BLANKS       ZACRT1           10                                
     C                   PARM      *BLANKS       ZACRT2           10                                
     C                   PARM      *BLANKS       ZASEOF            1                                
     C                   PARM      *ZEROS        ZASBAS           11 2                              
     C                   PARM      *ZEROS        ZASWIN           11 2                              
     C                   PARM      *ZEROS        ZASRPT           11 2                              
     C                   PARM      *ZEROS        ZASLWI           11 2                              
     C                   PARM      *ZEROS        ZASLRT           11 2                              
     C                   PARM      *ZEROS        ZASRVL           11 2                              
     C                   PARM      *BLANKS       ZASRT1           10                                
     C                   PARM      *BLANKS       ZASRT2           10                                
     C                   MOVE      ZASUPL        S1SUPL                                             
 ISL  *                                                                                             
 ISL C     @@EO5F        IFEQ      '5'                                                              
 ISL C                   MOVE      *BLANK        @@5U01          001                                
 ISL C                   EXSR      SBR001                                                           
 ISL C                   MOVE      *OFF          *IN21                                              
 ISL C     @@5P04        IFEQ      '1'                                                              
 ISL C     @@5P04        OREQ      '9'                                                              
 ISL C     X@IND1        IFEQ      '1'                                                              
 ISL C                   MOVE      *ON           *IN21                                              
 ISL C                   ENDIF                                                                      
 ISL C                   ENDIF                                                                      
 ISL C                   ELSE                                                                       
     c                   EXsr      Set_BPL                                                          
 ISL C                   EXSR      SBR003                                                           
 ISL C                   MOVE      *OFF          *IN21                                              
 ISL C     @@EP04        IFNE      *BLANKS                                                          
 ISL C                   MOVE      *ON           *IN21                                              
 ISL C                   ENDIF                                                                      
 ISL C                   ENDIF                                                                      
 ISL  *                                                                                             
     C     *IN21         IFEQ      *OFF                                         *IF 2               
     C***                MOVEL     SDNAME        WKDES1           26                                
     C***                MOVE      WKDES1        S1DESC                                             
     C                   MOVEL     SDNAME        S1SDSC                                             
     C                   END                                                                        
     C                   END                                                                        
     C*                                                                                             
     C                   MOVE      *BLANKS       S1STAT                                             
     C     CWLINA        IFEQ      '1'                                                              
     C                   MOVEL     'Sales   '    S1STAT                                             
     C                   END                                                                        
     C     CWLINA        IFEQ      '2'                                                              
     C                   MOVEL     'Purchase'    S1STAT                                             
     C                   END                                                                        
     C     CWDELF        IFEQ      'D'                                                              
     C                   MOVEL     '*DEL*   '    S1STAT                                             
     C                   END                                                                        
     C     CWSUSP        IFEQ      'C'                                                              
     C                   MOVEL     '*COMP   '    S1STAT                                             
     C                   END                                                                        
     C*                                                                                             
     C***  X             IFEQ      1                                            *IF 3               
     C***                Z-ADD     SFRCD1        RCDNBR                                             
     C***                ENDIF                                                  *ENDIF 3            
     C                   Z-ADD     1             RCDNBR                                             
      *                                                                                             
     C     *Nokey        Clear     *All          CMGCWR                                             
     C     CWKEY         Chain(N)  CMGCWR00                                                         
     C                   MOVEL     CWTYPE        S1TYPE                                             
      *                                                                                             
     C                   WRITE     CMGE051S                                                         
     C                   ADD       1             X                                                  
     C                   ELSE                                                   *ELSE 2             
     C                   Z-ADD     15            X                                                  
     C                   ENDIF                                                  *ENDIF 2            
      *                                                                                             
     C                   ENDDO                                                  *ENDDO 1            
      *                                                                                             
      *                                                                                             
     C                   ENDSR                                                                      
     C*****************************************************************                             
     C     PROSC1        BEGSR                                                                      
     C*****************************************************************                             
     C     TOPSC1        TAG                                                                        
     C*                                                                                             
     C                   Eval      ZzTcod = *Blanks                                                 
     C*                                                                                             
     C                   WRITE     CMGE0501                                                         
     C                   EXFMT     CMGE051C                                                         
     C                   Read      CMGE0501                                                         
     C*                                                                                             
     C                   SETOFF                                       90                            
     C                   MOVEL     'XAPMSG'      MSGF             10                                
     C                   MOVE      '9999'        MIC               4                                
     C*                                                                                             
     C                   Z-ADD     SFLRRN        RCDNBR                                             
     C*                                                                                             
     C* F3=EXIT                                                                                     
     C*                                                                                             
     C     *INKC         IFEQ      '1'                                                              
     C                   Exsr      Sc1Exit                                                          
     C                   GOTO      ENDSC1                                                           
     C                   END                                                                        
     C*                                                                                             
     C* F12=PREVIOUS                                                                                
     C*                                                                                             
     C     *INKL         IFEQ      '1'                                                              
     C  N86              If        @TabProc                                                         
     C                   Eval      Z#TabFunc = C#PrcTab                                             
     C                   Eval      Z#TabCode = C#Head2                                              
     C                   Eval      Z#FromTabCode = C#ItemList                                       
     C                   Endif                                                                      
     C**N86              SETON                                        LR                            
     C  N86              SETON                                        78                            
 ISL C**N86@@EO5F        IFEQ      '5'                                                              
 ISL C***                EXSR      SBR014                                                           
 ISL C***                ENDIF                                                                      
     C   86              EXSR      SETSF3                                                           
     C                   SETOFF                                       86                            
     C                   GOTO      ENDSC1                                                           
     C                   END                                                                        
     C*                                                                                             
     C*                                                                                             
     C* F16=SUPPLIERS / ALL ITEMS                                                                   
     C* NB NOW F16=SUPPLIERS/ALL SUPPS                                                              
     C*                                                                                             
     C     *INKQ         IFEQ      *ON                                                              
     C     *IN86         IFEQ      *OFF                                                             
     C                   EXSR      SETSF3                                                           
     C                   ELSE                                                                       
     C                   SETOFF                                       86                            
     C                   EXSR      SETSF1                                                           
     C                   END                                                                        
     C                   GOTO      ENDSC1                                                           
     C                   END                                                                        
     C*                                                                                             
     C* F17=SHOW COMPLETED/HIDE COMPLETED                                                           
     C*                                                                                             
     C     *INKR         IFEQ      *ON                                                              
     C     *IN48         IFEQ      *OFF                                                             
     C                   SETON                                        48                            
     C                   ELSE                                                                       
     C                   SETOFF                                       48                            
     C                   ENDIF                                                                      
     C                   EXSR      SETSF1                                                           
     C                   GOTO      ENDSC1                                                           
     C                   ENDIF                                                                      
                                                                                                    
                                                                                                    
      * F19=Order details                                                                           
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
                                                                                                    
                                                                                                    
      * F23=Clear filter                                                                            
      * NB Now F23=Show All                                                                         
     C                   If        ( *INKX )                                                        
     C                   Eval      gUseFilterList = false                                           
     C                   Eval      *IN33 = false                                                    
     C                   Eval      *IN86 = false                                                    
     C                   Eval      *IN48 = true                                                     
     C                   Eval      *IN85 = true                                                     
     C                   EXSR      SETSF1                                                           
     C                   GOTO      ENDSC1                                                           
     C                   Endif                                                                      
                                                                                                    
                                                                                                    
     C*                                                                                             
     C* F20=SHOW ACCEPTED/HIDE ACCEPTED                                                             
     C*                                                                                             
     C     *INKU         IFEQ      *ON                                                              
     C     *IN85         IFEQ      *OFF                                                             
     C                   SETON                                        85                            
     C                   ELSE                                                                       
     C                   SETOFF                                       85                            
     C                   ENDIF                                                                      
     C                   EXSR      SETSF1                                                           
     C                   GOTO      ENDSC1                                                           
     C                   ENDIF                                                                      
     C*                                                                                             
     C* F21=SALES INVOICES                                                                          
     C*                                                                                             
     C     *INKV         IFEQ      '1'                                                              
     C                   MOVE      WHCNTN        Z0CNTN                                             
     C                   MOVE      ' '           Z0SIYN                                             
     C                   CALL      'CMGE52'                                                         
     C                   PARM                    Z0CNTN            7                                
     C                   PARM                    Z0SIYN            1                                
     C*                                                                                             
     C     Z0SIYN        IFEQ      'N'                                                              
     C                   MOVE      '0079'        MIC                                           IIMM4
     C                   MOVEL     'CHSMSG'      MSGF                                               
     C                   SETON                                        90                            
     C                   END                                                                        
     C*                                                                                             
     C                   GOTO      TOPSC1                                                           
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
     C                             And ZzTcod = C#ItemList                                          
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
      *                                                                                             
      * Tab processing for valid tab code entered                                                   
     C                   If        ZzTcod <> *Blanks                                                
     C                   Eval      Z#TabFunc = C#PrcTab                                             
     C                   Eval      Z#TabCode = ZzTcod                                               
     C                   Eval      Z#FromTabCode = C#ItemList                                       
     C                   Eval      *In78 = *On                                                      
     C                   Goto      EndSc1                                                           
     C                   Endif                                                                      
      *                                                                                             
      * ARE THERE RECORDS IN THE SUBFILE ? *                                                        
      *                                                                                             
     C  N80                                                                                         
     CAN 81              GOTO      TOPSC1                                                           
     C*                                                                                             
     C* ROLL UP THE SUBFILE BEFORE DISPLAYING ANY ERRORS *                                          
     C     *IN89         IFEQ      '1'                                                              
     C     SFLRRN        ADD       10            SFRCD1                                             
     C                   EXSR      GETSF1                                                           
     C                   SETOFF                                       89                            
     C                   GOTO      TOPSC1                                                           
     C                   END                                                                        
     C*                                                                                             
     C* READ CHANGED RECORDS *                                                                      
     C                   SETOFF                                       212223                        
     C                   SETOFF                                       2425                          
     C                   Z-ADD     0             WKRECD                                             
     C                   Z-ADD     0             ERRECD                                             
     C                   Z-ADD     0             NBRSLT                                             
     C     *IN21         DOUEQ     '1'                                          *DO 1               
     C                   READC     CMGE051S                               21                        
     C     *IN21         IFEQ      '0'                                          *IF 2               
     C                   SETOFF                                       8283                          
     C     S1SLCT        IFEQ      '1'                                          *IF 3               
     C                   ADD       1             NBRSLT                                             
     C  N23              Z-ADD     SFRCD1        WKRECD                                             
     C                   SETON                                        82                            
     C                   SETON                                        23                            
     C                   END                                                    *END 3              
     C     S1SLCT        IFNE      ' '                                          *IF 3               
     C     S1SLCT        ANDNE     '1'                                                              
     C                   ADD       1             NBRSLT                                             
     C  N22              Z-ADD     SFRCD1        ERRECD                                             
     C                   SETON                                        8283                          
     C                   SETON                                        22                            
     C                   END                                                    *END 3              
      *                                                                                             
     C     CWKEY1        CHAIN(N)  CMGCW00                            25                            
     C                   EXSR      COLOUR                                                           
     C*                                                                                             
     C                   UPDATE    CMGE051S                                                         
     C                   END                                                    *END 2              
     C                   END                                                    *END 1              
     C*                                                                                             
     C*                                                                                             
     C* INVALID SELECTION NOT A 1 OR BLANK *                                                        
     C*                                                                                             
     C     *IN22         IFEQ      '1'                                                              
     C                   SETON                                        90                            
     C                   MOVE      '7025'        MIC                                                
     C                   GOTO      TOPSC1                                                           
     C                   END                                                                        
     C*                                                                                             
     C* MORE THAN ONE SELECTED *                                                                    
     C*                                                                                             
     C     NBRSLT        IFGT      1                                                                
     C                   SETON                                        90                            
     C                   MOVE      '0080'        MIC                                                
     C                   GOTO      TOPSC1                                                           
     C                   END                                                                        
     C*                                                                                             
     C* AT LEAST ONE SELECTION MUST BE MADE *                                                       
     C     NBRSLT        IFEQ      0                                                                
     C                   SETON                                        90                            
     C                   MOVE      '0082'        MIC                                                
     C                   GOTO      TOPSC1                                                           
     C                   END                                                                        
     C*                                                                                             
     C* GOT A VALID SELECTION - SO CHAIN TO SUBFILE *                                               
     C     WKRECD        CHAIN     CMGE051S                           21                            
     C*                                                                                             
     C* 1=SELECT                                                                                    
     C*                                                                                             
     C     S1SLCT        IFEQ      '1'                                          *IF 1               
     C     CWKEY1        CHAIN     CMGCW00                            21                            
     C                   Eval      CWETAI = *Zeros                                                  
     C**   CWKEY1        Chain(n)  CMGCWS00                                                         
     C     CWKEY1        Chain(n)  CMGCWZ00                                                         
     C***                Z-ADD     S1ITEM        WWITEM                                             
     C                   Z-ADD     S1ITEM        ZWITEM                                             
     C*                                                                                             
     C* Tab processing for select (enquire on) existing item                                        
     C                   If        @TabProc                                                         
     C                   Eval      Z#TabFunc = C#EnqItem                                            
     C                   Eval      Z#TabCode = *Blanks                                              
     C                   Eval      Z#FromTabCode = C#ItemList                                       
     C                   Move      ZwItem        Z#ItemNo                                           
     C                   Eval      *In78 = *On                                                      
     C*                                                                                             
     C* Normal processing for select (enquire on) existing item                                     
     C                   Else                                                                       
     C                   Z-add     ZwItem        WwItem                                             
     C                   EXSR      GETSC2                                                           
     C                   Endif                                                                      
     C*                                                                                             
     C                   GOTO      ENDSC1                                                           
     C                   END                                                    *END 1              
     C*                                                                                             
     C*                                                                                             
     C                   GOTO      TOPSC1                                                           
     C*                                                                                             
     C     ENDSC1        ENDSR                                                                      
     C*****************************************************************                             
     C     Sc1Exit       Begsr                                                                      
                                                                                                    
     C* Exit tab processing                                                                         
     C                   If        @TabProc                                                         
     C                   Eval      Z#TabFunc = C#ExitProg                                           
     C                   Eval      Z#TabCode = *Blanks                                              
     C                   Eval      Z#FromTabCode = C#ItemList                                       
     C                   Endif                                                                      
                                                                                                    
     C                   MOVE      'X'           Z#EXIT                                             
     C                   SETON                                        78                            
                                                                                                    
     C                   endsr                                                                      
     C*****************************************************************                             
     C     GETSC2        BEGSR                                                                      
     C*****************************************************************                             
     C*                                                                                             
     C                   Z-ADD     2             SCREEN                                             
                                                                                                    
                                                                                                    
      * Has the contract item been accepted?                                                        
     C                   MOVE      *OFF          gItemAccepted                                      
     C     CWLINA        IFNE      *BLANKS                                                          
     C                   MOVE      *ON           gItemAccepted                                      
     C                   ENDIF                                                                      
                                                                                                    
                                                                                                    
     C                   MOVEL     CWWTYP        WWWTYP                                             
     C                   MOVE      *BLANKS       WWWDES                                             
     C                   MOVE      *BLANKS       WKPROD           25                                
     C                   MOVEL     CWWTYP        WKPROD                                             
     C     WKPROD        CHAIN     IIMIM00                            21                            
     C  N21              MOVEL     IMDESC        WWWDES                                             
     C*                                                                                             
     C                   MOVEL     CWPROD        WWPROD                                             
     C                   MOVE      *BLANKS       WWPDSC                                             
     C                   MOVE      *BLANKS       WKPROD                                             
     C                   MOVEL     CWPROD        WKPROD                                             
     C     WKPROD        CHAIN     IIMIM00                            21                            
     C  N21              MOVEL     IMDESC        WWPDSC                                             
     C*                                                                                             
     C                   MOVEL     CWCEOF        WWCEOF                                             
     C                   MOVEL     CWSEOF        WWSEOF                                             
     C*                                                                                             
     C                   MOVEL     CWSUPL        WWSUPL                                             
     C                   MOVEL     CWCONT        WWSCON                                             
     C                   MOVEL     CWTELP        WWSTEL                                             
     C                   MOVEL     CWFAX         WWSFAX                                             
     C*                                                                                             
     C                   Z-ADD     CWCBAS        WWCBAS                                             
     C                   Z-ADD     CWCWIN        WWCWIN                                             
     C                   Z-ADD     CWCRPT        WWCRPT                                             
     C                   Z-ADD     CWPRVL        WWPRVL                                             
     C                   MOVEL     CWPRT1        WWPRT1                                             
     C                   MOVEL     CWPRT2        WWPRT2                                             
     C*                                                                                             
     C                   Z-ADD     CWSBAS        WWSBAS                                             
     C                   Z-ADD     CWSWIN        WWSWIN                                             
     C                   Z-ADD     CWSRPT        WWSRPT                                             
     C                   Z-ADD     CWSRVL        WWSRVL                                             
     C                   MOVEL     CWSRT1        WWSRT1                                             
     C                   MOVEL     CWSRT2        WWSRT2                                             
     C*                                                                                             
     C                   Z-ADD     CWESTW        WWESTW                                             
     C                   Z-ADD     CWAPPL        WWAPPL                                             
     C                   MOVEL     CWSPI1        WWSPI1                                             
     C                   MOVEL     CWSPI2        WWSPI2                                             
     C*                                                                                             
     C                   Z-ADD     0             WWAWNB            9 0                              
     C                   Z-ADD     0             WWALNB            5 0                              
     C                   Z-ADD     0             WWAWCS            9 0                              
     C                   Z-ADD     0             WWALCS            5 0                              
     C*                                                                                             
     C     ANKEY         CHAIN     CMGAN00                            21                            
     C     *IN21         IFEQ      *OFF                                                             
     C     ANLTOT        IFNE      0                                                                
     C     ANWTOT        DIV(H)    ANLTOT        WWAWNB                                             
     C                   END                                                                        
     C                   Z-ADD     ANLTOT        WWALNB                                             
     C                   END                                                                        
     C*                                                                                             
     C     ACKEY         CHAIN     CMGAC00                            21                            
     C     *IN21         IFEQ      *OFF                                                             
     C     ACLTOT        IFNE      0                                                                
     C     ACWTOT        DIV(H)    ACLTOT        WWAWCS                                             
     C                   END                                                                        
     C                   Z-ADD     ACLTOT        WWALCS                                             
     C                   END                                                                        
     C*                                                                                             
     C* MARGINS                                                                                     
     C*                                                                                             
     C                   EXSR      CLCMAR                                                           
      *                                                                                             
      * ... initial movement details                                                                
     C                   MOVEL     CWIMCD        WWIMCD                                             
     C                   CALL      'XXFDAT'                                                         
     C                   PARM                    CWIMDT                                             
     C                   PARM                    WWIMDT                                             
     C                   Z-ADD     CWIMQT        WWIMQT                                             
     C                   Eval      WWETAI = CWETAI                                                  
      *                                                                                             
      * IF THE STATUS OF ACCEPTANCE IS '2', THEN RETRIEVE THE PRICE                                 
      * INFORMATION FROM CMGA52 ELSE, USE THE INFO FROM CMGCW                                       
      *                                                                                             
     C                   MOVE      *OFF          *IN38                                              
      *                                                                                             
     C     CWLINA        IFEQ      '2'                                                              
      *                                                                                             
     C                   EXSR      RTVPRC                                                           
     C                   MOVE      *ON           *IN38                                              
      *                                                                                             
     C                   ELSE                                                                       
      *                                                                                             
     C                   MOVEL     CWCEOF        WWCEOF                                             
     C                   MOVEL     CWSEOF        WWSEOF                                             
      *                                                                                             
     C                   Z-ADD     CWCBAS        WWCBAS                                             
     C                   Z-ADD     CWCWIN        WWCWIN                                             
     C                   Z-ADD     CWCRPT        WWCRPT                                             
     C                   Z-ADD     CWPRVL        WWPRVL                                             
     C                   MOVEL     CWPRT1        WWPRT1                                             
     C                   MOVEL     CWPRT2        WWPRT2                                             
      *                                                                                             
     C                   Z-ADD     CWSBAS        WWSBAS                                             
     C                   Z-ADD     CWSWIN        WWSWIN                                             
     C                   Z-ADD     CWSRPT        WWSRPT                                             
     C                   Z-ADD     CWSRVL        WWSRVL                                             
     C                   MOVEL     CWSRT1        WWSRT1                                             
     C                   MOVEL     CWSRT2        WWSRT2                                             
      *                                                                                             
     C                   MOVEL     CWSUPL        WWSUPL                                             
     C                   MOVEL     CWCONT        WWSCON                                             
     C                   MOVEL     CWTELP        WWSTEL                                             
     C                   MOVEL     CWFAX         WWSFAX                                             
      *                                                                                             
     C                   ENDIF                                                                      
      *                                                                                             
      * GET ITEM NUMBERS REQUIRED AT ITEM LEVEL                                                     
      *                                                                                             
     C                   MOVE      *BLANK        WWONRQ                                             
     C     CWKEY3        CHAIN     CMGCWW00                           21                            
     C     *IN21         IFEQ      *OFF                                                             
     C                   MOVE      CWONRQ        WWONRQ                                             
     C                   ENDIF                                                                      
      *                                                                                             
      * Get Disposal Site Code                                                                      
     C     CWKEY3        CHAIN     CMGCWV00                           21                            
     C     *IN21         IFEQ      *OFF                                                             
     C                   MOVE      CWDSCD        WWCODE                                             
     C                   ELSE                                                                       
     C                   MOVE      *BLANKS       WWCODE                                             
     C                   ENDIF                                                                      
     C     WWCODE        CHAIN     CMGDS00                            21                            
     C     *IN21         IFEQ      *OFF                                                             
     C                   MOVEL     DSNAME        WWDESC                                             
     C                   ELSE                                                                       
     C                   MOVE      *BLANKS       WWDESC                                             
     C                   ENDIF                                                                      
      *                                                                                             
      * GET SUPPLIER DESCRIPTION ...                                                                
      *                                                                                             
     C                   MOVE      *BLANKS       WWSDSC                                             
 ISL  *                                                                                             
 ISL C     @@EO5F        IFEQ      '5'                                                              
 ISL C                   MOVE      *BLANK        @@5U01          001                                
 ISL C                   EXSR      SBR004                                                           
 ISL C                   MOVE      *OFF          *IN21                                              
 ISL C     @@5P04        IFEQ      '1'                                                              
 ISL C     @@5P04        OREQ      '9'                                                              
 ISL C     X@IND1        IFEQ      '1'                                                              
 ISL C                   MOVE      *ON           *IN21                                              
 ISL C                   ENDIF                                                                      
 ISL C                   ENDIF                                                                      
 ISL C                   ELSE                                                                       
     c                   Exsr      Set_BPL                                                          
 ISL C                   EXSR      SBR005                                                           
 ISL C                   MOVE      *OFF          *IN21                                              
 ISL C     @@EP04        IFNE      *BLANKS                                                          
 ISL C                   MOVE      *ON           *IN21                                              
 ISL C                   ENDIF                                                                      
 ISL C                   ENDIF                                                                      
 ISL  *                                                                                             
     C  N21              MOVEL     SDNAME        WWSDSC                                             
      *                                                                                             
      * Get suppliers Carrier Reg No & Expiry date                                                  
     C     WWSUPL        CHAIN     BPLSDS00                           21                            
     C     *IN21         IFEQ      *OFF                                                             
     C                   MOVEL     SDCREG        WWCREG                                             
     C                   CALL      'XXFDAT'                                                         
     C                   PARM                    SDCRXT                                             
     C                   PARM                    WWCRXT                                             
     C                   ELSE                                                                       
     C                   MOVE      *BLANKS       WWCREG                                             
     C                   Z-ADD     *ZEROS        WWCRXT                                             
     C                   ENDIF                                                                      
      *                                                                                             
      * MARGINS                                                                                     
      *                                                                                             
     C                   EXSR      CLCMAR                                                           
      *                                                                                             
      * RETRIVE TEMPLATE, ORDER NO & MAX ITEM VALUE (IF APPROPRIATE).                               
      *                                                                                             
     C     *IN43         IFEQ      *ON                                                              
     C     *IN46         OREQ      *ON                                                              
 ISL  *                                                                                             
 ISL C                   MOVE      *BLANK        @@5U01          001                                
 ISL C                   EXSR      SBR006                                                           
 ISL C                   MOVE      *OFF          *IN21                                              
 ISL C     @@5P04        IFEQ      '1'                                                              
 ISL C     @@5P04        OREQ      '9'                                                              
 ISL C     X@IND1        IFEQ      '1'                                                              
 ISL C                   MOVE      *ON           *IN21                                              
 ISL C                   ENDIF                                                                      
 ISL C                   ENDIF                                                                      
 ISL  *                                                                                             
     C                   ENDIF                                                                      
BS01  *                                                                                             
BS01  * 0206CR1 - RETRIEVE EWC NUMBER                                                               
BS01  *                                                                                             
BS01 C                   MOVE      *BLANKS       WWEWCN            6 0                              
BS01 C     CWKEY3        CHAIN     CMGCWY00                           21                            
BS01 C  N21              MOVE      CWEWCN        WWEWCN                                             
                                                                                                    
     C***  *Nokey        Clear                   CMGCWR                                             
     C     *Nokey        Clear     *All          CMGCWR                                             
     C     CWKEY3        Chain(N)  CMGCWR00                                                         
     C                   Eval      S2TYPE = CWTYPE                                                  
     C                   Eval      S2TDES = getContractTypeDesc(S2TYPE)                             
                                                                                                    
     C                   MOVEL     CWSPI3        WWSPI3                                             
     C                   MOVEL     CWSPI4        WWSPI4                                             
     C                   MOVEL     CWSPI5        WWSPI5                                             
     C                   MOVEL     CWSPI6        WWSPI6                                             
                                                                                                    
      * Initialise item definition fields                                                           
     C                   Exsr      IniItmDefFlds                                                    
                                                                                                    
                                                                                                    
     C                   ENDSR                                                                      
                                                                                                    
      **************************************************************************                    
     C     IniItmDefFlds Begsr                                                                      
                                                                                                    
                                                                                                    
      * Initialise the user maintained field controls                                               
     C                   Eval      MaintainDSCD = TRUE                                              
     C                   Eval      MaintainESTW = TRUE                                              
     C                   Eval      MaintainAPPL = TRUE                                              
     C                   Eval      MaintainEWCN = TRUE                                              
                                                                                                    
                                                                                                    
      * Initialise the user maintained labels                                                       
     C                   Eval      S2LBL1 = cDftLbl1                                                
     C                   Eval      S2LBL2 = cDftLbl2                                                
     C                   Eval      S2LBL3 = cDftLbl3                                                
     C                   Eval      S2LBL4 = cDftLbl4                                                
     C                   Eval      S2LBL5 = cDftLbl5                                                
                                                                                                    
                                                                                                    
      * Retrieve the item definition details for this product,                                      
      * via the unit of stock code                                                                  
     C                   Clear     *All          IIMIM                                              
     C                   Clear     *All          CMGUN                                              
     C                   Clear     *All          CMGID                                              
     C                   Eval      IMPROD = WWPROD                                                  
     C     IMPROD        Chain(N)  IIMIM00                                                          
     C                   Eval      UNCODE = IMUNOS                                                  
     C     UNCODE        Chain(N)  CMGUN00                                                          
     C                   Eval      IDCODE = UNIDCD                                                  
     C     IDCODE        Chain(N)  CMGID00                                                          
     C                   If        %Found(IIMIM00)                                                  
     C                             and %Found(CMGUN00)                                              
     C                             and %Found(CMGID00)                                              
                                                                                                    
      * Maintain disposal code                                                                      
     C                   If        IDMDIS <> 'Y'                                                    
     C                   Eval      MaintainDSCD = FALSE                                             
     C                   Endif                                                                      
      * Maintain estimated weight                                                                   
     C                   If        IDMESW <> 'Y'                                                    
     C                   Eval      MaintainESTW = FALSE                                             
     C                   Endif                                                                      
      * Maintain estimated lifts                                                                    
     C                   If        IDMESL <> 'Y'                                                    
     C                   Eval      MaintainAPPL = FALSE                                             
     C                   Endif                                                                      
      * Maintain EWC number                                                                         
     C                   If        IDMEWC <> 'Y'                                                    
     C                   Eval      MaintainEWCN = FALSE                                             
     C                   Endif                                                                      
                                                                                                    
      * Unit type label                                                                             
     C                   If        UNLUTY <> *Blank                                                 
     C                   Eval      S2LBL2 = cLbl2Prefix                                             
     C                                    + %Trim(UNLUTY)                                           
     C                   Endif                                                                      
      * Unit description label                                                                      
     C                   If        UNLUDS <> *Blank                                                 
     C                   Eval      S2LBL1 = %Trim(UNLUDS)                                           
     C                                    + cLbl1Suffix                                             
     C                   Eval      S2LBL4 = cLbl4Prefix                                             
     C                                    + %Trim(UNLUDS)                                           
     C                   Endif                                                                      
      * Additional details label                                                                    
     C                   If        UNLADT <> *Blank                                                 
     C                   Eval      S2LBL3 = %Trim(UNLADT)                                           
     C                                    + cLbl3Suffix                                             
     C                   Endif                                                                      
      * Estimated count label                                                                       
     C                   If        UNLECN <> *Blank                                                 
     C                   Eval      S2LBL5 = cLbl5Prefix                                             
     C                                    + %Trim(UNLECN)                                           
     C                   Endif                                                                      
                                                                                                    
     C                   Endif                                                                      
                                                                                                    
                                                                                                    
      /free                                                                                         
                                                                                                    
       // Determine the target margin for this contract item                                        
       s2marg = getContractTypeMargin( s2type : chccod : chcsub : chstwn );                         
                                                                                                    
      /end-free                                                                                     
                                                                                                    
                                                                                                    
                                                                                                    
     C                   Endsr                                                                      
                                                                                                    
      *****************************************************************                             
     C     RTVPRC        BEGSR                                                                      
      *****************************************************************                             
      *                                                                                             
      * RETRIEVE PRICE INFORMATION FOR SCREEN 2                                                     
      *                                                                                             
     C                   CALL      'CMGA52'                                                         
     C                   PARM      CWCNTN        ZACNTN            7 0                              
     C                   PARM      CWITEM        ZAITEM            3 0                              
     C                   PARM      TODAY         ZADATE            7 0                              
     C                   PARM      *ZEROS        ZALINE            3 0                              
     C                   PARM      *ZEROS        ZAACTF            7 0                              
     C                   PARM      *BLANKS       ZASUPL            6                                
     C                   PARM      *BLANKS       ZACONT           20                                
     C                   PARM      *BLANKS       ZATELP           15                                
     C                   PARM      *BLANKS       ZAFAX            15                                
     C                   PARM      *BLANKS       ZACEOF            1                                
     C                   PARM      *ZEROS        ZACBAS           11 2                              
     C                   PARM      *ZEROS        ZACWIN           11 2                              
     C                   PARM      *ZEROS        ZACRPT           11 2                              
     C                   PARM      *ZEROS        ZACLWI           11 2                              
     C                   PARM      *ZEROS        ZACLRT           11 2                              
     C                   PARM      *ZEROS        ZACRVL           11 2                              
     C                   PARM      *BLANKS       ZACRT1           10                                
     C                   PARM      *BLANKS       ZACRT2           10                                
     C                   PARM      *BLANKS       ZASEOF            1                                
     C                   PARM      *ZEROS        ZASBAS           11 2                              
     C                   PARM      *ZEROS        ZASWIN           11 2                              
     C                   PARM      *ZEROS        ZASRPT           11 2                              
     C                   PARM      *ZEROS        ZASLWI           11 2                              
     C                   PARM      *ZEROS        ZASLRT           11 2                              
     C                   PARM      *ZEROS        ZASRVL           11 2                              
     C                   PARM      *BLANKS       ZASRT1           10                                
     C                   PARM      *BLANKS       ZASRT2           10                                
      *                                                                                             
     C                   MOVEL     ZACEOF        WWCEOF                                             
     C                   MOVEL     ZASEOF        WWSEOF                                             
      *                                                                                             
     C                   Z-ADD     ZACBAS        WWCBAS                                             
     C                   Z-ADD     ZACWIN        WWCWIN                                             
     C                   Z-ADD     ZACRPT        WWCRPT                                             
     C                   Z-ADD     ZACRVL        WWPRVL                                             
     C                   MOVEL     ZACRT1        WWPRT1                                             
     C                   MOVEL     ZACRT2        WWPRT2                                             
      *                                                                                             
     C                   Z-ADD     ZASBAS        WWSBAS                                             
     C                   Z-ADD     ZASWIN        WWSWIN                                             
     C                   Z-ADD     ZASRPT        WWSRPT                                             
     C                   Z-ADD     ZASRVL        WWSRVL                                             
     C                   MOVEL     ZASRT1        WWSRT1                                             
     C                   MOVEL     ZASRT2        WWSRT2                                             
      *                                                                                             
     C                   MOVEL     ZASUPL        WWSUPL                                             
     C                   MOVEL     ZACONT        WWSCON                                             
     C                   MOVEL     ZATELP        WWSTEL                                             
     C                   MOVEL     ZAFAX         WWSFAX                                             
      *                                                                                             
     C                   ENDSR                                                                      
     C*****************************************************************                             
     C     PROSC2        BEGSR                                                              .       
     C**************************************************************************                    
     C     TOPSC2        TAG                                                                        
      *                                                                                             
     C                   Eval      ZzTcod = *Blanks                                                 
      *                                                                                             
      * SPECIAL INSTRUCTIONS ACTIVE                                                                 
     C     WWSPI1        IFNE      *BLANKS                                                          
     C     WWSPI2        ORNE      *BLANKS                                                          
     C     WWSPI3        ORNE      *BLANKS                                                          
     C     WWSPI4        ORNE      *BLANKS                                                          
     C     WWSPI5        ORNE      *BLANKS                                                          
     C     WWSPI6        ORNE      *BLANKS                                                          
     C     *IN90         IFEQ      *OFF                                                             
     C                   MOVE      '0189'        MIC                                                
     C                   MOVEL(P)  'CHSMSG'      MSGF                                               
     C                   ENDIF                                                                      
     C                   ENDIF                                                                      
      *                                                                                             
     C                   EXFMT     CMGE0502                                                         
     C*                                                                                             
     C                   SETOFF                                       90                            
     C                   MOVE      '9999'        MIC                                                
     C*                                                                                             
     C* F3=EXIT                                                                                     
     C*                                                                                             
     C     *INKC         IFEQ      '1'                                                              
     C                   Exsr      Sc2Exit                                                          
     C                   GOTO      ENDSC2                                                           
     C                   END                                                                        
     C*                                                                                             
     C* F12=PREVIOUS                                                                                
     C*                                                                                             
     C     *INKL         IFEQ      '1'                                                              
      *                                                                                             
      * Tab processing                                                                              
     C                   If        @TabProc                                                         
     C                   Eval      Z#TabFunc = C#ClsItem                                            
     C                   Eval      Z#TabCode = *Blanks                                              
     C                   Eval      Z#FromTabCode = C#ItemDetl                                       
     C                   Eval      *In78 = *On                                                      
      *                                                                                             
      * Normal processing                                                                           
     C                   Else                                                                       
     C                   EXSR      SETSF1                                                           
     C                   Endif                                                                      
      *                                                                                             
     C                   GOTO      ENDSC2                                                           
     C                   END                                                                        
     C*                                                                                             
     C     TOGGLE        TAG                                                                        
     C*                                                                                             
     C*                                                                                             
     C* F16=SALES PRICE HISTORY                                                                     
     C*                                                                                             
     C     *INKQ         IFEQ      '1'                                                              
     C     Z8F20         OREQ      *ON                                                              
     C*                                                                                             
     C                   MOVE      '0'           Z8F20                                              
     C*                                                                                             
     C                   MOVE      WHCNTN        Z6CNTN                                             
     C                   MOVEL     *BLANKS       Z6PROD                                             
     C                   MOVEL     WWPROD        Z6PROD                                             
     C                   MOVE      'E'           Z6MODE                                             
     C                   MOVE      *ZEROS        Z6COST                                             
     C                   MOVE      *ZEROS        Z6VALU                                             
     C                   CALL      'CMGE56  '                                                       
     C                   PARM                    Z6CNTN            7                                
     C                   PARM                    Z6PROD           25                                
     C                   PARM      '      '      Z6SUPL            6                                
     C                   PARM                    Z6CEOF            1                                
     C                   PARM                    Z6COST           11                                
     C                   PARM                    Z6CWIN           11                                
     C                   PARM                    Z6CRPT           11                                
     C                   PARM                    Z6CLWI           11                                
     C                   PARM                    Z6CLRT           11                                
     C                   PARM                    Z6SEOF            1                                
     C                   PARM                    Z6VALU           11                                
     C                   PARM                    Z6SWIN           11                                
     C                   PARM                    Z6SRPT           11                                
     C                   PARM                    Z6SLWI           11                                
     C                   PARM                    Z6SLRT           11                                
     C                   PARM                    Z6MODE            1                                
     C                   PARM      '0'           Z6F20             1                                
     C                   PARM      S2TYPE        Z6CTYP            1                                
     C*                                                                                             
     C* TOGGLE BETWEEN THE TWO PRICE HISTORY PROGRAMS                                               
     C*                                                                                             
     C     Z6F20         IFEQ      *ON                                                              
     C                   SETOFF                                       KQ                            
     C                   GOTO      TOGGLE                                                           
     C                   END                                                                        
     C*                                                                                             
     C                   GOTO      TOPSC2                                                           
     C                   END                                                                        
     C*                                                                                             
     C*                                                                                             
     C* F17=COST PRICE HISTORY                                                                      
     C*                                                                                             
     C     *INKR         IFEQ      '1'                                                              
     C     Z6F20         OREQ      *ON                                                              
     C*                                                                                             
     C                   MOVE      '0'           Z6F20                                              
     C*                                                                                             
     C                   MOVE      WHCNTN        Z8CNTN                                             
     C                   MOVE      S2TYPE        Z8CTYP                                             
     C                   MOVEL     *BLANKS       Z8PROD                                             
     C                   MOVEL     WWPROD        Z8PROD                                             
     C                   MOVE      'E'           Z8MODE                                             
     C                   MOVE      *ZEROS        Z8COST                                             
     C                   MOVE      *ZEROS        Z8VALU                                             
     C                   CALL      'CMGE58  '                                                       
     C                   PARM                    Z8CNTN            7                                
     C                   PARM                    Z8CTYP            1                                
     C                   PARM                    Z8PROD           25                                
     C                   PARM      '      '      Z8SUPL            6                                
     C                   PARM                    Z8CEOF            1                                
     C                   PARM                    Z8COST           11                                
     C                   PARM                    Z8CWIN           11                                
     C                   PARM                    Z8CRPT           11                                
     C                   PARM                    Z8CLWI           11                                
     C                   PARM                    Z8CLRT           11                                
     C                   PARM                    Z8SEOF            1                                
     C                   PARM                    Z8VALU           11                                
     C                   PARM                    Z8SWIN           11                                
     C                   PARM                    Z8SRPT           11                                
     C                   PARM                    Z8SLWI           11                                
     C                   PARM                    Z8SLRT           11                                
     C                   PARM                    Z8MODE            1                                
     C                   PARM      '0'           Z8F20             1                                
     C*                                                                                             
     C* TOGGLE BETWEEN THE TWO PRICE HISTORY PROGRAMS                                               
     C*                                                                                             
     C     Z8F20         IFEQ      *ON                                                              
     C                   SETOFF                                       KR                            
     C                   GOTO      TOGGLE                                                           
     C                   END                                                                        
     C*                                                                                             
     C                   GOTO      TOPSC2                                                           
     C                   END                                                                        
      *                                                                                             
      * F18=DISPLAY SHEDULE PROFILE IF ONE EXISTS.                                                  
      *                                                                                             
     C     *INKS         IFEQ      *ON                                                              
     C     CWZKEY        CHAIN     CMGCWZ00                           21                            
     C     *IN21         IFEQ      *OFF                                                             
     C                   MOVE      WHCNTN        Z#CNTN            7                                
     C                   MOVE      WWITEM        Z#ITEM            3                                
     C                   MOVE      'E'           Z#MODE            1                                
     C                   CALL      'CMGI24'                                                         
     C                   PARM                    Z#CNTN                                             
     C                   PARM                    Z#ITEM                                             
     C                   PARM                    Z#MODE                                             
     C                   ELSE                                                                       
     C                   SETON                                        90                            
     C                   MOVE      '1011'        MIC                                                
     C                   MOVEL     'CHSMSG'      MSGF                                               
     C                   GOTO      TOPSC2                                                           
     C                   ENDIF                                                                      
     C                   GOTO      TOPSC2                                                           
     C                   ENDIF                                                                      
                                                                                                    
                                                                                                    
      * F19=Order details                                                                           
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
                                                                                                    
                                                                                                    
      *                                                                                             
      * F21=SPECIAL INSTRUCTIONS                                                                    
      *                                                                                             
     C     *INKV         IFEQ      *ON                                                              
     C                   MOVE      WHCNTN        Z#CNTN                                             
     C                   MOVE      WWITEM        Z#ITEM                                             
     C                   MOVE      'E'           Z#MODE                                             
     C                   CALL      'CMGI32'                                                         
     C                   PARM      *BLANKS       Z#FKEY            1                                
     C                   PARM                    Z#CNTN            7                                
     C                   PARM                    Z#ITEM            3                                
     C                   PARM      WWSPI1        Z#SPI1           40                                
     C                   PARM      WWSPI2        Z#SPI2           40                                
     C                   PARM                    Z#MODE            1                                
     C                   PARM      WWSPI3        Z#SPI3           40                                
     C                   PARM      WWSPI4        Z#SPI4           40                                
     C                   PARM      WWSPI5        Z#SPI5           40                                
     C                   PARM      WWSPI6        Z#SPI6           40                                
     C                   GOTO      TOPSC2                                                           
     C                   ENDIF                                                                      
      *                                                                                             
      * F14 = Item comments                                                                         
      *                                                                                             
     C     *INKN         IFEQ      *ON                                                              
     C                   EVAL      P#CNTN = WHCNTN                                                  
     C                   EVAL      P#ITEM = WWITEM                                                  
4.7  C                   CALL      'CMGM05'                                                         
4.7  C                   PARM                    P#CNTN            7 0                              
4.7  C                   PARM                    P#ITEM            3 0                              
4.7  C                   PARM      '2'           ZZMODE            1                                
     C                   GOTO      TOPSC2                                                           
     C                   ENDIF                                                                      
      *                                                                                             
      * TAB CODE MUST BE VALID, IF ENTERED                                                          
      *                                                                                             
     C* Ignore current tab code                                                                     
     C                   If        ZzTcod <> *Blanks                                                
     C                             And ZzTcod = C#ItemDetl                                          
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
      * Tab processing for valid tab code entered                                                   
     C                   If        ZzTcod <> *Blanks                                                
     C                   Eval      Z#TabFunc = C#PrcTab                                             
     C                   Eval      Z#TabCode = ZzTcod                                               
     C                   Eval      Z#FromTabCode = C#ItemDetl                                       
     C                   Eval      *In78 = *On                                                      
     C                   Goto      EndSc2                                                           
     C                   Endif                                                                      
     C*                                                                                             
     C* IF CONTRACT IS LIVE PROCEED TO MOVEMENT SCREENS                                             
     C* - OTHERWISE RETURN TO LIST OF ITEMS                                                         
     C*                                                                                             
     C     CHSTAT        IFNE      'E'                                                              
     C*                                                                                             
     C* Tab processing                                                                              
     C                   If        @TabProc                                                         
     C                   Eval      Z#TabFunc = C#PrcTab                                             
     C                   Eval      Z#TabCode = C#MoveList                                           
     C                   Eval      Z#FromTabCode = C#ItemDetl                                       
     C                   Eval      *In78 = *On                                                      
     C* Normal processing                                                                           
     C                   Else                                                                       
     C                   EXSR      SETSF5                                                           
     C                   Endif                                                                      
     C*                                                                                             
     C                   ELSE                                                                       
     C*                                                                                             
     C* Tab processing                                                                              
     C                   If        @TabProc                                                         
     C                   Eval      Z#TabFunc = C#ClsItem                                            
     C                   Eval      Z#TabCode = *Blanks                                              
     C                   Eval      Z#FromTabCode = C#ItemDetl                                       
     C                   Eval      *In78 = *On                                                      
     C* Normal processing                                                                           
     C                   Else                                                                       
     C                   EXSR      SETSF1                                                           
     C                   Endif                                                                      
     C*                                                                                             
     C                   END                                                                        
     C*                                                                                             
     C*                                                                                             
     C     ENDSC2        ENDSR                                                                      
      *****************************************************************                             
     C     Sc2Exit       Begsr                                                                      
      *                                                                                             
      * Exit tab processing                                                                         
     C                   If        @TabProc                                                         
     C                   Eval      Z#TabFunc = C#ExitProg                                           
     C                   Eval      Z#TabCode = *Blanks                                              
     C                   Eval      Z#FromTabCode = C#ItemDetl                                       
     C                   Endif                                                                      
      *                                                                                             
     C                   MOVE      'X'           Z#EXIT                                             
     C                   SETON                                        78                            
                                                                                                    
     C                   Endsr                                                                      
      *****************************************************************                             
     C     CLCMAR        BEGSR                                                                      
      *****************************************************************                             
     C*                                                                                             
     C* CALCULATE ITEM MARGINS                                                                      
     C*                                                                                             
     C                   Z-ADD     0             WWCBAM                                             
     C                   Z-ADD     0             WWCRPM                                             
     C                   Z-ADD     0             WWSBAM                                             
     C                   Z-ADD     0             WWSRPM                                             
                                                                                                    
                                                                                                    
      * Calculate cost margin on basic prices                                                       
     C                   Eval      margin =                                                         
     C                               CMGZLCW_calcCostMargin( WWCBAS : WWSBAS )                      
     C                   Z-ADD     margin        WWCBAM                                             
                                                                                                    
      * Calculate sale margin on basic prices                                                       
     C                   Eval      margin =                                                         
     C                               CMGZLCW_calcSaleMargin( WWCBAS : WWSBAS )                      
     C                   Z-ADD     margin        WWSBAM                                             
                                                                                                    
      * Calculate cost margin on rate prices                                                        
     C                   Eval      margin =                                                         
     C                               CMGZLCW_calcCostMargin( WWCRPT : WWSRPT )                      
     C                   Z-ADD     margin        WWCRPM                                             
                                                                                                    
      * Calculate sale margin on rate prices                                                        
     C                   Eval      margin =                                                         
     C                               CMGZLCW_calcSaleMargin( WWCRPT : WWSRPT )                      
     C                   Z-ADD     margin        WWSRPM                                             
                                                                                                    
                                                                                                    
     C                   ENDSR                                                                      
      *****************************************************************                             
     C     SETSF3        BEGSR                                                                      
      *****************************************************************                             
     C                   SETOFF                                       808182                        
     C                   SETOFF                                       838889                        
     C                   WRITE     CMGE053C                                                         
      *                                                                                             
      * SETUP FIRST PAGE OF SUBFILE                                                                 
      *                                                                                             
     C                   Z-ADD     0             SFRCD3            4 0                              
     C                   MOVE      *BLANKS       S3SUPL                                             
     C                   Z-ADD     0             BTITEM            3 0                              
      *                                                                                             
     C     WHCNTN        SETLL     CMGCW01                                                          
     C                   EXSR      GETSF3                                                           
      *                                                                                             
      * CHECK THERE IS AT LEAST ONE RECORD IN THE SUBFILE                                           
      *                                                                                             
     C     SFRCD3        IFEQ      0                                                                
     C                   SETOFF                                       80                            
     C                   ELSE                                                                       
     C                   SETON                                        80                            
     C                   ENDIF                                                                      
     C                   SETON                                        81                            
      *                                                                                             
     C                   ENDSR                                                                      
      **************************************************************************                    
     C     GETSF3        BEGSR                                                                      
      **************************************************************************                    
     C                   SETOFF                                       8283                          
     C                   Z-ADD     3             SCREEN                                             
      *                                                                                             
     C     CWKEY8        SETGT     CMGCW01                                                          
      *                                                                                             
     C                   Z-ADD     1             X                 3 0                              
     C     X             DOUGE     12                                           *DO 1               
     C*                                                                                             
     C     WHCNTN        READE     CMGCW01                                88                        
      *                                                                                             
     C     *IN88         IFEQ      '0'                                          *IF 2               
      *                                                                                             
     C     CWSUPL        IFEQ      S3SUPL                                                           
     C                   ITER                                                                       
     C                   END                                                                        
      *                                                                                             
     C                   ADD       1             SFRCD3                                             
     C                   MOVEL     ' '           S3SLCT                                             
     C                   MOVE      CWSUPL        S3SUPL                                             
     C                   MOVE      *BLANKS       S3DESC                                             
 ISL  *                                                                                             
 ISL C     @@EO5F        IFEQ      '5'                                                              
 ISL C                   MOVE      *BLANK        @@5U01          001                                
 ISL C                   EXSR      SBR008                                                           
 ISL C                   MOVE      *OFF          *IN21                                              
 ISL C     @@5P04        IFEQ      '1'                                                              
 ISL C     @@5P04        OREQ      '9'                                                              
 ISL C     X@IND1        IFEQ      '1'                                                              
 ISL C                   MOVE      *ON           *IN21                                              
 ISL C                   ENDIF                                                                      
 ISL C                   ENDIF                                                                      
 ISL C                   ELSE                                                                       
     c                   Exsr      Set_BPL                                                          
 ISL C                   EXSR      SBR009                                                           
 ISL C                   MOVE      *OFF          *IN21                                              
 ISL C     @@EP04        IFNE      *BLANKS                                                          
 ISL C                   MOVE      *ON           *IN21                                              
 ISL C                   ENDIF                                                                      
 ISL C                   ENDIF                                                                      
 ISL  *                                                                                             
     C  N21              MOVEL     SDNAME        S3DESC                                             
     C                   Z-ADD     CWITEM        BTITEM                                             
     C     X             IFEQ      1                                            *IF 3               
     C                   Z-ADD     SFRCD3        RCDNB3                                             
     C                   ENDIF                                                  *ENDIF 3            
     C                   WRITE     CMGE053S                                                         
     C                   ADD       1             X                                                  
     C                   ELSE                                                   *ELSE 2             
     C                   Z-ADD     15            X                                                  
     C                   ENDIF                                                  *ENDIF 2            
      *                                                                                             
     C                   ENDDO                                                  *ENDDO 1            
      *                                                                                             
      *                                                                                             
     C                   ENDSR                                                                      
     C*****************************************************************                             
     C     PROSC3        BEGSR                                                                      
     C*****************************************************************                             
     C     TOPSC3        TAG                                                                        
     C*                                                                                             
     C                   WRITE     CMGE0503                                                         
     C                   EXFMT     CMGE053C                                                         
     C*                                                                                             
     C                   SETOFF                                       90                            
     C                   MOVE      '9999'        MIC               4                                
     C*                                                                                             
     C                   Z-ADD     SFLRRN        RCDNB3                                             
     C*                                                                                             
     C* F3=EXIT                                                                                     
     C*                                                                                             
     C     *INKC         IFEQ      '1'                                                              
     C                   Exsr      Sc3Exit                                                          
     C                   GOTO      ENDSC3                                                           
     C                   END                                                                        
     C*                                                                                             
     C* F12=PREVIOUS                                                                                
     C*                                                                                             
     C     *INKL         IFEQ      '1'                                                              
     C                   EXSR      SETSF1                                                           
     C                   GOTO      ENDSC3                                                           
     C                   END                                                                        
     C*                                                                                             
     C*                                                                                             
     C* ARE THERE RECORDS IN THE SUBFILE ? *                                                        
     C  N80                                                                                         
     CAN 81              GOTO      TOPSC3                                                           
     C*                                                                                             
     C* ROLL UP THE SUBFILE BEFORE DISPLAYING ANY ERRORS *                                          
     C     *IN89         IFEQ      '1'                                                              
     C     SFLRRN        ADD       10            SFRCD3                                             
     C                   EXSR      GETSF3                                                           
     C                   SETOFF                                       89                            
     C                   GOTO      TOPSC3                                                           
     C                   END                                                                        
     C*                                                                                             
     C* READ CHANGED RECORDS *                                                                      
     C                   SETOFF                                       212223                        
     C                   SETOFF                                       24                            
     C                   Z-ADD     0             WKRECD                                             
     C                   Z-ADD     0             ERRECD                                             
     C                   Z-ADD     0             NBRSLT                                             
     C     *IN21         DOUEQ     '1'                                          *DO 1               
     C                   READC     CMGE053S                               21                        
     C     *IN21         IFEQ      '0'                                          *IF 2               
     C                   SETOFF                                       8283                          
     C     S3SLCT        IFEQ      '1'                                          *IF 3               
     C                   ADD       1             NBRSLT                                             
     C  N23              Z-ADD     SFRCD3        WKRECD                                             
     C                   SETON                                        82                            
     C                   SETON                                        23                            
     C                   END                                                    *END 3              
     C     S3SLCT        IFNE      ' '                                          *IF 3               
     C     S3SLCT        ANDNE     '1'                                                              
     C                   ADD       1             NBRSLT                                             
     C  N22              Z-ADD     SFRCD3        ERRECD                                             
     C                   SETON                                        8283                          
     C                   SETON                                        22                            
     C                   END                                                    *END 3              
     C                   UPDATE    CMGE053S                                                         
     C                   END                                                    *END 2              
     C                   END                                                    *END 1              
     C*                                                                                             
     C*                                                                                             
     C* INVALID SELECTION NOT A 1 OR BLANK *                                                        
     C*                                                                                             
     C     *IN22         IFEQ      '1'                                                              
     C                   SETON                                        90                            
     C                   MOVE      '7025'        MIC                                                
     C                   GOTO      TOPSC3                                                           
     C                   END                                                                        
     C*                                                                                             
     C* MORE THAN ONE SELECTED *                                                                    
     C*                                                                                             
     C     NBRSLT        IFGT      1                                                                
     C                   SETON                                        90                            
     C                   MOVE      '0080'        MIC                                                
     C                   GOTO      TOPSC3                                                           
     C                   END                                                                        
     C*                                                                                             
     C* AT LEAST ONE SELECTION MUST BE MADE *                                                       
     C     NBRSLT        IFEQ      0                                                                
     C                   SETON                                        90                            
     C                   MOVE      '0082'        MIC                                                
     C                   GOTO      TOPSC3                                                           
     C                   END                                                                        
     C*                                                                                             
     C* GOT A VALID SELECTION - SO CHAIN TO SUBFILE *                                               
     C     WKRECD        CHAIN     CMGE053S                           21                            
     C*                                                                                             
     C* 1=SELECT                                                                                    
     C*                                                                                             
     C     S3SLCT        IFEQ      '1'                                          *IF 1               
     C                   MOVEL     S3SUPL        ZZSUPL                                             
     C                   MOVEL     S3DESC        ZZSDSC                                             
     C                   SETON                                        86                            
     C                   EXSR      SETSF1                                                           
     C                   GOTO      ENDSC3                                                           
     C                   END                                                    *END 1              
     C*                                                                                             
     C*                                                                                             
     C                   GOTO      TOPSC3                                                           
     C*                                                                                             
     C     ENDSC3        ENDSR                                                                      
      *****************************************************************                             
     C     Sc3Exit       Begsr                                                                      
                                                                                                    
     C* Exit tab processing                                                                         
     C                   If        @TabProc                                                         
     C                   Eval      Z#TabFunc = C#ExitProg                                           
     C                   Eval      Z#TabCode = *Blanks                                              
     C                   Eval      Z#FromTabCode = C#ItemDetl                                       
     C                   Endif                                                                      
                                                                                                    
     C                   MOVE      'X'           Z#EXIT                                             
     C                   SETON                                        78                            
                                                                                                    
     C                   Endsr                                                                      
      *****************************************************************                             
     C     SETSF5        BEGSR                                                                      
      *****************************************************************                             
     C                   SETOFF                                       808182                        
     C                   SETOFF                                       838889                        
     C                   SETOFF                                       16                            
     C                   WRITE     CMGE055C                                                         
      *                                                                                             
      * SETUP FIRST PAGE OF SUBFILE                                                                 
      *                                                                                             
     C                   Z-ADD     0             SFRCD5            4 0                              
     C                   Z-ADD     0             BTBOOK            7 0                              
     C**CA01               Z-ADD0         BTMVNO  30                                                
CA01 C                   Z-ADD     0             BTMVNO            5 0                              
     C                   Z-ADD     0             ZZMVDT                                             
      *                                                                                             
     C***  CWKEY1        SETLL     CMGMV13                                                          
     C     CWKEY3        SETLL     CMGMV13                                                          
     C                   EXSR      GETSF5                                                           
      *                                                                                             
      * CHECK THERE IS AT LEAST ONE RECORD IN THE SUBFILE                                           
      * (IF NONE THEN DON'T DISPLAY)                                                                
      *                                                                                             
     C     SFRCD5        IFEQ      0                                                                
     C                   SETOFF                                       80                            
     C                   ELSE                                                                       
     C                   SETON                                        80                            
     C                   ENDIF                                                                      
     C                   SETON                                        81                            
      *                                                                                             
     C                   ENDSR                                                                      
     C*****************************************************************                             
     C     GETSF5        BEGSR                                                                      
     C**************************************************************************                    
     C                   SETOFF                                       8283                          
     C                   Z-ADD     5             SCREEN                                             
      *                                                                                             
     C   16MVKEY1        SETGT     CMGMV13                                                          
     C  N16              SETON                                        16                            
      *                                                                                             
     C                   Z-ADD     1             X                 3 0                              
     C     X             DOUGE     11                                           *DO 1               
     C*                                                                                             
     C     MVKEY         READE     CMGMV13                                88                        
      *                                                                                             
     C     *IN88         IFEQ      '0'                                          *IF 2               
      *                                                                                             
     C                   ADD       1             SFRCD5                                             
     C                   MOVEL     ' '           S5SLCT                                             
     C                   Z-ADD     MVMVNO        S5MVNO                                             
     C                   Z-ADD     MVBOOK        BTBOOK                                             
     C                   Z-ADD     MVMVNO        BTMVNO                                             
     C                   Z-ADD     MVMVDT        ZZMVDT            7 0                              
     C                   MOVE      *BLANKS       S5MDSC                                             
     C     MVMCOD        CHAIN     CMGMC00                            21                            
     C     *IN21         IFEQ      *OFF                                                             
     C     MVDELF        IFEQ      'C'                                                              
     C                   MOVE      *BLANKS       TXT32            32                                
     C     CANC          CAT       MCDESC:1      TXT32                                              
     C                   MOVEL     TXT32         S5MDSC                                             
     C                   ELSE                                                                       
     C                   MOVEL     MCDESC        S5MDSC                                             
     C                   ENDIF                                                                      
     C                   ENDIF                                                                      
     C                   CALL      'XXFDAT'                                                         
     C                   PARM                    MVMVDT                                             
     C                   PARM                    S5MVDT                                             
     C                   Z-ADD     MVBOOK        S5BOOK                                             
     C                   Z-ADD     MVVALU        S5VALU                                             
     C                   MOVE      MVRECD        S5RECD                                             
     C                   MOVE      MVINVD        S5INVD                                             
      *                                                                                             
      * Get supplement details ...                                                                  
      *                                                                                             
     C     MVW_Key_1     Klist                                                                      
     C                   Kfld                    MVCNTN                                             
     C                   Kfld                    MVITEM                                             
     C                   Kfld                    MVMVNO                                             
      *                                                                                             
     C                   Eval      MVPICS = *Zeros                                                  
     C     MVW_Key_1     Chain(n)  CMGMVW00                                                         
     C                   Eval      S5PICT = *Blanks                                                 
     C                   If        MVPICS > *Zeros                                                  
     C                   Eval      S5PICT = '*'                                                     
     C                   Endif                                                                      
      *                                                                                             
      * HAS THE DOCUMENT BEEN SCANNED?                                                              
     C                   EXSR      CHKDOC                                                           
     C                   MOVE      *BLANKS       S5SCND                                             
     C     @SDOCF        IFEQ      *ON                                                              
     C                   MOVE      'Y'           S5SCND                                             
     C                   ENDIF                                                                      
      *                                                                                             
     C     X             IFEQ      1                                            *IF 3               
     C                   Z-ADD     SFRCD5        RCDNB5                                             
     C                   ENDIF                                                  *ENDIF 3            
     C                   WRITE     CMGE055S                                                         
     C                   ADD       1             X                                                  
     C                   ELSE                                                   *ELSE 2             
     C                   Z-ADD     15            X                                                  
     C                   ENDIF                                                  *ENDIF 2            
      *                                                                                             
     C                   ENDDO                                                  *ENDDO 1            
      *                                                                                             
      *                                                                                             
     C                   ENDSR                                                                      
      *****************************************************************                             
     C     CHKDOC        BEGSR                                                                      
      *****************************************************************                             
      *                                                                                             
      * INITIALISE THE SCANNED DOCUMENT FOUND INDICATORS                                            
      *                                                                                             
     C                   MOVE      *OFF          @SDOCF            1                                
                                                                                                    
      /free                                                                                         
                                                                                                    
       if (CMGZLMV_isBillingRunScanned(mvbook));                                                    
         @SDOCF = *ON;                                                                              
       endif;                                                                                       
                                                                                                    
      /end-free                                                                                     
                                                                                                    
     C                   ENDSR                                                                      
     C*****************************************************************                             
     C     PROSC5        BEGSR                                                                      
     C*****************************************************************                             
     C     TOPSC5        TAG                                                                        
     C*                                                                                             
     C                   Eval      ZzTcod = *Blanks                                                 
     C*                                                                                             
     C                   WRITE     CMGE0505                                                         
     C                   EXFMT     CMGE055C                                                         
     C                   Read      CMGE0505                                                         
     C*                                                                                             
     C                   SETOFF                                       90                            
     C                   MOVE      '9999'        MIC               4                                
     C*                                                                                             
     C                   Z-ADD     SFLRRN        RCDNB5                                             
     C*                                                                                             
     C* F3=EXIT                                                                                     
     C*                                                                                             
     C     *INKC         IFEQ      '1'                                                              
     C                   Exsr      Sc5Exit                                                          
     C                   GOTO      ENDSC5                                                           
     C                   END                                                                        
     C*                                                                                             
     C* F12=PREVIOUS                                                                                
     C*                                                                                             
     C     *INKL         IFEQ      '1'                                                              
      *                                                                                             
      * Tab processing                                                                              
     C                   If        @TabProc                                                         
     C                   Eval      Z#TabFunc = C#PrcTab                                             
     C                   Eval      Z#TabCode = C#ItemDetl                                           
     C                   Eval      Z#FromTabCode = C#MoveList                                       
     C                   Eval      *In78 = *On                                                      
      *                                                                                             
      * Normal processing                                                                           
     C                   Else                                                                       
     C                   EXSR      GETSC2                                                           
     C                   Endif                                                                      
      *                                                                                             
     C                   GOTO      ENDSC5                                                           
     C                   END                                                                        
                                                                                                    
                                                                                                    
      * F19=Order numbers                                                                           
     C                   If        ( *INKT )                                                        
     C                   Eval      gExitStatus = orderDetails( whcntn )                             
     C                   If        ( gExitStatus = CMGI48_EXIT_EXIT )                               
     C                   Exsr      Sc5Exit                                                          
     C                   Goto      ENDSC5                                                           
     C                   Endif                                                                      
     C                   If        ( gExitStatus = CMGI48_EXIT_NORMAL or                            
     C                               gExitStatus = CMGI48_EXIT_PREVIOUS )                           
     C                   Goto      TOPSC5                                                           
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
     C                             And ZzTcod = C#MoveList                                          
     C                   Goto      TopSc5                                                           
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
     C                   Goto      TopSc5                                                           
     C                   Endif                                                                      
     C                   Endif                                                                      
      *                                                                                             
      * Tab processing for valid tab code entered                                                   
     C                   If        ZzTcod <> *Blanks                                                
     C                   Eval      Z#TabFunc = C#PrcTab                                             
     C                   Eval      Z#TabCode = ZzTcod                                               
     C                   Eval      Z#FromTabCode = C#MoveList                                       
     C                   Eval      *In78 = *On                                                      
     C                   Goto      EndSc5                                                           
     C                   Endif                                                                      
     C*                                                                                             
     C* ARE THERE RECORDS IN THE SUBFILE ? *                                                        
     C********  N80 81             GOTO TOPSC5                                                      
     C     *IN80         IFEQ      *OFF                                                             
     C     *IN81         ANDEQ     *ON                                                              
      *                                                                                             
      * Tab processing                                                                              
     C                   If        @TabProc                                                         
     C                   Eval      Z#TabFunc = C#ClsItem                                            
     C                   Eval      Z#TabCode = *Blanks                                              
     C                   Eval      Z#FromTabCode = C#MoveList                                       
     C                   Eval      *In78 = *On                                                      
      *                                                                                             
      * Normal processing                                                                           
     C                   Else                                                                       
     C                   EXSR      SETSF1                                                           
     C                   Endif                                                                      
      *                                                                                             
     C                   GOTO      ENDSC5                                                           
     C                   END                                                                        
     C*                                                                                             
     C* ROLL UP THE SUBFILE BEFORE DISPLAYING ANY ERRORS *                                          
     C     *IN89         IFEQ      '1'                                                              
     C     SFLRRN        ADD       9             SFRCD5                                             
     C                   EXSR      GETSF5                                                           
     C                   SETOFF                                       89                            
     C                   GOTO      TOPSC5                                                           
     C                   END                                                                        
     C*                                                                                             
     C* READ CHANGED RECORDS *                                                                      
     C                   SETOFF                                       212223                        
     C                   SETOFF                                       24                            
     C                   Z-ADD     0             WKRECD                                             
     C                   Z-ADD     0             ERRECD                                             
     C                   Z-ADD     0             NBRSLT                                             
     c                   MOVE      *BLANKS       SVSEL1            1                                
     c                   MOVE      *BLANKS       SVSEL2            1                                
     c                   eval      @7selected = *Off                                                
     C     *IN21         DOUEQ     '1'                                          *DO 1               
     C                   READC     CMGE055S                               21                        
     C     *IN21         IFEQ      '0'                                          *IF 2               
     C                   SETOFF                                       8283                          
     C     S5SLCT        IFEQ      '1'                                          *IF 3               
     C     S5SLCT        OREQ      '6'                                          *IF 3               
     C     S5SLCT        OREQ      '7'                                          *IF 3               
     C                   ADD       1             NBRSLT                                             
     C  N23              Z-ADD     SFRCD5        WKRECD                                             
     C                   SETON                                        82                            
     C                   SETON                                        23                            
     C                   END                                                    *END 3              
     C     S5SLCT        IfEQ      '7'                                          *IF 3               
     c                   eval      @7selected = *On                                                 
     C                   endif                                                                      
                                                                                                    
     C     S5SLCT        IFNE      ' '                                          *IF 3               
     C     S5SLCT        ANDNE     '1'                                                              
     C     S5SLCT        ANDNE     '6'                                                              
     C     S5SLCT        ANDNE     '7'                                                              
     C                   ADD       1             NBRSLT                                             
     C  N22              Z-ADD     SFRCD5        ERRECD                                             
     C                   SETON                                        8283                          
     C                   SETON                                        22                            
                                                                                                    
      * Need to allow multiple '7's' to be selected but onlu 1 of the other options                 
      * and never a mix of options                                                                  
                                                                                                    
     C                   IF        SVSEL1 = *BLANKS                                                 
     C                   EVAL      SVSEL1 = S5SLCT                                                  
                                                                                                    
     C                   ELSE                                                                       
                                                                                                    
     C                   IF        SVSEL2 = *BLANKS AND                                             
     C                             S5SLCT <> SVSEL1                                                 
     C                   EVAL      SVSEL2 = S5SLCT                                                  
     C                   ENDIF                                                                      
                                                                                                    
     C                   IF        SVSEL2 = *BLANKS AND                                             
     C                             (S5SLCT = '1' OR S5SLCT = '6')                                   
     C                   EVAL      SVSEL2 = S5SLCT                                                  
     C                   ENDIF                                                                      
                                                                                                    
     C                   ENDIF                                                                      
     C                                                                                              
     C                   END                                                    *END 3              
     C                   UPDATE    CMGE055S                                                         
     C                   END                                                    *END 2              
     C                   END                                                    *END 1              
     C*                                                                                             
     C*                                                                                             
     C* INVALID SELECTION NOT A 1,6,7 OR BLANK *                                                    
     C*                                                                                             
     C     *IN22         IFEQ      '1'                                                              
     C                   SETON                                        90                            
     C                   MOVE      '7025'        MIC                                                
     C                   GOTO      TOPSC5                                                           
     C                   END                                                                        
     C*                                                                                             
     C* MORE THAN ONE SELECTED *                                                                    
     C*                                                                                             
     C**   NBRSLT        IFGT      1                                                                
     C                   IF        SVSEL2 <> *BLANKS                                                
     C                   SETON                                        90                            
     C                   MOVE      '0080'        MIC                                                
     C                   GOTO      TOPSC5                                                           
     C                   END                                                                        
     C*                                                                                             
     C* AT LEAST ONE SELECTION MUST BE MADE *                                                       
     C     NBRSLT        IFEQ      0                                                                
     C                   SETON                                        90                            
     C                   MOVE      '0082'        MIC                                                
     C                   GOTO      TOPSC5                                                           
     C                   END                                                                        
     C*                                                                                             
     C* GOT A VALID SELECTION - SO CHAIN TO SUBFILE *                                               
     C                   Eval      WKSLCT = *Blank                                                  
     C**   S5SLCT        IFNE      '7'                                          *IF 1               
     c                   if        not @7selected                                                   
     C     WKRECD        CHAIN     CMGE055S                           21                            
     C                   MOVE      S5SLCT        WKSLCT            1                                
     C                   MOVE      ' '           S5SLCT                                             
     C**                 MOVE      '1'           WKSLCT            1                                
     C                   UPDATE    CMGE055S                                                         
     C                   ENDIF                                                                      
     C*                                                                                             
     C* 1=SELECT                                                                                    
     C*                                                                                             
     C     WKSLCT        IFEQ      '1'                                          *IF 1               
     C                   MOVE      ' '           WKSLCT            1                                
     C*                                                                                             
     C* Tab processing for select (enquire on) existing movement                                    
     C                   If        @TabProc                                                         
     C                   Eval      Z#TabFunc = C#EnqMove                                            
     C                   Eval      Z#TabCode = *Blanks                                              
     C                   Eval      Z#FromTabCode = C#MoveList                                       
     C                   Move      WwItem        Z#ItemNo                                           
     C                   Move      S5MvNo        Z#MoveNo                                           
     C                   Eval      *In78 = *On                                                      
     C*                                                                                             
     C* Normal processing for select (enquire on) existing movement                                 
     C                   Else                                                                       
     C     MVKEY2        CHAIN     CMGMV00                            21                            
     C                   EXSR      MOVEMT                                                           
     C* F3=EXIT                                                                                     
     C     Z6FKEY        IFEQ      '03'                                                             
     C* Exit tab processing                                                                         
     C                   If        @TabProc                                                         
     C                   Eval      Z#TabFunc = C#ExitProg                                           
     C                   Eval      Z#TabCode = *Blanks                                              
     C                   Eval      Z#FromTabCode = C#MoveList                                       
     C                   Endif                                                                      
     C***                SETON                                        LR                            
     C                   SETON                                        78                            
 ISL C***  @@EO5F        IFEQ      '5'                                                              
 ISL C***                EXSR      SBR014                                                           
 ISL C***                ENDIF                                                                      
     C                   END                                                                        
     C*                                                                                             
     C                   Endif                                                                      
     C*                                                                                             
     C                   GOTO      ENDSC5                                                           
     C                   END                                                    *END 1              
                                                                                                    
     C* 6=SELECT                                                                                    
     C*                                                                                             
     C     WKSLCT        IFEQ      '6'                                          *IF 1               
     C                   Call      'CMGE57'                                                         
     C                   Parm                    WHCNTN                                             
     C                   Parm                    WWITEM                                             
     C                   Parm                    S5MVNO                                             
     C                   Endif                                                                      
                                                                                                    
     C* 7= Movement confirmation reprint                                                            
                                                                                                    
     C**   S5SLCT        IFEQ      '7'                                                              
     C                   if        @7selected                                                       
                                                                                                    
     C     *IN21         DOUEQ     '1'                                                              
     C                   READC     CMGE055S                               21                        
     C     *IN21         IFEQ      '0'                                                              
      * Write to the confirmations workfile                                                         
                                                                                                    
     C     MVKEY4        CHAIN(N)  CMGMV00                                                          
     C                   IF        %FOUND(CMGMV00)                                                  
     C                                                                                              
     C                   EVAL      CFCNTN = MVCNTN                                                  
     C                   EVAL      CFITEM = MVITEM                                                  
     C                   EVAL      CFMVNO = MVMVNO                                                  
     C                   EVAL      CFMCOD = MVMCOD                                                  
     C                   EVAL      CFMVDT = MVMVDT                                                  
     C                   EVAL      CFSUPL = MVSUPL                                                  
     C                   EVAL      CFETAS = MVETAS                                                  
     C                   EVAL      CFVALU = MVVALU                                                  
     C                   EVAL      CFCOST = MVCOST                                                  
     C                   EVAL      CFREPR = 'B'                                                     
     C                   IF        MVDELF = 'C'                                                     
     C                   EVAL      CFREPR = 'D'                                                     
     C                   ENDIF                                                                      
                                                                                                    
     C     MVKEY4        CHAIN     CMGCF00                                                          
     C                   IF        NOT%FOUND(CMGCF00)                                               
     C                   WRITE     CMGCF                                                            
     C                   ENDIF                                                                      
                                                                                                    
     C                   ENDIF                                                                      
     C                   MOVE      ' '           S5SLCT                                             
     C                   UPDATE    CMGE055S                                                         
     C                   ENDIF                                                                      
     C                   ENDDO                                                                      
                                                                                                    
     C                   ENDIF                                                                      
     C*                                                                                             
     C                   GOTO      TOPSC5                                                           
     C*                                                                                             
     C     ENDSC5        ENDSR                                                                      
     C**************************************************************************                    
     C     Sc5Exit       Begsr                                                                      
                                                                                                    
     C* Exit tab processing (suspension checks in calling program)                                  
     C                   If        @TabProc                                                         
     C                   Eval      Z#TabFunc = C#ExitProg                                           
     C                   Eval      Z#TabCode = *Blanks                                              
     C                   Eval      Z#FromTabCode = C#MoveList                                       
     C                   Endif                                                                      
                                                                                                    
     C                   MOVE      'X'           Z#EXIT                                             
     C                   SETON                                        78                            
                                                                                                    
     C                   endsr                                                                      
     C**************************************************************************                    
     C     MOVEMT        BEGSR                                                                      
     C**************************************************************************                    
     C*                                                                                             
     C* CALL MOVEMENTS PROGRAM                                                                      
     C*                                                                                             
     C                   MOVE      WHCNTN        Z6CNTN                                             
     C                   MOVE      WWITEM        Z6ITEM                                             
     C                   MOVE      MVMVNO        Z6MVNO                                             
     C                   MOVE      'E'           Z6MODE                                             
     C                   CALL      'CMGI16  '                                                       
     C                   PARM                    Z6CNTN            7                                
     C                   PARM                    Z6ITEM            3                                
     C**CA01               PARM           Z6MVNO  3                                                 
CA01 C                   PARM                    Z6MVNO            5                                
     C                   PARM                    Z6MODE            1                                
     C                   PARM      ' '           Z6NOTF            1                                
     C                   PARM      '  '          Z6FKEY            2                                
     C                   Parm      *Blanks       Z6Lr              1                                
     C                   Parm      *Blanks       Z6TabCodes                                         
     C                   Parm      *Blanks       Z6TabCode                                          
     C                   Parm      *Blanks       Z6TabFunc                                          
     C                   Parm      *Blanks       Z6FromTabCode                                      
     C*                                                                                             
     C*                                                                                             
     C                   ENDSR                                                                      
      **************************************************************************                    
     C     COLOUR        BEGSR                                                                      
      **************************************************************************                    
     C                   SETOFF                                       404142                        
      *                                                                                             
     C                   SELECT                                                                     
      * BLUE (LIVE CONTRACT ITEM WITH NO SKIPS ONSITE)                                              
     C     S1SKIP        WHENEQ    0                                                                
     C     CWLINA        ANDEQ     '2'                                                              
     C                   SETON                                        40                            
      * WHITE (LIVE CONTRACT ITEM WITH SKIPS ONSITE)                                                
     C     S1SKIP        WHENNE    0                                                                
     C     CWLINA        ANDEQ     '2'                                                              
     C                   SETON                                        41                            
     C                   ENDSL                                                                      
      *                                                                                             
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
     C                   PARM      *BLANKS       Z#TEXT          120                                
     C*                                                                                             
     C*                                                                                             
     C                   ENDSR                                                                      
     C**************************************************************************                    
     C     FSTCYC        BEGSR                                                                      
     C**************************************************************************                    
 ISL C***  *DTAARA       DEFINE    *LDA          LDA                                                
 ISL C***                IN        LDA                                                              
     c***                Move      LDMODL        Save_Mod          3                                
                                                                                                    
 ISL  /COPY CMGE05B                                                                                 
     C*                                                                                             
     C     0             CHAIN     CMGLC00                            21                            
     C     0             CHAIN(N)  CMGLCX00                           21                            
 ISL  *                                                                                             
 ISL C     @@EO5F        IFEQ      '5'                                                              
 ISL C                   MOVE      *BLANK        @@5U01          001                                
 ISL C                   EXSR      SBR010                                                           
 ISL C                   MOVE      *OFF          *IN21                                              
 ISL C     @@5P04        IFEQ      '1'                                                              
 ISL C     @@5P04        OREQ      '9'                                                              
 ISL C     X@IND1        IFEQ      '1'                                                              
 ISL C                   MOVE      *ON           *IN21                                              
 ISL C                   ENDIF                                                                      
 ISL C                   ENDIF                                                                      
 ISL C                   ELSE                                                                       
 ISL C                   EXSR      SBR012                                                           
 ISL C                   MOVE      *OFF          *IN21                                              
 ISL C     @@EP04        IFNE      *BLANKS                                                          
 ISL C                   MOVE      *ON           *IN21                                              
 ISL C                   ENDIF                                                                      
 ISL C                   ENDIF                                                                      
 ISL  *                                                                                             
     C*                                                                                             
     C                   MOVEA     SCA           ZCA                                                
     C*                                                                                             
     C                   MOVE      Z#CONT        WHCNTN            7 0                              
     C*                                                                                             
     C                   MOVE      *HIVAL        ZZHIVL            3 0                              
                                                                                                    
                                                                                                    
      * Retrieve todays date                                                                        
     C                   Eval      today = getCurrentDate()                                         
                                                                                                    
                                                                                                    
     C                   MOVE      '9999'        MIC               4                                
     C                   MOVE      *BLANKS       MSGF             10                                
     C                   MOVEL     'XAPMSG'      MSGF                                               
     C                   Move      *All'0'       *In                                                
     C*                                                                                             
     C* READ CONTRACT HEADER & STATUS                                                               
     C*                                                                                             
     C     WHCNTN        CHAIN     CMGCH00                            10                            
     C     *IN10         IFEQ      *OFF                                                             
     C     CHSTAT        ANDEQ     'L'                                                              
     C     *IN10         OREQ      *OFF                                                             
     C     CHSTAT        ANDEQ     'C'                                                              
     C                   SETON                                        32                            
      *                                                                                             
      * IF CONTRACT ORDER NUMBER REQUIRED.......                                                    
      *                                                                                             
     C                   SETOFF                                       4346                          
     C     CHONRQ        IFEQ      'I'                                                              
     C                   SETON                                        43                            
     C                   ENDIF                                                                      
     C     CHONRQ        IFEQ      'M'                                                              
     C                   SETON                                        46                            
     C                   ENDIF                                                                      
     C     CHONRQ        IFEQ      'X'                                                              
     C                   SETON                                        74                            
     C                   ENDIF                                                                      
     C                   END                                                                        
     C                   MOVEL     CHSNAM        WHSNAM                                             
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
     C                   ENDIF                                                                      
     C     CHSTAT        IFEQ      'D'                                                              
     C                   MOVEL     'DELETED '    WMODE                                              
     C                   ENDIF                                                                      
     C*                                                                                             
     C* GET MARGIN FROM CUSTOMER OR LEDGER                                                          
     C*                                                                                             
     C                   Z-ADD     0             ZZMARG           11 2                              
 ISL  *                                                                                             
 ISL C                   MOVE      *BLANK        @@5U01          001                                
 ISL C                   EXSR      SBR013                                                           
 ISL C                   MOVE      *OFF          *IN21                                              
 ISL C     @@5P04        IFEQ      '1'                                                              
 ISL C     @@5P04        OREQ      '9'                                                              
 ISL C     X@IND1        IFEQ      '1'                                                              
 ISL C                   MOVE      *ON           *IN21                                              
 ISL C                   ENDIF                                                                      
 ISL C                   ENDIF                                                                      
 ISL  *                                                                                             
     C     *IN21         IFEQ      *OFF                                                             
     C     CDMARG        ANDNE     0                                                                
     C                   Z-ADD     CDMARG        ZZMARG                                             
     C                   ELSE                                                                       
     C                   Z-ADD     LCMARG        ZZMARG                                             
     C                   END                                                                        
     C*                                                                                             
     C*                                                                                             
     C                   SETOFF                                       90                            
     C*                                                                                             
     C                   ENDSR                                                                      
     C**************************************************************************                    
     C*                                                                                             
     C     *LIKE         DEFINE    SFRCD1        ERRECD                                             
     C     *LIKE         DEFINE    SFRCD1        WKRECD                                             
     C     *LIKE         DEFINE    SFRCD1        NBRSLT                                             
     C     *LIKE         DEFINE    CWSPI1        WWSPI1                                             
     C     *LIKE         DEFINE    CWSPI2        WWSPI2                                             
     C     *LIKE         DEFINE    CWSPI3        WWSPI3                                             
     C     *LIKE         DEFINE    CWSPI4        WWSPI4                                             
     C     *LIKE         DEFINE    CWSPI5        WWSPI5                                             
     C     *LIKE         DEFINE    CWSPI6        WWSPI6                                             
     C*                                                                                             
     C     CDKEY         KLIST                                                                      
     C                   KFLD                    CHCCOD                                             
     C                   KFLD                    CHCSUB                                             
     C*                                                                                             
     C     CWKEY         KLIST                                                                      
     C                   KFLD                    CWCNTN                                             
     C                   KFLD                    CWITEM                                             
     C*                                                                                             
     C     CWKEY1        KLIST                                                                      
     C                   KFLD                    WHCNTN                                             
     C                   KFLD                    S1ITEM                                             
     C*                                                                                             
     C***  CWKEY2        KLIST                                                                      
     C***                KFLD                    WHCNTN                                             
     C***                KFLD                    S1SUPL                                             
     C***                KFLD                    S1ITEM                                             
     C*                                                                                             
     C     CWKEY3        KLIST                                                                      
     C                   KFLD                    WHCNTN                                             
     C                   KFLD                    WWITEM                                             
     C*                                                                                             
     C     CWKEY4        KLIST                                                                      
     C                   KFLD                    WHCNTN                                             
     C                   KFLD                    S3SUPL                                             
     C*                                                                                             
     C     CWKEY5        KLIST                                                                      
     C                   KFLD                    WHCNTN                                             
     C                   KFLD                    ZZHIVL                                             
     C*                                                                                             
     C     CWKEY6        KLIST                                                                      
     C                   KFLD                    WHCNTN                                             
     C                   KFLD                    BTITEM                                             
     C*                                                                                             
     C     CWKEY8        KLIST                                                                      
     C                   KFLD                    WHCNTN                                             
     C                   KFLD                    S3SUPL                                             
     C                   KFLD                    BTITEM                                             
     C*                                                                                             
     C     MVKEY         KLIST                                                                      
     C                   KFLD                    WHCNTN                                             
     C                   KFLD                    WWITEM                                             
     C*                                                                                             
     C     MVKEY1        KLIST                                                                      
     C                   KFLD                    WHCNTN                                             
     C                   KFLD                    WWITEM                                             
     C                   KFLD                    ZZMVDT                                             
     C                   KFLD                    BTBOOK                                             
     C                   KFLD                    BTMVNO                                             
     C*                                                                                             
     C     MVKEY2        KLIST                                                                      
     C                   KFLD                    WHCNTN                                             
     C                   KFLD                    WWITEM                                             
     C                   KFLD                    S5MVNO                                             
     C*                                                                                             
     C     ACKEY         KLIST                                                                      
     C                   KFLD                    CWPROD                                             
     C                   KFLD                    CHCCOD                                             
     C                   KFLD                    CHCSUB                                             
     C*                                                                                             
     C     MVKEY4        KLIST                                                                      
     C                   KFLD                    WHCNTN                                             
     C                   KFLD                    WWITEM                                             
     C                   KFLD                    S5MVNO                                             
     C*                                                                                             
     C     ANKEY         KLIST                                                                      
     C                   KFLD                    CWPROD                                             
     C                   KFLD                    CHA03                                              
     C*                                                                                             
     C     CWZKEY        KLIST                                                                      
     C                   KFLD                    WHCNTN                                             
     C                   KFLD                    WWITEM                                             
     C*                                                                                             
 ISL  /COPY CMGE05C                                                                                 
                                                                                                    
      //========================================================================                    
      // This subprocedure checks to see if the contract item is on the filter                      
      // list; returns boolean true if the item is on the filter list                               
      //========================================================================                    
     p onFilterList...                                                                              
     p                 b                                                                            
                                                                                                    
     d onFilterList...                                                                              
     d                 pi              n                                                            
     d   pContract                    7p 0 const                                                    
     d   pItem                        3p 0 const                                                    
                                                                                                    
       // CMGI05 item filter list user space header record                                          
     d filterListHeader...                                                                          
     d                 ds                  qualified                                                
     d   size                        10i 0 inz(%size(filterListHeader))                             
     d   entryCount                  10i 0                                                          
     d   filterDescription...                                                                       
     d                               40a                                                            
       // CMGI05 item filter list user space entry record                                           
     d filterListEntry...                                                                           
     d                 ds                  qualified                                                
     d   size                        10i 0 inz(%size(filterListEntry))                              
     d   contract                     7p 0                                                          
     d   item                         3p 0                                                          
                                                                                                    
                                                                                                    
     d startpos        s             10i 0                                                          
     d index           s              5i 0                                                          
                                                                                                    
                                                                                                    
      /free                                                                                         
                                                                                                    
       // Retrieve the header record                                                                
       filterListHeader =                                                                           
         UsrSpc_retrieve( CMGI05_USRSPC_LIBRARY : CMGI05_USRSPC_LIST :                              
           1 : filterListHeader.size );                                                             
                                                                                                    
                                                                                                    
       // Walk through all entries on the filter list user                                          
       // searching for this contract item                                                          
       startPos = 1;                                                                                
       for index = 1 to filterListHeader.entryCount;                                                
                                                                                                    
         startPos = 1 + filterListHeader.size +                                                     
           ( ( index -1 ) * filterListEntry.size );                                                 
                                                                                                    
         filterListEntry =                                                                          
           UsrSpc_retrieve( CMGI05_USRSPC_LIBRARY : CMGI05_USRSPC_LIST :                            
             startPos : filterListEntry.size );                                                     
                                                                                                    
         if ( filterListEntry.contract = pContract and                                              
              filterListEntry.item = pItem );                                                       
           return true; // on the filter list                                                       
         endif;                                                                                     
                                                                                                    
       endfor;                                                                                      
                                                                                                    
                                                                                                    
       return false; // not on the filter list                                                      
                                                                                                    
      /end-free                                                                                     
                                                                                                    
     p onFilterList...                                                                              
     p                 e                                                                            
                                                                                                    
      //========================================================================                    
      // This subprocedure checks to see if the contract item is on the filter                      
      // list; returns boolean true if the item is on the filter list                               
      //========================================================================                    
     p getFilterListDescription...                                                                  
     p                 b                                                                            
                                                                                                    
     d getFilterListDescription...                                                                  
     d                 pi            40a                                                            
                                                                                                    
       // CMGI05 item filter list user space header record                                          
     d filterListHeader...                                                                          
     d                 ds                  qualified                                                
     d   size                        10i 0 inz(%size(filterListHeader))                             
     d   entryCount                  10i 0                                                          
     d   filterDescription...                                                                       
     d                               40a                                                            
                                                                                                    
      /free                                                                                         
                                                                                                    
       // Retrieve the header record                                                                
       filterListHeader =                                                                           
         UsrSpc_retrieve( CMGI05_USRSPC_LIBRARY : CMGI05_USRSPC_LIST :                              
           1 : filterListHeader.size );                                                             
                                                                                                    
       return filterListHeader.filterDescription;                                                   
                                                                                                    
      /end-free                                                                                     
                                                                                                    
     p getFilterListDescription...                                                                  
     p                 e                                                                            
                                                                                                    
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