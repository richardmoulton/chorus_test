 ISL  **************************************************************************                    
 ISL C     SBR001        BEGSR                                                                      
 ISL  **************************************************************************                    
 ISL C                   MOVEA     *IN(01)       IN0199          099                                
 ISL  *                                                                                             
 ISL C                   MOVE      'X'           INSR01          001                                
 ISL  *                                                                                             
 ISL C                   MOVE      *BLANKS       KEY001          006                                
 ISL C                   MOVE      *BLANKS       KEY002          003                                
 ISL  *                                                                                             
 ISL C                   MOVE      CHCCOD        KEY001                                             
 ISL C                   MOVE      CHCSUB        KEY002                                             
 ISL  *                                                                                             
 ISL C                   CALL      'XXSSLCD'                                                        
 ISL C                   PARM                    KEY001                                             
 ISL C                   PARM                    KEY002                                             
 ISL C                   PARM      '3'           @@5P01          001                                
 ISL C                   PARM      '7'           @@5P02          001                                
 ISL C                   PARM      'W'           @@5P03          001                                
 ISL C                   PARM      *BLANKS       @@5P04          001                                
 ISL C                   PARM      *BLANKS       @@5P05          080                                
 ISL C                   PARM      'I'           @@5P06          001                                
 ISL  *                                                                                             
 ISL C                   IN        XSSLCD                                                           
 ISL  *                                                                                             
 ISL C                   MOVE      A@CONT        CDCONT                                             
 ISL C                   MOVE      A@STOP        CDSTOP                                             
 ISL  *                                                                                             
 ISL C                   IN        X@INDS                                                           
 ISL  *                                                                                             
 ISL C     @@5U01        IFEQ      'Y'                                                              
 ISL C     @@5P04        ANDNE     '1'                                                              
 ISL C                   CALL      'XXSSLCD'                                                        
 ISL C                   PARM                    KEY001                                             
 ISL C                   PARM                    KEY002                                             
 ISL C                   PARM      '4'           @@5P01          001                                
 ISL C                   PARM      '0'           @@5P02          001                                
 ISL C                   PARM      'W'           @@5P03          001                                
 ISL C                   PARM      *BLANKS       @@5P04          001                                
 ISL C                   PARM      *BLANKS       @@5P05          080                                
 ISL C                   PARM      'I'           @@5P06          001                                
 ISL C                   ENDIF                                                                      
 ISL  *                                                                                             
 ISL C                   MOVEA     IN0199        *IN(01)                                            
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   ENDSR                                                                      
 ISL  **************************************************************************                    
 ISL C     SBR002        BEGSR                                                                      
 ISL  **************************************************************************                    
 ISL C                   MOVE      *BLANKS       KEY003          006                                
 ISL C                   MOVE      *BLANKS       KEY004          003                                
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   CALL      'XXSSLCD'                                                        
 ISL C                   PARM                    KEY003                                             
 ISL C                   PARM                    KEY004                                             
 ISL C                   PARM      '0'           @@5P01          001                                
 ISL C                   PARM      '9'           @@5P02          001                                
 ISL C                   PARM      'O'           @@5P03          001                                
 ISL C                   PARM      *BLANKS       @@5P04          001                                
 ISL C                   PARM      *BLANKS       @@5P05          080                                
 ISL C                   PARM      'I'           @@5P06          001                                
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   ENDSR                                                                      
 ISL  **************************************************************************                    
 ISL C     SBR003        BEGSR                                                                      
 ISL  **************************************************************************                    
 ISL C                   MOVEA     *IN(01)       IN0199          099                                
 ISL  *                                                                                             
 ISL C                   CLEAR                   XAPDST                                             
 ISL C                   MOVEL     @@ER01        FDA001                                             
 ISL C                   MOVEL     @@ER02        FDA002                                             
 ISL C                   MOVE      *BLANKS       @@EWK1          010                                
 ISL C                   CAT       CHCCOD:0      @@EWK1                                             
 ISL C     CHCSUB        IFNE      *BLANKS                                                          
 ISL C                   CAT       @@ER04:0      @@EWK1                                             
 ISL C                   CAT       CHCSUB:0      @@EWK1                                             
 ISL C                   ENDIF                                                                      
 ISL C                   MOVEL     @@EWK1        FDA003                                             
 ISL  *                                                                                             
 ISL C                   MOVEL     @@EWK1        FDA020                                             
 ISL  *                                                                                             
 ISL C                   MOVE      *BLANKS       @@EWK1          010                                
 ISL C     @@ER05        CAT       CHCCOD:0      @@EWK1                                             
 ISL C                   MOVEL     @@EWK1        FDA005                                             
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   CALL      'XAPA86A'                                                        
 ISL C                   PARM      *BLANKS       @@EP01          100                                
 ISL C                   PARM      *BLANKS       @@EP02          010                                
 ISL C                   PARM      'FETCH'       @@EP03          010                                
 ISL C                   PARM      *BLANKS       @@EP04          010                                
 ISL C                   PARM      *BLANKS       @@EP05          080                                
 ISL C                   PARM      0718          @@EP06          004 0                              
 ISL C                   PARM                    XAPDST                                             
 ISL C                   PARM                    CDSTOP            1                                
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   CLEAR                   XBDSIO                                             
 ISL C                   MOVEL     @@ER01        FDB001                                             
 ISL C                   MOVEL     @@ER02        FDB003                                             
 ISL C                   MOVE      *BLANKS       @@EWK1          010                                
 ISL C                   CAT       CHCCOD:0      @@EWK1                                             
 ISL C     CHCSUB        IFNE      *BLANKS                                                          
 ISL C                   CAT       @@ER04:0      @@EWK1                                             
 ISL C                   CAT       CHCSUB:0      @@EWK1                                             
 ISL C                   ENDIF                                                                      
 ISL C                   MOVEL     @@EWK1        FDB002                                             
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   CALL      'XAPA98'                                                         
 ISL C                   PARM      *BLANKS       @@EP01          100                                
 ISL C                   PARM      *BLANKS       @@EP02          010                                
 ISL C                   PARM      'FETCH'       @@EP03          010                                
 ISL C                   PARM      *BLANKS       @@EP07          010                                
 ISL C                   PARM      *BLANKS       @@EP05          080                                
 ISL C                   PARM      0167          @@EP06          004 0                              
 ISL C                   PARM                    XBDSIO                                             
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   CLEAR                   XCDSIO                                             
 ISL C                   MOVEL     @@ER01        FDC001                                             
 ISL C                   MOVEL     @@ER02        FDC003                                             
 ISL C                   MOVE      *BLANKS       @@EWK1          010                                
 ISL C                   CAT       CHCCOD:0      @@EWK1                                             
 ISL C     CHCSUB        IFNE      *BLANKS                                                          
 ISL C                   CAT       @@ER04:0      @@EWK1                                             
 ISL C                   CAT       CHCSUB:0      @@EWK1                                             
 ISL C                   ENDIF                                                                      
 ISL C                   MOVEL     @@EWK1        FDC002                                             
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   CALL      'XCSLOO'                                                         
 ISL C                   PARM      *BLANKS       @@EP01          100                                
 ISL C                   PARM      *BLANKS       @@EP02          010                                
 ISL C                   PARM      'FETCH'       @@EP03          010                                
 ISL C                   PARM      *BLANKS       @@EP07          010                                
 ISL C                   PARM      *BLANKS       @@EP05          080                                
 ISL C                   PARM      0059          @@EP06          004 0                              
 ISL C                   PARM                    XCDSIO                                             
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   MOVEA     IN0199        *IN(01)                                            
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   ENDSR                                                                      
 ISL  **************************************************************************                    
 ISL C     SBR004        BEGSR                                                                      
 ISL  **************************************************************************                    
 ISL C                   MOVEA     *IN(01)       IN0199          099                                
 ISL  *                                                                                             
 ISL C                   MOVE      'X'           INSR02          001                                
 ISL  *                                                                                             
 ISL C                   MOVE      *BLANKS       KEY005          006                                
 ISL C                   MOVE      *BLANKS       KEY006          003                                
 ISL  *                                                                                             
 ISL C                   MOVE      CHCCOD        KEY005                                             
 ISL C                   MOVE      CHCSUB        KEY006                                             
 ISL  *                                                                                             
 ISL C                   CALL      'XXSSLCDS'                                                       
 ISL C                   PARM                    KEY005                                             
 ISL C                   PARM                    KEY006                                             
 ISL C                   PARM      '3'           @@5P01          001                                
 ISL C                   PARM      '7'           @@5P02          001                                
 ISL C                   PARM      'W'           @@5P03          001                                
 ISL C                   PARM      *BLANKS       @@5P04          001                                
 ISL C                   PARM      *BLANKS       @@5P05          080                                
 ISL C                   PARM      'I'           @@5P06          001                                
 ISL  *                                                                                             
 ISL C                   IN        XSSLXD                                                           
 ISL  *                                                                                             
 ISL C                   MOVE      B@CONT        CDCONT                                             
 ISL  *                                                                                             
 ISL C                   IN        X@INDS                                                           
 ISL  *                                                                                             
 ISL C     @@5U01        IFEQ      'Y'                                                              
 ISL C     @@5P04        ANDNE     '1'                                                              
 ISL C                   CALL      'XXSSLCDS'                                                       
 ISL C                   PARM                    KEY005                                             
 ISL C                   PARM                    KEY006                                             
 ISL C                   PARM      '4'           @@5P01          001                                
 ISL C                   PARM      '0'           @@5P02          001                                
 ISL C                   PARM      'W'           @@5P03          001                                
 ISL C                   PARM      *BLANKS       @@5P04          001                                
 ISL C                   PARM      *BLANKS       @@5P05          080                                
 ISL C                   PARM      'I'           @@5P06          001                                
 ISL C                   ENDIF                                                                      
 ISL  *                                                                                             
 ISL C                   MOVEA     IN0199        *IN(01)                                            
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   ENDSR                                                                      
 ISL  **************************************************************************                    
 ISL C     SBR005        BEGSR                                                                      
 ISL  **************************************************************************                    
 ISL C                   MOVE      *BLANKS       KEY007          006                                
 ISL C                   MOVE      *BLANKS       KEY008          003                                
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   CALL      'XXSSLCDS'                                                       
 ISL C                   PARM                    KEY007                                             
 ISL C                   PARM                    KEY008                                             
 ISL C                   PARM      '0'           @@5P01          001                                
 ISL C                   PARM      '9'           @@5P02          001                                
 ISL C                   PARM      'O'           @@5P03          001                                
 ISL C                   PARM      *BLANKS       @@5P04          001                                
 ISL C                   PARM      *BLANKS       @@5P05          080                                
 ISL C                   PARM      'I'           @@5P06          001                                
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   ENDSR                                                                      
 ISL  **************************************************************************                    
 ISL C     SBR006        BEGSR                                                                      
 ISL  **************************************************************************                    
 ISL C                   MOVEA     *IN(01)       IN0199          099                                
 ISL  *                                                                                             
 ISL C                   MOVE      'X'           INSR03          001                                
 ISL  *                                                                                             
 ISL C                   Z-ADD     *ZEROS        KEY009          002 0                              
 ISL C                   MOVE      *BLANKS       KEY010          004                                
 ISL  *                                                                                             
 ISL C                   MOVE      ANTYPE        KEY009                                             
 ISL C                   MOVE      ANCODE        KEY010                                             
 ISL  *                                                                                             
 ISL C                   CALL      'XXSSAMD'                                                        
 ISL C                   PARM                    KEY009                                             
 ISL C                   PARM                    KEY010                                             
 ISL C                   PARM      '3'           @@5P01          001                                
 ISL C                   PARM      '7'           @@5P02          001                                
 ISL C                   PARM      'W'           @@5P03          001                                
 ISL C                   PARM      *BLANKS       @@5P04          001                                
 ISL C                   PARM      *BLANKS       @@5P05          080                                
 ISL C                   PARM      'I'           @@5P06          001                                
 ISL  *                                                                                             
 ISL C                   IN        XSSAMD                                                           
 ISL  *                                                                                             
 ISL C                   MOVE      C@DESC        MDDESC                                             
 ISL  *                                                                                             
 ISL C                   IN        X@INDS                                                           
 ISL  *                                                                                             
 ISL C     @@5U01        IFEQ      'Y'                                                              
 ISL C     @@5P04        ANDNE     '1'                                                              
 ISL C                   CALL      'XXSSAMD'                                                        
 ISL C                   PARM                    KEY009                                             
 ISL C                   PARM                    KEY010                                             
 ISL C                   PARM      '4'           @@5P01          001                                
 ISL C                   PARM      '0'           @@5P02          001                                
 ISL C                   PARM      'W'           @@5P03          001                                
 ISL C                   PARM      *BLANKS       @@5P04          001                                
 ISL C                   PARM      *BLANKS       @@5P05          080                                
 ISL C                   PARM      'I'           @@5P06          001                                
 ISL C                   ENDIF                                                                      
 ISL  *                                                                                             
 ISL C                   MOVEA     IN0199        *IN(01)                                            
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   ENDSR                                                                      
 ISL  **************************************************************************                    
 ISL C     SBR007        BEGSR                                                                      
 ISL  **************************************************************************                    
 ISL C                   Z-ADD     *ZEROS        KEY011          002 0                              
 ISL C                   MOVE      *BLANKS       KEY012          004                                
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   CALL      'XXSSAMD'                                                        
 ISL C                   PARM                    KEY011                                             
 ISL C                   PARM                    KEY012                                             
 ISL C                   PARM      '0'           @@5P01          001                                
 ISL C                   PARM      '9'           @@5P02          001                                
 ISL C                   PARM      'O'           @@5P03          001                                
 ISL C                   PARM      *BLANKS       @@5P04          001                                
 ISL C                   PARM      *BLANKS       @@5P05          080                                
 ISL C                   PARM      'I'           @@5P06          001                                
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   ENDSR                                                                      
 ISL  **************************************************************************                    
 ISL C     SBR008        BEGSR                                                                      
 ISL  **************************************************************************                    
 ISL C                   MOVEA     *IN(01)       IN0199          099                                
 ISL  *                                                                                             
 ISL C                   CLEAR                   XDDSIO                                             
 ISL C                   MOVEL     @@ER01        FDD001                                             
 ISL C                   MOVEL     ANTYPE        FDD002                                             
 ISL C                   MOVEL     ANCODE        FDD003                                             
 ISL  *                                                                                             
 ISL C                   CALL      'XAPA26'                                                         
 ISL C                   PARM      *BLANKS       @@EP01          100                                
 ISL C                   PARM      *BLANKS       @@EP02          010                                
 ISL C                   PARM      'FETCH'       @@EP03          010                                
 ISL C                   PARM      *BLANKS       @@EP04          010                                
 ISL C                   PARM      *BLANKS       @@EP05          080                                
 ISL C                   PARM      0062          @@EP06          004 0                              
 ISL C                   PARM                    XDDSIO                                             
 ISL  *                                                                                             
 ISL C                   MOVEL     FDD004        MDDESC                                             
 ISL  *                                                                                             
 ISL C                   MOVEA     IN0199        *IN(01)                                            
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   ENDSR                                                                      
 ISL  **************************************************************************                    
 ISL C     SBR009        BEGSR                                                                      
 ISL  **************************************************************************                    
 ISL C                   MOVEA     *IN(01)       IN0199          099                                
 ISL  *                                                                                             
 ISL C                   MOVE      'X'           INSR04          001                                
 ISL  *                                                                                             
 ISL C                   Z-ADD     *ZERO         LDGKEY          001 0                              
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   CALL      'XXSSLSC'                                                        
 ISL C                   PARM                    LDGKEY                                             
 ISL C                   PARM      '3'           @@5P01          001                                
 ISL C                   PARM      '7'           @@5P02          001                                
 ISL C                   PARM      'W'           @@5P03          001                                
 ISL C                   PARM      *BLANKS       @@5P04          001                                
 ISL C                   PARM      *BLANKS       @@5P05          080                                
 ISL C                   PARM      'I'           @@5P06          001                                
 ISL  *                                                                                             
 ISL C                   IN        XSSLSC                                                           
 ISL  *                                                                                             
 ISL C                   MOVE      D@A01         SCA(1)                                             
 ISL C                   MOVE      D@A02         SCA(2)                                             
 ISL C                   MOVE      D@A03         SCA(3)                                             
 ISL C                   MOVE      D@A04         SCA(4)                                             
 ISL C                   MOVE      D@A05         SCA(5)                                             
 ISL C                   MOVE      D@A06         SCA(6)                                             
 ISL C                   MOVE      D@A07         SCA(7)                                             
 ISL C                   MOVE      D@A08         SCA(8)                                             
 ISL C                   MOVE      D@A09         SCA(9)                                             
 ISL C                   MOVE      D@A10         SCA(10)                                            
 ISL C                   MOVE      D@A11         SCA(11)                                            
 ISL C                   MOVE      D@A12         SCA(12)                                            
 ISL C                   MOVE      D@A13         SCA(13)                                            
 ISL C                   MOVE      D@A14         SCA(14)                                            
 ISL C                   MOVE      D@A15         SCA(15)                                            
 ISL C                   MOVE      D@A16         SCA(16)                                            
 ISL C                   MOVE      D@A17         SCA(17)                                            
 ISL C                   MOVE      D@A18         SCA(18)                                            
 ISL C                   MOVE      D@A19         SCA(19)                                            
 ISL C                   MOVE      D@A20         SCA(20)                                            
 ISL  *                                                                                             
 ISL C                   IN        X@INDS                                                           
 ISL  *                                                                                             
 ISL C     @@5U01        IFEQ      'Y'                                                              
 ISL C     @@5P04        ANDNE     '1'                                                              
 ISL C                   CALL      'XXSSLSC'                                                        
 ISL C                   PARM                    LDGKEY                                             
 ISL C                   PARM      '4'           @@5P01          001                                
 ISL C                   PARM      '0'           @@5P02          001                                
 ISL C                   PARM      'W'           @@5P03          001                                
 ISL C                   PARM      *BLANKS       @@5P04          001                                
 ISL C                   PARM      *BLANKS       @@5P05          080                                
 ISL C                   PARM      'I'           @@5P06          001                                
 ISL C                   ENDIF                                                                      
 ISL  *                                                                                             
 ISL C                   MOVEA     IN0199        *IN(01)                                            
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   ENDSR                                                                      
 ISL  **************************************************************************                    
 ISL C     SBR010        BEGSR                                                                      
 ISL  **************************************************************************                    
 ISL C                   Z-ADD     *ZERO         LDGKEY          001 0                              
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   CALL      'XXSSLSC'                                                        
 ISL C                   PARM                    LDGKEY                                             
 ISL C                   PARM      '0'           @@5P01          001                                
 ISL C                   PARM      '9'           @@5P02          001                                
 ISL C                   PARM      'O'           @@5P03          001                                
 ISL C                   PARM      *BLANKS       @@5P04          001                                
 ISL C                   PARM      *BLANKS       @@5P05          080                                
 ISL C                   PARM      'I'           @@5P06          001                                
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   ENDSR                                                                      
 ISL  **************************************************************************                    
 ISL C     SBR011        BEGSR                                                                      
 ISL  **************************************************************************                    
 ISL C                   MOVEA     *IN(01)       IN0199          099                                
 ISL  *                                                                                             
 ISL C                   CLEAR                   XEDSIO                                             
 ISL C                   MOVEL     @@ER01        FDE001                                             
 ISL C                   MOVEL     @@ER02        FDE002                                             
 ISL  *                                                                                             
 ISL C                   CALL      'XAPA77'                                                         
 ISL C                   PARM      *BLANKS       @@EP01          100                                
 ISL C                   PARM      *BLANKS       @@EP02          010                                
 ISL C                   PARM      'FETCH'       @@EP03          010                                
 ISL C                   PARM      *BLANKS       @@EP04          010                                
 ISL C                   PARM      *BLANKS       @@EP05          080                                
 ISL C                   PARM      0093          @@EP06          004 0                              
 ISL C                   PARM                    XEDSIO                                             
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   CLEAR                   XFDSIO                                             
 ISL  *                                                                                             
 ISL C                   CALL      'XAPA94'                                                         
 ISL C                   PARM      *BLANKS       @@EP01          100                                
 ISL C                   PARM      *BLANKS       @@EP02          010                                
 ISL C                   PARM      'FETCH'       @@EP03          010                                
 ISL C                   PARM      *BLANKS       @@EP07          010                                
 ISL C                   PARM      *BLANKS       @@EP05          080                                
 ISL C                   PARM      0273          @@EP06          004 0                              
 ISL C                   PARM                    XFDSIO                                             
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   CLEAR                   XGDSIO                                             
 ISL  *                                                                                             
 ISL C                   CALL      'XAPA95'                                                         
 ISL C                   PARM      *BLANKS       @@EP01          100                                
 ISL C                   PARM      *BLANKS       @@EP02          010                                
 ISL C                   PARM      'FETCH'       @@EP03          010                                
 ISL C                   PARM      *BLANKS       @@EP07          010                                
 ISL C                   PARM      *BLANKS       @@EP05          080                                
 ISL C                   PARM      1200          @@EP06          004 0                              
 ISL C                   PARM                    XGDSIO                                             
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   CLEAR                   XHDSIO                                             
 ISL C                   MOVEL     @@ER01        FDH001                                             
 ISL C                   MOVEL     @@ER02        FDH002                                             
 ISL  *                                                                                             
 ISL C                   CALL      'XCSLCP'                                                         
 ISL C                   PARM      *BLANKS       @@EP01          100                                
 ISL C                   PARM      *BLANKS       @@EP02          010                                
 ISL C                   PARM      'FETCH'       @@EP03          010                                
 ISL C                   PARM      *BLANKS       @@EP07          010                                
 ISL C                   PARM      *BLANKS       @@EP05          080                                
 ISL C                   PARM      0080          @@EP06          004 0                              
 ISL C                   PARM                    XHDSIO                                             
 ISL  *                                                                                             
 ISL C                   MOVEL     'Y'           SCMCYN          001                                
 ISL  *                                                                                             
 ISL C                   CLEAR                   XIDSIO                                             
 ISL C                   MOVEL     @@ER01        FDI001                                             
 ISL C                   MOVEL     @@ER02        FDI002                                             
 ISL  *                                                                                             
 ISL C**                 CALL      'XCSLRA'                                                         
 ISL C                   CALL      'XAPAA7'                                                         
 ISL C                   PARM      *BLANKS       @@EP01          100                                
 ISL C                   PARM      *BLANKS       @@EP02          010                                
 ISL C                   PARM      'FETCH'       @@EP03          010                                
 ISL C                   PARM      *BLANKS       @@EP07          010                                
 ISL C                   PARM      *BLANKS       @@EP05          080                                
 ISL C                   PARM      0430          @@EP06          004 0                              
 ISL C                   PARM                    XIDSIO                                             
 ISL  *                                                                                             
 ISL C                   MOVEL     FDI004        SCA(1)                                             
 ISL C                   MOVEL     FDI006        SCA(2)                                             
 ISL C                   MOVEL     FDI008        SCA(3)                                             
 ISL C                   MOVEL     FDI010        SCA(4)                                             
 ISL C                   MOVEL     FDI012        SCA(5)                                             
 ISL C                   MOVEL     FDI014        SCA(6)                                             
 ISL C                   MOVEL     FDI016        SCA(7)                                             
 ISL C                   MOVEL     FDI018        SCA(8)                                             
 ISL C                   MOVEL     FDI020        SCA(9)                                             
 ISL C                   MOVEL     FDI022        SCA(10)                                            
 ISL C                   MOVEL     FDI024        SCA(11)                                            
 ISL C                   MOVEL     FDI026        SCA(12)                                            
 ISL C                   MOVEL     FDI028        SCA(13)                                            
 ISL C                   MOVEL     FDI030        SCA(14)                                            
 ISL C                   MOVEL     FDI032        SCA(15)                                            
 ISL C                   MOVEL     FDI034        SCA(16)                                            
 ISL C                   MOVEL     FDI036        SCA(17)                                            
 ISL C                   MOVEL     FDI038        SCA(18)                                            
 ISL C                   MOVEL     FDI040        SCA(19)                                            
 ISL C                   MOVEL     FDI042        SCA(20)                                            
 ISL  *                                                                                             
 ISL C                   DO        20            Y                 3 0                              
 ISL C                   CLEAR                   XRDSIO                                             
 ISL C                   MOVEL     @@ER01        FDR001                                             
 ISL C                   MOVEL     SCA(Y)        FDR002                                             
 ISL C                   CALL      'XCTRAC'                                                         
 ISL C                   PARM      *BLANKS       @@EP01          100                                
 ISL C                   PARM      *BLANKS       @@EP02          010                                
 ISL C                   PARM      'FETCH'       @@EP03          010                                
 ISL C                   PARM      *BLANKS       @@EP07          010                                
 ISL C                   PARM      *BLANKS       @@EP05          080                                
 ISL C                   PARM      0070          @@EP06          004 0                              
 ISL C                   PARM                    XRDSIO                                             
 ISL C                   MOVEL     FDR004        SCA(Y)                                             
 ISL C                   ENDDO                                                                      
 ISL  *                                                                                             
 ISL C                   MOVEA     IN0199        *IN(01)                                            
 ISL  *                                                                                             
 ISL  *                                                                                             
 ISL C                   ENDSR                                                                      
 ISL  **************************************************************************                    
 ISL C     SBR012        BEGSR                                                                      
 ISL  **************************************************************************                    
 ISL C     INSR03        IFNE      *BLANKS                                                          
 ISL C                   EXSR      SBR007                                                           
 ISL C                   ENDIF                                                                      
 ISL C     INSR02        IFNE      *BLANKS                                                          
 ISL C                   EXSR      SBR005                                                           
 ISL C                   ENDIF                                                                      
 ISL C     INSR01        IFNE      *BLANKS                                                          
 ISL C                   EXSR      SBR002                                                           
 ISL C                   ENDIF                                                                      
 ISL C     INSR04        IFNE      *BLANKS                                                          
 ISL C                   EXSR      SBR010                                                           
 ISL C                   ENDIF                                                                      
 ISL  *                                                                                             
 ISL C                   ENDSR       