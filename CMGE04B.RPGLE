 ISL C     *DTAARA       DEFINE                  XSSLCD                                             
 ISL C                   IN        XSSLCD                                                           
 ISL C                   UNLOCK    XSSLCD                                                           
 ISL C     *DTAARA       DEFINE                  X@INDS                                             
 ISL C                   IN        X@INDS                                                           
 ISL C                   UNLOCK    X@INDS                                                           
 ISL  *                                                                                             
 ISL C                   MOVE      *BLANKS       CDCONT          020                                
 ISL C                   MOVE      *BLANKS       CDSTOP          001                                
 ISL C     *DTAARA       DEFINE                  XSSLXD                                             
 ISL C                   IN        XSSLXD                                                           
 ISL C                   UNLOCK    XSSLXD                                                           
 ISL  *                                                                                             
 ISL C                   MOVE      *BLANKS       CDCONT          020                                
 ISL C     *DTAARA       DEFINE                  XSSAMD                                             
 ISL C                   IN        XSSAMD                                                           
 ISL C                   UNLOCK    XSSAMD                                                           
 ISL  *                                                                                             
 ISL C                   MOVE      *BLANKS       MDDESC          020                                
 ISL C     *DTAARA       DEFINE                  XSSLSC                                             
 ISL C                   IN        XSSLSC                                                           
 ISL C                   UNLOCK    XSSLSC                                                           
 ISL  *                                                                                             
 ISL C                   MOVE      *BLANKS       SCA(1)          010                                
 ISL C                   MOVE      *BLANKS       SCA(2)          010                                
 ISL C                   MOVE      *BLANKS       SCA(3)          010                                
 ISL C                   MOVE      *BLANKS       SCA(4)          010                                
 ISL C                   MOVE      *BLANKS       SCA(5)          010                                
 ISL C                   MOVE      *BLANKS       SCA(6)          010                                
 ISL C                   MOVE      *BLANKS       SCA(7)          010                                
 ISL C                   MOVE      *BLANKS       SCA(8)          010                                
 ISL C                   MOVE      *BLANKS       SCA(9)          010                                
 ISL C                   MOVE      *BLANKS       SCA(10)         010                                
 ISL C                   MOVE      *BLANKS       SCA(11)         010                                
 ISL C                   MOVE      *BLANKS       SCA(12)         010                                
 ISL C                   MOVE      *BLANKS       SCA(13)         010                                
 ISL C                   MOVE      *BLANKS       SCA(14)         010                                
 ISL C                   MOVE      *BLANKS       SCA(15)         010                                
 ISL C                   MOVE      *BLANKS       SCA(16)         010                                
 ISL C                   MOVE      *BLANKS       SCA(17)         010                                
 ISL C                   MOVE      *BLANKS       SCA(18)         010                                
 ISL C                   MOVE      *BLANKS       SCA(19)         010                                
 ISL C                   MOVE      *BLANKS       SCA(20)         010                                
 ISL  *                                                                                             
 ISL C                   CALL      'XAPA90'                                                         
 ISL C                   PARM      *BLANKS       @@EO5F          001                                
 ISL  *                                                                                             
 ISL C     @@EO5F        IFEQ      'E'                                                              
 ISL C                   CALL      'XAPA93'                                                         
 ISL C                   PARM      *BLANKS       @@ER01          010                                
 ISL C                   PARM      *BLANKS       @@ER02          010                                
 ISL C                   PARM      *BLANKS       @@ER03          010                                
 ISL C                   PARM      *BLANKS       @@ER04          001                                
 ISL C                   PARM      *BLANKS       @@ER05          004                                
 ISL C                   ENDIF    