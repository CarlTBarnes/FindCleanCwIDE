                    MEMBER()
!--------------------------
! CbFindCleanClass by Carl Barnes
! FindCleanCwIDE utility code split into a Class for reuse
!--------------------------

    INCLUDE('EQUATES.CLW')
    INCLUDE('CbFindCleanCls.INC'),ONCE

  PRAGMA('link(C%V%DOS%X%%L%.LIB)')

LoadSaveFile FILE,DRIVER('DOS'),PRE(LSFile),CREATE,THREAD
          RECORD
Hunk      STRING(8192)
          END
        END        
ReadOnly_DenyNone    EQUATE(40H)      !00h+40h     !Load for Query
ReadOnly_DenyWrite   EQUATE(20H)      !00h+20h     !Load for Write (may not happen)
!ReadWrite_DenyAll    EQUATE (12H)      !02h+10h   
ReadWrite_DenyWrite  EQUATE(22H)      !02h+20h     !Save Open

                    MAP
                    END

!----------------------------------------
CbFindCleanClass.Construct  PROCEDURE()
!----------------------------------------
Something  STRING('<<Construct>')      !Prevent accidental use of Null and GPF
    CODE
    SELF.NewXmlStr(SIZE(Something))      
    SELF.XmlStr=Something
    CLOSE(LoadSaveFile)
    RETURN
!---------------------------------------
CbFindCleanClass.Destruct  PROCEDURE()
!---------------------------------------
    CODE
    CLOSE(LoadSaveFile)
    DISPOSE(SELF.XmlStr)
    RETURN
!-----------------------------------
CbFindCleanClass.NewXmlStr  PROCEDURE(LONG pNewLen)
!-----------------------------------
    CODE
    IF pNewLen < 1 THEN pNewLen=2.    !Always have something
    SELF.XmlLen=pNewLen
    DISPOSE(SELF.XmlStr)
    SELF.XmlStr &= NEW(STRING(SELF.XmlLen)) !Prevent accidental use and GPF
    RETURN
!-----------------------------------
CbFindCleanClass.NewXmlString  PROCEDURE(STRING pNewString)
!-----------------------------------
    CODE
    SELF.NewXmlStr(SIZE(pNewString))
    SELF.XmlStr = pNewString
    RETURN 
!-----------------------------------
CbFindCleanClass.FileLoad  PROCEDURE(STRING pFileName, BYTE pQuery)!,BOOL
RetBool     BOOL
FileBytes   LONG,AUTO
Slice       LONG,AUTO
EndSlice    LONG,AUTO
    CODE 
    CLEAR(Self.LastError)
    DO LoadRtn 
    IF STATUS(LoadSaveFile) THEN CLOSE(LoadSaveFile).
    RETURN RetBool
    
LoadRtn ROUTINE
    IF ~SELF.FileOpen(pFileName, CHOOSE(~pQuery,ReadOnly_DenyWrite ,ReadOnly_DenyNone )) THEN
        EXIT
    END 
    FileBytes = LoadSaveFile{PROP:FileSize}
    IF FileBytes < 1 THEN 
       SELF.LastError='File is Empty ' & FileBytes
       EXIT    
    END
    SELF.NewXmlStr(FileBytes)
    LOOP Slice = 1 TO FileBytes BY SIZE(LSFile:Hunk)
       EndSlice = Slice + SIZE(LSFile:Hunk) - 1
       IF EndSlice > FileBytes THEN EndSlice = FileBytes.
       GET(LoadSaveFile, Slice+0, EndSlice - Slice + 1)
       IF Errorcode() THEN
          SELF.LastError='GET Error ' & ErrorCode()&' '&Error()  !Should never happen
?         ASSERT(0,'CbFindCleanClass.FileLoad GET(,' & Slice &') '& ErrorCode()&' '& Error())
          BREAK
       END
       SELF.XmlStr[ Slice : EndSlice ] = LSFile:Hunk
    END
    RetBool=TRUE
    EXIT
!-----------------------------------
CbFindCleanClass.FileOpen  PROCEDURE(STRING pFileName, LONG pAccessMask, BOOL pCanCreate=0)!,BOOL
RetBool     BOOL
    CODE 
    CLEAR(Self.LastError)
    CLOSE(LoadSaveFile)  
    LoadSaveFile{PROP:Name}=pFileName 
    IF pCanCreate AND ~EXISTS(pFileName) THEN
       CREATE(LoadSaveFile)
       IF ERRORCODE() THEN 
          SELF.LastError = 'CREATE Error ' & ErrorCode()&' '&Error()
          RETURN False
       END       
    END    
    OPEN(LoadSaveFile, pAccessMask)
    IF ERRORCODE() THEN
       SELF.LastError='OPEN Error ' & ErrorCode()&' '&Error()
    ELSE
       RetBool=TRUE
    END
    RETURN RetBool
!-----------------------------------
CbFindCleanClass.FileSave  PROCEDURE(STRING pFileName)!,BOOL
RetBool     BOOL
FileBytes   LONG,AUTO
Slice       LONG,AUTO
EndSlice    LONG,AUTO
    CODE 
    CLEAR(Self.LastError)
    DO SaveRtn 
    IF STATUS(LoadSaveFile) THEN CLOSE(LoadSaveFile).
    RETURN RetBool
    
SaveRtn ROUTINE
    IF ~SELF.FileOpen(pFileName, ReadWrite_DenyWrite, True) THEN
        EXIT
    END
    EMPTY(LoadSaveFile)     !Could Create? Or Remove and Open Will
    IF ERRORCODE() THEN 
       SELF.LastError = 'EMPTY Error ' & ErrorCode()&' '&Error()
       EXIT
    END
    FileBytes=LEN(CLIP(SELF.XmlStr)) 
    IF FileBytes > 0 THEN
       LOOP Slice = 1 TO FileBytes BY SIZE(LSFile:Hunk)
          EndSlice = Slice + SIZE(LSFile:Hunk) - 1
          IF EndSlice > FileBytes THEN EndSlice = FileBytes.
          LSFile:Hunk = SELF.XmlStr[ Slice : EndSlice ]
          ADD(LoadSaveFile, EndSlice - Slice + 1) 
          IF ERRORCODE() THEN 
             SELF.LastError='ADD Error ' & ErrorCode()&' '&Error() !Disk could be full
             IF Message('Failed ADD at Position ' & Slice & |
                        '||Error:' & ErrorCode() &' '& Error() &|
                        '||File: ' & ErrorFile() ,|
                        'CbFindCleanClass.FileSave',,'Close|Assert')=2 THEN Assert(0,'FileSave') .
             BREAK
          END !If err
       END    !Loop
    END       !If bytes
    RetBool=TRUE
    EXIT
!-----------------------------------
CbFindCleanClass.PatternCount PROCEDURE(LONG PatBegin, LONG PatEnd)!,LONG
CntDlm  LONG
X       LONG,AUTO 
    CODE
    IF ~SELF.SliceXmlIsOk(PatBegin, PatEnd,'PatternCount') THEN
        RETURN 0
    END 
    LOOP X=PatBegin TO PatEnd-1
         IF SELF.XmlStr[ X : X+1 ] = PatternDelimC3BF THEN 
            CntDlm += 1
            X += 1
         END            
    END
    CntDlm += 1
    RETURN CntDlm
!-----------------------------------
CbFindCleanClass.PatternShrink PROCEDURE(LONG PatBegin, LONG PatEnd, LONG MinCount)!,LONG
CntDlm  LONG
X       LONG,AUTO 
LenAfter    LONG
    CODE
    IF ~SELF.SliceXmlIsOk(PatBegin, PatEnd,'PatternTruncate') THEN
        RETURN 0
    END
    IF MinCount < 2 THEN MinCount=2.
    LOOP X=PatBegin TO PatEnd-1
         IF SELF.XmlStr[ X : X+1 ] = PatternDelimC3BF THEN 
            CntDlm += 1
            IF CntDlm >= MinCount THEN 
               LenAfter = X - PatBegin
               SELF.XmlStr[ X : SELF.XmlLen ] = SELF.XmlStr[ PatEnd+1 : SELF.XmlLen ] 
               SELF.XmlLen -= (PatEnd+1 - X) 
               BREAK
            END
            X += 1
         END !if Delim          
    END !loop  x
    RETURN LenAfter
!-----------------------------------
CbFindCleanClass.Patterns2Queue PROCEDURE(PatternQType PatternQ,LONG PatBegin, LONG PatEnd)
Beg1        LONG
End1        LONG
LenUnEscaped  LONG
UnEscCnt    LONG
X           LONG
MaxLenWhat  SHORT                        
ValueLen    LONG,AUTO
ValueTxt    &STRING
!MaxSeenLen  LONG                     
    CODE
    IF ~SELF.SliceXmlIsOk(PatBegin, PatEnd, 'Patterns2Queue') THEN RETURN.
    ValueLen = PatEnd-PatBegin +1 +2  !+2=Delim on End
    ValueTxt &= NEW(STRING(ValueLen))
    ValueTxt = SELF.XmlStr[ PatBegin : PatEnd ] & PatternDelimC3BF  !Add Delim to end
    MaxLenWhat=SIZE(PatternQ.WhatTxt)
    Beg1 = 1
    LOOP X=2 TO ValueLen - 1  !Go 1 past End to get last
         IF ValueTxt[ X : X+1 ]=PatternDelimC3BF THEN
            End1 = X-1
            IF End1 >= Beg1 THEN
               UnEscCnt=SELF.XmlUnEscape(ValueTxt[ Beg1 : End1 ], LenUnEscaped)
                  ! IF MaxSeenLen < LenUnEscaped THEN MaxSeenLen = LenUnEscaped. !If curious of max              
               IF LenUnEscaped > 0 THEN 
                   IF LenUnEscaped > MaxLenWhat THEN
                      LenUnEscaped = MaxLenWhat
                   END
                   PatternQ.WhatLen  = LenUnEscaped
                   PatternQ.WhatTxt  = ValueTxt[ Beg1 : Beg1 + LenUnEscaped-1 ]                  
                   PatternQ.LowerTxt = lower(PatternQ.WhatTxt)
                   IF PatternQ.WhatTxt ! AND UnEscCnt !<--just escaped xml &xxx;
                      ADD(PatternQ) 
                   END 
               END !if len>0
            END    !if end >= beg
            X += 1     !Move past byte 2 of Delim
            Beg1 = X+1 !Start of next pattern
         END !if =Delim
    END !loop X
    DISPOSE(ValueTxt)   ! ; MESSAGE('MaxSeenLen=' & MaxSeenLen)
    RETURN 
!-----------------------------------
CbFindCleanClass.PatternsQ2String   PROCEDURE(PatternQType PatQ, *LONG OutLength)!,*STRING 
X           LONG
UnEscaped   &STRING
Escaped     STRING(512) 
EscLength   LONG                                          
AnyBytes    LONG 
AnyXml      ANY  !Don't need a String Class for 40 concats of < 1000 bytes 
XmlString   &STRING                 
    CODE
    LOOP X=1 TO RECORDS(PatQ)
        GET(PatQ,X) 
        IF ~PatQ:WhatLen THEN CYCLE.
        UnEscaped &= PatQ:WhatTxt[1 : PatQ:WhatLen]
        IF ~SELF.XmlEscape(UnEscaped,Escaped,EscLength) OR EscLength=0 THEN 
            Message('Failed Escape '& PatQ:WhatLen &' bytes.||Input=' & UnEscaped,'PatternsQ2String')
            CYCLE
        END
        IF ~AnyBytes THEN
            AnyXml = Escaped[1 : EscLength] 
            AnyBytes -= 2
        ELSE
            AnyXml = AnyXml & PatternDelimC3BF & Escaped[1 : EscLength]
        END 
        AnyBytes += (EscLength + 2)
    END 
    IF ~AnyBytes THEN
        AnyXml='No Patterns'
        AnyBytes=LEN(AnyXml)
    END 
    OutLength = AnyBytes    
    XmlString &= NEW(STRING( AnyBytes ))
    XmlString = AnyXml
    RETURN XmlString    
!-----------------------------------
CbFindCleanClass.FindXmlInSlice  PROCEDURE(STRING FindWhat, LONG SliceBeg=1, LONG SliceEnd=0, BOOL pNoCase=0)!,LONG
FindPos LONG 
    CODE
    IF SELF.SliceXmlIsOk(SliceBeg, SliceEnd,'FindXmlInSlice') THEN 
       IF pNoCase THEN
          FindPos=INSTRING(lower(FindWhat),lower(SELF.XmlStr[SliceBeg : SliceEnd]),1)
       ELSE          
          FindPos=INSTRING(FindWhat,SELF.XmlStr[SliceBeg : SliceEnd],1)
       END
       IF FindPos THEN FindPos += (SliceBeg-1).
    END 
    RETURN  FindPos
!-----------------------------------
CbFindCleanClass.FindXmlBetween PROCEDURE(STRING LeftText, STRING RightText, *LONG OutBeginPos, *LONG OutEndPos, LONG SliceBeg=1, LONG SliceEnd=0, BOOL pNoCase=0, BOOL PosIncludesLeftRight=0)!,BOOL
WasFoundBetween BOOL
RightBeg LONG,AUTO
    CODE
    OutBeginPos=0 ; OutEndPos=0 
    DO FindBtwRtn               
    RETURN WasFoundBetween 
FindBtwRtn ROUTINE !Note Found=False if EXIT before the last line 
    IF ~SELF.SliceXmlIsOk(SliceBeg, SliceEnd,'FindXmlBetween') THEN EXIT. 
    OutBeginPos=SELF.FindXmlInSlice(LeftText,SliceBeg, SliceEnd, pNoCase)
    IF ~OutBeginPos THEN EXIT.
    RightBeg = OutBeginPos + Size(LeftText)
    IF RightBeg > SliceEnd THEN EXIT.
    OutEndPos=SELF.FindXmlInSlice(RightText,RightBeg, SliceEnd, pNoCase)
    IF ~OutEndPos THEN EXIT. 
    IF ~PosIncludesLeftRight THEN
        OutBeginPos += Size(LeftText)
        OutEndPos   -= 1
        IF OutBeginPos > OutEndPos THEN  !If Value="" there is Nothing Between
           EXIT                          !so exit to return flase but return values
        END
    ELSE
        OutEndPos   += (Size(RightText)-1)
    END
    WasFoundBetween=True   !Saul Good Man, tell caller we found 
    EXIT
!-----------------------------------
CbFindCleanClass.FindXmlElement PROCEDURE(STRING ltElementName, *LONG OutBeginPos, *LONG OutEndPos, LONG SliceBeg=1, LONG SliceEnd=0, BOOL IncludeElementInSlice=1)!,BOOL
    CODE
    IF ~SELF.FindXmlBetween(ltElementName&' ','>',OutBeginPos,OutEndPos,SliceBeg,SliceEnd, |
                                                 0, IncludeElementInSlice) THEN     !0=NoCase
        RETURN FALSE
    END
    IF ~IncludeElementInSlice THEN OutBeginPos -= 1.    !Start with Space after <Name 
    RETURN True
    !TODO This assumes whitespace of space but could be 9,13,10. Use StrPos() RegEx
!-----------------------------------
CbFindCleanClass.FindXmlAttribute PROCEDURE(STRING ltElementName, STRING AttribName, *LONG OutBeginPos, *LONG OutEndPos, LONG SliceBeg=1, LONG SliceEnd=0, BOOL IncludeAttribEqAndQts=0, <*STRING OutError>)!,BOOL
ElementBegin LONG
ElementEnd   LONG
    CODE
    OutBeginPos=0 ; OutEndPos=0
    IF ~SELF.FindXmlElement(ltElementName, ElementBegin,ElementEnd,SliceBeg,SliceEnd,1) THEN
       IF ~OMITTED(OutError) THEN 
           OutError='Did not find Element "' & ltElementName &'>"'
       END
       RETURN False        !^^ Element Begin / End Pos here is used below to search Attrib=
    END
    IF ~SELF.FindXmlBetween(' '&AttribName &'="','"', | !' value="' to '"'
                            OutBeginPos,  OutEndPos,  | !Out Position of value=""
                            ElementBegin, ElementEnd, | !In Slice Begin/End of <Element >
                            0 ,                       | !0=NoCase (default)
                            IncludeAttribEqAndQts) THEN !0=Exclude Name=" " Pos is inside Quotes ""   (default)
       IF ~OMITTED(OutError) THEN 
           OutError='No " '&  AttribName &'=" in "'& ltElementName &'>"'
           IF ElementBegin AND OutBeginPos-1=OutEndPos THEN  !Was it empty: value="" cannot handle
              OutError='Empty / ' & CLIP(OutError) &' Slice[' & OutBeginPos &':'& OutEndPos &']'
           END 
       END
       RETURN False        !^^ Element Begin / End Pos here is used below to search Attrib=
    END
    RETURN True
    !TODO This assumes whitespace of space but could be 9,13,10. Use StrPos() RegEx
!-----------------------------------
CbFindCleanClass.SliceXmlIsOk  PROCEDURE(*LONG SliceBeg, *LONG SliceEnd, <STRING MsgIfBadSlice>)!,BOOL
SzStr LONG,AUTO
    CODE 
    SzStr = SELF.XmlLen 
    IF ~SliceEnd THEN SliceEnd=SzStr.
    IF SliceBeg < 1        OR SliceBeg > SzStr |
    OR SliceEnd < SliceBeg OR SliceEnd > SzStr THEN
        IF ~OMITTED(MsgIfBadSlice) AND MsgIfBadSlice[1] THEN !No Message if Omit Msg or [1]=' '
            IF Message('Invalid slice [' & SliceBeg &' : '& SliceEnd&']',  |
                       MsgIfBadSlice,,'Close|Assert')=2 THEN Assert(0,'SliceXmlIsOk') .
        ELSE
 !This is called quietly to check Slice so normally do not ASSERT
 ! Assert(0,'SliceXmlIsOk Invalid [' & SliceBeg &' : '& SliceEnd &'] Size='& SzStr & CHOOSE(~OMITTED(MsgIfBadSlice),'  '&MsgIfBadSlice,'')) 
        END
        RETURN False
    END
    RETURN True 
!==================================================================================
CbFindCleanClass.ViewString PROCEDURE(STRING StrValue, <STRING CapTxt>)
  CODE
  SELF.ViewString(StrValue,CapTxt)

CbFindCleanClass.ViewSlice  PROCEDURE(*STRING StrValue, LONG SliceBeg, LONG SliceEnd=0, <STRING CapTxt>)
SzStr LONG,AUTO
    CODE
    SzStr = SIZE(StrValue)
    IF ~SliceEnd THEN SliceEnd=SzStr.
    IF SliceBeg < 1        OR SliceBeg > SzStr |
    OR SliceEnd < SliceBeg OR SliceEnd > SzStr THEN
        Message('Invalid slice ' & SliceBeg &' : '& SliceEnd,'ViewSlice') 
    ELSE 
        SELF.ViewString( StrValue[ SliceBeg : SliceEnd ] , CapTxt)
    END
    RETURN
CbFindCleanClass.ViewString PROCEDURE(*STRING StrValue, <STRING CapTxt>)
LenTxt     LONG,AUTO
ShowHex    BYTE
HScrollTxt BYTE(1),STATIC
VScrollTxt BYTE(1),STATIC
Window WINDOW('S'),AT(,,310,140),GRAY,SYSTEM,MAX,FONT('Consolas',10),RESIZE
        TOOLBAR,AT(0,0,325),USE(?TB1)
            CHECK('Show HEX'),AT(2,0),USE(ShowHex),TIP('See Value in Hex'),disable
            CHECK('HScroll'),AT(74,0),USE(HScrollTxt)
            CHECK('VScroll'),AT(126,0),USE(VScrollTxt)
        END
        TEXT,AT(0,2),FULL,USE(?Txt),HVSCROLL,READONLY,FLAT
        TEXT,AT(0,2),FULL,USE(?HexTxt),HIDE,HVSCROLL,READONLY,FLAT
    END
P LONG,DIM(4),STATIC
  CODE
  LenTxt=LEN(CLIP(StrValue))
  IF ~LenTxt THEN Message('No Text','ValueView') ; RETURN.
  OPEN(Window)
  IF P[4] THEN SETPOSITION(0,P[1],P[2],P[3],P[4]).
  IF LenTxt > 0FFF0h THEN DISABLE(?HScrollTxt,?VScrollTxt). !System Error @ 64k in 11.13505 - Message('Risk GPF?',LenTxt,,'No|Risk')
  ?Txt{PROP:Use}=StrValue
  0{PROP:Text}=CHOOSE(~OMITTED(CapTxt) AND CapTxt,CLIP(CapTxt),'StringTheory Value') & ' - Length ' & LenTxt  
  IF ~HScrollTxt THEN ?Txt{PROP:HScroll}=0. ; IF ~VScrollTxt THEN ?Txt{PROP:VScroll}=0. 
  ACCEPT
    CASE ACCEPTED()
    OF ?HScrollTxt ; ?Txt{PROP:HScroll}=HScrollTxt
    OF ?VScrollTxt ; ?Txt{PROP:VScroll}=VScrollTxt
    OF ?ShowHex
!        IF ~HexTxt.length() THEN
!           SELF.HexDump(pSt, HexTxt)
!           ?HexTxt{PROP:Use}=HexTxt.valuePtr[1 : HexTxt._DataEnd]
!        END
!        ?Txt{PROP:Hide}=ShowHex ; ?HexTxt{PROP:Hide}=1-ShowHex
    END
  END
  GETPOSITION(0,P[1],P[2],P[3],P[4])
  RETURN

!==============================================
CbFindCleanClass.XmlEscape   PROCEDURE(*STRING pStr, *STRING pOutXml, *LONG OutLen, BOOL pClipInStr=0)!,BOOL,PROC !Returns False if Overrun
FitOutXmlOK BOOL,AUTO
Token   PSTRING(7),AUTO 
TokLen  BYTE,AUTO 
InX     LONG,AUTO
OutX    LONG,AUTO
OutMax  LONG,AUTO
    CODE
    OutX=0 
    FitOutXmlOK=1
    OutMax = SIZE(pOutXml)
    LOOP InX = 1 TO CHOOSE(~pClipInStr,SIZE(pStr),LEN(CLIP(pStr)))
         CASE VAL(pStr[InX])  
         OF 60 ; Token= '&lt;'  
         OF 62 ; Token= '&gt;'  
         OF 38 ; Token= '&amp;' 
         OF 39 ; Token= '&apos;'
         OF 34 ; Token= '&quot;'
         OF 0 TO 31 ; Token=' '      !Low Ascii make spaces
         OF 60h ; Token=' '          !``` Grave Accent is space trailing space marker. TODO Caller handles?
         ELSE           !123456
            IF OutX >= OutMax THEN FitOutXmlOK = False  ; BREAK. !Out+1 > Max
            OutX += 1
            pOutXml[OutX] = pStr[Inx] 
            CYCLE   !Cycle so do NOT fall thru, that's for Tokens 
         END
         TokLen=LEN(Token)
         IF OutX + TokLen > OutMax THEN FitOutXmlOK = False  ; BREAK.
         pOutXml[OutX+1 : OutX+TokLen ] = Token
         OutX += TokLen
    END 
    OutLen=OutX 
    return FitOutXmlOK 
!==============================================
CbFindCleanClass.XmlUnEscape PROCEDURE(*STRING pXML, *LONG OutLen)!,LONG,PROC   !Returns Change Out
LenXml LONG,AUTO
EscChr STRING(1),AUTO
X      LONG,AUTO
ChangeCnt LONG
AmpPos LONG !Last &  Ampersand   starts &Token;
SemPos LONG !Last ;  Semicolon     ends &Token;
WarnBad STRING(12),STATIC 
DbgXML  PSTRING(256)
    CODE
 DbgXML = pXml
    LenXml=SIZE(pXML)
    LOOP X = LenXml TO 1 BY -1  !Work backwards thru string in place
        CASE pXML[X]
        OF ';' ; SemPos=X      ! &lt; ends   with ;  =SemPos
        OF '&' ; AmpPos=x      ! &lt; begins with &  =AmpPos
                 DO AmpSemiParseRtn
                 SemPos=0
        END !Case Chr
    END     !Loop
    OutLen = LenXml
    RETURN ChangeCnt 
    
AmpSemiParseRtn  ROUTINE
    ChangeCnt+=1
    IF ~SemPos THEN         !Bad XML has & and no ; ?
       SemPos=AmpPos 
       DO TellCarlRtn ; EXIT
    END 
    CASE pXML[AmpPos : SemPos]
    OF '&amp;'  ; EscChr='&'
    OF '&lt;'   ; EscChr='<<'
    OF '&gt;'   ; EscChr='>'
    OF '&apos;' ; EscChr=''''
    OF '&quot;' ; EscChr='"'
    ELSE 
        DO TellCarlRtn ; EXIT 
    END
    pXML[AmpPos] = EscChr                                 !Put true char over the & 
    IF SemPos+1 <= LenXml THEN                            !Was &xxx; Not at end of String?
       pXML[AmpPos+1 : LenXml] = pXML[SemPos+1 : LenXml]  !   then Shift back string 
    END 
    LenXml -= (SemPos-AmpPos)  !Reduce length for xxx; leaving pXML[AmpPos] = EscChr
    EXIT

TellCarlRtn ROUTINE    !Also Fixes XML 
    IF WarnBad <> pXML[AmpPos : SemPos] THEN 
       WarnBad  = pXML[AmpPos : SemPos]
       Message('Tell Carl need to handle Escape Xml: ' & WarnBad & |
               '||AmpPos='& AmpPos &'  SemPos='& SemPos &'  X='& X &'  LenXml='& LenXml & |
               '|[Amp]=' & SUB(pXML,AmpPos,1) &'  [Sem]=' & SUB(pXML,SemPos,1) & |
               '||Xml="' & pXML &'"'& |
               '||DbgXML='& DbgXML , 'XmlUnEscape')
    END
    pXML[AmpPos]='@'     !don't leave bad XML so change & to Q
    EXIT

