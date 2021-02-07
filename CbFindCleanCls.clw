                    MEMBER()
!--------------------------
! CbFindCleanClass by Carl Barnes
!
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
CbFindCleanClass.Construct        PROCEDURE()
!----------------------------------------
Something  STRING('<<Construct>')      !Prevent accidental use of Null and GPF
    CODE
    SELF.NewXmlStr(SIZE(Something))      
    SELF.XmlStr=Something
    CLOSE(LoadSaveFile)
    RETURN

!---------------------------------------
CbFindCleanClass.Destruct PROCEDURE()
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
sDelim  EQUATE('<0C3h,0BFh>')
    CODE
    IF ~SELF.SliceXmlIsOk(PatBegin, PatEnd,'PatternCount') THEN
        RETURN 0
    END 
    LOOP X=PatBegin TO PatEnd-1
         IF SELF.XmlStr[ X : X+1 ] = sDelim THEN 
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
lDelim  EQUATE('<0C3h,0BFh>')
LenAfter    LONG
    CODE
    IF ~SELF.SliceXmlIsOk(PatBegin, PatEnd,'PatternTruncate') THEN
        RETURN 0
    END
    IF MinCount < 2 THEN MinCount=2.
    LOOP X=PatBegin TO PatEnd-1
         IF SELF.XmlStr[ X : X+1 ] = lDelim THEN 
            CntDlm += 1
            IF CntDlm >= MinCount THEN 
               LenAfter = X - PatBegin
               SELF.XmlStr[ X : SELF.XmlLen ] = SELF.XmlStr[ PatEnd+1 : SELF.XmlLen ] 
               SELF.XmlLen -= (PatEnd+1 - X) 
               BREAK
            END
            X += 1
         END            
    END

    RETURN LenAfter
    
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
CbFindCleanClass.FindXmlAttribute PROCEDURE(STRING ltElementName, STRING AttribName, *LONG OutBeginPos, *LONG OutEndPos, LONG SliceBeg=1, LONG SliceEnd=0, BOOL IncludeAttribEqAndQts=0)!,BOOL
ElementBegin LONG
ElementEnd   LONG
    CODE
    OutBeginPos=0 ; OutEndPos=0
    IF ~SELF.FindXmlElement(ltElementName, ElementBegin,ElementEnd,SliceBeg,SliceEnd,1) THEN 
       RETURN False        !^^ Element Begin / End Pos here is used below to search Attrib=
    END
    RETURN SELF.FindXmlBetween(' '&AttribName &'="','"',           |
                               OutBeginPos,  OutEndPos,   | !Out position of Value=""
                               ElementBegin, ElementEnd,  | !In Slice Begin/End of <Element >
                               0 ,  IncludeAttribEqAndQts ) !0=NoCase 0=Exclude Name=" "
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
HScrollTxt BYTE(1)
VScrollTxt BYTE(1)
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