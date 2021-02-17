    OMIT('_EndOfInclude_',_IFDef_CbFindCleanCls_)
_IFDef_CbFindCleanCls_  EQUATE(1)
!------------------------------------------------------------------
! CbFindCleanClass by Carl Barnes 
! Built to work on the ClarionProperties.XML <FindPatterns value=""
!------------------------------------------------------------------
! 01/30/2021 - First Release
!------------------------------------------------------------------
!    INCLUDE('CbFindCleanCls.INC'),ONCE      ! FindClnCls1  CLASS(CbFindCleanClass).
!------------------------------------------------------------------

ClarionProperties_xml   EQUATE('ClarionProperties.xml')
ltFindPatterns          EQUATE('<<FindPatterns')
ltReplacePatterns       EQUATE('<<ReplacePatterns')
valueEqQt               EQUATE('value="') 
PatternDelimC3BF        EQUATE('<0C3h,0BFh>')

PatternQType QUEUE,TYPE
WhatLen         BYTE        !PatQ:WhatLen
WhatTxt         STRING(96)  !PatQ:WhatTxt
LowerTxt        STRING(96)  !PatQ:LowerTxt
             END
!----------------- CbFindCleanClass -----------------
CbFindCleanClass    CLASS,TYPE,MODULE('CbFindCleanCls.CLW'),LINK('CbFindCleanCls.CLW',1),DLL(0)   !,_ABCLinkMode_),DLL(_ABCDllMode_)
!
!Properties
!----------
LastError               STRING(500)

!Methods
!-------
FileLoad            PROCEDURE(STRING ClaPropXmlFileName, BYTE pQuery),BOOL !Load File into .XmlStr
FileSave            PROCEDURE(STRING ClaPropXmlFileName),BOOL              !Save File from .XmlStr

                    !!!<summary>Count Delimeter C3BFh in Self.XmlStr limited to Slice Begin and End </summary><Returns>LONG Count</Returns>
PatternCount        PROCEDURE(LONG PatBegin, LONG PatEnd),LONG
                    !!!<summary>Shrink Self.XmlStr to Minimum Count of Delimeter C3BFh in Slice Begin and End </summary><Returns>LONG Length of Shrinked XML</Returns>
PatternShrink       PROCEDURE(LONG PatBegin, LONG PatEnd, LONG MinCount),LONG
                    !!!<summary>Parse the patterns slice by delimeter C3BFh and place in Queue</summary>
Patterns2Queue      PROCEDURE(PatternQType OutPatternQ,LONG PatBeginSlice, LONG PatEndSlice)

                    !!!<summary>Instring(FindWhat,Self.XmlStr)  optionally limited to Slice Begin and End </summary><Returns>LONG Position, 1 based</Returns>
FindXmlInSlice      PROCEDURE(STRING FindWhat, LONG SliceBeg=1, LONG SliceEnd=0, BOOL NoCase=False),LONG

                    !Find Begin/End of Left/Right text. False if Left/Right not found. False if noting between i.e. "" and PosIncludesLeftRight=False
                     !!!<summary>Instring in Self.XmlStr for Left then Right Text optionally limited to Slice Begin and End.</summary><Returns>BOOL True if found with *OutBegin and *OutEnd as Positions, 1 based</Returns>
FindXmlBetween      PROCEDURE(STRING LeftText, STRING RightText, *LONG OutBeginPos, *LONG OutEndPos, LONG SliceBeg=1, LONG SliceEnd=0, BOOL NoCase=False, BOOL PosIncludesLeftRight=False),BOOL

                    !!!<summary>Find '<Element ' inside '>' Returns slice begin/end including <Element> unless pass IncludeElementInSlice=False </summary>
FindXmlElement      PROCEDURE(STRING ltElementName, *LONG OutBeginPos, *LONG OutEndPos, LONG SliceBeg=1, LONG SliceEnd=0, BOOL IncludeElementInSlice=True),BOOL

                    !Find <Element Attrib="123"> Assumes ="Quotes", Returns inside Quotes
                    !!!<summary>Find <Element Attrib="123"> Assumes ="Quotes", Returns Begin/End Position inside Quotes. optionally limited to Slice Begin and End. </summary><Returns>BOOL True if found</Returns>
FindXmlAttribute    PROCEDURE(STRING ltElementName, STRING AttribName, *LONG OutBeginPos, *LONG OutEndPos, LONG SliceBeg=1, LONG SliceEnd=0, BOOL IncludeQuotes=0, <*STRING OutError>),BOOL

                    !!!<summary>Verify Slice Begin/End is >0 and <= Xml length. To see a message pass 'ShowMsgIfBadSlice'.</summary><Returns>BOOL True if Slice Valid</Returns>
SliceXmlIsOk        PROCEDURE(*LONG SliceBeg, *LONG SliceEnd, <STRING ShowMsgIfBadSlice>),BOOL

XmlEscape           PROCEDURE(*STRING InString, *STRING OutXml, *LONG OutLength, BOOL ClipInString=0),BOOL,PROC !Returns False if Overrun
XmlUnEscape         PROCEDURE(*STRING InOutXML, *LONG OutLen),LONG,PROC

ViewString PROCEDURE(STRING StrValue, <STRING CapTxt>)
ViewString PROCEDURE(*STRING StrValue, <STRING CapTxt>)
ViewSlice  PROCEDURE(*STRING StrValue, LONG SliceBeg, LONG SliceEnd=0, <STRING CapTxt>)

!Properties Internal
!-------------------
XmlStr                  &STRING
XmlLen                  LONG

!Methods Internal
!----------------
Construct               PROCEDURE()
Destruct                PROCEDURE(),VIRTUAL
NewXmlStr               PROCEDURE(LONG pNewXmlLen)
NewXmlString            PROCEDURE(STRING pNewString)  !Pass String Value to New Size

!Test <Comments> they must be in the INC to show in code that Includes the Class
!!! <summary>Open the LoadSaveFile - INTERNAL USE</summary>
!!! <param name="ClaPropXmlFileName">STRING - Name of File to Open</param>
!!! <param name="pAccessMask">LONG - Name of File to Open</param>
!!! <param name="pCanCreate">BOOL - Pass True to Create file if it does not exist</param>
!!! <returns>True if file opened, False if failed with .LastError set </returns>

FileOpen PROCEDURE(STRING ClaPropXmlFileName, LONG pAccessMask, BOOL pCanCreate=0),BOOL 

                    END

    _EndOfInclude_