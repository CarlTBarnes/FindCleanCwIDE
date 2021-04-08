!  MIT License
!  Copyright (c) 2021 Carl T. Barnes
!---------------------------------------------------------------
!My first utility FindCleanCwIDE shrunk the FindPatterns so they load faster 
!   I rarely used the patterns written to the list by this.
!This tool shrinks then appends your Favorites to the end of <FindPatterns value>
!   so they are always in the same place ready to use
!This will eventually be folded into FindClean so it always saves your list
!----------------------------------------------------------------
! 04-Apr-2021 Fix Favorite Sort to save to INI
!----------------------------------------------------------------
  PROGRAM
  INCLUDE('KeyCodes.CLW')
  INCLUDE('ResDef.CLW'),ONCE  !Project needs ResCode.CLW
  INCLUDE('CbFindCleanCls.INC'),ONCE

  INCLUDE('CBWndPreview.INC'),ONCE             !From https://github.com/CarlTBarnes/WindowPreview
    COMPILE('!-WndPrv-',_IFDef_CBWndPreview_)  !Comment this line to remove
WndPrvCls   CBWndPreviewClass  
             !-WndPrv-

  MAP
FavoriteFindWindow  PROCEDURE() 
DB                  PROCEDURE(STRING xMessage) 
!!!PatternsQ2String    PROCEDURE(PatternQType PatQ, *LONG OutLength),*STRING
GetSpecialFolder    PROCEDURE(LONG CDSL),STRING 
     MODULE('Windows')
        SHGetFolderPath(Long hwndOwner=0, LONG nFolder, Long hToken, Long dwFlags, *CSTRING pszPath),LONG,RAW,PASCAL,DLL(1),NAME('SHGetFolderPathA')
        OutputDebugString(*Cstring Msg),PASCAL,RAW,DLL(1),NAME('OutputDebugStringA')
     END
  END

!Region Global Data and Equates 
GraveAccent             EQUATE('`')     !``For trailing space, Not a '' Quote
Glo:ClaPropFileName     STRING(260)
Glo:LastSavedClaProp    STRING(260)     !if saved then save in Settings
Glo:TestFavorite        BYTE(0)
ltFavPatternsElement    EQUATE(ltFindPatterns)  !Only change <FindPatterns

DbView                  SHORT(0)            !Open View String and debug Windows
SettingFile             EQUATE('.\FavFndSettings.ini') 
Glo:FavMinPatterns      USHORT(10)  !Drop list is 30  
Glo:AlphaRecent         BYTE(1)
FavoriteHeading         EQUATE('={9} Favorites ={9}')

Glo:AppDataSVpath   CSTRING(256) 
CSIDL_APPDATA       EQUATE(001ah)   !CSIDL_APPDATA 0x001a // <user name>\Application Data
ClaPropFileQ        QUEUE,PRE(ClaPropQ)  !Drop list From Q to pick file
XmlFile                STRING(260)       !ClaPropQ:XmlFile
                    END            
!EndRegion 
  CODE
    SYSTEM{7A58H}=1                 !C10 PROP:PropVScroll    EQUATE(7A58H)  ! boolean: kind of listbox vertical scrollbar
    SYSTEM{7A7Dh}=MSGMODE:CANCOPY   !C11 PROP:MsgModeDefault EQUATE(7A7DH)  ! uint SYSTEM only, default value to be uysed in the MESSGE Style attribute when ommited
    
    !Quick drop Queue of AppData \SoftVelocity\Clarion
    Glo:AppDataSVpath=GetSpecialFolder(CSIDL_APPDATA) & '\SoftVelocity\Clarion'
    LOOP v#=8 TO 13
         ClaPropQ:XmlFile=Glo:AppDataSVpath &'\'& v# &'.0\' & ClarionProperties_xml 
         IF v#=13 then ClaPropQ:XmlFile = '.\TestFavor\ClarionProperties.xml'.
         IF EXISTS(ClaPropQ:XmlFile) THEN 
            ADD(ClaPropFileQ,1) 
            Glo:ClaPropFileName = ClaPropQ:XmlFile
         END
    END     
    FavoriteFindWindow()
    RETURN
  
!===========================================
FavoriteFindWindow   PROCEDURE() 
FavFind_Txt_FN      EQUATE('FavFind.txt')
FavFind_TEXT        STRING(8000)
FavorSort           BYTE(0)

FindQ       QUEUE,PRE(FndQ) ! FindQ          MergeQ         FavorQ
Seq             SHORT       ! FndQ:Seq       MerQ:Seq       FavQ:Seq     
WhatLen         BYTE        ! FndQ:WhatLen   MerQ:WhatLen   FavQ:WhatLen 
WhatText        STRING(96)  ! FndQ:WhatText  MerQ:WhatText  FavQ:WhatText
LowerTxt        STRING(96)  ! FndQ:LowerTxt  MerQ:LowerTxt  FavQ:LowerTxt 
            END 
MergeQ      QUEUE(FindQ),PRE(MerQ)  !Merge FindQ and Favorite 
IsFavor         BYTE                !MerQ:IsFavor really Pointer so 1 is === Fav ===
            END 
FavorQ      QUEUE(FindQ),PRE(FavQ)   !Favorites
Head1         BYTE                   !FavQ:Head1=1 else Lines=0
            END  
            
MergeQFavorCount    LONG        
MergeQRecentCount   LONG        
Head_FindQ          STRING(50)
Head_MergeQ         STRING(50)         
FavFindTextEditThis BYTE
WriteOK             BYTE  !else Glo:TestFavorite

Window WINDOW('Favorite Find Patterns Append in ClarionProperties.xml'),AT(,,465,206),GRAY,IMM,SYSTEM,ICON('FavFind.ico'), |
            FONT('Segoe UI',10,,FONT:regular),RESIZE
        SHEET,AT(3,3),FULL,USE(?Sheet1)
            TAB(' Favorites - FavFind.Txt '),USE(?TAB:FavFindTxt)
                CHECK('Edit This'),AT(10,33),USE(FavFindTextEditThis),SKIP,TRN,FONT(,9),TIP('Allow Editing ConfigDir.TXT')
                BUTTON('Re-Parse'),AT(9,42,42,11),USE(?FavFindTxtReParseBtn),DISABLE,SKIP,FONT(,9),TIP('Re-Parse the edi' & |
                        'tted ConfigDir.Txt')
                BUTTON('Clear Text'),AT(9,56,42,11),USE(?FavFindTxtClearTextBtn),DISABLE,SKIP,FONT(,9),TIP('Clear Config' & |
                        'Dir.TXT')
                BUTTON('Reload'),AT(9,71,42,11),USE(?FavFindTxtReloadBtn),SKIP,FONT(,9),TIP('Reload ConfigDir.Txt')
                BUTTON('Notepad'),AT(9,85,42,11),USE(?FavFindTxtNotepadBtn),SKIP,FONT(,9),TIP('Open ConfigDir.Txt in Notepad')
                BUTTON('Explore'),AT(9,103,42,11),USE(?FavFindTxtExploreBtn),SKIP,FONT(,9)
                BUTTON('Clear<13,10>List'),AT(9,166,42,20),USE(?FavFindTxtFreeListBtn),SKIP,FONT(,9),TIP('Clear / Free L' & |
                        'ist of ConfigDir Paths')
                BOX,AT(7,18,460,15),USE(?Box_Cfg),FILL(COLOR:INACTIVECAPTION),LINEWIDTH(1)
                PROMPT('Create a file named "FavFind.TXT" with your Favorite Search strings. An "END" on line alone stop' & |
                        's parsing FavFind.Txt.<13,10>For a Trailing Space use a Grave Accent ` for the last space.'), |
                        AT(58,18,361,16),USE(?PROMPT:CfgDirFYI),TRN,FONT(,9)
                TEXT,AT(58,36,170),FULL,USE(FavFind_TEXT),SKIP,HVSCROLL,FONT('Consolas',,COLOR:BTNTEXT),TIP('From FavFin' & |
                        'd.Txt.<13><10>Edits are not saved.'),READONLY
                PROMPT('Sort'),AT(10,125),USE(?FavorSort:Pmt)
                LIST,AT(10,135,36,10),USE(FavorSort),TIP('Sort order for Favorites loaded from file FavFind.txt'),DROP(9,50), |
                        FROM('As Is|#0|Reverse|#1|A-Z|#2|Z-A|#3')
                LIST,AT(234,36,180),FULL,USE(?List:FavorQ),VSCROLL,FONT('Consolas'),VCR,FROM(FavorQ),FORMAT('17R(2)|M~Se' & |
                        'q~L(2)@n3@17R(2)|M~Len~L(2)@n-4b@120L(2)|FM~Find What~@s96@?'),ALRT(DeleteKey)
                BUTTON,AT(424,42,16,16),USE(?FavorQCopyBtn),ICON(ICON:Copy),TIP('Copy sorted Favorite list to Clipboard'),FLAT
            END
            TAB(' Write ClarionProperties.Xml '),USE(?TAB:Main)
                BUTTON('&Load'),AT(9,19,37,12),USE(?ClaPropLoadBtn),TIP('Load Clarion Prop XML')
                COMBO(@s255),AT(51,19,403,12),USE(Glo:ClaPropFileName),VSCROLL,FONT('Consolas'),DROP(9),FROM(ClaPropFileQ), |
                        ALRT(MouseLeft2),FORMAT('20L(2)|M~Folder and Clarion Properties.xml~@s255@')
                BUTTON('&Refresh'),AT(9,45,37),USE(?RefreshMergedBtn),TIP('Merge Favorites and History')
                PROMPT('Recent Count to Write:'),AT(10,75,35,24),USE(?Glo:FavPatterns:Pmt),TRN
                SPIN(@n3),AT(10,102,35,10),USE(Glo:FavMinPatterns),HVSCROLL,RIGHT(1),TIP('Number of Recent Searches to k' & |
                        'eep<13,10,13,10>Drop list shows 30 items<13,10>Can press Alt+Down or F4, then Ctrl+PageDown'), |
                        RANGE(0,999)
                CHECK('&Alpha'),AT(10,114),USE(Glo:AlphaRecent),FONT(,9),TIP('Sort Recent by Alpha in Merged list')
                STRING(@s50),AT(51,34,171),USE(Head_FindQ),FONT(,,COLOR:Black),COLOR(0E7D4C6H)
                LIST,AT(51,46,171),FULL,USE(?LIST:FindQ),VSCROLL,FONT('Consolas'),VCR,FROM(FindQ),FORMAT('24R(2)|FM~Seq~' & |
                        'C(0)@n5-@16R(2)|FM~Len~C(0)@n-3b@28L(2)F~String~@s96@?'),ALRT(CtrlC), ALRT(DeleteKey)
                STRING(@s50),AT(235,34,171),USE(Head_MergeQ),FONT(,,COLOR:Black),COLOR(0E7D4C6H)
                LIST,AT(235,46,171),FULL,USE(?LIST:MergeQ),VSCROLL,FONT('Consolas'),VCR,FROM(MergeQ),FORMAT('24R(2)|FM~S' & |
                        'eq~C(0)@n-5@16R(2)|FM~Len~C(0)@n-3@28L(2)F~String~@s96@?'),ALRT(CtrlC), ALRT(DeleteKey)
                BUTTON('Refres&h'),AT(414,45,37),USE(?RefreshMergedBtn2),TIP('Merge Favorites and History')
                BUTTON('&Write'),AT(414,96,39,28),USE(?ClaPropWriteBtn),DISABLE,ICON(ICON:Save),TIP('Write Clarion Prop XML')
                CHECK('Write &Xml'),AT(414,129),USE(WriteOK),FONT(,9),TIP('Check "Write to Clarion"  to allow writing Cl' & |
                        'arionProperties.XML<13,10>with the "Favorite Patterns" for Find/Replace.<13><10,13><10>Two back' & |
                        'up copies are kept with extension ".b4favor" to allow recovery.<13,10,13><10>To test this tool ' & |
                        'check the ".TestFavor" box<13,10>to instead write to file ClarionProperties.Xml.TestFavor ')
                CHECK('.&TestFavor'),AT(414,139),USE(Glo:TestFavorite),SKIP,FONT(,9),TIP('Write to .TestFavor')
                PROMPT('IDE Must be Closed to Write'),AT(414,66,38,27),USE(?MustbeClosed),FONT(,,COLOR:Red),COLOR(COLOR:White), |
                        CENTER
                PROMPT('Delete Key for lines unwnated'),AT(7,165,40,26),USE(?Delete:FYI)
                PROMPT('Delete Key for lines unwnated'),AT(414,165,40,26),USE(?Delete:FYI2)
            END
        END
        CHECK('DbView'),AT(417,2),USE(DbView),SKIP,FONT(,9),TIP('Show Debug Messages')
    END
DOO     CLASS                         
FavFindTxtLoadFile      PROCEDURE()         !Load FavFind_TEXT from FavFind.Txt
FavorQLoadFavFind_Txt   PROCEDURE()         !Load FavorQ from FavFind_TEXT    
FavorQSortAsDesired     PROCEDURE()         !Sort FavorQ based on FavorSort
FavorQCopy2Clip         PROCEDURE() 
ClarionPropLoadQueues   PROCEDURE()         !Load FindQ from Cla Prop.Xml
ClarionPropLoadFindQ    PROCEDURE(),STRING  
Head_FindQ_MergeQ       PROCEDURE()         !Set  Head_FindQ Head_MergeQ 
MergeQueueBuild         PROCEDURE()         !Build MergeQ = FindQ + FavorQ
MergeQ_AddFavorites     PROCEDURE(BYTE SeqAsZero) 
MergeQDeleteKey         PROCEDURE()         !Delete MergeQ also Deletes FindQ and FavQ
WriteFavorites          PROCEDURE(STRING pClaPropXmlFN)        !Write back to File 
WriteFavorFile          PROCEDURE(*STRING pClaPropXmlFN, CONST *STRING PatternsXml, LONG PatLength, *STRING OutMsg),BOOL  !Write back to File 
WriteFavorSetValue      Procedure(CbFindCleanClass FindCln, STRING ltPatternsElement, CONST *STRING PatString, LONG PatLength, *STRING OutMsg),BOOL
SettingsLoad            PROCEDURE()
SettingsSave            PROCEDURE()
        END
    CODE
    DOO.SettingsLoad()
    OPEN(Window)               !<--- Windows is Open 
    COMPILE('!-WndPrv-',_IFDef_CBWndPreview_)  
       WndPrvCls.Init(1)   
       WndPrvCls.InitList(?List:FavorQ, FavorQ, 'FavorQ')        !InitList() NOT required >= 11.13505 
       WndPrvCls.InitList(?List:FindQ,  FindQ,  'FindQ')
       WndPrvCls.InitList(?List:MergeQ, MergeQ, 'MergeQ')
             !-WndPrv-   
    0{PROP:MinWidth}=0{PROP:Width} ; 0{PROP:MaxWidth}=0{PROP:Width}
    0{PROP:MinHeight}=0{PROP:Height} 
    DL#=?Glo:ClaPropFileName
    DL#{PROP:LineHeight}=DL#{PROP:LineHeight}+2 
    DL#=DL#{PROP:ListFEQ} ; DL#{PROP:Background}=COLOR:InfoBackground ; DL#{PROP:FontColor}=COLOR:InfoText
    ?FavFind_TEXT{PROP:Background}=COLOR:BTNFACE 
    ?Sheet1{PROP:TabSheetStyle}=1
    DISPLAY
    DOO.FavFindTxtLoadFile()
    DOO.FavorQLoadFavFind_Txt()  
    IF EXISTS(Glo:ClaPropFileName) THEN 
       DOO.ClarionPropLoadQueues()
       DOO.MergeQueueBuild() 
    END   
    ACCEPT 
        CASE EVENT()
        OF EVENT:OpenWindow ; SELECT(?TAB:FavFindTxt)
        END
        CASE ACCEPTED()
        OF ?FavFindTxtReloadBtn  ; DOO.FavFindTxtLoadFile() 
                                     POST(EVENT:Accepted, ?FavFindTxtReParseBtn )
        OF ?FavFindTxtReParseBtn ; DOO.FavorQLoadFavFind_Txt()     
        OF ?FavorSort            ; DOO.FavorQSortAsDesired() 
        OF ?FavorQCopyBtn        ; DOO.FavorQCopy2Clip()        
        OF ?FavFindTxtReloadBtn  ; DOO.FavFindTxtLoadFile() ; DISPLAY
        OF ?FavFindTxtNotepadBtn ; RUN('Notepad ' & FavFind_Txt_FN)  
        OF ?FavFindTxtExploreBtn ; RUN('Explorer /select,"' & FavFind_Txt_FN &'"')
        OF ?FavFindTxtClearTextBtn ; CLEAR(FavFind_TEXT)  ; DISPLAY 
        OF ?FavFindTxtFreeListBtn  ; FREE(FavorQ) ; DISPLAY 
!        
        OF ?FavFindTextEditThis
           Disable(?)
           ?FavFind_TEXT{PROP:FontColor}=COLOR:None
           ?FavFind_TEXT{PROP:Background}=COLOR:None
           ?FavFind_TEXT{PROP:ReadOnly}=False
           ?FavFind_TEXT{PROP:Skip}=False 
           ENABLE(?FavFindTxtReParseBtn)
           ENABLE(?FavFindTxtClearTextBtn) 
           ?FavFindTxtReloadBtn{PROP:Text}='clear + load'
           ?FavFindTxtReloadBtn{PROP:Tip}='Clear Editted Text and Reload ... Losing any edits' & |
                                            '<13,10,13,10>Press ReLoad button to load Editted Text paths'
           DISPLAY ; SELECT(?FavFind_TEXT)
        !------------------------------------
        OF ?ClaPropLoadBtn      ; DOO.ClarionPropLoadQueues()
                                  POST(EVENT:Accepted,?RefreshMergedBtn) 
        OF ?Glo:ClaPropFileName ; DISABLE(?ClaPropWriteBtn) 
                                  POST(EVENT:Accepted, ?ClaPropLoadBtn) 
        OF ?Glo:AlphaRecent     ; DOO.MergeQueueBuild()
        OF ?RefreshMergedBtn    ; DOO.MergeQueueBuild() ; SELECT(?LIST:FindQ)
        OF ?RefreshMergedBtn2   ; DOO.MergeQueueBuild() ; SELECT(?LIST:MergeQ)
        OF ?ClaPropWriteBtn     ; DOO.WriteFavorites(Glo:ClaPropFileName) 

        OF ?WriteOK          ;  IF WriteOK THEN Glo:TestFavorite=0 ; ?WriteOK{PROP:Background}=-1. ; DISPLAY
        OF ?Glo:TestFavorite ;  IF Glo:TestFavorite THEN WriteOK=0 ; ?WriteOK{PROP:Background}=-1. ; DISPLAY
        END
        
        CASE FIELD()
        OF ?Glo:ClaPropFileName
            IF KEYCODE()=MouseLeft2 THEN
                RUN('Explorer /select,"' & CLIP(Glo:ClaPropFileName) &'"')
            END 
        OF ?List:FavorQ
            GET(FavorQ,CHOICE(?List:FavorQ)) 
            CASE EVENT()
            OF EVENT:AlertKey 
               CASE KEYCODE()
               OF CtrlC     ; SETCLIPBOARD(FavQ:WhatText )  
               OF DeleteKey ; IF FavQ:Seq > 0 THEN DELETE(FavorQ).
               END
            END !Case Event 
        OF ?List:FindQ
            GET(FindQ,CHOICE(?List:FindQ)) 
            CASE EVENT()
            OF EVENT:AlertKey 
               CASE KEYCODE()
               OF CtrlC     ; SETCLIPBOARD(FndQ:WhatText )  
               OF DeleteKey ; DELETE(FindQ) ; DOO.Head_FindQ_MergeQ()
               END
            END !Case Event 

        OF ?LIST:MergeQ
            GET(MergeQ,CHOICE(?LIST:MergeQ)) 
            CASE EVENT()
            OF EVENT:AlertKey 
               CASE KEYCODE()
               OF CtrlC     ; SETCLIPBOARD(MerQ:WhatText )  
               OF DeleteKey ; DOO.MergeQDeleteKey()  
               END
            END !Case Event
        END     !Case Field()
    END !Accept 
    DOO.SettingsSave()
    RETURN
!-------------------------------- 
DOO.SettingsLoad     PROCEDURE() 
LastClaPropXml       LIKE(Glo:ClaPropFileName)
    CODE 
    FavorSort         =GETINI('Config','FavorSort'  ,FavorSort         ,SettingFile)
    Glo:FavMinPatterns=GETINI('Config','FavMinPats' ,Glo:FavMinPatterns,SettingFile)    
    Glo:AlphaRecent   =GETINI('Config','AlphaRecent',Glo:AlphaRecent   ,SettingFile) 

    LastClaPropXml =GETINI('Config','ClaPropXml','',SettingFile)    
    IF LastClaPropXml AND EXISTS(LastClaPropXml) THEN 
       Glo:ClaPropFileName = LastClaPropXml 
    END
    RETURN 
DOO.SettingsSave     PROCEDURE() 
    CODE 
    PUTINI('Config','FavorSort'  ,FavorSort   ,SettingFile)
    PUTINI('Config','FavMinPats' ,Glo:FavMinPatterns,SettingFile)    
    PUTINI('Config','AlphaRecent',Glo:AlphaRecent   ,SettingFile)     
    IF Glo:LastSavedClaProp AND EXISTS(Glo:LastSavedClaProp) THEN 
       PUTINI('Config','ClaPropXml',Glo:LastSavedClaProp,SettingFile)     
    END
    RETURN     
!--------------------------------
DOO.FavFindTxtLoadFile PROCEDURE()
FndCln  CbFindCleanClass    !Use class to Load file
    CODE
    IF ~FndCln.FileLoad(FavFind_Txt_FN,1) THEN 
        FavFind_TEXT='FavFind.txt<13,10>Load Failed: ' & CLIP(FndCln.LastError) & |
                   '<13,10><13,10>Create FavFind.txt file<13,10>with your Favorites in Notepad'
        POST(EVENT:Accepted,?FavFindTxtNotepadBtn)                   
        RETURN         
    END 
    FavFind_TEXT = FndCln.XmlStr
    RETURN
!--------------------------------
DOO.FavorQLoadFavFind_Txt PROCEDURE()  !Load FavFind.Txt into FavorQ
LineNo  LONG 
PTxt1   PSTRING(97) 
TxtLen  BYTE
X       LONG
    CODE
    LOOP X=1 TO SIZE(FavFind_TEXT) !Remove Tabs and Low ASCII
        CASE VAL(FavFind_TEXT[X])
        OF 13 OROF 10 ! keep
        OF 0 TO 31    ; FavFind_TEXT[X]='' 
        END
    END ; DISPLAY
    FREE(FavorQ) 
    CLEAR(FavorQ) 
    LOOP LineNo=1 TO ?FavFind_TEXT{PROP:LineCount} 
         PTxt1=CLIP(?FavFind_TEXT{PROP:Line,LineNo})
         TxtLen  = LEN(PTxt1) 
         IF ~TxtLen OR PTxt1 <= ' ' THEN CYCLE. 
         IF UPPER(PTxt1) = 'END' THEN BREAK.  !Line with just END stops loading         
         IF PTxt1[TxtLen]=GraveAccent THEN    !Trailing space?
            IF TxtLen=1 OR ~PTxt1[1 : TxtLen-1] THEN CYCLE. !ALl blank
         END  
         FavQ:WhatText = PTxt1
         FavQ:LowerTxt = lower(PTxt1)
         FavQ:WhatLen  = TxtLen
         FavQ:Seq      = LineNo 
         ADD(FavorQ)
    END
    CLEAR(FavorQ) 
    FavQ:WhatText = FavoriteHeading
    FavQ:LowerTxt = lower(FavQ:WhatText)
    FavQ:WhatLen  = LEN(FavoriteHeading) 
    FavQ:Seq      = 0 
    FavQ:Head1    = 1
    ADD(FavorQ,1)
    DOO.FavorQSortAsDesired()
    ?List:FavorQ{PROPLIST:Header,3}='Favorites - ' & Records(FavorQ)
    RETURN     
!--------------------------------------
DOO.FavorQSortAsDesired PROCEDURE()  !Sort FavorQ based on FavorSort
    CODE 
    CASE FavorSort
    OF 0 ; SORT(FavorQ, -FavQ:Head1,  FavQ:Seq)
    OF 1 ; SORT(FavorQ, -FavQ:Head1, -FavQ:Seq)
    OF 2 ; SORT(FavorQ, -FavQ:Head1,  FavQ:LowerTxt)
    OF 3 ; SORT(FavorQ, -FavQ:Head1, -FavQ:LowerTxt) 
    END    
    RETURN 
!-------------------------------------- 
DOO.FavorQCopy2Clip PROCEDURE()
CB ANY 
X  LONG
    CODE 
    LOOP X=2 TO RECORDS(FavorQ)   
         GET(FavorQ,X)
         CB=CB & CLIP(FavQ:WhatText) & '<13,10>' 
    END
    SETCLIPBOARD(CB)
    RETURN
!--------------------------------------
DOO.ClarionPropLoadQueues   PROCEDURE()
ErrorMsg    STRING(128)    
    CODE
    FREE(FindQ)   ; FREE(MergeQ)
    Head_FindQ='' ; Head_MergeQ='' ; DISPLAY 
    ErrorMsg=DOO.ClarionPropLoadFindQ()
    IF ErrorMsg THEN 
       Message(ErrorMsg &'||' & Glo:ClaPropFileName,'ClarionPropLoadQueues Failed') 
       RETURN 
    END 
    DOO.Head_FindQ_MergeQ() 
    RETURN 
!--------------------------------    
DOO.Head_FindQ_MergeQ PROCEDURE()  !Set  Head_FindQ Head_MergeQ 
    CODE
    ?LIST:FindQ{PROPLIST:Header,3} ='String - ' & RECORDS(FindQ)  & ' records'
    ?LIST:MergeQ{PROPLIST:Header,3}='String - ' & RECORDS(MergeQ) & ' records'
    Head_FindQ =' Recent: ' & RECORDS(FindQ) & ' in ' & ltFavPatternsElement &'>'
    Head_MergeQ=' Write: '  & RECORDS(MergeQ) &            |
                    ' = ' & MergeQRecentCount &' Recent' & |
                    ' + ' & MergeQFavorCount &' Favorites + 1'
    ?ClaPropWriteBtn{PROP:Disable}=CHOOSE(~Records(MergeQ))
    DISPLAY
    RETURN 
!-------------------------    
DOO.ClarionPropLoadFindQ   PROCEDURE()!,STRING 
PatternQ    QUEUE(PatternQType),PRE(PatQ)
            END  
FindCln     CbFindCleanClass
ErrorMsg    STRING(128)
Pos1        LONG
Pos2        LONG
X           LONG
MruSeq      LONG
    CODE                
    IF ~FindCln.FileLoad(Glo:ClaPropFileName,1) THEN
        RETURN  'Error Load: ' & FindCln.LastError
    END
    IF ~FindCln.FindXmlAttribute(ltFavPatternsElement,'value',Pos1,Pos2, ,,, ErrorMsg) THEN
          IF ~ErrorMsg THEN ErrorMsg='Failed FindXmlAttribute'.
          RETURN ErrorMsg
    END 
    
    FindCln.Patterns2Queue(PatternQ,Pos1,Pos2)   !Parse Slice into Pat Q 
    IF DbView THEN WndPrvCls.QueueReflection(PatternQ,'PatternQ').
    LOOP X=1 TO RECORDS(PatternQ)
        GET(PatternQ,X)
        FndQ:WhatLen  = PatQ:WhatLen  !Len includes Trailing Space
        FndQ:WhatText = PatQ:WhatTxt 
        IF ~FndQ:WhatLen OR ~FndQ:WhatText THEN CYCLE.   !Skip Blanks
        IF FndQ:WhatLen > SIZE(FndQ:WhatText) THEN 
           FndQ:WhatLen = SIZE(FndQ:WhatText)
        END
        IF FndQ:WhatText[FndQ:WhatLen]='' THEN      !Is Trailing Space?
           FndQ:WhatText[FndQ:WhatLen]=GraveAccent  !Grave Accent to prerserve 
        END 
        FndQ:LowerTxt = lower(FndQ:WhatText)
        MruSeq += 1
        FndQ:Seq = MruSeq
        ADD(FindQ)
    END 
    IF DbView THEN WndPrvCls.QueueReflection(FindQ,'FindQ').
    RETURN('')
!----------------------------------------
DOO.MergeQueueBuild PROCEDURE() !Build MergeQ = FindQ + FavorQ
X           LONG
Seq LONG 
    CODE
    FREE(MergeQ) ; MergeQRecentCount=0
    DOO.MergeQ_AddFavorites(1)  !(1)=Seq as Zero so Sorts to top
    SORT(MergeQ,MerQ:LowerTxt)
    LOOP X=1 TO RECORDS(FindQ)
         GET(FindQ,X)
            IF FndQ:Seq < 0 THEN  !Unmark - as + (prev Marked)
               FndQ:Seq *= -1
               PUT(FindQ) 
            END 
            IF FndQ:WhatLen<=0 THEN CYCLE.  !No <DEL> Deleted in MergeQ
         IF Seq >= Glo:FavMinPatterns THEN CYCLE.   !Keep 20
         CLEAR(MergeQ)
         MergeQ :=: FindQ 
         MerQ:IsFavor = 0
         GET(MergeQ,MerQ:LowerTxt)
         IF ~ERRORCODE() THEN CYCLE.    !Duplicate
         Seq+=1
         MerQ:Seq = Seq 
         ADD(MergeQ,MerQ:LowerTxt) 
         FndQ:Seq = -1 * ABS(FndQ:Seq) ; PUT(FindQ)  !Mark with -# in Find to highlight
    END
    MergeQRecentCount=Seq
    CASE Glo:AlphaRecent 
    OF 0 ; SORT(MergeQ, MerQ:Seq)
    OF 1 ; SORT(MergeQ, MerQ:WhatText)
    END
    DOO.MergeQ_AddFavorites(0)
    DOO.Head_FindQ_MergeQ()
    RETURN 
!--------------------------------    
DOO.MergeQ_AddFavorites PROCEDURE(BYTE SeqAsZero)
Seq LONG
X   LONG
    CODE 
    IF ~SeqAsZero THEN      !2nd Time with Seq
       LOOP X=RECORDS(MergeQ) TO 1 BY -1
            GET(MergeQ,X)
            IF MerQ:IsFavor THEN DELETE(MergeQ).
       END 
    END
    MergeQFavorCount = -1
    Seq=RECORDS(MergeQ)
    CLEAR(MergeQ)
    LOOP X=1 TO RECORDS(FavorQ)   
         GET(FavorQ,X) 
         IF FavQ:WhatLen<=0 THEN CYCLE.  !Nt <DEL> Deleted in MergeQ
         Seq += 1
         IF ~SeqAsZero THEN MerQ:Seq = Seq .
         MerQ:WhatLen  = FavQ:WhatLen
         MerQ:WhatText = FavQ:WhatText
         MerQ:LowerTxt = FavQ:LowerTxt 
         MerQ:IsFavor  = X
         ADD(MergeQ) 
         MergeQFavorCount += 1
    END
    RETURN 
!--------------------------------
DOO.MergeQDeleteKey PROCEDURE()
Seq LONG
X   LONG
    CODE
    CASE MerQ:IsFavor
    OF 0                                !FindQ Delete
         FndQ:LowerTxt = MerQ:LowerTxt
         GET(FindQ,FndQ:LowerTxt)
         IF ~ERRORCODE() THEN
             FndQ:WhatLen = 0
             FndQ:WhatText='  <<DEL> ' & FndQ:WhatText
             FndQ:LowerTxt=lower(FndQ:WhatText)
             PUT(FindQ) ! DELETE(FindQ)
         END
    OF 1                                !=== Favor === Heading
         RETURN                         !Cannot delete

    ELSE                                !Delete from Favor Q
         LOOP   !Can be dups
             FavQ:LowerTxt = MerQ:LowerTxt
             GET(FavorQ,FavQ:LowerTxt)  !Delete from Find too
             IF ERRORCODE() THEN BREAK.
             FavQ:WhatLen = 0
             FavQ:WhatText='  <<DEL> ' & FavQ:WhatText
             FavQ:LowerTxt=lower(FavQ:WhatText)
             PUT(FavorQ) ! DELETE(FavorQ)
        END
    END !case
    DELETE(MergeQ)    !last, Delete Merge Q
    DOO.MergeQueueBuild()
    RETURN
!====================================================================================
DOO.WriteFavorites  PROCEDURE(STRING pClaPropXmlFN)  !Write back to File 
PatternQ    QUEUE(PatternQType),PRE(PatQ)
!WhatLen         BYTE        !PatQ:WhatLen
!WhatTxt         STRING(96)  !PatQ:WhatTxt
!LowerTxt        STRING(96)  !PatQ:LowerTxt
            END 
PatString   &STRING
PatLength   LONG
X           LONG 
DidSaveOk   BOOL
WriteOutMsg STRING(500)
FindCln     CbFindCleanClass
    CODE 
    IF ~WriteOK AND ~Glo:TestFavorite THEN  
        ?WriteOK{PROP:TRN}=''
        ?WriteOK{PROP:Background}=COLOR:Yellow
        ?WriteOK{PROP:FontColor} =COLOR:Navy
        SELECT(?WriteOK) 
        Message(?WriteOK{PROP:Tip},'Check "Write Xml"',ICON:Save)                 
        RETURN 
    END
    
    LOOP X=1 TO RECORDS(MergeQ)
        GET(MergeQ,X)          
       !PatQ WhatTxt String(size) could be < MergeQ so recalc WhatLEN
        PatQ:WhatTxt  = MerQ:WhatText
        PatQ:WhatLen  = LEN(CLIP(PatQ:WhatTxt))  !no PatQ:WhatLen=MerQ:WhatLen
        IF PatQ:WhatTxt[PatQ:WhatLen]=GraveAccent THEN 
           PatQ:WhatTxt[PatQ:WhatLen]=' '
        END
        PatQ:LowerTxt = lower(PatQ:WhatTxt)
        ADD(PatternQ)
    END
    IF ~RECORDS(PatternQ) THEN 
        Message('No patterns to save.','WriteFavorites')
        RETURN 
    END 
    IF DbView THEN WndPrvCls.QueueReflection(PatternQ,'PatternQ to Write').
    PatString &= FindCln.PatternsQ2String(PatternQ,PatLength) 
    IF ~PatLength THEN 
        Message('Unexpected Zero PatLength=' & PatLength &'  Len()=' & len(PatString),'WriteFavorites')
    ELSE 
        DidSaveOk=DOO.WriteFavorFile(pClaPropXmlFN, PatString, PatLength, WriteOutMsg)
        Message(CHOOSE(~DidSaveOk,'Save Failed :(','Saved OK!') & |
                '||' & CLIP(WriteOutMsg), 'WriteFavorites' )
    END
    DISPOSE(PatString)  
    IF DidSaveOk THEN Glo:LastSavedClaProp=pClaPropXmlFN.
    RETURN
!----------------------    
DOO.WriteFavorFile  PROCEDURE(*STRING pClaPropXmlFN, CONST *STRING PatString, LONG PatLength, *STRING OutMsg )  !Write back to File 
FindCln     CbFindCleanClass
ErrorMsg    STRING(128)
Pos1        LONG
Pos2        LONG
X           LONG
B4FavorName CSTRING(261)
B4CleanMax  CSTRING(261)  !02/10/21 Keep the First Clean assume Maximi 
DidSaveOk   BOOL
    CODE
    B4FavorName=CLIP(pClaPropXmlFN) & '.b4favor'
    B4CleanMax =CLIP(pClaPropXmlFN) & '.b4cleanMax'
    IF ~FindCln.FileLoad(pClaPropXmlFN,0) THEN  !0=Open For write
        OutMsg='Error Load: ' & FindCln.LastError
        RETURN False
    END
    
    IF ~DOO.WriteFavorSetValue(FindCln,ltFavPatternsElement,PatString,PatLength,OutMsg) THEN
        IF ~OutMsg THEN OutMsg='WriteFavorSetValue Failed'.
        RETURN False
    END
    
    IF Glo:TestFavorite THEN     !Write TEST so write .TestShrink so leave real file alone
       DidSaveOk = FindCln.FileSave(CLIP(pClaPropXmlFN) & '.TestFavor')  
       OutMsg=CHOOSE(DidSaveOk<>false,'Saved .TestFavor file of '& PatLength &' bytes', |
                                      'Error Save TestFavor: ' & FindCln.LastError) 
       RETURN DidSaveOk
    END
    !-- Save .b4favor backup before write real Cla Prop.XML ... 
    IF ~EXISTS(B4CleanMax) THEN         !02/10/21 No Max file? Keep First Clean as Max
        COPY(pClaPropXmlFN   ,B4CleanMax)    !Save current file as MAX
    END
    COPY(B4FavorName,B4FavorName &'2')          !Save 2nd backup .b4clean2 
    COPY(pClaPropXmlFN,B4FavorName)             !Save .b4clean backup with Copy
    IF ERRORCODE() THEN                         !Cannot Backup file?
       OutMsg='Error Copy .b4favor: ' & ErrorCode() &' '& Error()
       Message(CLIP(OutMsg) & |                 !Should not happen so so show msg
               '||From: ' & CLIP(pClaPropXmlFN) & '||To: ' & CLIP(B4FavorName), 'Copy Fail', ICON:Exclamation) 
       RETURN False   !Copy failed
    END
    !-- Write to real Cla Prop.XML -------------
    DidSaveOk = FindCln.FileSave(pClaPropXmlFN)
    OutMsg=CHOOSE(DidSaveOk<>false,'*{9} Cleaned Ok *{5}','Error Save: ' & FindCln.LastError) 
    RETURN DidSaveOk 
!-------------------------------
DOO.WriteFavorSetValue Procedure(CbFindCleanClass FindCln, STRING ltPatternsElement, CONST *STRING PatString, LONG PatLength, *STRING OutMsg )!,BOOL
RetBool     BOOL
Pos1        LONG
Pos2        LONG
    CODE
    IF ~FindCln.FindXmlElement(ltPatternsElement,Pos1,Pos2) THEN            !Test no <FindPatterns
        OutMsg='Did not find "' & ltPatternsElement &'"  '& OutMsg
    ELSIF ~FindCln.FindXmlAttribute(ltPatternsElement,'value',Pos1,Pos2) THEN  !Test no value= in <Find
        OutMsg='No value= in "' & ltPatternsElement &'"  '& OutMsg
    ELSE
       IF DbView THEN FindCln.ViewSlice(FindCln.XmlStr,Pos1,Pos2,'FindXmlAttrib Value ' & ltPatternsElement) .
       FindCln.NewXmlString( FindCln.XmlStr[1 : Pos1-1] & |
                             PatString & |           
                             FindCln.XmlStr[Pos2+1 : FindCln.XmlLen ] )
       RetBool=True
         IF DbView AND FindCln.FindXmlElement(ltPatternsElement,Pos1,Pos2)
            FindCln.ViewSlice(FindCln.XmlStr,Pos1,Pos2,'After FindXmlElement ' & ltPatternsElement )
         END
    END
    RETURN RetBool
    
!==============================================
GetSpecialFolder    PROCEDURE(LONG CDSL)
SHGFP_TYPE_CURRENT          EQUATE(0)       !current value for user, verify it exists
!SHGFP_TYPE_DEFAULT          EQUATE(1)       !default value, may not exist
CSIDL_FLAG_DONT_VERIFY      EQUATE(4000h)   !CSIDL_FLAG_DONT_VERIFY 0x4000 // combine with 
OutPathCStr                 CSTRING(261)    !pszPath [out] Null-terminated string of length MAX_PATH which will receive the path, does NOT have backslash
CSIDLAdd        LONG(CSIDL_FLAG_DONT_VERIFY)
Access_Token    EQUATE(0)   !0=normal  -1=Default user file locations
HR              LONG,AUTO
    CODE
    HR = SHGetFolderPath(0, CDSL+CSIDLAdd, Access_Token, SHGFP_TYPE_CURRENT, OutPathCStr)
    IF HR < 0 THEN  !If this function succeeds, it returns S_OK. Otherwise, it returns an HRESULT error code.
       OutPathCStr='C:\Failed\SHGetFolderPath\' &HR 
    END
    RETURN OutPathCStr
!==============================================
DB   PROCEDURE(STRING xMessage)
Prfx EQUATE('FindCln: ')
sz   CSTRING(SIZE(Prfx)+SIZE(xMessage)+3),AUTO
  CODE 
  sz  = Prfx & CLIP(xMessage) & '<13,10>'
  OutputDebugString( sz )