!Region MIT License and About Comments
!  MIT License
!  Copyright (c) 2021 Carl T. Barnes
!  Permission is hereby granted, free of charge, to any person obtaining a copy
!  of this software and associated documentation files (the "Software"), to deal
!  in the Software without restriction, including without limitation the rights
!  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
!  copies of the Software, and to permit persons to whom the Software is
!  furnished to do so, subject to the following conditions:
!  The above copyright notice and this permission notice shall be included in all
!  copies or substantial portions of the Software.
!  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
!  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
!  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
!  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
!  SOFTWARE.
!---------------------------------------------------------------------------------- 
!About
!The Clarion IDE writes Find and Replace strings in ClarionProperties.XML
!There is no purge of these so they grow to 1000's slowing the Find Dialog open.
!Another problem is the list is no longer sorted in LIFO order making it useless.
!This utility will trim those to a reasonable number for fast open and LIFO sorting. 
!It finds <FindPatterns value="111<C3BF>222<C3BF>333<C3BF>444" /> and shrinks
!---------------------------------------------------------------------------------- 
!endRegion License and Comments
  PROGRAM
  INCLUDE('KeyCodes.CLW')
  INCLUDE('ResDef.CLW'),ONCE  !Project needs ResCode.CLW
  INCLUDE('CbFindCleanCls.INC'),ONCE

!2 steps to use StringTheory: 1. Change _Have_ to (1), 2. Uncomment INCLUDE(..._ST')
_Have_StringTheory_   EQUATE(0)              !1. Set as (1)=Have StringTheory   (2)=Have but NOT Default
!  INCLUDE('FindCleanCwIDE_ST.clw','GLOBAL') !2. Uncomment ! Include(StringTheory

Glo:UseStringTheory BYTE !(_Have_StringTheory_)
  
  INCLUDE('CBWndPreview.INC'),ONCE             !From https://github.com/CarlTBarnes/WindowPreview
    COMPILE('!-WndPrv-',_IFDef_CBWndPreview_)  !Comment this line to remove
WndPrvCls   CBWndPreviewClass  
             !-WndPrv-
  MAP
FindCleanCwWindow   PROCEDURE()
CleanClaPropXmlFile PROCEDURE(STRING ClaPropFullName, BYTE pQuery, *ClnStatsType ClnStats, *STRING OutMsg ),BOOL
CleanXmlFindPattern PROCEDURE(CbFindCleanClass FindCln, STRING ltPatternsElement, BYTE pQuery, *IOStatsType IOStats, *STRING OutMsg ),BOOL
CleanViewPatterns   PROCEDURE(STRING ClaPropFullName, STRING ltPatternsElement) 
CleanViewSelectFile PROCEDURE(<STRING pOpenFileName>)  !FileDialog to Pick the file to view

DB                  PROCEDURE(STRING OutDebugMessage) 
DBClear             PROCEDURE() 
ExplorerOpen        PROCEDURE(STRING FolderName)
NotepadOpen         PROCEDURE(STRING FileName)
GetRegistryInstallsOfCW  PROCEDURE(QUEUE ClarionQ, *STRING ClaQ_RootPath, <*STRING ClaQ_ClarionName>, <*DECIMAL ClaQ_VersioNo>)
GetFileDateTimeSize PROCEDURE(STRING inFileName,<*LONG outDate>,<*LONG outTime>,<*LONG outSize>,<*STRING outStamp>),BOOL
GetSpecialFolder    PROCEDURE(LONG CDSL),STRING 
ListHeaderColor     PROCEDURE(LONG ListFEQ)
SetAboutText        PROCEDURE(LONG AboutFEQ) 
XmlUnEscape         PROCEDURE(*STRING InOutXML, BYTE Clip1_Size0=1),LONG,PROC   !Returns Change Out
     MODULE('Windows')
        SHGetFolderPath(Long hwndOwner=0, LONG nFolder, Long hToken, Long dwFlags, *CSTRING pszPath),LONG,RAW,PASCAL,DLL(1),NAME('SHGetFolderPathA')
        OutputDebugString(*Cstring Msg),PASCAL,RAW,DLL(1),NAME('OutputDebugStringA')
     END
  END

!Region Global Data and Equates
DbView                  SHORT(0)            !Open View String and debug Windows
ClarionProperties_xml   EQUATE('ClarionProperties.xml')
ltFindPatterns          EQUATE('<<FindPatterns')
ltReplacePatterns       EQUATE('<<ReplacePatterns')
valueEqQt               EQUATE('value="')
SettingFile             EQUATE('.\FndClnSettings.ini')
Glo:MaxPatterns     USHORT(50)      !When count is >= Max 
Glo:MinPatterns     USHORT(25)      !  then reduce to Min
Glo:AppDataSVpath   CSTRING(256) 
Glo:TestShrink      BYTE            !Write to .TestShrink.xml 
CSIDL_APPDATA       EQUATE(001ah)   !CSIDL_APPDATA 0x001a // <user name>\Application Data
IOStatsType GROUP,TYPE
CntIN        LONG
BytesIN      LONG
CntOUT       LONG
BytesOUT     LONG
            END
ClnStatsType  GROUP,TYPE
Find            GROUP(IOStatsType).
Repl            GROUP(IOStatsType).
              END
!EndRegion 

  CODE
  Glo:AppDataSVpath=GetSpecialFolder(CSIDL_APPDATA) & '\SoftVelocity\Clarion'
  !Should be: !C:\Users\ your user \AppData\Roaming\SoftVelocity\Clarion 
  FindCleanCwWindow()
  RETURN
!================================================================
FindCleanCwWindow PROCEDURE()

ClarionInstallQ QUEUE,PRE(ClaInstQ) 
VersionNo DECIMAL(5,1)  ! ClaInstQ:VersionNo E.g. 11 Number for sorting to find newest  (highest)
Clarion   STRING(16)    ! ClaInstQ:Clarion   E.g. Clarion11 ... Clarion9.1
Root      STRING(128)   ! ClaInstQ:Root      E.g. C:\Clarion11
          END

AppDataSvQ  QUEUE,PRE(AppSvQ)
SubFolder      STRING(255)       !AppSvQ:SubFolder  !SoftVelocity\Clarion\#.# folder
ClaPropFN      STRING(255)       !AppSvQ:ClaPropFN   !ClarionProperties.XML or blank
Date           LONG              !AppSvQ:Date        !Date of Cla Prop.Xml
Time           LONG              !AppSvQ:Time     
Size           LONG              !AppSvQ:Size     
PathBS         PSTRING(256)      !AppSvQ:PathBS     !Full Path c:\Users\U\AppData\Roaming\SoftVelocity\Clarion\11.0\
PathTip        STRING(256)       !AppSvQ:PathTip    
VersionNo      DECIMAL(5,2)      !AppSvQ:VersionNo  !Version Number for Sort 
Root           STRING(128)       !AppSvQ:Root       !Only used with BinSettngQ
            END
BinSettngQ  QUEUE(AppDataSvQ),PRE(BinSetQ)  !CW Install Queue for C:\Clarion11\bin\Settings
            END        
ConfigDirQ  QUEUE(AppDataSvQ),PRE(CfgDirQ)
            END 
ConfigDir_Txt_FN    EQUATE('ConfigDir.txt')
ConfigDirTxt_TEXT   STRING(128*80)    !Contents of ConfigDir.Txt  80 paths x 128 bytes
ConfigTextEditThis  BYTE
JustDoConfigDirs    BYTE        !For testing ignore AppData and BinSettings

CleanQ  QUEUE  !,PRE(ClnQ)
FromQ       STRING(7)           !CleanQ.FromQ     = 
SubFolder   STRING(20)          !CleanQ.SubFolder = 
Find        GROUP(IOStatsType). !CleanQ.Find      
Repl        GROUP(IOStatsType). !CleanQ.Repl      
PathBS      PSTRING(256)        !CleanQ.PathBS    = 
PathTip     STRING(256)         !CleanQ.PathTip   = 
CleanMsg    STRING(512)         !CleanQ.CleanMsg  = 
CleanTip    STRING(512)         !CleanQ.CleanTip  = 
VersionNo   DECIMAL(5,2)        !CleanQ.VersionNo =  
PathLwrBS   PSTRING(256)        !CleanQ.PathLwrBS = 
        END 

ClnGrp  GROUP
FromQ       STRING(7)           !CleanQ.FromQ     = 
SubFolder   STRING(20)          !CleanQ.SubFolder = 
Find        GROUP(IOStatsType). !CleanQ.Find      = 
Repl        GROUP(IOStatsType). !CleanQ.Repl      = 
        END       
NoShowAbout BYTE
WriteOK     BYTE      
IDEClosed   BYTE      
Window WINDOW('Clarion IDE Find Patterns Clean / Shrink in ClarionProperties.xml'),AT(,,476,203),GRAY,IMM,SYSTEM, |
            ICON('FindCln.ico'),FONT('Segoe UI',10,,FONT:regular),RESIZE
        SHEET,AT(3,3,470,197),USE(?Sheet1)
            TAB(' ClarionProperties.Xml '),USE(?TAB:Clean)
                BOX,AT(9,17,457,10),USE(?Box_Clean),FILL(COLOR:INACTIVECAPTION),LINEWIDTH(1)
                PROMPT('This tool Shrinks the ClarionProperties.xml <<FindPatterns /> that the Editor Find Dialog grows ' & |
                        'forever which slows Find opening.'),AT(13,17),USE(?WhatsCleanFor),TRN
                LIST,AT(10,55,456,140),USE(?List:CleanQ),VSCROLL,FROM(CleanQ),FORMAT('[75L(2)|FM~<13,10>From Tab~@s8@/75' & |
                        'R(2)|_FM~Folder~@s255@]|[24R(2)|M~Cnt~C(0)@n4@#4#34R(2)|M~Bytes~C(0)@n7@/24R(2)|_M@n4b@Q''Find ' & |
                        'Count After''34R(2)|_M@n7b@]|~Find Patterns<13,10>Before/After~[24R(2)|M~Cnt~C(0)@n4@#9#34R(2)|' & |
                        'M~Bytes~C(0)@n7@/24R(2)|_M@n4b@Q''Replace Count After''34R(2)|_M@n7b@]|~Replace Pattrns<13,10>B' & |
                        'efore/After~[257L(2)P~Delete Key will remove rows to omit from shrink<13,10>Path  -  Double Cli' & |
                        'ck to Open - Right Click for Options~@s255@Z(1)/257R(2)_P~Clean message~@s255@]'),ALRT(DeleteKey)
                BUTTON('Query'),AT(10,32,52,16),USE(?QueryBtn),ICON(ICON:Zoom),TIP('Scan the files and show pattern counts'), |
                        LEFT
                PROMPT('Maximum Count:'),AT(72,30,58),USE(?Glo:MaxPatterns:Prompt),TRN,RIGHT
                ENTRY(@n3),AT(134,30,19,9),USE(Glo:MaxPatterns),CENTER,TIP('When count is >= Maximum<13,10>then reduce t' & |
                        'o "Shrink To"')
                PROMPT('Shrink To Count:'),AT(72,41,58),USE(?Glo:MinPatterns:Prompt),TRN,RIGHT
                ENTRY(@n3),AT(134,41,19,9),USE(Glo:MinPatterns),CENTER
                BUTTON('Shrink Patterns'),AT(166,32,82,16),USE(?ShrinkBtn),ICON(ICON:Save),TIP('Shrink the patterns in t' & |
                        'he Cla Prop XML'),LEFT
                BUTTON,AT(453,32,12,12),USE(?CopyCleanBtn),SKIP,ICON(ICON:Copy),TIP('Copy table tab delimited for paste ' & |
                        'into Excel. <13,10>A way to log history'),FLAT
                CHECK('Write to Clarion Prop Xml'),AT(260,32),USE(WriteOK),FONT(,9),TIP('Check "Write to Clarion"  to al' & |
                        'low writing ClarionProperties.XML<13,10>with the "Shrink Patterns" for Find/Replace.<13><10,13>' & |
                        '<10>Two backup copies are kept with extension ".b4clean" to allow recovery.<13,10,13><10>To tes' & |
                        't this tool check the "Write to Test Shrink" box<13,10>to instead write to file ClarionProperti' & |
                        'es.Xml.TestShrink ')
                CHECK('Write to .TestShrink.Xml'),AT(260,40),USE(Glo:TestShrink),FONT(,9),TIP('Check to write shrunk fil' & |
                        'e to ClarionProperties.Xml.TestShrink.Xml<13,10><13,10>Compare the .TestShrink file to the orig' & |
                        'inal .Xml to view changes.')
                CHECK('Clarion IDE Closed ?'),AT(361,32),USE(IDEClosed),FONT(,9),TIP('All Clarion IDE''s updated must be' & |
                        ' closed to "Shrink Patterns".<13,10><13,10>If Clarion is left open it will re-write the XML fil' & |
                        'e with the too BIG patterns when closed.<13,10><13,10>TestShrink can be written with Clarion open.')
                CHECK('Capesoft StringTheory'),AT(361,40),USE(Glo:UseStringTheory),FONT(,9,095491FH,FONT:bold), |
                        TIP('Check for Geoff''s code using StringTheory from www.CapeSoft.com <13,10>Suggested for Devel' & |
                        'opers South of the Equator<13,10><13,10>Uncheck to use Carl''s code - "The Northern Solution"')
            END
            TAB(' 1. AppData SoftVelocity '),USE(?TAB:AppData)
                BOX,AT(9,18,457,10),USE(?Box_AppData),FILL(COLOR:INACTIVECAPTION),LINEWIDTH(1)
                PROMPT('AppData Roaming is the default location the Clarion IDE saves ClarionProperties.Xml. The /Config' & |
                        'Dir [=path] overrides the location.'),AT(12,18),USE(?AppDataSVpath:FYI),TRN
                ENTRY(@s255),AT(10,29,458,10),USE(Glo:AppDataSVpath),SKIP,TRN,FONT('Consolas'),READONLY
                LIST,AT(9,41,457,153),USE(?List:AppDataSvQ),VSCROLL,FROM(AppDataSvQ),FORMAT('32L(2)|FM~Folder~C(0)@s255@' & |
                        '80L(2)|M~Clarion Properties~@s255@34R(2)|M~Date~C(0)@d1b@34R(2)|M~Time~C(0)@T3b@40R(2)|M~Size~C' & |
                        '(0)@n11b@257L(2)P~Path  -  Double Click to Open~@s255@Z(1)'),ALRT(DeleteKey)
            END
            TAB(' 2. CW Bin \ Settings '),USE(?TAB:Installs)
                BOX,AT(9,18,459,10),USE(?Box_Reg),FILL(COLOR:INACTIVECAPTION),LINEWIDTH(1)
                PROMPT('Clarion Installs found in Registry:  HKLM\SOFTWARE\SoftVelocity\Clarion'),AT(12,18),USE(?Regsitry:FYI), |
                        TRN
                LIST,AT(9,31,458,58),USE(?LIST:ClarionInstallQ),VSCROLL,FROM(ClarionInstallQ),FORMAT('33R(7)|M~Verison~C' & |
                        '(0)@n4.1@70L(2)|M~Clarion  (SubKey)~@s16@120L(2)~Root Path  (HKLM,SOFTWARE\SoftVelocity\Clarion' & |
                        '#,ROOT)~@s128@'),ALRT(DeleteKey)
                PROMPT('The above installs normally look for ClarionProperties.Xml in AppData (see AppData Tab) unless r' & |
                        'un with /ConfigDir switch.'),AT(9,92),USE(?PROMPT:Regsitry:AppData),TRN
                PROMPT('If you have other installs, not in the Registry, they will need to be listed on the "/ConfigDir ' & |
                        '.Txt" tab.'),AT(9,101),USE(?PROMPT:Installs1),TRN
                PROMPT('Running "Clarion.Exe /ConfigDir" (without =Path) will look in the Root\Bin\Setting folder for Cl' & |
                        'arionProperties.xml as shown below.'),AT(9,110),USE(?PROMPT:Installs2),TRN
                LIST,AT(9,122,458,73),USE(?List:BinSettngQ),HVSCROLL,FROM(BinSettngQ),FORMAT('45L(2)|FM~Folder~C(0)@s255' & |
                        '@80L(2)|M~Clarion Properties~@s255@34R(2)|M~Date~C(0)@d1b@34R(2)|M~Time~C(0)@T3b@40R(2)|M~Size~' & |
                        'C(0)@n11b@257L(2)P~Root Path or Settings Path  -  Double Click to Open~@s255@'),ALRT(DeleteKey)
            END
            TAB(' 3. /ConfigDir .Txt '),USE(?TAB:Other)
                CHECK('Just This'),AT(10,20),USE(JustDoConfigDirs),SKIP,TRN,FONT(,9),TIP('Only Process ConfigDir files. ' & |
                        'Omit AppData and Bin Settings.<13><10>Good for testing specific files.<13,10>Put JUST on first ' & |
                        'line of ConfigDir.TXT to check this box.')
                CHECK('Edit This'),AT(10,33),USE(ConfigTextEditThis),SKIP,TRN,FONT(,9),TIP('Allow Editing ConfigDir.TXT')
                BUTTON('Re-Parse'),AT(9,42,42,11),USE(?ConfigDirTxtReParseBtn),DISABLE,SKIP,FONT(,9),TIP('Re-Parse the e' & |
                        'ditted ConfigDir.Txt')
                BUTTON('Clear Text'),AT(9,56,42,11),USE(?ConfigDirTxtClearTextBtn),DISABLE,SKIP,FONT(,9),TIP('Clear Conf' & |
                        'igDir.TXT')
                BUTTON('Reload'),AT(9,71,42,11),USE(?ConfigDirTxtReloadBtn),SKIP,FONT(,9),TIP('Reload ConfigDir.Txt')
                BUTTON('Notepad'),AT(9,85,42,11),USE(?ConfigDirTxtNotepadBtn),SKIP,FONT(,9),TIP('Open ConfigDir.Txt in N' & |
                        'otepad')
                BUTTON('Explore'),AT(9,103,42,11),USE(?ConfigDirTxtExploreBtn),SKIP,FONT(,9)
                BUTTON('Clear<13,10>List'),AT(9,144,42,20),USE(?ConfigDirTxtFreeListBtn),SKIP,FONT(,9),TIP('Clear / Free' & |
                        ' List of ConfigDir Paths')
                BOX,AT(7,18,460,15),USE(?Box_Cfg),FILL(COLOR:INACTIVECAPTION),LINEWIDTH(1)
                PROMPT('Create a file named "ConfigDir.TXT" and list folders used with \ConfigDir= or non-registry insta' & |
                        'll Bin\Settings folders.<13,10>End Path with \* to load all sub folders containing ClarionPrope' & |
                        'rties.xml. "END" on line alone stops parsing Txt.'),AT(58,18,361,16),USE(?PROMPT:CfgDirFYI),TRN, |
                        FONT(,9)
                TEXT,AT(58,36,408,60),USE(ConfigDirTxt_TEXT),SKIP,HVSCROLL,FONT('Consolas',,02E00H),COLOR(0F9F9F9H), |
                        TIP('From ConfigDir.Txt.<13><10>Edits are not saved.'),READONLY
                LIST,AT(58,103,409,92),USE(?List:ConfigDirQ),VSCROLL,FROM(ConfigDirQ),FORMAT('36L(2)|FM~Line~C(0)@s255@8' & |
                        '0L(2)|M~Clarion Properties~@s255@34R(2)|M~Date~C(0)@d1b@34R(2)|M~Time~C(0)@T3b@40R(2)|M~Size~C(' & |
                        '0)@n11b@257L(2)P~Path  -  Double Click to Open~@s255@'),ALRT(DeleteKey)
            END
            TAB('  About  '),USE(?TAB:About)
                BOX,AT(9,18,459,10),USE(?Box_About),FILL(COLOR:INACTIVECAPTION),LINEWIDTH(1)
                PROMPT('Find Clean written by Carl Barnes. Inspired by Bruce Johnson. StringTheory code by Geoff Robinson'), |
                        AT(12,18),USE(?About:FYI),TRN
                CHECK(' Stop Showing About'),AT(383,18),USE(NoShowAbout),SKIP,TRN,FONT(,9),TIP('Check box to not show about')
                TEXT,AT(10,31,457,164),USE(?AboutTEXT),VSCROLL,FONT('Calibri',11),READONLY
            END
        END
    END

DOO   CLASS         !Routines in a Class
WindowInitPretty    PROCEDURE()                 !Make LIST and BOX Look better. Init Resize
LoadAppDataQueue    PROCEDURE()                 !Load \AppData\Roaming\SV\Clarion\*
LoadBinSettngQueue  PROCEDURE()                 !Load installs in Registry \Bin\Settings 
LoadConfigDirTxt    PROCEDURE(BOOL ParseOnly=0) !Load ConfigDir.Txt with /ConfigDir=xxx paths
LoadCleanQueue      PROCEDURE()                 !Load 1 Q of files to process from AppData and Cw Installs and ConfigDir
LoadCleanQFrom1Q    PROCEDURE(AppDataSvQ Add1Q, STRING FromQName, BYTE From123 )  !Load Add1Q into CleanQ
CleanListPopup      PROCEDURE()                 !Rt Mouse Popup for Clean List
CleanTheFiles       PROCEDURE(BYTE pQuery)      !Scan CleanQ files and Shrink
CopyCleanQ2Clip     PROCEDURE()
ValidateParms       PROCEDURE()                 !Check Min/Max and maybe other are good
YellowControl       PROCEDURE(LONG pFEQ, BOOL pOnOff=1)   !Make Checks Yellow to hightlight
      END
WindowInitialized BOOL
SettingsCls   CLASS
LoadAll PROCEDURE()      
SaveAll PROCEDURE() 
Get1    PROCEDURE(STRING EntryName, STRING DefaultValue),STRING 
Put1    PROCEDURE(STRING EntryName, STRING SaveValue)
             END 
WinResize WindowResizeType            
    CODE
    SYSTEM{7A58H}=1                 !C10 PROP:PropVScroll    EQUATE(7A58H)  ! boolean: kind of listbox vertical scrollbar
    SYSTEM{7A7Dh}=MSGMODE:CANCOPY   !C11 PROP:MsgModeDefault EQUATE(7A7DH)  ! uint SYSTEM only, default value to be uysed in the MESSGE Style attribute when ommited
    SettingsCls.LoadAll()
    GetRegistryInstallsOfCW(ClarionInstallQ, ClaInstQ:Root, ClaInstQ:Clarion, ClaInstQ:VersionNo)
    SORT(ClarionInstallQ, -ClaInstQ:VersionNo, -ClaInstQ:Clarion, -ClaInstQ:Root )
    DOO.LoadAppDataQueue()
    DOO.LoadBinSettngQueue() 
    OPEN(Window)               !<--- Windows is Open
    COMPILE('!-WndPrv-',_IFDef_CBWndPreview_)  
       WndPrvCls.Init(1)   
       WndPrvCls.InitList(?List:CleanQ,     CleanQ,    'CleanQ')        !InitList() NOT required >= 11.13505 
       WndPrvCls.InitList(?List:AppDataSvQ, AppDataSvQ,'AppDataSvQ')    !InitList() adds FromQ to List window
       WndPrvCls.InitList(?List:ClarionInstallQ,ClarionInstallQ ,'ClarionInstallQ')   
       WndPrvCls.InitList(?List:BinSettngQ, BinSettngQ ,'BinSettngQ')   
       WndPrvCls.InitList(?List:ConfigDirQ, ConfigDirQ ,'ConfigDirQ')   
             !-WndPrv-
    DOO.WindowInitPretty()            !Make LIST and BOX Look better. Init Window Resize
    DOO.LoadConfigDirTxt()            !Window MUST be open, uses TEXT for getting lines 
    ?ConfigDirTxtExploreBtn{PROP:Tip}='Open Explorer to ' & LongPath()
    SetAboutText(?AboutTEXT)
    WindowInitialized = 1
    DOO.LoadCleanQueue() ; DOO.CleanTheFiles(1)  ! POST(EVENT:Accepted, ?QueryBtn) 
    IF ~NoShowAbout AND ~JustDoConfigDirs THEN SELECT(?TAB:About).
    ACCEPT
        CASE EVENT()
        OF EVENT:Sized  ; POST(EVENT:DoResize,0,THREAD())   !for WinRsz  
        END
        CASE ACCEPTED()
        OF ?QueryBtn ; DOO.LoadCleanQueue() ; DOO.CleanTheFiles(1) ; DISPLAY 
        OF ?ShrinkBtn
            DOO.ValidateParms()
            IF ~Glo:TestShrink THEN 
               IF ~WriteOK THEN 
                   DOO.YellowControl(?WriteOK)
                   IF ~IDEClosed THEN DOO.YellowControl(?IDEClosed).
                   SELECT(?WriteOK) 
                   Message(?WriteOK{PROP:Tip},'Check Write Ok',ICON:Save)                 
                   CYCLE 
               END
               IF ~IDEClosed THEN 
                   DOO.YellowControl(?IDEClosed)
                   SELECT(?IDEClosed) 
                   Message(?IDEClosed{PROP:Tip},'Check IDE Closed')                 
                   CYCLE 
               END 
               
            END
            DOO.YellowControl(?WriteOK,False)
            DOO.YellowControl(?IDEClosed,False)
            DOO.CleanTheFiles(0) 
            DISPLAY
            
        OF ?WriteOK        ; IF WriteOK THEN Glo:TestShrink=0. ; DISPLAY            
        OF ?Glo:TestShrink ; IF Glo:TestShrink THEN WriteOK=0. ; DISPLAY 
        OF ?Glo:UseStringTheory ; IF ~_Have_StringTheory_ THEN
                                     Message('StringTheory is not INCLUDE() in build.' & |
                                             '||Change "_Have_StringTheory_ Equate(0)" to (1)' & |
                                             '||About line 33 in FindCleanCwIDE.clw','StringTheory') 
                                     Glo:UseStringTheory=0  ; DISPLAY        
                                  END
        OF ?Glo:MinPatterns
            IF Glo:MinPatterns < 2   THEN Glo:MinPatterns = 2.
            IF Glo:MinPatterns > 900 THEN Glo:MinPatterns = 900.
            IF Glo:MaxPatterns < Glo:MinPatterns THEN Glo:MaxPatterns = Glo:MinPatterns + 20.
            DISPLAY
        OF ?Glo:MaxPatterns
            IF Glo:MaxPatterns < Glo:MinPatterns THEN Glo:MaxPatterns = Glo:MinPatterns + 20.
            DISPLAY 
        OF ?CopyCleanBtn             ; DOO.CopyCleanQ2Clip()
        OF ?ConfigDirTxtReloadBtn    ; DOO.LoadConfigDirTxt(False) ; DISPLAY
        OF ?ConfigDirTxtNotepadBtn   ; RUN('Notepad ' & ConfigDir_Txt_FN)  
        OF ?ConfigDirTxtExploreBtn   ; ExplorerOpen(LongPath()) 
        OF ?ConfigDirTxtReParseBtn   ; DOO.LoadConfigDirTxt(True)  ; DISPLAY 
        OF ?ConfigDirTxtClearTextBtn ; CLEAR(ConfigDirTxt_TEXT)  ; DISPLAY 
        OF ?ConfigDirTxtFreeListBtn  ; FREE(ConfigDirQ) ; JustDoConfigDirs=0 ; DISPLAY 
        
        OF ?ConfigTextEditThis
           Disable(?)
           ?ConfigDirTxt_TEXT{PROP:FontColor}=COLOR:None
           ?ConfigDirTxt_TEXT{PROP:Background}=COLOR:None
           ?ConfigDirTxt_TEXT{PROP:ReadOnly}=False
           ?ConfigDirTxt_TEXT{PROP:Skip}=False 
           ENABLE(?ConfigDirTxtReParseBtn)
           ENABLE(?ConfigDirTxtClearTextBtn) 
           ?ConfigDirTxtReloadBtn{PROP:Text}='clear + load'
           ?ConfigDirTxtReloadBtn{PROP:Tip}='Clear Editted Text and Reload ... Losing any edits' & |
                                            '<13,10,13,10>Press ReLoad button to load Editted Text paths'
           DISPLAY ; SELECT(?ConfigDirTxt_TEXT)
        END

        CASE FIELD()
        OF ?List:CleanQ
           GET(CleanQ,CHOICE(?List:CleanQ))
           CASE EVENT()
           OF EVENT:AlertKey
              IF KEYCODE()=DeleteKey THEN DELETE(CleanQ).
           OF EVENT:NewSelection
              CASE KEYCODE()
              OF MouseLeft2 ; ExplorerOpen(CleanQ.PathBS)
              OF MouseRight ; DOO.CleanListPopup()                
              END
           END
                       
        OF ?List:AppDataSvQ
           GET(AppDataSvQ,CHOICE(?List:AppDataSvQ))
           CASE EVENT()
           OF EVENT:AlertKey
              IF KEYCODE()=DeleteKey THEN DELETE(AppDataSvQ).
           OF EVENT:NewSelection 
              CASE KEYCODE()
              OF MouseLeft2 ; ExplorerOpen(AppSvQ:PathBS)
              END
           END 
           
        OF ?LIST:ClarionInstallQ
           GET(ClarionInstallQ,CHOICE(?LIST:ClarionInstallQ))
           CASE EVENT()
           OF EVENT:AlertKey
              IF KEYCODE()=DeleteKey THEN DELETE(ClarionInstallQ).
           OF EVENT:NewSelection
              IF KEYCODE()=MouseLeft2 THEN ExplorerOpen(ClaInstQ:Root).
           END

        OF ?List:BinSettngQ
           GET(BinSettngQ,CHOICE(?List:BinSettngQ))
           CASE EVENT()
           OF EVENT:AlertKey
              IF KEYCODE()=DeleteKey THEN DELETE(BinSettngQ).
           OF EVENT:NewSelection
              IF KEYCODE()=MouseLeft2 THEN ExplorerOpen(BinSetQ:PathBS).
           END

        OF ?List:ConfigDirQ
           GET(ConfigDirQ,CHOICE(?List:ConfigDirQ))
           CASE EVENT()
           OF EVENT:AlertKey
              IF KEYCODE()=DeleteKey THEN DELETE(ConfigDirQ).
           OF EVENT:NewSelection
              CASE KEYCODE()
              OF MouseLeft2 ; ExplorerOpen(CfgDirQ:PathBS)
              END
           END
           
        END
    END 
    SettingsCls.SaveAll()
    RETURN
!================================================================
DOO.ValidateParms PROCEDURE()    !Check Min/Max and maybe other are good
    CODE  
    IF Glo:MinPatterns < 2   THEN Glo:MinPatterns = 2.
    IF Glo:MinPatterns > 900 THEN Glo:MinPatterns = 900.
    IF Glo:MaxPatterns < Glo:MinPatterns THEN Glo:MaxPatterns = Glo:MinPatterns + 20.
    IF WindowInitialized THEN DISPLAY.
    RETURN 
!================================================================
DOO.YellowControl PROCEDURE(LONG pFEQ, BOOL pOnOff=1)   !Make Checks Yellow to hightlight
    CODE
    pFEQ{PROP:TRN}       =CHOOSE(~pOnOff)  !No TRN to have Background
    pFEQ{PROP:Background}=CHOOSE(~pOnOff,COLOR:None,COLOR:Yellow)
    pFEQ{PROP:FontColor} =CHOOSE(~pOnOff,COLOR:None,COLOR:Navy)
    RETURN    
!====================================================================================
DOO.LoadAppDataQueue    PROCEDURE()   !Load \AppData\Roaming\SV\Clarion\...
QNdx    LONG,AUTO 
DirQ    QUEUE(FILE:Queue),PRE(DirQ)
        END ! DirQ:Name  DirQ:ShortName  DirQ:Date  DirQ:Time  DirQ:Size  DirQ:Attrib
    CODE
    FREE(AppDataSvQ) 
    DIRECTORY(DirQ,Glo:AppDataSVpath & '\*.*',ff_:NORMAL+ff_:DIRECTORY)
    
    LOOP QNdx = 1 TO RECORDS(DirQ)   !--Keep directories ##.## skip others
         GET(DirQ,QNdx)
         IF ~BAND(DirQ:Attrib,FF_:Directory) OR DirQ:Name='.' OR DirQ:Name='..' THEN   !. or .. Dirs
            CYCLE
         END
         IF ~STRPOS(DirQ:Name,'^[1-9][0-9]?.[0-9]+$') THEN   !Only numeric folders  #.# 
            CYCLE  !If AppData has "11.0 - Copy" skip that, could be option? 
         END
         CLEAR(AppDataSvQ)
         AppSvQ:SubFolder = DirQ:Name 
         AppSvQ:ClaPropFN = ClarionProperties_xml
         AppSvQ:PathBS    = Glo:AppDataSVpath & '\' & CLIP(AppSvQ:SubFolder) &'\' 
         AppSvQ:PathTip   = AppSvQ:PathBS
         AppSvQ:VersionNo = DEFORMAT(DirQ:Name) 
         IF ~GetFileDateTimeSize(AppSvQ:PathBS & AppSvQ:ClaPropFN ,AppSvQ:Date,AppSvQ:Time,AppSvQ:Size) THEN 
             AppSvQ:ClaPropFN ='' 
         END
         ADD(AppDataSvQ,-AppSvQ:VersionNo,AppSvQ:SubFolder)
         
    END !LOOP files
    RETURN
 
!================================================================
DOO.LoadBinSettngQueue  PROCEDURE()   !Load installs in Registry \Bin\Settings
QNdx    LONG,AUTO 
    CODE
    FREE(BinSettngQ)     
    LOOP QNdx = 1 TO RECORDS(ClarionInstallQ)   !--Keep directories ##.## skip others
         GET(ClarionInstallQ,QNdx)
         CLEAR(BinSettngQ)
         BinSetQ:SubFolder = ClaInstQ:Clarion 
         BinSetQ:ClaPropFN = ClarionProperties_xml
         BinSetQ:PathBS    = CLIP(ClaInstQ:Root) & '\BIN\Settings\' 
         BinSetQ:VersionNo = ClaInstQ:VersionNo
         IF ~GetFileDateTimeSize(BinSetQ:PathBS & BinSetQ:ClaPropFN ,BinSetQ:Date,BinSetQ:Time,BinSetQ:Size) THEN 
             BinSetQ:ClaPropFN =''
             BinSetQ:PathBS    = CLIP(ClaInstQ:Root) & '\'  !No Settings
         END
         BinSetQ:PathTip = BinSetQ:PathBS
         BinSetQ:Root    = CLIP(ClaInstQ:Root)
         ADD(BinSettngQ,-BinSetQ:VersionNo,BinSetQ:SubFolder,BinSetQ:Root)         
    END !LOOP files
    RETURN
!================================================================  
DOO.LoadConfigDirTxt    PROCEDURE(BOOL pParseOnly=0)
FndCln  CbFindCleanClass    !Use class to Load file
LineNo  LONG 
LenPth  LONG 
LenFN   LONG 
Pth     STRING(300)
    CODE
    FREE(ConfigDirQ) 
    IF ~pParseOnly THEN 
        IF ~EXISTS(ConfigDir_Txt_FN) THEN
            ConfigDirTxt_TEXT='; ' & ConfigDir_Txt_FN & '  file does not exist. ' & |
                '<13,10>; Enter one per line each of for your /ConfigDir=Path folders and other installs. ' & |
                '<13,10>; Or end the Path with "\*" to load all sub folders containing: ' & ClarionProperties_xml & | 
                '<13,10>; Can find all sub folders with DOS command: DIR /S /B ' & ClarionProperties_xml & | 
                '<13,10>; Limit testing by checking "Just This" and only these folders are scanned. ' & | 
                '<13,10>; Put "JUST" on first line to check "Just This". Put "END" on a line to stop loading. '
            RETURN 
        END 
        IF ~FndCln.FileLoad(ConfigDir_Txt_FN,1) THEN 
            ConfigDirTxt_TEXT=ConfigDir_Txt_FN & ' Failed Load: ' & FndCln.LastError
            RETURN         
        END 
        ConfigDirTxt_TEXT = FndCln.XmlStr ; DISPLAY
    END !If Parse
    
    !Use TEXT control to read Prop:Line's with 1 folder each line
    LOOP LineNo=1 TO ?ConfigDirTxt_TEXT{PROP:LineCount}
         Pth=LEFT(?ConfigDirTxt_TEXT{PROP:Line,LineNo})
         IF UPPER(Pth) = 'END' THEN BREAK.  !Line with just END stops loading
         CASE UPPER(Pth) 
         OF 'JUST' 
         OROF 'JUSTTHIS'
         OROF 'JUST THIS'               ; JustDoConfigDirs=1 
                                          IF ~WindowInitialized THEN SELECT(?JustDoConfigDirs).
                                          CYCLE
         OF 'TEST SHRINK' 
         OROF 'TESTSHRINK'              ; Glo:TestShrink=1    ; WriteOK=0 ; IDEClosed=0 ; CYCLE
         OF 'END' OROF '[END]'          ; BREAK      !Line with just END stops loading
         END
         IF Pth <= ' '   THEN CYCLE.        !No Blanks
         IF Pth[1] = ';' THEN CYCLE.        ! ; Comment skip
         IF Pth[1] = '!' THEN CYCLE.        ! ! Comment

         LenPth = LEN(CLIP(Pth)) 
         IF LenPth > 260 THEN CYCLE.    ! assume too long is bad so skip
         LenFN  = LEN(ClarionProperties_xml) 
         !Allow ConfigDir file line to end with "ClarionProperties.xml" in case gets in there
         IF UPPER(SUB(Pth,LenPth - LenFN  ,99 )) = '\' & UPPER(ClarionProperties_xml) THEN  
            Pth = SUB(Pth,1,LenPth - LenFN - 1)        !Cut off \ClaProp.xml
            IF ~Pth THEN CYCLE.
            LenPth = LEN(CLIP(Pth))
         END
         IF SUB(Pth,LenPth-1,99)='\*' THEN  
            DO AsteriskLoadsSubDirsRtn 
            CYCLE
         END
         IF SUB(Pth,LenPth,99)<>'\' THEN Pth[LenPth+1] = '\'.                 !Add trailing \
         IF Pth[2:3]=':\' OR Pth[1:3]='..\' THEN Pth[1:4]=UPPER(Pth[1:4]).    !   X:\U
         IF Pth[1:2]='\\' OR Pth[1:2]='.\'  THEN Pth[3]=UPPER(Pth[3]).        !  \\U or .\U
         
         CLEAR(ConfigDirQ)
         CfgDirQ:SubFolder = 'Line# ' & LineNo 
         CfgDirQ:ClaPropFN = ClarionProperties_xml
         CfgDirQ:PathBS    = CLIP(Pth) 
         CfgDirQ:PathTip   = CfgDirQ:PathBS                                 
         CfgDirQ:VersionNo = LineNo
         IF ~GetFileDateTimeSize(CfgDirQ:PathBS & CfgDirQ:ClaPropFN ,CfgDirQ:Date,CfgDirQ:Time,CfgDirQ:Size) THEN 
             CfgDirQ:ClaPropFN ='' 
         END         
         ADD(ConfigDirQ)
    END !LOOP ConfigDirTxt_TEXT PROP:Line,#
    RETURN

AsteriskLoadsSubDirsRtn ROUTINE 
    DATA
AsteriskPathBS  PSTRING(256)  
QNdx    LONG,AUTO 
DirQ    QUEUE(FILE:Queue),PRE(DirQ)
        END                 ! DirQ:Name  DirQ:ShortName  DirQ:Date  DirQ:Time  DirQ:Size  DirQ:Attrib
!WndPrv CBWndPreviewClass        
    CODE
    AsteriskPathBS = SUB(Pth,1,LenPth-1)    !Cutoff * 
    DIRECTORY(DirQ,AsteriskPathBS & '*.*' ,ff_:NORMAL+ff_:DIRECTORY) 
    ! WndPrv.QueueReflection(DirQ,'DirQ ' & AsteriskPathBS )
    LOOP QNdx = RECORDS(DirQ) TO 1 BY -1
         GET(DirQ,QNdx)
         IF ~BAND(DirQ:Attrib,FF_:Directory) OR DirQ:Name='.' OR DirQ:Name='..' THEN   !. or .. Dirs
            DELETE(DirQ)
            CYCLE
         END         
         DirQ:Name[1]=UPPER(DirQ:Name[1])
         DirQ:ShortName=UPPER(DirQ:Name)
         PUT(DirQ)
    END
    SORT(DirQ,DirQ:ShortName,DirQ:Name) 
    LOOP QNdx = 1 TO RECORDS(DirQ)   !--Keep directories with ClaProp.xml
         GET(DirQ,QNdx)
         CLEAR(ConfigDirQ)
         CfgDirQ:SubFolder = 'Line# ' & LineNo &'*'
         CfgDirQ:ClaPropFN = ClarionProperties_xml
         CfgDirQ:PathBS    = AsteriskPathBS & CLIP(DirQ:Name) &'\'
         CfgDirQ:PathTip   = CfgDirQ:PathBS                                 
         CfgDirQ:VersionNo = LineNo
         IF ~GetFileDateTimeSize(CfgDirQ:PathBS & CfgDirQ:ClaPropFN ,CfgDirQ:Date,CfgDirQ:Time,CfgDirQ:Size) THEN 
             CYCLE   !Omit folders w/o Cla Prop
             !CfgDirQ:ClaPropFN ='' 
         END         
         ADD(ConfigDirQ)         
    END !LOOP DirQ files 
    EXIT
!================================================================
DOO.LoadCleanQueue      PROCEDURE()  !Load 1 Q of all files to process from AppData and Cw Installs and ConfigDir
QNdx    LONG,AUTO
    CODE 
    FREE(CleanQ) 
    IF WindowInitialized THEN DISPLAY. ; YIELD()  
    IF ~JustDoConfigDirs THEN 
       DOO.LoadCleanQFrom1Q(AppDataSvQ, 'AppData', 1 )   
       DOO.LoadCleanQFrom1Q(BinSettngQ, 'CW Bin' , 2 )
    END
    DOO.LoadCleanQFrom1Q(ConfigDirQ, 'CfgDir' , 3 ) 
    RETURN 
!-----------------------------
DOO.LoadCleanQFrom1Q  PROCEDURE(AppDataSvQ Add1Q, STRING FromQName, BYTE From123 )  !Load Add1Q into CleanQ
QNdx    LONG,AUTO
    CODE 
    LOOP QNdx=1 TO RECORDS(Add1Q)
        GET(Add1Q,QNdx)
        IF ~Add1Q.ClaPropFN THEN CYCLE.     !no ClarionProperties_xml   
        
        IF From123=3 THEN                   !ConfigDir could duplicate so prevent
           CleanQ.PathLwrBS = lower(Add1Q.PathBS)
           GET(CleanQ,CleanQ.PathLwrBS)
           IF ~ERRORCODE() THEN CYCLE.
        ENd
        CLEAR(CleanQ)
        CleanQ.FromQ     = FromQName        !e.g. 'AppData'
        CleanQ.SubFolder = Add1Q.SubFolder
       !CleanQ.Find      = 
       !CleanQ.Repl      = 
        CleanQ.PathBS    =       Add1Q.PathBS
        CleanQ.PathTip   =       Add1Q.PathBS
        CleanQ.PathLwrBS = lower(Add1Q.PathBS)
        CleanQ.VersionNo = Add1Q.VersionNo
        ADD(CleanQ)
    END
    RETURN 
!================================================================
DOO.CleanListPopup PROCEDURE() !Rt Mouse Popup for Clean List
 
RowFileName CSTRING(261)
B4_FileName CSTRING(261),DIM(4) 
B4_Stamp    PSTRING(32),DIM(4)
B4_Tilde    PSTRING(2),DIM(4)
DateStamp   STRING(32)
X   USHORT
    CODE
    SETKEYCODE(0)
    RowFileName = CleanQ.PathBS & ClarionProperties_xml
    LOOP X=1 TO 4
        B4_FileName[X]=RowFileName & CHOOSE(X,'.b4clean','.b4clean2','.b4cleanMax','.old')
        IF ~GetFileDateTimeSize(B4_FileName[X],,,,DateStamp) THEN 
           B4_Tilde[X]='~'
        ELSE
           B4_Stamp[X]='<9>(' & CLIP(DateStamp) &')' 
        END  
    END
    EXECUTE POPUP('Explore Folder	Double Click' & |  !#1
                '|Open XML in Notepad'   & |  !#2
          '|-'& '|View Find Patterns'    & |  !#3
                '|View Replace Patterns' & |  !#4
                '|View Patterns in Other File' & |  !SubMenu
                 '{{' & |
                      B4_Tilde[1] &'.B4 Clean  Backup' & B4_Stamp[1] & |  !#5
                 '|'& B4_Tilde[2] &'.B4 Clean2 Backup' & B4_Stamp[2] & |  !#6
                 '|'& B4_Tilde[3] &'.B4 CleanMax Save' & B4_Stamp[3] & |  !#7
                 '|'& B4_Tilde[4] &'.Xml.Old IDE Copy' & B4_Stamp[4] & |  !#8
           '|-'& '|Select File to View Patterns ...' & |  !#9
                 '}' & |
          '|-'& '|Delete Row	Delete'   )    !#10
     
      ExplorerOpen(CleanQ.PathBS)  ! #1
      NotepadOpen(CleanQ.PathBS & ClarionProperties_xml)
      START(CleanViewPatterns,,RowFileName,ltFindPatterns)
      START(CleanViewPatterns,,RowFileName,ltReplacePatterns)
      CleanViewSelectFile(B4_FileName[1])  !#5
      CleanViewSelectFile(B4_FileName[2])
      CleanViewSelectFile(B4_FileName[3])
      CleanViewSelectFile(B4_FileName[4])
      CleanViewSelectFile()
      DELETE(CleanQ)               ! # 9  Delete Row`Delete
    END !EXECUTE Popup 
    RETURN      
!================================================================
DOO.CleanTheFiles  PROCEDURE(BYTE pQuery)   !Scan CleanQ files and Shrink
QNdx    LONG,AUTO
ClnStats   GROUP(ClnStatsType).  
CleanOk    BOOL
    CODE
    DOO.ValidateParms()
    LOOP QNdx=1 TO RECORDS(CleanQ)
        GET(CleanQ,QNdx)
        CLEAR(CleanQ.Find)
        CLEAR(CleanQ.Repl)
        CleanQ.CleanMsg = ''
        CLEAR(ClnStats)   
        IF ~Glo:UseStringTheory THEN 
            CleanOk = CleanClaPropXmlFile(CleanQ.PathBS & ClarionProperties_xml , pQuery, |
                                          ClnStats, CleanQ.CleanMsg) 
        ELSE   !String Theory  
            COMPILE('!**END ST Clean**', _Have_StringTheory_ > 0)    
            CleanOk = ST_CleanClaPropXmlFile(CleanQ.PathBS & ClarionProperties_xml , pQuery, |  !StringTheory clean code 
                                          ClnStats, CleanQ.CleanMsg)                            !in FindCleanCwIDE_ST.clw
            !end COMPILE('!**END ST Clean**'
        END  
        IF CleanOk THEN
           IF pQuery AND ~CleanQ.CleanMsg THEN 
              CleanQ.CleanMsg = 'Query only  ' & CleanQ.CleanMsg
           END
        END                            
        CleanQ.Find     = ClnStats.Find
        CleanQ.Repl     = ClnStats.Repl        
        CleanQ.CleanTip = CleanQ.CleanMsg
        PUT(CleanQ)  
    END 
    DISPLAY
    RETURN
!=========================================
DOO.CopyCleanQ2Clip     PROCEDURE()
CB  ANY
CpDate  STRING(10),AUTO
CpTime  STRING(5),AUTO
QNdx    LONG,AUTO 
TB9     EQUATE('<9>')
    CODE
    CpDate=FORMAT(TODAY(),@d02) ; CpTime=FORMAT(CLOCK(),@t01) 
    CB='Date<9>Time<9>FromQ<9>Folder<9>Find Cnt<9>Find Bytes<9>Repl Cnt<9>Repl Bytes<9>Path'
    LOOP QNdx=1 TO RECORDS(CleanQ)
        GET(CleanQ,QNdx)   
        CB=CB & '<13,10>' & CpDate & Tb9 & CpTime & |
           Tb9 & CLIP(CleanQ.FromQ)     & |
           Tb9 & CLIP(CleanQ.SubFolder) & |
           Tb9 & CleanQ.Find.CntIN      & |
           Tb9 & CleanQ.Find.BytesOUT   & |
           Tb9 & CleanQ.Repl.CntIN      & |
           Tb9 & CleanQ.Repl.BytesIN    & |
           Tb9 & '"'& CLIP(CleanQ.PathBS) &'"'
    END
    SETCLIPBOARD(CB)
    RETURN             
!=========================================
DOO.WindowInitPretty PROCEDURE() 
Fld LONG  
    CODE  
    WinResize.Init(AppStrategy:NoResize,Resize:SetMinSize)
    ?SHEET1{PROP:NoTheme}=True
    ?SHEET1{PROP:TabSheetStyle}=1   
    ?List:CleanQ{PROPSTYLE:FontName,1}='Consolas'
    !?List:AppDataSvQ{PROPSTYLE:FontName,1}='Consolas'   too big after resize
    LOOP
        Fld=0{PROP:NextField,Fld} ; IF ~Fld THEN BREAK.
        CASE Fld{PROP:Type}
        OF CREATE:list  
           ListHeaderColor(Fld)  !Fix Manifest Header White
         !  Fld{PROP:LineHeight} = Fld{PROP:LineHeight} + 1    fine w/o space
           CASE Fld
           OF ?LIST:ClarionInstallQ 
                  DO ResizeFillWidth
           ELSE ; DO ResizeFillBoth
           END
        OF CREATE:sheet     ;  DO ResizeFillBoth
        OF CREATE:text    
           CASE Fld
           OF ?AboutTEXT
                  DO ResizeFillBoth
           ELSE ; DO ResizeFillWidth
           END       
        OF CREATE:box 
           IF Fld{PROP:Fill}=COLOR:INACTIVECAPTION THEN 
              Fld{PROP:GradientType}=GradientTypes:Horizontal
              Fld{PROP:GradientFromColor}=8000001CH                 !Light color    C11 added COLOR:GradientInactiveCaption
              Fld{PROP:GradientToColor}  =COLOR:InactiveCaption     !to Dark color
              (Fld+1){PROP:FontColor}=COLOR:InactiveCaptionText     !Assume box before string
              DO ResizeFillWidth
           END    
           
        END
    END        
    RETURN 
ResizeFillBoth ROUTINE
    WinResize.SetStrategy(Fld, Resize:LockXPos+Resize:LockYPos, Resize:ConstantRight+Resize:ConstantBottom)
ResizeFillWidth ROUTINE
    WinResize.SetStrategy(Fld, Resize:LockXPos+Resize:LockYPos, Resize:ConstantRight+Resize:LockHeight)
   
!####################################################################################  
SettingsCls.LoadAll  PROCEDURE()       !SettingFile  EQUATE('.\FndClnSettings.ini')
    CODE
    Glo:MinPatterns=SELF.Get1('MinPats',Glo:MinPatterns)
    Glo:MaxPatterns=SELF.Get1('MaxPats',Glo:MaxPatterns) 
    NoShowAbout    =SELF.Get1('NoAbout',0)  
    
    !Put these in INI to have them be defaults, If you are doing Dev it gets old
    WriteOK       =SELF.Get1('WriteOK'   ,WriteOK)    
    IDEClosed     =SELF.Get1('IDEClosed' ,IDEClosed)    
    Glo:TestShrink=SELF.Get1('TestShrink',Glo:TestShrink) 

    IF _Have_StringTheory_ = 1 THEN 
      Glo:UseStringTheory=SELF.Get1('UseStringTheory',Glo:UseStringTheory) 
    END   
    RETURN
SettingsCls.SaveAll  PROCEDURE()
    CODE
    SELF.Put1('MinPats',Glo:MinPatterns)
    SELF.Put1('MaxPats',Glo:MaxPatterns) 
    SELF.Put1('NoAbout',NoShowAbout)

    SELF.Put1('_remove_WriteOK'   ,WriteOK)           !Write to INI as "; " so does not read
    SELF.Put1('_remove_IDEClosed' ,IDEClosed)         !but can edit INI to make default load
    SELF.Put1('_remove_TestShrink',Glo:TestShrink)    !Can also put TEST SHRINK in ConfigDir.Txt
    IF _Have_StringTheory_ = 1 THEN 
       SELF.Put1('UseStringTheory',Glo:UseStringTheory)
    END
    RETURN    
SettingsCls.Get1  PROCEDURE(STRING EntryName, STRING DefaultValue)!,STRING 
    CODE
    RETURN GETINI('Config',EntryName,DefaultValue,SettingFile)
SettingsCls.Put1  PROCEDURE(STRING EntryName, STRING SaveValue)
    CODE
    PUTINI('Config',EntryName,SaveValue,SettingFile)
    RETURN       
!#################################################################################### 
  COMPILE('!**END ST Clean**', _Have_StringTheory_ > 0)
     INCLUDE('FindCleanCwIDE_ST.clw','Functions') !String Theory
  !end COMPILE('!**END ST Clean**'
!ST_CleanClaPropXmlFile Procedure(STRING ClaPropFullName, BYTE pQuery, *ClnStatsType ClnStats, *STRING OutMsg )!,BOOL
!--st--^^--st--^^--st--^^--st--^^--st--^^--st--^^--st--^^--st--^^--st--^^
CleanClaPropXmlFile Procedure(STRING pClaPropXmlFN, BYTE pQuery, *ClnStatsType ClnStats, *STRING OutMsg )!,BOOL
DidSaveOk   BOOL
FindCln     CbFindCleanClass
CleanCnt    LONG
B4CleanName CSTRING(261)
B4CleanMax  CSTRING(261)  !02/10/21 Keep the First Clean assume Maximi
    CODE
    B4CleanName=CLIP(pClaPropXmlFN) & '.b4clean'
    B4CleanMax =CLIP(pClaPropXmlFN) & '.b4cleanMax'
    CLEAR(ClnStats)
    OutMsg=''
    IF ~FindCln.FileLoad(pClaPropXmlFN,CHOOSE(pQuery OR Glo:TestShrink) ) THEN
        OutMsg='Error Load: ' & FindCln.LastError
        RETURN False 
    END

    CleanCnt = CleanXmlFindPattern(FindCln, ltFindPatterns,    pQuery, ClnStats.Find, OutMsg ) |
             + CleanXmlFindPattern(FindCln, ltReplacePatterns, pQuery, ClnStats.Repl, OutMsg )
    IF CleanCnt=0 THEN 
       IF ~OutMsg THEN OutMsg = 'Below Max ' & Glo:MaxPatterns .
       RETURN False   
    END     
    IF DbView THEN FindCln.ViewString(FindCln.XmlStr,'After Clean').

    IF pQuery THEN         !Query does not Save so we're done
        OutMsg=''
        RETURN True        !Clean had Count with no problems so return True       

    ELSIF Glo:TestShrink THEN     !Write TEST so write .TestShrink so leave real file alone
       DidSaveOk = FindCln.FileSave(CLIP(pClaPropXmlFN) & '.TestShrink')
       IF ~DidSaveOk THEN       
          OutMsg='Err Save TestShrink: ' & FindCln.LastError
       ELSE
          OutMsg='~{5} .TestShrink Saved OK ~{5}'
       END    
       RETURN DidSaveOk
    END

    !--- This is NOT a Test...so WRITE to real ClaProps.XML --------
    IF ~EXISTS(B4CleanMax) THEN         !02/10/21 No Max file? Keep First Clean as Max
        COPY(pClaPropXmlFN   ,B4CleanMax)    !Save current file 
        COPY(B4CleanName     ,B4CleanMax)    !I did this late so if there's a prior Clean
        COPY(B4CleanName &'2',B4CleanMax)    !or 2 prior cleans
    END
    COPY(B4CleanName,B4CleanName &'2')   !Save 2nd backup .b4clean2 
    COPY(pClaPropXmlFN,B4CleanName)            !Save .b4clean backup with Copy
    IF ERRORCODE() THEN                        !Cannot Backup file?
       OutMsg='Error Copy .b4clean: ' & ErrorCode() &' '& Error()
       Message(CLIP(OutMsg) & |                !Should not happen so so show msg
               '||From: ' & CLIP(pClaPropXmlFN) & '||To: ' & CLIP(B4CleanName), 'Copy Fail', ICON:Exclamation) 
       RETURN False   !Copy failed
    END 
    DidSaveOk = FindCln.FileSave(pClaPropXmlFN)
    IF ~DidSaveOk THEN
       OutMsg='Error Save: ' & FindCln.LastError
    ELSE
       OutMsg='*{9} Cleaned Ok *{5}'
    END
    RETURN DidSaveOk 
!----------------------------------------------------------------------------
CleanXmlFindPattern Procedure(CbFindCleanClass FindCln, STRING ltPatternsElement, BYTE pQuery, *IOStatsType IOStats, *STRING OutMsg )!,BOOL
RetBool     BOOL
Pos1        LONG
Pos2        LONG
! ClarionProperties.xml
! <FindPatterns value="firstÿfunctionÿy.squeezÿsqueezÿguidÿ" />
! <ReplacePatterns value="MethQ:ÿMethodQÿ.StrÿBigBangSystemStringÿ" />
! Delimeter                     <0C3h,0BFh>
    CODE
    CLEAR(IOStats) 
    IF ~FindCln.FindXmlElement(ltPatternsElement,Pos1,Pos2) THEN            !Test no <FindPatterns
        OutMsg='Did not find "' & ltPatternsElement &'"  '& OutMsg
    ELSIF ~FindCln.FindXmlAttribute(ltPatternsElement,'value',Pos1,Pos2) THEN  !Test no value= in <Find
        OutMsg='No value= in "' & ltPatternsElement &'"  '& OutMsg
    ELSE

       IF DbView THEN FindCln.ViewSlice(FindCln.XmlStr,Pos1,Pos2,'FindXmlAttrib Value ' & ltPatternsElement) .
       IOStats.BytesIN = Pos2 - Pos1 + 1
       IOStats.CntIN = FindCln.PatternCount(Pos1,Pos2)
       IF DbView THEN Message('Count=' & IOStats.CntIN &'|Bytes=' & IOStats.BytesIN ,ltPatternsElement ).
       IF IOStats.CntIN > Glo:MaxPatterns THEN
          IOStats.BytesOUT = FindCln.PatternShrink(Pos1,Pos2,Glo:MinPatterns)
          IF IOStats.BytesOUT > 0 THEN
             IOStats.CntOUT = Glo:MinPatterns 
             RetBool = 1
          END
          IF DbView THEN message('PatternShrink ||Size=' & FindCln.XmlLen &'||Len=' & LEN(CLIP(FindCln.XmlStr)) ).
       END
        IF DbView AND FindCln.FindXmlElement(ltPatternsElement,Pos1,Pos2)
           FindCln.ViewSlice(FindCln.XmlStr,Pos1,Pos2,'After FindXmlElement ' & ltPatternsElement )
        END
    END
    RETURN RetBool
!=================================================================================== 
CleanViewSelectFile PROCEDURE(<STRING pOpenFileName>)  !FileDialog to Pick the file to view
OpenFN  STRING(260)
    CODE
    IF ~OMITTED(pOpenFileName) THEN 
        OpenFN = pOpenFileName      
    ELSIF ~FileDialog('View Find Patterns '& ClarionProperties_xml & ' File', |
                    OpenFN, |
                    'Cla Props XML|Cla*.XML;Cla*.b4clean*' & |
                    '|B4Clean Backups|Cla*.b4clean*' & |
                    '|TestShrink|*.TestShrink' & |
                    '|XML Files|*.XML|All Files|*.*', |
                    FILE:LongName + FILE:KeepDir) THEN 
            RETURN
    END                       
    IF START(CleanViewPatterns,,OpenFN,ltFindPatterns).
    START(CleanViewPatterns,,OpenFN,ltReplacePatterns) 
    RETURN 
!===================================================================================
CleanViewPatterns     Procedure(STRING pClaPropXmlFN, STRING ltPatternsElement) 
!Simple idea view the Find strings. Looking at 7000 strings useless... so more work later have Unique 
FindQ   QUEUE,PRE(FndQ)  ! FindQ        AlMruQ      CountQ      AlphaQ    
MRU          LONG        ! FndQ:MRU    AruQ::MRU    CntQ:MRU    AlpQ:MRU  
Count        LONG        ! FndQ:Count  AruQ::Count  CntQ:Count  AlpQ:Count
What         STRING(32)  ! FndQ:What   AruQ::What   CntQ:What   AlpQ:What 
Lower        STRING(32)  ! FndQ:Lower  AruQ::Lower  CntQ:Lower  AlpQ:Lower
        END 
AlMruQ  QUEUE(FindQ),PRE(AruQ)  !MRU in Alpha Order
        END        
CountQ  QUEUE(FindQ),PRE(CntQ)  
        END 
AlphaQ  QUEUE(FindQ),PRE(AlpQ)  !Counts in Alpha Order
        END 
TotalCnt   LONG    
Unique1Cnt LONG    
Unique3Cnt LONG   

Window WINDOW('Find Patterns'),AT(,,310,220),GRAY,SYSTEM,MAX,ICON('FindCln.ico'),FONT('Segoe UI',10),RESIZE
        ENTRY(@s255),AT(3,2,,10),FULL,USE(?XmlFN),SKIP,TRN,READONLY
        STRING(@n6),AT(126,19,30),USE(TotalCnt),TRN,RIGHT,FONT(,12,,FONT:bold)
        STRING(@n6),AT(161,15,30),USE(Unique1Cnt),TRN,RIGHT
        STRING(@n6),AT(161,24,30),USE(Unique3Cnt),TRN,RIGHT
        STRING('Unique Strings'),AT(194,15),USE(?Unique1:pmt),TRN
        STRING('Unique Count >= 3'),AT(194,24),USE(?Unique3:pmt),TRN
        SHEET,AT(1,17),FULL,USE(?SHEET1)
            TAB(' MRU '),USE(?TAB:Mru),TIP('Most Recently Used')
                LIST,AT(1,36),FULL,USE(?LIST:FindQ),VSCROLL,VCR,FROM(FindQ),FORMAT('28L(2)|FM~MRU~C(0)@n6@28L(2)|FM~Coun' & |
                        't~C(0)@n6@28L(2)F~String - Ctrl+C to Copy~@s32@?'),ALRT(CtrlC)
            END
            TAB(' ARU '),USE(?TAB:ARU),TIP('MRU in Alpha Order')
                LIST,AT(1,36),FULL,USE(?LIST:AlMruQ),VSCROLL,VCR,FROM(AlMruQ),ALRT(CtrlC)
            END
            TAB(' Count '),USE(?TAB:Cnt),TIP('Count Summary of Unique Searches')
                LIST,AT(1,36),FULL,USE(?LIST:CountQ),VSCROLL,VCR,FROM(CountQ),ALRT(CtrlC)
            END
            TAB(' Alpha '),USE(?TAB:Alpha),TIP('Count Summary in Alpha Order')
                LIST,AT(1,36),FULL,USE(?LIST:AlphaQ),VSCROLL,VCR,FROM(AlphaQ),ALRT(CtrlC)
            END
        END
    END
    
DDD     CLASS
LoadQueue   PROCEDURE(),STRING
BuidCounts  PROCEDURE() 
CopyLine    PROCEDURE(FindQ TheQue, LONG ListFEQ) 
ListInit    PROCEDURE(LONG ListFEQ)
        END 
FailMsg STRING(500)  
WP      LONG,DIM(4),STATIC 
PrvCls  CBWndPreviewClass
    CODE
    FailMsg = DDD.LoadQueue()
    IF FailMsg THEN 
        Message('Failed File: ' & CLIP(pClaPropXmlFN) & '||' & FailMsg,'Find List View')
        RETURN
    END
    DDD.BuidCounts()
    OPEN(Window)
    IF WP[4] THEN SETPOSITION(0,WP[1],WP[2],WP[3],WP[4]). 
    PrvCls.Init()
    ?XmlFN{PROP:Use}=pClaPropXmlFN
    ?SHEET1{PROP:NoTheme}=1
    ?Sheet1{PROP:TabSheetStyle}=1 
    ListHeaderColor(?LIST:FindQ)
    DDD.ListInit(?LIST:AlMruQ)
    DDD.ListInit(?LIST:AlphaQ)
    DDD.ListInit(?LIST:CountQ)
    ?SHEET1{PROP:NoSheet}=1
    ?SHEET1{PROP:Below}=1
    0{PROP:Text}=ltPatternsElement &' Patterns>  '& TotalCnt &' Strings'
    ACCEPT 
        CASE EVENT()
        OF Event:AlertKey 
           IF KEYCODE()=CtrlC THEN 
              EXECUTE CHOICE(?SHEET1)
                DDD.CopyLine(FindQ ,?List:FindQ)
                DDD.CopyLine(AlMruQ,?List:AlMruQ)
                DDD.CopyLine(CountQ,?List:CountQ) 
                DDD.CopyLine(AlphaQ,?List:AlphaQ)
              END
           END
        END
    END 
    GETPOSITION(0,WP[1],WP[2],WP[3],WP[4])
    CLOSE(Window) 
    RETURN
!------------  
DDD.CopyLine  PROCEDURE(FindQ TheQue, LONG ListFEQ)
    CODE 
    GET(TheQue, CHOICE(ListFEQ))
    SETCLIPBOARD(FindQ.What)
!------------  
DDD.ListInit  PROCEDURE(LONG ListFEQ)
    CODE
    ListHeaderColor(ListFEQ)
    ListFEQ{PROP:Format} = ?LIST:FindQ{PROP:Format}
!------------
DDD.BuidCounts  PROCEDURE()
QX  LONG
    CODE
    CLEAR(CountQ)
    CntQ:Lower='<0,255,0>'                  !Build Count Q from FindQ 
    SORT(FindQ, FndQ:MRU)                
    SORT(AlMruQ,AruQ:Lower,AruQ:MRU)        !Find the counts in Alpha Order
    LOOP QX=1 TO RECORDS(AlMruQ)
        GET(AlMruQ,QX)
        IF CntQ:Lower <> AruQ:Lower THEN   !Cnt already loaded
           CountQ = AlMruQ
           CntQ:Count = 1
           CntQ:What[1]=UPPER(CntQ:What[1])
           ADD(CountQ,CntQ:Lower)
        ELSE
           CntQ:Count += 1
           PUT(CountQ)
        END
    END
    !----Now Counts are in Alpha Order ----
    CntQ:Lower='<0,255,0>'
    LOOP QX=1 TO RECORDS(AlMruQ)             !Count and Find in Alpha right now
        GET(AlMruQ,QX)
        IF AruQ:Lower <> CntQ:Lower THEN    !Cnt already loaded
           CntQ:Lower = AruQ:Lower
           GET(CountQ,CntQ:Lower)
           IF ERRORCODE() THEN
              SETCLIPBOARD(AruQ:Lower)
              STOP('BuidCounts  QX=' & QX &' MRU=' & AruQ:MRU &'  GET(CountQ,CntQ:Lower)   Lower=' & CntQ:Lower)
           END
        END
        AruQ:Count = CntQ:Count
        PUT(AlMruQ)
        GET(FindQ, 0+AruQ:MRU)    !+0 flags MRU is Record Number in Find
        FndQ:Count = AruQ:Count
        PUT(FindQ)
    END

    TotalCnt = RECORDS(FindQ)
    Unique1Cnt = RECORDS(CountQ)
    LOOP QX=RECORDS(CountQ) TO 1 BY -1   !Just Keep Counts 2 +
        GET(CountQ,QX)
        IF CntQ:Count < 3 THEN
           DELETE(CountQ)
        ELSE
           AlphaQ = CountQ
           ADD(AlphaQ,1)    !Count in Alpha right now
        END
    END
    Unique3Cnt = RECORDS(CountQ)
    SORT(AlphaQ,AlpQ:Lower,-AlpQ:Count)
    SORT(CountQ,-CntQ:Count,CntQ:Lower)  ! ;  PrvCls.QueueReflection(CountQ,'CountQ')
    RETURN
!------------
DDD.LoadQueue   PROCEDURE()!,BOOL
FindCln     CbFindCleanClass
RetBool     BOOL
Pos1        LONG
Pos2        LONG
Beg1        LONG
X           LONG
MruSeq      LONG
sDelim  EQUATE('<0C3h,0BFh>')
    CODE
    IF ~FindCln.FileLoad(pClaPropXmlFN,1) THEN
        RETURN  'Error Load: ' & FindCln.LastError
    END
    !Code from CleanXmlFindPattern(). should have made that do this?
    IF ~FindCln.FindXmlElement(ltPatternsElement,Pos1,Pos2) THEN            !Test no <FindPatterns
          RETURN 'Did not find "' & ltPatternsElement &'"'
    ELSIF ~FindCln.FindXmlAttribute(ltPatternsElement,'value',Pos1,Pos2) THEN  !Test no value= in <Find
          RETURN 'No value= in "' & ltPatternsElement &'"'
    ELSIF Pos1 < 1 OR Pos1 > Pos2 OR Pos2 > FindCln.XmlLen OR Pos2 - Pos1 < 4 THEN
          RETURN 'Invalid Slice [ ' & Pos1 &' : '& Pos2 &' ]'
    END
    Pos2 += 2                               !HACK put Delim at end over what should be " />
    FindCln.XmlStr[ Pos2-1 : Pos2 ]=sDelim  !     so split easier
    !FindCln.ViewSlice(FindCln.XmlStr,Pos1,Pos2,'Slice of ' & ltPatternsElement )  !Debug see Slice

    CLEAR(FindQ)    !Find Slices ending with Delim and add to Queue
    Beg1 = Pos1
    LOOP X=Pos1+1 TO Pos2-1
         IF FindCln.XmlStr[ X : X+1 ] = sDelim THEN
            IF X-1 >= Beg1 THEN
               !"&" only-->  c#=INSTRING('&',FindCln.XmlStr[ Beg1 : X-1 ] )
               XmlUnEscape(FindCln.XmlStr[ Beg1 : X-1 ], 0)  !0=Use SIZE
               FndQ:What = LEFT(FindCln.XmlStr[ Beg1 : X-1 ])
               FndQ:Lower=lower(FndQ:What)
               MruSeq += 1
               FndQ:MRU = MruSeq
               !FndQ:What='['& Beg1 &':'& X-1 &'] '& FndQ:What  !Debug slice
               ADD(FindQ)
               !  if c# THEN ADD(FindQ).  !<-- see & only
               AlMruQ=FindQ ; ADD(AlMruQ)
            END
            X += 1
            Beg1 = X+1
         END
    END
    RETURN('')     !Load good

!====================================================================================    
!Below code from: https://github.com/CarlTBarnes/Clarion-Root-Find
GetRegistryInstallsOfCW      PROCEDURE(QUEUE Cla_Queue, *STRING ClaQ_RootPath,<*STRING ClaQ_ClarionName>,<*DECIMAL ClaQ_VersioNo>)
SVSubKeysQ  QUEUE,PRE(SVSubQ)      !Keys under 'SOFTWARE\SoftVelocity'
ClarionKey      STRING(16)         !Want Clarion10 Clarion9.1 etc
            END
Ndx  LONG,AUTO
Root STRING(256)
SOFTWARE_SoftVelocity EQUATE('SOFTWARE\SoftVelocity')
    CODE
    GetRegSubKeys(REG_LOCAL_MACHINE, SOFTWARE_SoftVelocity, SVSubKeysQ) 
    LOOP Ndx=1 TO RECORDS(SVSubKeysQ)
        GET(SVSubKeysQ,Ndx)
        IF lower(SvSubQ:ClarionKey[1:7])<>'clarion' THEN CYCLE.  !Only take 'Clarion', SV could add other products.
        Root = GETREG(REG_LOCAL_MACHINE, SOFTWARE_SoftVelocity &'\'& SvSubQ:ClarionKey  ,'ROOT')
        IF ~Root         |                  !Blank indicates Root Value NOT found in Registry
        OR ~EXISTS(Root) THEN CYCLE.        !Folder not on disk, then was deleted or renamed

        IF ~OMITTED(ClaQ_ClarionName) THEN       
            ClaQ_ClarionName  = SvSubQ:ClarionKey           !E.g. Clarion10 Clarion9.1
        END 
        IF ~OMITTED(ClaQ_VersioNo) THEN       
            ClaQ_VersioNo  = DEFORMAT(SvSubQ:ClarionKey)    !E.g. 10 or 9.1
        END
        ClaQ_RootPath = Root                 !E.g. C:\Clarion10  or C:\Cw10
        ADD(Cla_Queue)
    END
    RETURN
!####################################################################################
ExplorerOpen  Procedure(STRING FolderName)
    CODE
    IF ~EXISTS(FolderName) THEN |
        Message('Folder does not exist:||' & FolderName, 'ExplorerOpen')
    ELSE 
        RUN('Explorer.exe /e,"' & CLIP(FolderName) &'"')
    END
    RETURN  
NotepadOpen Procedure(STRING pFileName) 
NotepadEXE  CSTRING(300),AUTO
    CODE
    IF ~EXISTS(pFileName) THEN |
        Message('File does not exist:||' & pFileName, 'NotepadOpen')
    ELSE 
        !Stick your Editor Exe in .INI [Config] Notepad=x:\xxx 
        NotepadEXE=CLIP(GETINI('Config','Notepad','Notepad.exe',SettingFile))
        RUN(NotepadEXE & ' "' & CLIP(pFileName) &'"')
        IF ERRORCODE() THEN  
           Message('Run Error ' & ERRORCODE() &' '& ERROR() & |
                   '||Editor: ' & NotepadEXE & '||File: ' & pFileName,'NotepadOpen')
        END
    END
    RETURN     
!=====================================
GetFileDateTimeSize PROCEDURE(STRING pFileName,<*LONG outDate>,<*LONG outTime>,<*LONG outSize>,<*STRING outStamp>)!,BOOL
DirQ    QUEUE(FILE:Queue),PRE(DirQ)
        END ! DirQ:Name  DirQ:ShortName  DirQ:Date  DirQ:Time  DirQ:Size  DirQ:Attrib 
RetBool BOOL        
    CODE
    DIRECTORY(DirQ,pFileName,ff_:NORMAL)
    IF RECORDS(DirQ)=1 THEN
       GET(DirQ,1)
       RetBool=True
    ELSE
       CLEAR(DirQ)
    END
    IF ~OMITTED(outDate) THEN outDate=DirQ:Date.
    IF ~OMITTED(outTime) THEN outTime=DirQ:Time.
    IF ~OMITTED(outSize) THEN outSize=DirQ:Size.
    IF ~OMITTED(outStamp) THEN 
        outStamp=CHOOSE(~RetBool,'',LEFT(FORMAT(DirQ:Date,@d17))&' '& |
                                         FORMAT(DirQ:Time,@t1)  &'  '& |
                                    LEFT(FORMAT(DirQ:Size,@n11)) ) .
    RETURN RetBool
!==============================================
GetSpecialFolder    PROCEDURE(LONG CDSL)
SHGFP_TYPE_CURRENT          EQUATE(0)       !current value for user, verify it exists
SHGFP_TYPE_DEFAULT          EQUATE(1)       !default value, may not exist
CSIDL_FLAG_DONT_VERIFY      EQUATE(4000h)   !CSIDL_FLAG_DONT_VERIFY 0x4000 // combine with 
OutPathCStr                 CSTRING(261)    !pszPath [out] Null-terminated string of length MAX_PATH which will receive the path, does NOT have backslash
CSIDLAdd        LONG(CSIDL_FLAG_DONT_VERIFY)
Access_Token    EQUATE(0)   !0=normal  -1=Default user file locations
HR              LONG,AUTO
    CODE
!    if SpclPerUserInit then CSIDLAdd += CSIDL_FLAG_PER_USER_INIT .
!    if SpclNoAlias     then CSIDLAdd += CSIDL_FLAG_NO_ALIAS .
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
DBClear PROCEDURE()
DbgClear CSTRING('DBGVIEWCLEAR')    !Message to Clear the buffer. Must UPPER and first i.e. without a Prefix
    CODE 
    OutputDebugString(DbgClear)     !Cannot have Prefix, must be first .. so call API directly
!==============================================
XmlUnEscape PROCEDURE(*STRING pXML, BYTE pClip=1)!,LONG,PROC   !Returns Change Out
LenXml LONG,AUTO
EscNdx LONG,AUTO
X      LONG,AUTO
ChangeCnt LONG
SemiPos   LONG
    CODE   
    LenXml=CHOOSE(~pClip, SIZE(pXML), LEN(CLIP(pXML)) ) 
    LOOP X = LenXml TO 1 BY -1  !Work backwards thru string in place
        CASE pXML[X]
        OF ';' ; SemiPos=X      ! &amp; ends with ;
        OF '&'                  ! &amp; begins with &
           IF SemiPos AND INRANGE(SemiPos-X+1, 3,6) THEN    ! 123           ! 123456
              EscNdx=INLIST(pXML[X : SemiPos],'&amp;','&lt;','&gt;','&apos;','&quot;') 
              IF EscNdx THEN                 !   &       <     >       '        "
                 pXML[X] = SUB('&<<>''"',EscNdx,1) 
                 IF SemiPos+1 > LenXml THEN   !&xxx; was last in String?
                    pXML[X+1 : LenXml] = ''   !Blank end of String after &
                 ELSE                         !else shift back string
                    pXML[X+1 : LenXml] = pXML[SemiPos+1 : LenXml]
                 END
                 ChangeCnt+=1
              END 
           END !If SemiPos   
           SemiPos=0
        END !Case Chr
    END     !Loop
    RETURN ChangeCnt 
!============================================== 
ListHeaderColor PROCEDURE(LONG ListFEQ)
    CODE !With Manifest and Windows 10 the header is White like Data so Color it 
    ListFEQ{PROPLIST:DefHdrBackColor} = 80000016H    !C11 added COLOR:3DLight EQUATE(80000016H) !Light color for 3D display elements (edge facing light source)
    ListFEQ{PROPLIST:DefHdrTextColor} = COLOR:BTNTEXT
    RETURN    
    ! ListFEQ{PROP:NoTheme}=True  !Remove theme for old look ... or better colors below?
   ! ListFEQ{PROPLIST:DefHdrBackColor} = COLOR:GradientInactiveCaption  !too Blue?
   ! ListFEQ{PROPLIST:DefHdrTextColor} = COLOR:INACTIVECAPTIONTEXT
   ! ListFEQ{PROPLIST:DefHdrBackColor} = COLOR:BTNFACE                !Same as Window
!==============================================
SetAboutText PROCEDURE(LONG AboutFEQ)
AboutTxt STRING('The Clarion Editor Find/Replace dialog saves the text you enter in ClarionProperties.xml ' &|
     'file as <<FindPatterns value=""> and <<ReplacePatterns value="">. These are never purged ' &|
     'by the IDE so grow continually. As they grow large the Find dialog opens much slower and ' &|
     'the drop list stops ordering properly with the most recent first. My C11 Find after 2 ' &|
     'years had 7600 items taking 70kb.' &|
     '<13,10>' &|
     '<13,10>This utility will load the ClarionProperties.XML files and show the Find / Replace ' &|
     'counts and bytes. Clicking the Shrink button will truncate the Find and Replace patterns ' &|
     'list to the number of entries you specify. I suggest once the list exceeds 50 shrink it ' &|
     'to 25. The below 3 folders are scanned:' &|
     '<13,10>' &| 
     '<13,10>Place 1 -- C:\Users... AppData \ SoftVelocity \ Clarion \ #.# -- The normal Configuration folder ' &|
     '<13,10>Place 2 -- Installs: X:\Clarion#.# \ Bin \ Settings -- Configuration folder when run as Clarion.exe /ConfigDir ' &|
     '<13,10>Place 3 -- /ConfigDir=PathName -- Create a ConfigDir.txt file with Configuration folder path names ' &|
     '<13,10>' &|
     '<13,10>The Xml original file is saved with an .Xml.b4clean extension as a backup in case ' &|
     'something goes wrong and you wish to recover. The previous backup is saved as .b4clean2 ' &|
     'so you have 2 backups. ' &|
     '<13,10>' &|
     '<13,10>You can test by checking the "Write to Test Shrink" so the original file is ' &|
     'left untouched. You can limit folders touched by pressing the Delete key to remove folders ' &|
     'from the lists. Another way to test is  on the /ConfigDir= tab checking the "Just This" ' &|
     'box to limit to folders in that list. ' &|
     '<13,10>' &|
     '<13,10>This utility could be repurposed to perform other processing on the Configuration ' &|
     'folders in the 3 possible places.' &|
     '<13,10>' &|
     '<13,10>Thanks to Bruce Johnson who noticed the Editor Find window was opening progressively ' &|
     'slower. He figured out the cause and the fix then started the thread "FIND popup dialog ' &|
     'window appears slowly in IDE": https://clarionhub.com/t/find-popup-dialog-window-appears-slowly' &|
     '-in-ide/3764. I had noticed the drop list seemed to be in random order so had become useless. ' &|
     'I was pleased this was also fixed by shrinking the size. ' ),STATIC
    CODE 
    AboutFEQ{PROP:Use}=AboutTxt
    RETURN 
!------------------------------------------------------
