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
_Have_StringTheory_   EQUATE(0)             !1. Set as (1) = Have StringTheory 
!  INCLUDE('FindCleanCwIDE_ST.clw','GLOBAL') !2. Uncomment ! Include(StringTheory

Glo:UseStringTheory BYTE !(_Have_StringTheory_)
  
  INCLUDE('CBWndPreview.INC'),ONCE             !From https://github.com/CarlTBarnes/WindowPreview
    COMPILE('!-WndPrv-',_IFDef_CBWndPreview_)  !Comment this line to remove
WndPrvCls   CBWndPreviewClass  
             !-WndPrv-
  MAP
FindCleanCwWindow   PROCEDURE()
CleanClaPropXmlFile Procedure(STRING ClaPropFullName, BYTE pQuery, *ClnStatsType ClnStats, *STRING OutMsg ),BOOL
CleanXmlFindPattern Procedure(CbFindCleanClass FindCln, STRING ltPatternsElement, BYTE pQuery, *IOStatsType IOStats, *STRING OutMsg ),BOOL
 
ExplorerOpen        PROCEDURE(STRING FolderName) 
GetRegistryInstallsOfCW  PROCEDURE(QUEUE ClarionQ, *STRING ClaQ_RootPath, <*STRING ClaQ_ClarionName>, <*DECIMAL ClaQ_VersioNo>)
GetFileDateTimeSize PROCEDURE(STRING inFileName,<*LONG outDate>,<*LONG outTime>,<*LONG outSize>),BOOL
GetSpecialFolder    PROCEDURE(LONG CDSL),STRING 
SetAboutText        PROCEDURE(LONG AboutFEQ)
     MODULE('Windows')          !Windows Shell Folder SHFolder.DLL that must be distributed
        SHGetFolderPath(Long hwndOwner=0, LONG nFolder, Long hToken, Long dwFlags, *CSTRING pszPath),LONG,RAW,PASCAL,DLL(1),NAME('SHGetFolderPathA')
     END
  END

!Region Global Data and Equates
DbView                  SHORT(0)    !Open windows to view strings
ClarionProperties_xml   EQUATE('ClarionProperties.xml')
ltFindPatterns          EQUATE('<<FindPatterns')
ltReplacePatterns       EQUATE('<<ReplacePatterns')
valueEqQt               EQUATE('value="')

Glo:MinPatterns     USHORT(25)
Glo:MaxPatterns     USHORT(50)  !When count is this reduce to Min
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
  !Should be: !C:\Users\CBarnes\AppData\Roaming\SoftVelocity\Clarion 
  FindCleanCwWindow()

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
ConfigDirTxt_TEXT   STRING(128*80)    !Contents of ConfigDir.Txt
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
            ICON(ICON:Clarion),FONT('Segoe UI',10,,FONT:regular),RESIZE
        SHEET,AT(3,3,470,197),USE(?Sheet1)
            TAB(' ClarionProperties.Xml '),USE(?TAB:Clean)
                BOX,AT(6,17,460,10),USE(?Box_Clean),FILL(COLOR:INACTIVECAPTION),LINEWIDTH(1)
                PROMPT('This tool Shrinks the ClarionProperties.xml <<FindPatterns /> that the Editor Find Dialog grows ' & |
                        'forever which slows Find opening.'),AT(10,17),USE(?WhatsCleanFor),TRN
                LIST,AT(10,55,456,140),USE(?List:CleanQ),VSCROLL,FROM(CleanQ),FORMAT('[75L(2)|FM~<13,10>From Tab~@s8@/75' & |
                        'R(2)|_FM~Folder~@s255@]|[24R(2)|M~Cnt~C(0)@n4@#4#34R(2)|M~Bytes~C(0)@n7@/24R(2)|_M@n4b@Q''Find ' & |
                        'Count After''34R(2)|_M@n7b@]|~Find Patterns<13,10>Before/After~[24R(2)|M~Cnt~C(0)@n4@#9#34R(2)|' & |
                        'M~Bytes~C(0)@n7@/24R(2)|_M@n4b@Q''Replace Count After''34R(2)|_M@n7b@]|~Replace Pattrns<13,10>B' & |
                        'efore/After~[257L(2)P~Delete Key will remove rows to omit from shrink<13,10>Path  -  Double Cli' & |
                        'ck to Open~@s255@Z(1)/257R(2)_P~Clean message~@s255@]'),ALRT(DeleteKey)
                GROUP,AT(5,25,400,25),USE(?GroupTab1)
                    BUTTON('Query'),AT(10,32,54,16),USE(?QueryBtn),ICON(ICON:Zoom),TIP('Scan the files and show pattern ' & |
                            'counts'),LEFT
                    PROMPT('Maximum Count:'),AT(77,30,58),USE(?Glo:MaxPatterns:Prompt),TRN,RIGHT
                    ENTRY(@n3),AT(138,30,19,9),USE(Glo:MaxPatterns),CENTER,TIP('When count is >= Maximum<13,10>then redu' & |
                            'ce to "Shrink To"')
                    PROMPT('Shrink To Count:'),AT(77,41,58),USE(?Glo:MinPatterns:Prompt),TRN,RIGHT
                    ENTRY(@n3),AT(138,41,19,9),USE(Glo:MinPatterns),CENTER
                    BUTTON('Shrink Patterns'),AT(173,32,82,16),USE(?ShrinkBtn),ICON(ICON:Save),TIP('Shrink the patterns ' & |
                            'in the Cla Prop XML'),LEFT
                    CHECK('Write to Clarion Prop Xml'),AT(266,32),USE(WriteOK),FONT(,9),TIP('Check "Write to Clarion" bo' & |
                            'x to allow writing ClarionProperties.XML<13,10>with Find/Replace Patterns shrunk.<13,10,13>' & |
                            '<10>Two backup copies are kept with extension .b4clean.<13,10,13,10>Check the "Write to Tes' & |
                            't Shrink" box to instead test by writing to ClarionProperties.XML.TestShrink ')
                    CHECK('Write to .TestShrink.Xml'),AT(266,40),USE(Glo:TestShrink),FONT(,9),TIP('Write shrunk file to ' & |
                            'ClarionProperties.Xml.TestShrink.Xml<13,10><13,10>Compare this file to original xml to view' & |
                            ' changes.')
                    CHECK('Clarion IDE Closed ?'),AT(370,32),USE(IDEClosed),FONT(,9),TIP('All Clarion IDE''s must be clo' & |
                            'sed to "Shrink Patterns".<13,10><13,10>If Clarion is left open it will write the TOO BIG pa' & |
                            'tterns on Close.<13,10><13,10>')
                    CHECK('Capesoft StringTheory'),AT(370,40),USE(Glo:UseStringTheory),FONT(,9,095491FH,FONT:bold), |
                            TIP('Check for Geoff''s code using StringTheory from www.CapeSoft.com <13,10>Suggested for Developers South of the Equator<13,10><13,10>Uncheck ' & |
                            'to use Carl''s code - "The Northern Solution"')
                END
            END
            TAB(' 1. AppData SoftVelocity '),USE(?TAB:AppData)
                BOX,AT(6,18,460,10),USE(?Box_AppData),FILL(COLOR:INACTIVECAPTION),LINEWIDTH(1)
                PROMPT('AppData Roaming is the default location the Clarion IDE saves ClarionProperties.Xml. The /Config' & |
                        'Dir [=path] overrides the location.'),AT(9,18),USE(?AppDataSVpath:FYI),TRN
                ENTRY(@s255),AT(7,29,458,10),USE(Glo:AppDataSVpath),SKIP,TRN,FONT('Consolas'),READONLY
                LIST,AT(9,41,457,153),USE(?List:AppDataSvQ),VSCROLL,FROM(AppDataSvQ),FORMAT('32L(2)|FM~Folder~C(0)@s255@80L' & |
                        '(2)|M~Clarion Properties~@s255@34R(2)|M~Date~C(0)@d1b@34R(2)|M~Time~C(0)@T3b@40R(2)|M~Size~C(0)' & |
                        '@n11b@257L(2)P~Path  -  Double Click to Open~@s255@Z(1)'),ALRT(DeleteKey)
            END
            TAB(' 2. CW Bin \ Settings '),USE(?TAB:Installs)
                BOX,AT(7,18,461,10),USE(?Box_Reg),FILL(COLOR:INACTIVECAPTION),LINEWIDTH(1)
                PROMPT('Clarion Installs found in Registry:  HKLM\SOFTWARE\SoftVelocity\Clarion'),AT(9,18),USE(?Regsitry:FYI), |
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
                CHECK('Just This'),AT(9,20),USE(JustDoConfigDirs),SKIP,TRN,FONT(,9),TIP('Only Process ConfigDir files. O' & |
                        'mit AppData and Bin Settings.<13><10>Good for testing specific files.<13,10>Put JUST on first l' & |
                        'ine of ConfigDir.TXT to check this box.')
                CHECK('Edit This'),AT(9,33),USE(ConfigTextEditThis),SKIP,TRN,FONT(,9),TIP('Allow Editing ConfigDir.TXT')
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
                BOX,AT(7,18,461,10),USE(?Box_About),FILL(COLOR:INACTIVECAPTION),LINEWIDTH(1)
                PROMPT('Find Clean written by Carl Barnes. Inspired by Bruce Johnson. StringTheory code by Geoff Robinson'), |
                        AT(9,18),USE(?About:FYI),TRN
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
CleanTheFiles       PROCEDURE(BYTE pQuery)      !Scan CleanQ files and Shrink
ValidateParms       PROCEDURE()                 !Check Min/Max and maybe other are good
      END
WindowInitialized BOOL
SettingFile  EQUATE('.\FndClnSettings.ini')
SettingsCls   CLASS
LoadAll PROCEDURE()      
SaveAll PROCEDURE() 
Get1    PROCEDURE(STRING EntryName, STRING DefaultValue),STRING 
Put1    PROCEDURE(STRING EntryName, STRING SaveValue)
             END 
WinResize WindowResizeType            
    CODE
    SYSTEM{PROP:PropVScroll}=1
    SYSTEM{PROP:MsgModeDefault}=MSGMODE:CANCOPY 
    SettingsCls.LoadAll()
    GetRegistryInstallsOfCW(ClarionInstallQ, ClaInstQ:Root, ClaInstQ:Clarion, ClaInstQ:VersionNo)
    SORT(ClarionInstallQ, -ClaInstQ:VersionNo, -ClaInstQ:Clarion, -ClaInstQ:Root )
    DOO.LoadAppDataQueue()
    DOO.LoadBinSettngQueue() 
    OPEN(Window)               !<--- Windows is Open
    COMPILE('!-WndPrv-',_IFDef_CBWndPreview_)  
       WndPrvCls.Init(1)   
       WndPrvCls.InitList(?List:CleanQ,     CleanQ,    'CleanQ')      !NOT required >= 11.13505 
       WndPrvCls.InitList(?List:AppDataSvQ, AppDataSvQ,'AppDataSvQ') 
       WndPrvCls.InitList(?List:ClarionInstallQ,ClarionInstallQ ,'ClarionInstallQ')   
       WndPrvCls.InitList(?List:BinSettngQ, BinSettngQ ,'BinSettngQ')   
       WndPrvCls.InitList(?List:ConfigDirQ, ConfigDirQ ,'ConfigDirQ')   
             !-WndPrv-
    DOO.WindowInitPretty()            !Make LIST and BOX Look better. Init Resize
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
                   ?WriteOK{PROP:Background}=COLOR:Yellow 
                   IF ~IDEClosed THEN ?IDEClosed{PROP:Background}=COLOR:Yellow.
                   SELECT(?WriteOK) 
                   Message(?WriteOK{PROP:Tip},'Check Write Ok')                 
                   CYCLE 
               END
               IF ~IDEClosed THEN 
                   ?IDEClosed{PROP:Background}=COLOR:Yellow
                   SELECT(?IDEClosed) 
                   Message(?IDEClosed{PROP:Tip},'Check IDE Closed')                 
                   CYCLE 
               END 
               
            END
            ?WriteOK{PROP:Background}=COLOR:None 
            ?IDEClosed{PROP:Background}=COLOR:None 
           ! ?WriteOK{PROP:Skip}=1
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
        OF ?ConfigDirTxtReloadBtn  ; DOO.LoadConfigDirTxt(False) ; DISPLAY
        OF ?ConfigDirTxtNotepadBtn ; RUN('Notepad ' & ConfigDir_Txt_FN)  
        OF ?ConfigDirTxtExploreBtn ; ExplorerOpen(LongPath()) 
        OF ?ConfigDirTxtReParseBtn ; DOO.LoadConfigDirTxt(True)  ; DISPLAY 
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
            CYCLE  !If AppData has "11.0 - Copy" skip that
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
         
    END !LOOP Delete files 
 
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
    END !LOOP Delete files 
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
                '<13,10>; Put "JUST" on first line to check "Just This". Put "END" on a line to stop loading. ' & | 
                '' !'<13,10>;  ' & |  
            
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
    END
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
    LOOP QNdx = 1 TO RECORDS(DirQ)   !--Keep directories ##.## skip others
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
            COMPILE('!**END ST Clean**', _Have_StringTheory_)
            CleanOk = ST_CleanClaPropXmlFile(CleanQ.PathBS & ClarionProperties_xml , pQuery, |
                                          ClnStats, CleanQ.CleanMsg)  
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
DOO.WindowInitPretty PROCEDURE() 
Fld LONG  
    CODE 
    !0{PROP:MinWidth}=0{PROP:Width}  ; 0{PROP:MinHeight}=0{PROP:Height} * .60 
    WinResize.Init(AppStrategy:NoResize,Resize:SetMinSize)
    ?SHEET1{PROP:NoTheme}=True
    ?SHEET1{PROP:TabSheetStyle}=1   
    ?List:CleanQ{PROPSTYLE:FontName,1}='Consolas'
    !?List:AppDataSvQ{PROPSTYLE:FontName,1}='Consolas'         too big after resize
    LOOP
        Fld=0{PROP:NextField,Fld} ; IF ~Fld THEN BREAK.
        CASE Fld{PROP:Type}
        OF CREATE:list  
            !With Manifest and Windows 10 the header is White like Data so Color it 
           !?List:CleanQ{PROP:NoTheme}=True  !Remove theme for old look ... or better below?
         !  Fld{PROPLIST:DefHdrBackColor} = COLOR:GradientInactiveCaption  !too Blue?
         !  Fld{PROPLIST:DefHdrTextColor} = COLOR:INACTIVECAPTIONTEXT
           Fld{PROPLIST:DefHdrBackColor} = COLOR:BTNFACE                  !Too Gray
          ! Fld{PROPLIST:DefHdrBackColor} = COLOR:3DLight                   !Just right :)
           Fld{PROPLIST:DefHdrTextColor} = COLOR:BTNTEXT 
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
              Fld{PROP:GradientFromColor}=COLOR:GradientInactiveCaption  !Light color
              Fld{PROP:GradientToColor}  =COLOR:InactiveCaption          !to Dark color
              (Fld+1){PROP:FontColor}=COLOR:InactiveCaptionText          !Assume box before string
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

    IF _Have_StringTheory_ THEN 
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
    IF _Have_StringTheory_ THEN 
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
  COMPILE('!**END ST Clean**', _Have_StringTheory_)
     INCLUDE('FindCleanCwIDE_ST.clw','Functions') !String Theory
  !end COMPILE('!**END ST Clean**'
!ST_CleanClaPropXmlFile Procedure(STRING ClaPropFullName, BYTE pQuery, *ClnStatsType ClnStats, *STRING OutMsg )!,BOOL
!--st--^^--st--^^--st--^^--st--^^--st--^^--st--^^--st--^^--st--^^--st--^^
CleanClaPropXmlFile Procedure(STRING pClaPropXmlFN, BYTE pQuery, *ClnStatsType ClnStats, *STRING OutMsg )!,BOOL
DidSaveOk   BOOL
FindCln     CbFindCleanClass
CleanCnt    LONG
Pos1        LONG
Pos2        LONG
B4CleanName STRING(260)
    CODE
    B4CleanName=CLIP(pClaPropXmlFN) & '.b4clean'
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
    COPY(B4CleanName,CLIP(B4CleanName) &'2')   !Save 2nd backup .b4clean2 
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

!#################################################################################### 
 
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
        Message('Folder does not exist:||' & FolderName, 'ExplorerOpen') . 
    RUN('Explorer.exe /e,"' & CLIP(FolderName) &'"')
    RETURN  
!=====================================
GetFileDateTimeSize PROCEDURE(STRING pFileName,<*LONG outDate>,<*LONG outTime>,<*LONG outSize>)!,BOOL
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
     '<13,10>You can test by checking the "Write to Test Shrink" box so the original file is ' &|
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
    