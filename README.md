# FindCleanCwIDE 

ClarionProperties.XML contains the Find Dialog search history in `<FindPatterns value=""/>`.
 This can grow very large and needs to be shrunk or the dialog gets very slow.
 Read the About tab for more info. Also see ClarionHub thread: [FIND popup dialog window appears slowly in IDE](https://clarionhub.com/t/find-popup-dialog-window-appears-slowly-in-ide/3764)

Below is the main window where you set your Max and Min counts then press Query to View the count of Find and Replace patterns
 in the XML file.
 If it looks good press the Shrink button. The Clarion IDE must be closed.
 More to come...


![tab cln](images/tabClean.png)

![tab App](images/tabAppData.png)

![tab BIN](images/tabBIN.png)

The Config tab is where you can list folders you use with the  Clarion.exe /ConfigDir= switch. 
 In the below example I am using it to load test files from other folders.
![tab Cfg](images/tabConfig.png)

## Big Bang Class for StringTheory

This contains an example of using my Big Bang class to show StringTheory call result string or queue in a Window. This lets you visually confirm your code is getting the desired results.

The FindCleanCwIDE_ST.clw module contains all the ST code. Look in there for calls to BangCls.
 To see them at all the BangView variable must be (1). You can stop the display of the windows by checking "No Bang".

In the below screen capture you see the call to st.FindMatchPosition() then
 the call to BangCls.StringView() will display the slice in a Window.
 There are many other screen captures of the steps of this process.
 E.g. at the bottom you can see a List view of the split lines queue.

  st.FindMatchPosition(' value *= *".*"',lStart,lEnd)
  IF lStart <= 0 OR lStart > lEnd THEN RETURN false.
    IF BangView THEN                             |
       BangCls.StringView( 

![tab ](images/BigBang.png)

https://github.com/CarlTBarnes/StringTheory-LoadFile-Split-Viewer


## Window Preview Class

To show the many features of my Window Preview class I added it to Find Clean.
 It's accessed with the "secret" flat button in top left corner, just hover up there andit will pop up.
 It opens a new widow showing all controls on the Window in a List.
 You can examine PROP's of Controls, Window and SYSTEM. You can resize and restyle controls.
 For a LIST you can view the Format(), all the PROPLIST for one column and the Styles. You can see the From Queue design and records. 
 You can "Re-Format" the LIST i.e. change the FORMAT string, e.g. to resize columns.
 
Don't think too hard about it, just click the secret top-left button and then look at everything, click every button and double on every row.
 All this from 3 lines of code to Include, Declare and Init() my WndPreview class. 

![Prv](images/wndPreview.png)

Below is a capture of the "Resizer". It also allows changing many other properties like Font and Colors.
 Below I'm resizing the List by setting the number of Items so it has no partial rows. I also bumped up the line height to 9.
  So 5 items and 9 line height works out to a List height of 118. This is done on the live window so is perfect. 
 
![Prv](images/wndPrvResize.png)

The LIST button shows the Format() split into lines so you can see the many attributes in columns.
 E.g. it can be handy to see all the pictures in one column.

![Prv](images/wndPrvList1.png)

Double click on a column above and it shows all the PROPLIST for that single column. There is also a Styles tab, this column uses Style Z(1) that only defined a Consolas font.

![Prv](images/wndPrvList2.png)

The FromQ tab shows the Declaration the the Queue feeding the List. It shows the values of the current record.
 It shows Queue fields not visible in the List.

![Prv](images/wndPrvListFromQ1.png)

The View Queue button shows the records in the Queue. You delete or add records. You can copy them to the clipboard.
 This lets you see Queue fields not visible in the Lists, e.g. the color or style Longs.

![Prv](images/wndPrvListFromQ2.png)

The Re-Format lets you change the columns in a List and see it live. This can be useful for sizing columns to actual data.
 Below I have changed the picture from T3 to T1 which allowed reducing the width from 34 to 23. I also checked Underline.

![Prv](images/wndPrvListReformat.png)

https://github.com/CarlTBarnes/WindowPreview
