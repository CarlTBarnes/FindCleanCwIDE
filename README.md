# FindCleanCwIDE 

ClarionProperties.XML contains the Find Dialog search history in /<FindPatterns value=""/>. This grows very large and needs to be shrunk. More to come...

![tab cln](images/tabClean.png)

![tab App](images/tabAppData.png)

![tab BIN](images/tabBIN.png)

![tab Cfg](images/tabConfig.png)

## Big Bang Class for StringTheory

This contains an example of using my Big Bang class to show StringTheory call results in a Window. This lets you visually confirm your code is getting the desired results.

The FindCleanCwIDE_ST.clw module contains all the ST code. Look in there for calls to BangCls. To see them at all the BangView variable must be (1). You can stop the display of the windows by checking "No Bang".

In the below screen capture you see the call to st.FindMatchPosition() then the call to BangCls.StringView() will display the passed slice in a Window.
 There are many other captures of the process. At the bottom you can see List view of the string split lines queue.

  st.FindMatchPosition(' value *= *".*"',lStart,lEnd)
  IF lStart <= 0 OR lStart > lEnd THEN RETURN false.
    IF BangView THEN                             |
       BangCls.StringView( 

![tab ](images/BigBang.png)

https://github.com/CarlTBarnes/StringTheory-LoadFile-Split-Viewer

## Window Preview Class

Window Preview class is acessed the "secret" flat button in top left corner, just hover.
 It opens a widow showing all fields on the Window.
 You can examine PROP's of Controls, Window and SYSTEM. You can resize and restyle controls.
 For a LIST you can view the Format or all the PROPLIST for one column. You can view Styles. You can see the From Queue design and records. 
 You can "Re-Format" the LIST i.e. change the FORMAT string, e.g. to resize columns.
 Just click the secret top-left button and the look at everything.
 All this from 3 lines of code to include, declare and init my WndPreview class. 

![Prv](images/wndPreview.png)
![Prv](images/wndPrvResize.png)
![Prv](images/wndPrvList1.png)
![Prv](images/wndPrvList2.png)
![Prv](images/wndPrvListFromQ1.png)
![Prv](images/wndPrvListReformat.png)
![Prv](images/wndPrvListReformat.png)

https://github.com/CarlTBarnes/WindowPreview
