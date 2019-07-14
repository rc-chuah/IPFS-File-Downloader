DataSection
  gatewaysa:
    IncludeBinary "gateways"
  gatewaysb:
EndDataSection

  UseSHA2Fingerprint()
  UseMD5Fingerprint()

  Global NewList GateWays.s ()
  Global link$
  Global *mem
  Global ok
  Global kill
  
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
; The downloader will look for a 'preferences' file in the same directory by default. To use a different preference file simply pass the filename as a switch.
  
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  If CountProgramParameters() > 0
    
    preferences$ = ProgramParameter(0)
    
  Else
    
    preferences$ = GetPathPart(ProgramFilename())+"preferences"
      
  EndIf
  
  
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
OpenPreferences(preferences$)
  
  Global download$ = ReadPreferenceString("ipfs","")                                        ; <----- The IPFS hash
  
  Global md5$ = ReadPreferenceString("md5","")                                              ; <----- The MD5 hash
  
  Global sha256$ = ReadPreferenceString("sha256","")                                        ; <----- The SHA256 hash
    
  Global size = Val( ReadPreferenceString("size","") )                                      ; <----- Filesize in bytes
  
  Global title$ = ReadPreferenceString("title","")                                          ; <----- The window title 
  
  Global saveas$ = ReadPreferenceString("saveas","")                                        ; <----- Default file name used in the Savefile dialog 
  
  Global saveastitle$ = ReadPreferenceString("saveastitle","")                              ; <----- The title used in the Savefile dialog 
    
  Global filepattern$ = ReadPreferenceString("pattern","")                                  ; <----- Filepattern used in the Savefile dialog
  
  Global width = Val( ReadPreferenceString("width","300") )                                 ; <----- Window width (Default 300)
  
  Global height = Val( ReadPreferenceString("height","65") )                                ; <----- Window height (Default 65)
  
  Global invisible = Val( ReadPreferenceString("invisible","0") )                           ; <----- Set this to 1 to hide the main window, savefile dialog and messages (Default 0)
  
  
PreferenceGroup("msgbox")

  
  Global done$ = ReadPreferenceString("done","Done.")                                       ; <----- Message shown when the download completes
  
  Global error$ = ReadPreferenceString("error","Error")                                     ; <----- Messagebox title when an error occurs
  
  Global dlfail$ = ReadPreferenceString("errdl","Download failed")                          ; <----- Message shown when the download fails
  
  Global errsave$ = ReadPreferenceString("errsave","Unable to save the file")               ; <----- File write error message
  
  Global erric$ = ReadPreferenceString("erric","Integrity check failed")                    ; <----- Integrity check error message
  
  Global errnetw$ = ReadPreferenceString("errnetw","Couldn't initiliaze the network.")      ; <----- Network initialization error message
  

ClosePreferences()

;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

; Error codes

;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

; 0 = Success
; 1 = Download failed
; 2 = Network initialization error
; 3 = Integrity check error
; 4 = File write error

;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Macro DownloadProgress()
  
  p = HTTPProgress(down)
  
Select p
    
Case #PB_HTTP_Failed
  
  Failed()
   
Case #PB_HTTP_Success
  
  CloseWindow(0)
  *mem = FinishHTTP(down)
  
  IntegrityCheck()
  
  Break
  
Default
  
  SetGadgetState(1,p)
  
EndSelect
  
EndMacro    
Macro Timer()
  
If EventTimer()=1
  
If kill=1
  Failed()
EndIf
  
If ok=1
  
  RemoveWindowTimer(0,1)
  
  SetGadgetAttribute(1,#PB_ProgressBar_Maximum,size)
  
  down = ReceiveHTTPMemory( link$ , #PB_HTTP_Asynchronous )
  
  AddWindowTimer(0,2,5)
  
Else
  
If IsThread(t)
  KillThread(t)
EndIf

  NextElement(GateWays())
  t=CreateThread(@GetHeader(),UTF8(GateWays()))
    
EndIf 

ElseIf EventTimer()=2
  
  DownloadProgress()
  
EndIf
  
EndMacro
Macro IPFS()
  
  ; Try to download from 'http://127.0.0.1:8080/ipfs/' first before using public gateways - This should work if the IPFS daemon is running
  
  i$ = "http://127.0.0.1:8080/ipfs/"
  
  *ipfs = ReceiveHTTPMemory(i$) 
  
If *ipfs

  FreeMemory(*ipfs)
  
  AddElement( GateWays() )
  GateWays() = i$
  
EndIf
  
EndMacro

Procedure MsgBox(mbtitle.s,mbtext.s, f = #PB_MessageRequester_Error)
  
If invisible=0
  
  MessageRequester(mbtitle,mbtext,f)
  
EndIf
  
EndProcedure
Procedure Failed()
  
If IsWindow(0)
  CloseWindow(0)
EndIf

  MsgBox(error$,dlfail$)
   
  End 1
  
EndProcedure
Procedure Save()
  
If invisible
  
  s$ = saveas$
  
Else
  
  s$=SaveFileRequester(saveastitle$, saveas$, filepattern$, 0)
  
EndIf
  
If s$
  
If CreateFile(0,s$)
  
  WriteData(0,*mem,size)
  
  CloseFile(0)
  
  MsgBox( "" , done$, #PB_MessageRequester_Ok )
  
  End 0
  
Else
  
If invisible
  
  End 4
  
EndIf
  
  MsgBox(err$,errsave$)
  
  Save()
  
EndIf

Else
  
  End 4
  
EndIf

  ProcedureReturn

EndProcedure
Procedure IntegrityCheck()
  
If Fingerprint(*mem, size, #PB_Cipher_SHA2,256) = sha256$ And Fingerprint(*mem, size, #PB_Cipher_MD5) = md5$
  
  Save()

Else
  
  MsgBox(error$, erric$)
  
  End 3
  
EndIf
  
EndProcedure
Procedure GetSize(l$)
  
Static fail
  
If fail=>ListSize(GateWays())
  
  kill=1
  ProcedureReturn 0
        
EndIf

  h$ = GetHTTPHeader(l$+download$)
  cl$="Content-Length:"
      
  status$ = StringField( Mid( h$,FindString(h$,"http",0,#PB_String_NoCase ) ) , 1, Chr(10) )
  
If FindString(status$, "200", 0, #PB_String_NoCase) And FindString(status$, "ok", 0, #PB_String_NoCase)
    
  pos = FindString(h$,cl$)
    
If pos
  
  h$ = StringField( Mid(h$,pos) , 1, Chr(10) )
  h$ = Trim( StringField( h$ , 2, ":" ) )
  
  v = Val(h$)
  
EndIf

Else
  
  fail+1
    
EndIf

  ProcedureReturn v
  
EndProcedure
Procedure GetHeader(*a)
    
If GetSize(PeekS(*a,-1,#PB_UTF8)) = size
  
  link$ = PeekS(*a,-1,#PB_UTF8) +download$
  ok=1
    
EndIf
  
  ProcedureReturn

EndProcedure  
Procedure Download()
  
  FirstElement(GateWays())
    
If OpenWindow(0, 0, 0, width, height, title$, #PB_Window_MinimizeGadget|#PB_Window_SystemMenu | #PB_Window_ScreenCentered | #PB_Window_Invisible)
  
If invisible=0
  
  HideWindow(0,0)
  
EndIf
  
  AddWindowTimer(0,1,1000)
  
CompilerIf #PB_Compiler_OS = #PB_OS_Windows
  
  ProgressBarGadget(1,  20, WindowHeight(0) / 2 - 10, WindowWidth(0)-40,  20, 0, 1)
  
CompilerElse
  
  ProgressBarGadget(1,  20, WindowHeight(0) / 2 - 12, WindowWidth(0)-40,  20, 0, 1)
  
CompilerEndIf
  
Repeat
  
Select WaitWindowEvent()
    
Case #PB_Event_Timer
  
  Timer()

Case #PB_Event_CloseWindow
  
  End
  
EndSelect
    
ForEver      
    
EndIf

  ProcedureReturn

EndProcedure
Procedure Main()

  IPFS()

  gw$=PeekS(?gatewaysa,?gatewaysb-?gatewaysa,#PB_UTF8)
  
  c=CountString(gw$,Chr(10))
  
For a=1 To c
  
  AddElement( GateWays() )
  GateWays() = StringField(gw$,a,Chr(10))
    
Next

  Download()
  
EndProcedure

OnErrorCall(@Failed())

If InitNetwork()
  
  Main()
  
Else
  
  MsgBox(error$,errnetw$)
  
  End 2
  
EndIf
; IDE Options = PureBasic 5.70 LTS (Linux - x64)
; Folding = AQ-
; EnableThread
; EnableXP
; Executable = downloader
; CompileSourceDirectory
