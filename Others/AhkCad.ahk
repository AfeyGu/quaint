!SPACE::  Winset, Alwaysontop, , A

#`::Run taskmgr

Ins::`
`::0

<!t::send %A_YYYY%-%A_MM%-%A_DD%
>!t::send %A_YYYY%-%A_MM%-%A_DD% %A_Hour%.%A_Min%.%A_Sec%

<!r:: 
random, rr, 1, 100000000
send %rr%

#IfWinActive , ahk_exe acad.exe
	XButton1::send p
	F1::F8

#IfWinActive , ahk_exe ConEmu64.exe
	XButton1::send )

;;CapsLock & XButton1::WinMinimize,A 

