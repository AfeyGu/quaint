; Ctrl:^, Shift:+, Alt:!, Win:#


!SPACE::  Winset, Alwaysontop, , A

#`::Run taskmgr

Ins::`
`::0


<!r:: 
	random, rr, 1, 100000000
	send %rr%
Return
<!t::send %A_YYYY%-%A_MM%-%A_DD%
>!t::send %A_YYYY%-%A_MM%-%A_DD% %A_Hour%.%A_Min%.%A_Sec%
<!2::send dg1231201@163.com

#IfWinActive , ahk_exe acad.exe
	XButton1::send p
	F1::F8

#IfWinActive , ahk_exe ConEmu64.exe
	XButton1::send )
return

;;; 长按组合键，短按还是切换大小写
CapsLock::
KeyWait, CapsLock     
if (A_ThisHotkey="CapsLock")
        {
        SetCapsLockState, % GetKeyState("CapsLock","T") ? "Off" : "On"    
        }
Return

#if GetKeyState("Capslock", "P")
SPACE::SendInput,{Enter}
a::SendInput, {Home}
d::SendInput, {End}