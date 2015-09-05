' emx.vbs : starts a emacsclient with no command prompt

Dim emxCmd

Set WshShell = Createobject("WScript.Shell")
Set Args = WScript.Arguments
emxCmd = """emacsclient"" ""-cqa"" ""emacs"""
For index = 0 To (Args.Count - 1)
emxCmd = emxCmd & Chr(32) & Chr(34) & Args(index) & Chr(34)
Next
WshShell.Run emxCmd, 0
Set WshShell = Nothing
