Add-Type -TypeDefinition @'
using System;
using System.Text;
using System.Runtime.InteropServices;
public class My_WinActivate {
  public delegate bool EnumProc(IntPtr hWnd, IntPtr lParam);
  [DllImport("user32.dll")] public static extern bool EnumWindows(EnumProc proc, IntPtr lParam);
  [DllImport("user32.dll")] public static extern int GetWindowText(IntPtr hWnd, StringBuilder sb, int n);
  [DllImport("user32.dll")] public static extern bool IsWindowVisible(IntPtr hWnd);
  [DllImport("user32.dll")] public static extern bool SetForegroundWindow(IntPtr hWnd);
  [DllImport("user32.dll")] public static extern bool BringWindowToTop(IntPtr hWnd);
}
'@
# $target is prepended by the caller
$cb = {
  param($h, $l)
  if ([My_WinActivate]::IsWindowVisible($h)) {
    $sb = New-Object System.Text.StringBuilder 256
    [void][My_WinActivate]::GetWindowText($h, $sb, 256)
    $t = $sb.ToString()
    if ($t -eq $target -or $t -like "$target (*") {
      [void][My_WinActivate]::BringWindowToTop($h)
      [void][My_WinActivate]::SetForegroundWindow($h)
    }
  }
  return $true
}
[void][My_WinActivate]::EnumWindows($cb, [IntPtr]::Zero)

