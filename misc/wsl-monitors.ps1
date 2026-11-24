Add-Type -AssemblyName System.Windows.Forms
Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;
public class MonDD {
  [StructLayout(LayoutKind.Sequential, CharSet=CharSet.Ansi)]
  public struct DEV {
    public int cb;
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst=32)] public string Name;
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst=128)] public string Str;
    public int Flags;
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst=128)] public string Id;
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst=128)] public string Key;
  }
  [DllImport("user32.dll", CharSet=CharSet.Ansi)]
  public static extern bool EnumDisplayDevices(string dev, uint n, ref DEV d, uint flags);
}
'@
$w = Get-CimInstance -Namespace root/wmi -ClassName WmiMonitorBasicDisplayParams
foreach ($s in [System.Windows.Forms.Screen]::AllScreens) {
  $d = New-Object MonDD+DEV
  $d.cb = [Runtime.InteropServices.Marshal]::SizeOf($d)
  [void][MonDD]::EnumDisplayDevices($s.DeviceName, 0, [ref]$d, 0)
  $c = $d.Id.Split([char]92)[1]
  $m = $w | Where-Object { $_.InstanceName.Split([char]92)[1] -eq $c } | Select-Object -First 1
  if ($m) { '{0} {1} {2} {3} {4}' -f $s.Bounds.X, $s.Bounds.Y, $s.Bounds.Width, $s.Bounds.Height, ($m.MaxHorizontalImageSize * 10) }
}

