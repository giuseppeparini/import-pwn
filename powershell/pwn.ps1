$client = [System.Net.Sockets.TcpClient]::new()
$client.Connect("piecewise.challs.cyberchallenge.it", 9110)
$stream = $client.GetStream()
$reader = [System.IO.StreamReader]::new($stream, [System.Text.Encoding]::UTF8)
$numberMatcher = [System.Text.RegularExpressions.Regex]::new("^Please send me the number (\d+) as a (32|64)-bit (big|little)-endian ", [System.Text.RegularExpressions.RegexOptions]::Compiled)
[byte[]]$nl = 10

while (($line = $reader.ReadLine()) -ne $null) {
  echo $line

  $match = $numberMatcher.Match($line)
  if ($match.Success) {
    $msg = [System.BitConverter]::GetBytes(@([UInt64],[UInt32])[$match.Groups[2].Value -eq "32"]::Parse($match.Groups[1].Value))
    if (@("big","little")[[System.BitConverter]::IsLittleEndian] -ne $match.Groups[3].Value) {
      [Array]::Reverse($msg)
    }
    $stream.Write($msg, 0, $msg.Length)
    $stream.Flush()
  } else {
    $stream.Write($nl, 0, 1)
    $stream.Flush()
  }

  echo $reader.ReadLine()
}
