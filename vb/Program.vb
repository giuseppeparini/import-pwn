Imports System
Imports System.Text
Imports System.Net.Sockets
Imports System.IO
Imports System.Text.RegularExpressions

Module Program
  Sub Main(args As String())
    Dim client As New System.Net.Sockets.TcpClient()
    client.Connect("piecewise.challs.cyberchallenge.it", 9110)
    Dim reader As New StreamReader(client.GetStream, Encoding.UTF8)
    Dim numberMatcher As New Regex("^Please send me the number (\d+) as a (32|64)-bit (big|little)-endian ", RegexOptions.Compiled)
    Dim nl(1) as Byte
    nl(0) = 10
    Dim line As String

    line = reader.ReadLine

    Do While (Not line Is Nothing)
      Console.WriteLine(line)

      Dim match As Match = numberMatcher.Match(line)
      If match.Success Then
        Dim msg() As byte = ParseNum(match.Groups(1).Value, UInt32.Parse(match.Groups(2).Value))
        Dim endianness As string = match.Groups(3).Value

        If If(BitConverter.IsLittleEndian,"little","big") <> match.Groups(3).Value Then
          Array.Reverse(msg)
        End If

        client.GetStream.Write(msg, 0, msg.Length)
      Else If line.StartsWith("Please send me an empty line") then
        client.GetStream.Write(nl, 0, 1)
        client.GetStream.Flush
      Else
        Debug.Assert(False, "unreachable")
      End If

      line = reader.ReadLine
      Console.WriteLine(line)
      line = reader.ReadLine
    Loop
  End Sub

  Function ParseNum(t As string, size As System.UInt32) As byte()
    If size = 32 Then
      Dim n As System.UInt32 = UInt32.Parse(t)
      Return BitConverter.GetBytes(n)
    Else If size = 64 Then
      Dim n As System.UInt64 = UInt64.Parse(t)
      Return BitConverter.GetBytes(n)
    Else
      Debug.Assert(False, "unreachable")
      Return {} ' meme return so VB compiler STFU
    End If
  End Function
End Module
