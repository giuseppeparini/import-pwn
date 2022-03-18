using System.Net.Sockets;
using System.Text;
using System.Text.RegularExpressions;

namespace Csharp
{
    class Program
    {
        const string HOST = "piecewise.challs.cyberchallenge.it";
        const int PORT = 9110;

        public static void Main(string[] args)
        {
            var client = new TcpClient();
            client.Connect(HOST, PORT);

            var matcher = new Regex(@"^Please send me the number (\d+) as a (32|64)-bit (big|little)-endian ", RegexOptions.Compiled);

            var streamReader = new StreamReader(client.GetStream(), Encoding.UTF8);
            string? line;
            while ((line = streamReader.ReadLine()) != null)
            {
                Console.WriteLine(line);

                var match = matcher.Match(line);
                if (match.Success)
                {
                    var message = Parser(match.Groups[1].Value, uint.Parse(match.Groups[2].Value));
                    string ness = match.Groups[3].Value;

                    if ((BitConverter.IsLittleEndian ? "little" : "big") != ness)
                    {
                        Array.Reverse(message);
                    }

                    client.GetStream().Write(message, 0, message.Length);
                    client.GetStream().Flush();
                }
                else if (line.StartsWith("Please send me an empty line"))
                {
                    client.GetStream().Write(new byte[] { 10 }, 0, 1);
                    client.GetStream().Flush();
                }
                else
                {
                    throw new Exception("unreachable");
                }

                if ((line = streamReader.ReadLine()) != null)
                {
                    Console.WriteLine(line);
                }
            }
        }

        public static byte[] Parser(string text, uint size)
        {
            if (size == 32L)
            {
                uint n = uint.Parse(text);
                return BitConverter.GetBytes(n);
            }
            else if (size == 64L)
            {
                ulong n = ulong.Parse(text);
                return BitConverter.GetBytes(n);
            }
            else
            {
                throw new Exception("unreachable");
            }
        }
    }
}
