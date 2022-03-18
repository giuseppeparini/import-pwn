import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class Pwn {
  private static byte[] toBuffer(long n) {
    ByteBuffer buffer = ByteBuffer.allocate(Long.BYTES);
    buffer.putLong(n);
    return buffer.array();
  }

  private static byte[] toBuffer(int n) {
    ByteBuffer buffer = ByteBuffer.allocate(Integer.BYTES);
    buffer.putInt(n);
    return buffer.array();
  }

  private static void arrayReverse(byte[] array) {
    for (int i = 0; i < array.length / 2; ++i) {
      byte tmp = array[i];
      array[i] = array[array.length - i - 1];
      array[array.length - i - 1] = tmp;
    }
  }

  public static void main(String args[]) throws IOException {
    Pattern numberPattern = Pattern
        .compile("^Please send me the number (\\d+) as a (32|64)-bit (big|little)-endian .*$");

    try (Socket socket = new Socket("piecewise.challs.cyberchallenge.it", 9110)) {
      BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
      String line;
      while ((line = reader.readLine()) != null) {
        System.out.println(line);

        byte[] res;
        Matcher matcher = numberPattern.matcher(line);

        if (matcher.matches()) {
          switch (matcher.group(2)) {
            case "32":
              res = toBuffer(Integer.parseUnsignedInt(matcher.group(1)));
              break;
            case "64":
              res = toBuffer(Long.parseUnsignedLong(matcher.group(1)));
              break;
            default:
              throw new AssertionError("unreachable");
          }

          // JVM forces big endian
          if (matcher.group(3).equals("little")) {
            arrayReverse(res);
          }
        } else if (line.startsWith("Please send me an empty line ")) {
          res = new byte[] { '\n' };
        } else {
          throw new AssertionError("unreachable");
        }

        socket.getOutputStream().write(res);

        System.out.println(reader.readLine());
      }
    }
  }
}
