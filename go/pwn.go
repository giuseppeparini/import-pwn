package main

import (
	"bufio"
	"encoding/binary"
	"fmt"
	"io"
	"net"
	"regexp"
	"strconv"
	"strings"
)

func main() {
	numberRegex := regexp.MustCompile("^Please send me the number (\\d+) as a (32|64)-bit (big|little)-endian ")
	sock, err := net.Dial("tcp", "piecewise.challs.cyberchallenge.it:9110")

	if err != nil {
		panic(err)
	}

	defer sock.Close()

	reader := bufio.NewReader(sock)

	var line string

	for {
		if line, err = reader.ReadString('\n'); err != nil {
			if err == io.EOF {
				break
			} else {
				panic(err)
			}
		}

		fmt.Print(line)

		var buf []byte
		if match := numberRegex.FindStringSubmatch(line); match != nil {
			var bits int
			if b, err := strconv.ParseInt(match[2], 10, 8); err == nil {
				bits = int(b)
			} else {
				panic(err)
			}

			if n, err := strconv.ParseUint(match[1], 10, bits); err == nil {
				var endian binary.ByteOrder
				if match[3] == "big" {
					endian = binary.BigEndian
				} else if match[3] == "little" {
					endian = binary.LittleEndian
				} else {
					panic("unreachable")
				}

				if bits == 32 {
					buf = make([]byte, 4)
					endian.PutUint32(buf, uint32(n))
				} else if bits == 64 {
					buf = make([]byte, 8)
					endian.PutUint64(buf, n)
				}
			} else {
				panic(err)
			}
		} else if strings.HasPrefix(line, "Please send me an empty line ") {
			buf = make([]byte, 1)
			buf[0] = '\n'
		} else {
			panic("unreachable")
		}

		if _, err = sock.Write(buf); err != nil {
			panic(err)
		}

		if line, err = reader.ReadString('\n'); err != nil && err != io.EOF {
			panic(err)
		}
		fmt.Print(line)
	}
}
