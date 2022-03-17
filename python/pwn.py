#!/usr/bin/env python

import socket
import re

HOST = "piecewise.challs.cyberchallenge.it"
PORT = 9110

def readline(socket):
    buffer = socket.recv(4096)
    buffering = True
    while buffering:
        if b"\n" in buffer:
            (line, buffer) = buffer.split(b"\n", 1)
            yield line.decode("utf-8")
        else:
            more = socket.recv(4096)
            if not more:
                buffering = False
            else:
                buffer += more
    if buffer:
        yield buffer.decode("utf-8")

NUMBER_MATCHER = re.compile("^Please send me the number (\\d+) as a (32|64)-bit (big|little)-endian ")

with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
    s.connect((HOST, PORT))
    reader = readline(s)

    for line in reader:
        print(line)
        matches = NUMBER_MATCHER.match(line)
        if matches:
            number = int(matches[1])
            bits = int(matches[2])
            endianness = matches[3]
            assert endianness == "big" or endianness == "little", "unreachable"
            s.send(number.to_bytes(int(bits / 8), endianness))
        elif line.startswith("Please send me an empty line"):
            s.send(b"\n")
        else:
            assert False, "unreachable"

        print(next(reader))

