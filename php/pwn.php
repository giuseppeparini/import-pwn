<?php

$socket = socket_create(AF_INET, SOCK_STREAM, SOL_TCP);

if (!$socket) {
    die("unreachable");
}

if (!socket_connect($socket, 'piecewise.challs.cyberchallenge.it', 9110)) {
    die(socket_strerror(socket_last_error()));
}

while (!empty($line = @socket_read($socket, 666, PHP_NORMAL_READ))) {
    echo $line;
    if (preg_match('/^Please send me the number (\\d+) as a (32|64)-bit (big|little)-endian /', $line, $matches)) {
        if (!socket_write($socket, pack(
            $matches[2] == 32 ? ($matches[3] == 'big' ? 'N' : 'V') : ($matches[3] == 'big' ? 'J' : 'P'),
            $matches[1]
        ))) {
            die(socket_strerror(socket_last_error()));
        }
    } elseif (str_starts_with($line, 'Please send me an empty line')) {
        if (!socket_write($socket, "\n")) {
            die(socket_strerror(socket_last_error()));
        }
    } else {
        die("unreachable");
    }

    if (empty($line = socket_read($socket, 666, PHP_NORMAL_READ))) {
        die(socket_strerror(socket_last_error()));
    }
    echo $line;
}
